/*
 * Copyright (C) 2007 by
 * 
 * 	Xuan-Hieu Phan
 *	hieuxuan@ecei.tohoku.ac.jp or pxhieu@gmail.com
 * 	Graduate School of Information Sciences
 * 	Tohoku University
 *
 * GibbsLDA++ is a free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published
 * by the Free Software Foundation; either version 2 of the License,
 * or (at your option) any later version.
 *
 * GibbsLDA++ is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with GibbsLDA++; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */

/* 
 * References:
 * + The Java code of Gregor Heinrich (gregor@arbylon.net)
 *   http://www.arbylon.net/projects/LdaGibbsSampler.java
 * + "Parameter estimation for text analysis" by Gregor Heinrich
 *   http://www.arbylon.net/publications/text-est.pdf
 */

#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include "constants.h"
//#include "strtokenizer.h"
//#include "utils.h"
//#include "dataset.h"
#include "model.h"

using namespace std;

model::~model() {
    if (p) {
	    delete p;
    }

    if (ptrndata) {
	    delete ptrndata;
    }
    
    if (pnewdata) {
	    delete pnewdata;
    }

    if (z) {
    	for (int m = 0; m < M; m++) {
    	    if (z[m]) {
        		delete z[m];
    	    }
    	}
    }
    
    if (nw) {
    	for (int w = 0; w < V; w++) {
    	    if (nw[w]) {
    		    delete nw[w];
    	    }
    	}
    }

    if (nd) {
    	for (int m = 0; m < M; m++) {
    	    if (nd[m]) {
    		    delete nd[m];
    	    }
    	}
    } 
    
    if (nwsum) {
	    delete nwsum;
    }   
    
    if (ndsum) {
	    delete ndsum;
    }
    
    if (theta) {
    	for (int m = 0; m < M; m++) {
    	    if (theta[m]) {
        		delete theta[m];
    	    }
    	}
    }
    
    if (phi) {
    	for (int k = 0; k < K; k++) {
    	    if (phi[k]) {
    		    delete phi[k];
    	    }
    	}
    }

    // only for inference
    if (newz) {
    	for (int m = 0; m < newM; m++) {
    	    if (newz[m]) {
    		    delete newz[m];
    	    }
    	}
    }
    
    if (newnw) {
    	for (int w = 0; w < newV; w++) {
    	    if (newnw[w]) {
    		    delete newnw[w];
    	    }
    	}
    }

    if (newnd) {
    	for (int m = 0; m < newM; m++) {
    	    if (newnd[m]) {
    		    delete newnd[m];
    	    }
    	}
    } 
    
    if (newnwsum) {
	    delete newnwsum;
    }   
    
    if (newndsum) {
	    delete newndsum;
    }
    
    if (newtheta) {
    	for (int m = 0; m < newM; m++) {
    	    if (newtheta[m]) {
    		    delete newtheta[m];
    	    }
    	}
    }
    
    if (newphi) {
    	for (int k = 0; k < K; k++) {
    	    if (newphi[k]) {
    		    delete newphi[k];
    	    }
    	}
    }
}

void model::set_default_values() {
    wordmapfile = "wordmap.txt";
    trainlogfile = "trainlog.txt";
    tassign_suffix = ".tassign";
    theta_suffix = ".theta";
    phi_suffix = ".phi";
    others_suffix = ".others";
    twords_suffix = ".twords";
    
    dir = "./";
    dfile = "trndocs.dat";
    model_name = "model-final";    
    model_status = MODEL_STATUS_UNKNOWN;
    
    ptrndata = NULL;
    pnewdata = NULL;
    
    M = 0;
    V = 0;
    K = 100;
    alpha = 50.0 / K;
    beta = 0.1;
    niters = 2000;
    liter = 0;
    savestep = 200;    
    twords = 0;
    withrawstrs = 0;
    
    p = NULL;
    z = NULL;
    nw = NULL;
    nd = NULL;
    nwsum = NULL;
    ndsum = NULL;
    theta = NULL;
    phi = NULL;
    
    newM = 0;
    newV = 0;
    newz = NULL;
    newnw = NULL;
    newnd = NULL;
    newnwsum = NULL;
    newndsum = NULL;
    newtheta = NULL;
    newphi = NULL;
}

void model::set_data(arma::sp_mat &mt) {
    data = mt;
    M = (int)mt.n_cols; 
    V = (int)mt.n_rows;
}

// NOTE added for Rcpp
arma::mat model::get_model_phi() {
    arma::mat res(K, V);
    for (int k = 0; k < K; k++) {
        for (int w = 0; w < V; w++) {
            res(k, w) = phi[k][w];
        }
    }
    return res;    
}

// NOTE added for Rcpp
arma::mat model::get_model_theta() {
    arma::mat res(M, K);
    for (int m = 0; m < M; m++) {
        for (int k = 0; k < K; k++) {
            res(m, k) = theta[m][k];
        }
    }
    return res;    
}

// NOTE modify this method to fit the model with a matrix
int model::init_est() {
    int m, n, w, k;

    p = new double[K];

    // + read training data
    // ptrndata = new dataset;
    // // NOTE change to load from memory
    // if (ptrndata->read_trndata(dir + dfile, dir + wordmapfile)) {
    //     printf("Fail to read training data!\n");
    //     return 1;
    // }
		
    // + allocate memory and assign values for variables
    //M = ptrndata->M; // NOTE number of documents
    //V = ptrndata->V; // NOTE number of unique words in id2word
    // K: from command line or default value
    // alpha, beta: from command line or default values
    // niters, savestep: from command line or default values
    
    printf("M = %d, V = %d\n", M, V);

    // NOTE topic-word distribution
    nw = new int*[V];
    for (w = 0; w < V; w++) {
        nw[w] = new int[K];
        for (k = 0; k < K; k++) {
    	    nw[w][k] = 0;
        }
    }
	
	// NOTE topic-document distribution
    nd = new int*[M];
    for (m = 0; m < M; m++) {
        nd[m] = new int[K];
        for (k = 0; k < K; k++) {
    	    nd[m][k] = 0;
        }
    }
	
	// NOTE overall topic distribution
    nwsum = new int[K];
    for (k = 0; k < K; k++) {
	    nwsum[k] = 0;
    }
    
    // NOTE overall word distribution
    ndsum = new int[M];
    for (m = 0; m < M; m++) {
	    ndsum[m] = 0;
    }
    
    arma::sp_vec len = arma::sum(data, 1);
    srandom(time(0)); // initialize for random number generation
    z = new int*[M];
    for (m = 0; m < M; m++) {
    	//int N = ptrndata->docs[m]->length; // NOTE use dfm
    	//int N = len[m]; // NOTE use dfm
    	//z[m] = new int[N];
	
        // initialize for z
    //     for (n = 0; n < N; n++) {
    // 	    int topic = (int)(((double)random() / RAND_MAX) * K);
    // 	    z[m][n] = topic;
    // 	    
    // 	    // number of instances of word i assigned to topic j
    // 	    nw[ptrndata->docs[m]->words[n]][topic] += 1; // NOTE Captures frequency of words
    // 	    // number of words in document i assigned to topic j
    // 	    nd[m][topic] += 1;
    // 	    // total number of words assigned to topic j
    // 	    nwsum[topic] += 1;
    //     } 
        
        int N = len[m]; // NOTE use dfm
        z[m] = new int[N];
        
        // initialize for z
        int n = 0;
        arma::sp_mat::const_row_iterator it = data.begin_row(m);
        arma::sp_mat::const_row_iterator it_end = data.end_row(m);
        for(; it != it_end; ++it) {
            int F = *it;
            int w = it.col();
            for (int f = 0; f < F; f++) {
                int topic = (int)(((double)random() / RAND_MAX) * K);
                z[m][n] = topic;
                // number of instances of word i assigned to topic j
                nw[w][topic] += 1;
                // number of words in document i assigned to topic j
                nd[m][topic] += 1;
                // total number of words assigned to topic j
                nwsum[topic] += 1;
                n++;
            } 
            // total number of words in document i
            ndsum[m] = N;      
        }
    }
    
    theta = new double*[M];
    for (m = 0; m < M; m++) {
        theta[m] = new double[K];
    }
	
    phi = new double*[K];
    for (k = 0; k < K; k++) {
        phi[k] = new double[V];
    }    
    
    return 0;
}

void model::estimate() {
    
    // NOTE Delete
    // if (twords > 0) {
    //     // print out top words per topic
    //     dataset::read_wordmap(dir + wordmapfile, &id2word);
    // }

    printf("Sampling %d iterations!\n", niters);
    
    int last_iter = liter;
    for (liter = last_iter + 1; liter <= niters + last_iter; liter++) {
    	printf("Iteration %d ...\n", liter);
    	
    	// for all z_i
    	for (int m = 0; m < M; m++) {
    	    int n = 0;
    	    arma::sp_mat::const_row_iterator it = data.begin_row(m);
    	    arma::sp_mat::const_row_iterator it_end = data.end_row(m);
    	    for(; it != it_end; ++it) {
    	        int F = *it;
    	        int w = it.col();
    	        for (int f = 0; f < F; f++) {
    	            //z[m][n] = sampling(m, n, w);
    	            n++;
    	        }
    	    }
    	}
    }
    
    printf("Gibbs sampling completed!\n");
    printf("Saving the final model!\n");
    compute_theta();
    compute_phi();
    liter--;
    // NOTE delete
    //save_model(utils::generate_model_name(-1));
}

int model::sampling(int m, int n, int w) {
    // remove z_i from the count variables
    int topic = z[m][n];
    nw[w][topic] -= 1;
    nd[m][topic] -= 1;
    nwsum[topic] -= 1;
    ndsum[m] -= 1;

    double Vbeta = V * beta;
    double Kalpha = K * alpha;    
    // do multinomial sampling via cumulative method
    for (int k = 0; k < K; k++) {
    	p[k] = (nw[w][k] + beta) / (nwsum[k] + Vbeta) *
    		   (nd[m][k] + alpha) / (ndsum[m] + Kalpha);
    }
    // cumulate multinomial parameters
    for (int k = 1; k < K; k++) {
	    p[k] += p[k - 1];
    }
    // scaled sample because of unnormalized p[]
    double u = ((double)random() / RAND_MAX) * p[K - 1];
    
    for (topic = 0; topic < K; topic++) {
    	if (p[topic] > u) {
    	    break;
    	}
    }
    
    // add newly estimated z_i to count variables
    nw[w][topic] += 1;
    nd[m][topic] += 1;
    nwsum[topic] += 1;
    ndsum[m] += 1;    
    
    return topic;
}

void model::compute_theta() {
    for (int m = 0; m < M; m++) {
    	for (int k = 0; k < K; k++) {
    	    theta[m][k] = (nd[m][k] + alpha) / (ndsum[m] + K * alpha);
    	}
    }
}

void model::compute_phi() {
    for (int k = 0; k < K; k++) {
    	for (int w = 0; w < V; w++) {
    	    phi[k][w] = (nw[w][k] + beta) / (nwsum[k] + V * beta);
    	}
    }
}
