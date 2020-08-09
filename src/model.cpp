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

#include "lib.h"
#include "dev.h"
#include "model.h"
using namespace Rcpp;

void model::set_default_values() {
    
    M = 0;
    V = 0;
    K = 100;
    alpha = 50.0 / K;
    beta = 0.1;
    niters = 2000;
    liter = 0;
    
}

void model::set_data(arma::sp_mat mt) {
    data = mt.t();
    M = data.n_cols; 
    V = data.n_rows;
    printf("M = %d, V = %d\n", M, V);
}

int model::init_est() {
    
    printf("Initialize\n");
    
    std::uniform_real_distribution<double> random_prob(0, 1);
    std::uniform_int_distribution<int> random_topic(0, K - 1);
    
    z = Texts(M);
    p = arma::vec(K);
    nw = arma::umat(V, K, arma::fill::zeros);
    nd = arma::umat(M, K, arma::fill::zeros);
    nwsum = arma::urowvec(K, arma::fill::zeros);
    ndsum = arma::conv_to<arma::ucolvec>::from(arma::mat(arma::sum(data, 0)));
    
    dev::Timer timer;
    dev::start_timer("Set z", timer);
    srandom(time(0)); // initialize for random number generation
    
    for (int m = 0; m < M; m++) {
        
        z[m] = Text(ndsum[m]);
        int n = 0;
        
        arma::sp_mat::const_col_iterator it = data.begin_col(m);
        arma::sp_mat::const_col_iterator it_end = data.end_col(m);
        for(; it != it_end; ++it) {
            int w = it.row();
            int F = *it;
            for (int f = 0; f < F; f++) {
                int topic = random_topic(generator);
                //int topic = rand() % K + 1;
                z[m][n] = topic;
                // number of instances of word i assigned to topic j
                nw(w, topic) += 1;
                // number of words in document i assigned to topic j
                nd(m, topic) += 1;
                // total number of words assigned to topic j
                nwsum[topic] += 1;
                n++;
            }
        }
    }
    dev::stop_timer("Set z", timer);
    theta = arma::mat(M, K, arma::fill::zeros);
    phi = arma::mat(K, V, arma::fill::zeros);
    return 0;
}

void model::estimate() {
    
    printf("Sampling %d iterations!\n", niters);
    
    int last_iter = liter;
    for (liter = last_iter + 1; liter <= niters + last_iter; liter++) {
        
        if (liter % 100 == 0) {
            checkUserInterrupt();
            printf("Iteration %d ...\n", liter);
        }
        
        // for all z_i
        for (int m = 0; m < M; m++) {
            int n = 0;
            arma::sp_mat::const_col_iterator it = data.begin_col(m);
            arma::sp_mat::const_col_iterator it_end = data.end_col(m);
            for(; it != it_end; ++it) {
                int w = it.row();
                int F = *it;
                //printf("Sampling %d %d %d %d\n", liter, m, w, F);
                for (int f = 0; f < F; f++) {
                    z[m][n] = sampling(m, n, w);
                    n++;
                }
            }
        }
    }
    
    printf("Gibbs sampling completed!\n");
    compute_theta();
    compute_phi();
    liter--;
    
}

int model::sampling(int m, int n, int w) {
    
    // remove z_i from the count variables
    int topic = z[m][n];
    nw(w, topic) -= 1;
    nd(m, topic) -= 1;
    nwsum[topic] -= 1;
    ndsum[m] -= 1;
    
    double Vbeta = V * beta;
    double Kalpha = K * alpha;    
    // do multinomial sampling via cumulative method
    for (int k = 0; k < K; k++) {
        p[k] = (nw(w, topic) + beta) / (nwsum[k] + Vbeta) *
               (nd(m, topic) + alpha) / (ndsum[m] + Kalpha);
    }
    // cumulate multinomial parameters
    for (int k = 1; k < K; k++) {
        p[k] += p[k - 1];
    }
    // scaled sample because of unnormalized p[]
    //double u = (random() / RAND_MAX) * p[K - 1];
    double u = random_prob(generator) * p[K - 1];
    
    for (int k = 0; k < K; k++) {
        topic = k;
        if (p[k] > u) {
            break;
        }
    }
    
    // add newly estimated z_i to count variables
    nw(w, topic) += 1;
    nd(m, topic) += 1;
    nwsum[topic] += 1;
    ndsum[m] += 1;    
    
    return topic;
}

void model::compute_theta() {
    for (int m = 0; m < M; m++) {
        for (int k = 0; k < K; k++) {
            theta(m, k) = (nd(m, k) + alpha) / (ndsum[m] + K * alpha);
        }
    }
}

void model::compute_phi() {
    for (int k = 0; k < K; k++) {
        for (int w = 0; w < V; w++) {
            phi(k, w) = (nw(w, k) + beta) / (nwsum[k] + V * beta);
        }
    }
}
