/*

 * Original code by Xuan-Hieu Phan modified by Bettina Gruen
 *
 * Copyright (C) 2007 by
 * 
 * 	Xuan-Hieu Phan
 *	hieuxuan@ecei.tohoku.ac.jp or pxhieu@gmail.com
 * 	Graduate School of Information Sciences
 * 	Tohoku University
 *
 * Copyright (C) 2009 by
 * 
 * 	Bettina Gruen
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

#ifndef	_MODEL_H
#define	_MODEL_H

#include <R.h>
#include "constants.h"
#include "dataset.h"

using namespace std;

// LDA model
class model {
public:
    // fixed options
    string tassign_suffix;	// suffix for topic assignment file
    string theta_suffix;	// suffix for theta file
    string phi_suffix;		// suffix for phi file
    string others_suffix;	// suffix for file containing other parameters

    string dir;			// model directory
    string model_name;		// model name
    int save;                   // save results into files
    int keep;                   // keep every keep log-likelihood value
    dataset * ptrndata;	        // pointer to training dataset object

    // --- model parameters and variables ---    
    int M; // dataset size (i.e., number of docs)
    int V; // vocabulary size
    int K; // number of topics
    double alpha, beta1;  // LDA hyperparameters 
    double ** beta; // LDA hyperparameter
    double * Vbeta; // LDA hyperparameter
    int niters; // number of Gibbs sampling iterations
    int liter; // the iteration at which the model was saved
    int verbose; // saving period
    int estimate_phi; // topic distribution estimated or fixed
    int seeded; // is the LDA hyperparameter beta a matrix or a scalar

    double loglikelihood; // log P (w | z)
    double * logLiks; // vector of intermediate log-likelihood values
    double * p; // temp variable for sampling
    int ** z; // topic assignments for words, size M x doc.size()
    int ** wordassign; // most probable topic assignments for words, size M x doc.size()
    int ** nw; // cwt[i][j]: number of instances of word/term i assigned to topic j, size V x K
    int ** nd; // na[i][j]: number of words in document i assigned to topic j, size M x K
    int * nwsum; // nwsum[j]: total number of words assigned to topic j, size K
    int * ndsum; // nasum[i]: total number of words in document i, size M
    double ** theta; // theta: document-topic distributions, size M x K
    double ** phi; // phi: topic-word distributions, size K x V
    
    // --------------------------------------
    
    model() {
	set_default_values();
    }
          
    ~model();
    
    // set default values for variables
    void set_default_values();   

    // initialize the model
    int init(int *i, int *j, int *v, int total, double *delta, int *Z, double *Phi, int init);
    int get_z(int m, int n, double *Phi, int *Z, int index, int init);
    
    // save LDA model to files
    // model_name.tassign: topic assignments for words in docs
    // model_name.theta: document-topic distributions
    // model_name.phi: topic-word distributions
    // model_name.others: containing other parameters of the model (alpha, beta, M, V, K)
    int save_model(string new_model_name);
    int save_model_tassign(string filename);
    int save_model_theta(string filename);
    int save_model_phi(string filename);
    int save_model_others(string filename);
    
    // estimate LDA model using Gibbs sampling
    void estimate();
    void inference();
    int sampling(int m, int n);
    int get_wordassign(int m, int n);
    void compute_theta();
    void compute_phi();
    
};

#endif

