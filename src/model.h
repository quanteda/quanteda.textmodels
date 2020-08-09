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

#ifndef	_MODEL_H
#define	_MODEL_H

#include "lib.h"
//#include "constants.h"
//#include "dataset.h"

using namespace std;
using namespace Rcpp;
using namespace quanteda;
// LDA model
class model {
public:
    // --- model parameters and variables ---    
    int M; // dataset size (i.e., number of docs)
    int V; // vocabulary size
    int K; // number of topics
    double alpha, beta; // LDA hyperparameters 
    int niters; // number of Gibbs sampling iterations
    int liter; // the iteration at which the model was saved
    
    Texts data; // vector of integer vectors (zero is reserved for padding)
    Texts z; // topic assignments for words, size M x doc.size()
    arma::vec p; // temp variable for sampling
    arma::umat nw; // cwt[i][j]: number of instances of word/term i assigned to topic j, size V x K
    arma::umat nd; // na[i][j]: number of words in document i assigned to topic j, size M x K
    arma::urowvec nwsum; // nwsum[j]: total number of words assigned to topic j, size K
    arma::ucolvec ndsum; // nasum[i]: total number of words in document i, size M
    arma::mat theta; // theta: document-topic distributions, size M x K
    arma::mat phi; // phi: topic-word distributions, size K x V
    
    // random number generators
    std::default_random_engine generator;
    std::uniform_real_distribution<double> random_prob;
    std::uniform_int_distribution<int> random_topic;
    
    // --------------------------------------
    
    model() {
	    set_default_values();
    }
          
    // set default values for variables
    void set_default_values();   
    void set_data(const Texts &texts, const Types &types);

    // init for estimation
    int init_est();
	
    // estimate LDA model using Gibbs sampling
    void estimate();
    int sampling(int m, int n, int w);
    void compute_theta();
    void compute_phi();
    
};

#endif

