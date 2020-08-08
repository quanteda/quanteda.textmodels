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

#include "lib.h"
#include "model.h"
using namespace quanteda;
using namespace arma;
using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
List qatd_cpp_lda(arma::sp_mat &mt, int k, std::string dir) {
    model lda;
    lda.K = k;
    lda.dir = dir;
    Rcpp::NumericMatrix phi, theta;
    if (lda.init_est() == 0) {
        phi = wrap(lda.get_model_phi());
        theta = wrap(lda.get_model_theta());
    }
    return List::create(Rcpp::Named("phi") = phi,
                        Rcpp::Named("theta") = theta);
}

/*
int main(int argc, char ** argv) {
    model lda;

    if (lda.init(argc, argv)) {
	show_help();
	return 1;
    }
    
    if (lda.model_status == MODEL_STATUS_EST || lda.model_status == MODEL_STATUS_ESTC) {
	// parameter estimation
	lda.estimate();
    }
    
    if (lda.model_status == MODEL_STATUS_INF) {
	// do inference
	lda.inference();
    }
    
    return 0;
}

void show_help() {
    printf("Command line usage:\n");
    printf("\tlda -est -alpha <double> -beta <double> -ntopics <int> -niters <int> -savestep <int> -twords <int> -dfile <string>\n");
    printf("\tlda -estc -dir <string> -model <string> -niters <int> -savestep <int> -twords <int>\n");
    printf("\tlda -inf -dir <string> -model <string> -niters <int> -twords <int> -dfile <string>\n");
    // printf("\tlda -inf -dir <string> -model <string> -niters <int> -twords <int> -dfile <string> -withrawdata\n");
}
*/
