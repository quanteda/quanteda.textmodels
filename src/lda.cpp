#include "lib.h"
#include "dev.h"
#include "model.h"
using namespace quanteda;
using namespace arma;
using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
List qatd_cpp_lda(arma::sp_mat &mt, int k, int max_iter) {
    model lda;
    lda.set_data(mt); 
    lda.K = k;
    lda.niters = max_iter;
    Rcpp::NumericMatrix phi, theta;
    if (lda.init_est() == 0) {
        lda.estimate();
    }
    //return List::create(Rcpp::Named("nw") = wrap(lda.nw),
    //                    Rcpp::Named("nd") = wrap(lda.nd));
    return List::create(Rcpp::Named("phi") = wrap(lda.phi),
                        Rcpp::Named("theta") = wrap(lda.theta));
}
