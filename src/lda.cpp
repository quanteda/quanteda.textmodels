#include "lib.h"
#include "dev.h"
#include "lda.h"
using namespace quanteda;
using namespace arma;
using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
List qatd_cpp_lda(arma::sp_mat &mt, int k, int max_iter, double alpha, double beta,
                  bool verbose) {
    model lda;
    lda.set_data(mt);
    
    lda.K = k;
    if (max_iter > 0)
        lda.niters = max_iter;
    if (alpha > 0)
        lda.alpha = alpha;
    if (beta > 0)
        lda.beta = beta;
    if (verbose)
        lda.verbose = verbose;
    if (lda.init_est() == 0) {
        lda.estimate();
    }

    return List::create(Rcpp::Named("iter") = lda.liter,
                        Rcpp::Named("phi") = wrap(lda.phi),
                        Rcpp::Named("theta") = wrap(lda.theta));
}
