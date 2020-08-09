#include "lib.h"
#include "dev.h"
#include "model.h"
using namespace quanteda;
using namespace arma;
using namespace std;
using namespace Rcpp;

// [[Rcpp::export]]
List qatd_cpp_lda(const List &texts_, 
                  const CharacterVector &types_,
                  int k, int max_iter) {
    
    Texts texts = as<Texts>(texts_);
    Types types = Rcpp::as<Types>(types_);
    
    model lda;
    lda.set_data(texts, types); 
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
