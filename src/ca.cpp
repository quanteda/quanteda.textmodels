#include "lib.h"
using namespace quanteda;
using namespace arma;


//find the principle elements for the sparse residual matrix
void create_residual_ca(std::size_t row_num, 
                        const arma::sp_mat& dfm, 
                        const arma::colvec &rsum, 
                        const arma::rowvec &csum,
                        const double residual_floor, 
                        const std::size_t K, Triplets &residual_tri)
{
    for (std::size_t k = 0; k < K; k++){
        double residual = (dfm(row_num, k) - rsum(row_num) * csum(k)) / sqrt(rsum(row_num) * csum(k) );
        if (fabs(residual) > residual_floor) {
            Triplet mat_triplet = std::make_tuple(row_num, k, residual);
            residual_tri.push_back(mat_triplet);
        }
    }
}

// [[Rcpp::export]]
S4 cpp_ca(const arma::sp_mat &dfm, 
          const double residual_floor){
    
    const std::size_t N = dfm.n_rows;
    const std::size_t K = dfm.n_cols;
    
    // Construct Chi-Sq Residuals	
    const arma::colvec rsum(sum(dfm,1));
    const arma::rowvec csum(sum(dfm,0));
    
    //create the residual matrix
    Triplets residual_tri;
    residual_tri.reserve(N * K / 1000); // assume 99.9% sparsity

    for (std::size_t i = 0; i < N; i++) {
        create_residual_ca(i, dfm, rsum, csum, residual_floor, K, residual_tri);
    }

    return to_matrix( residual_tri, N, K, false );

}

/***R
smoke <- matrix(c(4,2,3,2, 4,5,7,4,25,10,12,4,18,24,33,13,10,6,7,2), nrow = 5, ncol = 4, byrow = T)
residual_floor <- 0.1
n = 195
P <- as.dfm(smoke) / n
qatd_cpp_ca(P, residual_floor/sqrt(n))
*/
