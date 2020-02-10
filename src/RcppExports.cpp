// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

// qatd_cpp_ca
S4 qatd_cpp_ca(const arma::sp_mat& dfm, const double residual_floor);
RcppExport SEXP _quanteda_textmodels_qatd_cpp_ca(SEXP dfmSEXP, SEXP residual_floorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< const arma::sp_mat& >::type dfm(dfmSEXP);
    Rcpp::traits::input_parameter< const double >::type residual_floor(residual_floorSEXP);
    rcpp_result_gen = Rcpp::wrap(qatd_cpp_ca(dfm, residual_floor));
    return rcpp_result_gen;
END_RCPP
}
// qatd_cpp_is_grouped_numeric
bool qatd_cpp_is_grouped_numeric(NumericVector values_, IntegerVector groups_);
RcppExport SEXP _quanteda_textmodels_qatd_cpp_is_grouped_numeric(SEXP values_SEXP, SEXP groups_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type values_(values_SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type groups_(groups_SEXP);
    rcpp_result_gen = Rcpp::wrap(qatd_cpp_is_grouped_numeric(values_, groups_));
    return rcpp_result_gen;
END_RCPP
}
// qatd_cpp_is_grouped_character
bool qatd_cpp_is_grouped_character(CharacterVector values_, IntegerVector groups_);
RcppExport SEXP _quanteda_textmodels_qatd_cpp_is_grouped_character(SEXP values_SEXP, SEXP groups_SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type values_(values_SEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type groups_(groups_SEXP);
    rcpp_result_gen = Rcpp::wrap(qatd_cpp_is_grouped_character(values_, groups_));
    return rcpp_result_gen;
END_RCPP
}
// qatd_cpp_tbb_enabled
bool qatd_cpp_tbb_enabled();
RcppExport SEXP _quanteda_textmodels_qatd_cpp_tbb_enabled() {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    rcpp_result_gen = Rcpp::wrap(qatd_cpp_tbb_enabled());
    return rcpp_result_gen;
END_RCPP
}
// qatd_cpp_wordfish_dense
Rcpp::List qatd_cpp_wordfish_dense(SEXP wfm, SEXP dir, SEXP priors, SEXP tol, SEXP disp, SEXP dispfloor, bool abs_err);
RcppExport SEXP _quanteda_textmodels_qatd_cpp_wordfish_dense(SEXP wfmSEXP, SEXP dirSEXP, SEXP priorsSEXP, SEXP tolSEXP, SEXP dispSEXP, SEXP dispfloorSEXP, SEXP abs_errSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type wfm(wfmSEXP);
    Rcpp::traits::input_parameter< SEXP >::type dir(dirSEXP);
    Rcpp::traits::input_parameter< SEXP >::type priors(priorsSEXP);
    Rcpp::traits::input_parameter< SEXP >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< SEXP >::type disp(dispSEXP);
    Rcpp::traits::input_parameter< SEXP >::type dispfloor(dispfloorSEXP);
    Rcpp::traits::input_parameter< bool >::type abs_err(abs_errSEXP);
    rcpp_result_gen = Rcpp::wrap(qatd_cpp_wordfish_dense(wfm, dir, priors, tol, disp, dispfloor, abs_err));
    return rcpp_result_gen;
END_RCPP
}
// qatd_cpp_wordfish
Rcpp::List qatd_cpp_wordfish(arma::sp_mat& wfm, IntegerVector& dirvec, NumericVector& priorvec, NumericVector& tolvec, IntegerVector& disptype, NumericVector& dispmin, bool ABS, bool svd_sparse, double residual_floor);
RcppExport SEXP _quanteda_textmodels_qatd_cpp_wordfish(SEXP wfmSEXP, SEXP dirvecSEXP, SEXP priorvecSEXP, SEXP tolvecSEXP, SEXP disptypeSEXP, SEXP dispminSEXP, SEXP ABSSEXP, SEXP svd_sparseSEXP, SEXP residual_floorSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< arma::sp_mat& >::type wfm(wfmSEXP);
    Rcpp::traits::input_parameter< IntegerVector& >::type dirvec(dirvecSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type priorvec(priorvecSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type tolvec(tolvecSEXP);
    Rcpp::traits::input_parameter< IntegerVector& >::type disptype(disptypeSEXP);
    Rcpp::traits::input_parameter< NumericVector& >::type dispmin(dispminSEXP);
    Rcpp::traits::input_parameter< bool >::type ABS(ABSSEXP);
    Rcpp::traits::input_parameter< bool >::type svd_sparse(svd_sparseSEXP);
    Rcpp::traits::input_parameter< double >::type residual_floor(residual_floorSEXP);
    rcpp_result_gen = Rcpp::wrap(qatd_cpp_wordfish(wfm, dirvec, priorvec, tolvec, disptype, dispmin, ABS, svd_sparse, residual_floor));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_quanteda_textmodels_qatd_cpp_ca", (DL_FUNC) &_quanteda_textmodels_qatd_cpp_ca, 2},
    {"_quanteda_textmodels_qatd_cpp_is_grouped_numeric", (DL_FUNC) &_quanteda_textmodels_qatd_cpp_is_grouped_numeric, 2},
    {"_quanteda_textmodels_qatd_cpp_is_grouped_character", (DL_FUNC) &_quanteda_textmodels_qatd_cpp_is_grouped_character, 2},
    {"_quanteda_textmodels_qatd_cpp_tbb_enabled", (DL_FUNC) &_quanteda_textmodels_qatd_cpp_tbb_enabled, 0},
    {"_quanteda_textmodels_qatd_cpp_wordfish_dense", (DL_FUNC) &_quanteda_textmodels_qatd_cpp_wordfish_dense, 7},
    {"_quanteda_textmodels_qatd_cpp_wordfish", (DL_FUNC) &_quanteda_textmodels_qatd_cpp_wordfish, 9},
    {NULL, NULL, 0}
};

RcppExport void R_init_quanteda_textmodels(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
