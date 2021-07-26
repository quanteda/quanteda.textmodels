#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
 Check these declarations against the C/Fortran source code.
 */

/* .C calls */
extern void predictLinear(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void trainLinear(void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *, void *);

/* .Call calls */
extern SEXP _quanteda_textmodels_qatd_cpp_ca(SEXP, SEXP);
extern SEXP _quanteda_textmodels_qatd_cpp_tbb_enabled();
extern SEXP _quanteda_textmodels_qatd_cpp_wordfish(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _quanteda_textmodels_qatd_cpp_wordfish_dense(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _quanteda_textmodels_svmlin_rcpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CMethodDef CEntries[] = {
  {"predictLinear", (DL_FUNC) &predictLinear, 16},
  {"trainLinear",   (DL_FUNC) &trainLinear,   21},
  {NULL, NULL, 0}
};

static const R_CallMethodDef CallEntries[] = {
  {"_quanteda_textmodels_qatd_cpp_ca",             (DL_FUNC) &_quanteda_textmodels_qatd_cpp_ca,              2},
  {"_quanteda_textmodels_qatd_cpp_tbb_enabled",    (DL_FUNC) &_quanteda_textmodels_qatd_cpp_tbb_enabled,     0},
  {"_quanteda_textmodels_qatd_cpp_wordfish",       (DL_FUNC) &_quanteda_textmodels_qatd_cpp_wordfish,        9},
  {"_quanteda_textmodels_qatd_cpp_wordfish_dense", (DL_FUNC) &_quanteda_textmodels_qatd_cpp_wordfish_dense,  7},
  {"_quanteda_textmodels_svmlin_rcpp",             (DL_FUNC) &_quanteda_textmodels_svmlin_rcpp,             12},
  {NULL, NULL, 0}
};

void R_init_quanteda_textmodels(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, CallEntries, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}