#include "lib.h"
using namespace quanteda.core;

// [[Rcpp::export]]
bool qatd_cpp_tbb_enabled(){
#if QUANTEDA_USE_TBB
    return true;
#else
    return false;
#endif
}
