# quanteda.textmodels 0.9.10

* Removed `textmodel_svm()` due to #72. We are working with the authors of **LibLineaR** on a fix, at which time we will restore the function to the CRAN version of the package.

# quanteda.textmodels 0.9.9

* Fixes namespace issues caused by removing RcppArmadillo from quanteda 4.1.0. 
* Fixes extraneous output from the spelling check.

# quanteda.textmodels 0.9.7

Fixes the compilation errors caused by changes to the TBB library and RcppParallel. This version links to quanteda v4.0 that does not rely on RcppParallel.

Also fixes some minor method class and methods signature issues caught by the improved CRAN checks in the forthcoming R v4.

# quanteda.textmodels 0.9.6

Remedies the problems that led to the package being archived on Mar 15, 2023, so that it would be restored to CRAN.

- Remove C++11 specification
- Fixed compiler warnings appearing in devel versions
- Fixed URL for large dataset used in vignette
- Upon resubmission to be restored from the archive, a number of new issues were raised that we also addressed:  We combed through the entire documentation set and have added or improved return value descriptions to many functions that had lacked them.

# quanteda.textmodels 0.9.5

* Fixes for compatibility with Matrix 1.4.2.

# quanteda.textmodels 0.9.4

* removed `data_dfm_lbgexample` as this is already in **quanteda**.

# quanteda.textmodels 0.9.3

* Fixed `textmodel_svm()` to default to the "L2-regularized L2-loss support vector classification (dual)" model, and added the `type` argument to allow this to be overridden. (#45)
* Loosened a test tolerance that was causing a text to fail on the macM1 platform.

# quanteda.textmodels 0.9.2

* Added a logistic regression classifier, `textmodel_lr()`.
* Simplified `textmodel_svmlin()` to implement only the L2-SVM-MFN from Sindhwani and Keerthi (2006), and now uses the C++ code natively.

# quanteda.textmodels 0.9.1

* Adds `data_corpus_dailnoconf1991` and `data_corpus_irishbudget2010` corpus objects moved from the **quanteda** package.
* Adds the Pang and Lee (2004) movie review polarity dataset as `data_corpus_moviereviews`.
* Fixed a bug in `textmodel_nb(x, distribution = "Bernoulli")` that affected the class conditional probabilities when the number of classes was > 2.
* Doubled the performance of `textmodel_nb()` - it's now officially the fastest implementation in R.  (See the vignette.)

# quanteda.textmodels 0.9.0

* First implementation of the package, after splitting these functions from the **quanteda** package.
