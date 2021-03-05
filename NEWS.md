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
