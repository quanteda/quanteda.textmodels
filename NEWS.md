# quanteda.textmodels 0.9.2

* Added a logistic regression classifier, `textmodel_lr()`.

# quanteda.textmodels 0.9.1

* Adds `data_corpus_dailnoconf1991` and `data_corpus_irishbudget2010` corpus objects moved from the **quanteda** package.
* Adds the Pang and Lee (2004) movie review polarity dataset as `data_corpus_moviereviews`.
* Fixed a bug in `textmodel_nb(x, distribution = "Bernoulli")` that affected the class conditional probabilities when the number of classes was > 2.
* Doubled the performance of `textmodel_nb()` - it's now officially the fastest implementation in R.  (See the vignette.)


# quanteda.textmodels 0.9.0

* First implementation of the package, after splitting these functions from the **quanteda** package.
