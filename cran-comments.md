## Purpose

To remedy the problems that led to the package being archived on Mar 15, 2023, so that it is restored to CRAN.

* Remove C++11 specification
* Fix compiler warnings appearing in devel versions
* Fix URL for large dataset used in vignette

Upon resubmission to be restored from the archive, a number of new issues were raised that we also addressed:

> Please add \value to .Rd files regarding exported methods and explain the
> functions results in the documentation. Please write about the structure of the
> output (class) and also what the output means. (If a function does not return a
> value, please document that too, e.g. \value{No return value, called for side
> effects} or similar)
> Missing Rd-tags in up to 20 .Rd files, e.g.:
>      as.coefficients_textmodel.Rd: \value
>      as.statistics_textmodel.Rd: \value
>      as.summary.textmodel.Rd: \value
>      coef.textmodel_ca.Rd: \value
>      influence.predict.textmodel_affinity.Rd: \value
>      print.coefficients_textmodel.Rd: \value
>      ...

We combed through the entire documentation set and have added or improved return value descriptions to the following functions.
* `affinity()`
* `as.coefficients_textmodel()`
* `as.matrix.csr.dfm()`
* `as.statistics_textmodel()`
* `as.summary.textmodel()`
* `coef.textmodel_ca()`
* `force_conformance()`
* `influence.predict.textmodel_affinity()`
* `summary.textmodel_lr()`
* `summary.textmodel_nb()`
* `summary.textmodel_svm()`
* `summary.textmodel_svmlin()`
* `summary.textmodel_wordfish()`
* `summary.textmodel_affinity()`
* `textmodel_affinity()`
* `textmodel_lr()`
* `textmodel_lsa()`
* `textmodel_svm()`
* `textmodel_svmlin()`
* `textmodel_wordscores()`
* `textplot_influence()`

We did not document returns for the `print` methods, as base::print.default() does not have a documented return value (and there is no return value).

> You have examples for unexported functions. Please either omit these examples or export these functions.
> Examples for unexported function
>   data_corpus_EPcoaldebate() in:
>      force_conformance.Rd
>   data_corpus_irishbudget2010() in:
>      friendly_class_undefined_message.Rd

`data_corpus_EPcoaldebate` is a data object in the package, not an unexported function.  It is fully documented. It is also not referenced in force_conformance.Rd so we are not sure why this point was raised.

`data_corpus_irishbudget2010` is a data object in the package, not an unexported function.  It is fully documented. It is also not referenced in friendly_class_undefined_message.Rd so we are not sure why this point was raised.


> Some code lines in examples are commented out.
> Please never do that. Ideally find toy examples that can be regularly executed
> and checked. Lengthy examples (> 5 sec), can be wrapped in \donttest{}.
> Examples in comments in:
>      friendly_class_undefined_message.Rd

> \dontrun{} should only be used if the example really cannot be executed (e.g.
> ecause of missing additional software, missing API keys, ...) by the user.
> That's why wrapping examples in \dontrun{} adds the comment ("# Not run:") as a
> warning for the user.
> Does not seem necessary.
> Please unwrap the examples if they are executable in < 5 sec, or replace
> \dontrun{} with \donttest{}.

Those functions are from quanteda and had to be included as duplicates here dating back to the partition of the main functions in quanteda.textmodels from quanteda.  We've now simply redefined those as functions from quanteda, and removed the code from quanteda.textmodels.


> Please always make sure to reset to user's options(), working directory or par() after you changed it in examples and vignettes and demos. > -> man/data_corpus_dailnoconf1991.Rd
> e.g.:
> oldpar <- par(mfrow = c(1,2))
> ...
> par(oldpar)

Fixed now.

# Checks

## Test environments

* local macOS 13.2.1, R 4.2.2
* Ubuntu 22.04 LTS, R 4.2.2
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()
* Windows oldrelease via devtools::check_win_oldrelease()

## R CMD check results

No ERRORs, NOTEs, or WARNINGs produced.

## Reverse dependency and other package conflicts

None, according to revdepcheck::revdep_check().
