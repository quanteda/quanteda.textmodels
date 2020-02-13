## Summary

A new package containing the scaling and classification "textmodels" split from the **quanteda** package.  This will make maintenance easier and bring the size of **quanteda** to within the CRAN 5MB limit.

When this package has been accepted and published, we will update **quanteda** to a v2 major release soon after.

## Test environments

* local R installation, R 3.6.2
* ubuntu 16.04 (on travis-ci), R 3.6.2
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Reverse dependency and other package conflicts

None - this is a new package.  The existing functions mask all of the `quanteda::textmodel_*()` functions, but this will cease to be the case once we update **quanteda** after this package has been published.

