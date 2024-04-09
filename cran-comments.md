## Purpose

To fix the compilation errors caused by changes to the TBB library and RcppParallel. This version links to quanteda v4.0 that does not rely on RcppParallel.

Also fixes some minor method class and methods signature issues caught by the improved CRAN checks in the forthcoming R v4.

# Checks

## Test environments

* local macOS 14.2.1, R 4.3.3
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()
* Windows oldrelease via devtools::check_win_oldrelease()

## R CMD check results

No ERRORs, NOTEs, or WARNINGs produced, except:

* checking installed package size ... NOTE
  installed size is  7.2Mb
  sub-directories of 1Mb or more:
    data   3.8Mb
    libs   3.0Mb

On devtools::check_win_oldrelease(), we saw this false positive:

* using log directory 'd:/RCompile/CRANguest/R-oldrelease/quanteda.textmodels.Rcheck'
* using R version 4.2.3 (2023-03-15 ucrt)
* using platform: x86_64-w64-mingw32 (64-bit)
* using session charset: UTF-8
* checking for file 'quanteda.textmodels/DESCRIPTION' ... OK
* checking extension type ... Package
* this is package 'quanteda.textmodels' version '0.9.7'
* package encoding: UTF-8
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Kenneth Benoit <kbenoit@lse.ac.uk>'

Found the following (possibly) invalid DOIs:
  DOI: 10.1111/j.1540-5907.2008.00338.x
    From: DESCRIPTION
    Status: Forbidden
    Message: 403

## Reverse dependency and other package conflicts

None, according to revdepcheck::revdep_check().
