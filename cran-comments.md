## Purpose

* Fixes namespace issues caused by removing RcppArmadillo from quanteda 4.1.0. 
* Fixes extraneous output from the spelling check.

# Checks

## Test environments

* local macOS 14.4.1, R 4.4.1
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

## Reverse dependency and other package conflicts

None, according to revdepcheck::revdep_check().
