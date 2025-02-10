## Purpose

Fixed a UBSAN issue caused by **quanteda.textmodels** calling tests using
LibLinear that are not attempted by that package, which is not showing the
issues that we encountered by simply calling their code. We have been in touch
with the **LibLineaR** package authors, but they have not responded. To fix this
issue and get our archived package back onto CRAN, we have simply removed this
function and the package dependency on **LibLinear** from
**quanteda.textmodels**.

I also changed my maintainer email to a quanteda email address, since I changed
university affiliations and wanted to have a more stable address.

# Checks

## Test environments

* local macOS 15.0, R 4.4.2
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()
* Windows oldrelease via devtools::check_win_oldrelease()

## R CMD check results

No ERRORs, NOTEs, or WARNINGs produced, except:

N  checking installed package size ...
     installed size is  5.1Mb
     sub-directories of 1Mb or more:
       data   3.8Mb

## Reverse dependency and other package conflicts

None, according to revdepcheck::revdep_check().
