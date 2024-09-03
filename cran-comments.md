## Purpose

Re-submission: Adds conditionality to tests involving packages that are only in 
Suggests.

To rescue quanteda.textmodels from CRAN archives, after fixing the issues with
it that led to its archiving on 25 August 2024.

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
