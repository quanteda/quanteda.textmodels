## Purpose

To remedy the problems that led to the package being archived on Mar 15, 2023, so that it is restored to CRAN.

* Remove C++11 specification
* Fix compiler warnings appearing in devel versions
* Fix URL for large dataset used in vignette


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
