## Purpose

RESUBMISSION: We forgot to .Rbuildignore some files used for creating the data objects, and this made the package size too large.  Fixed now.

Minor compatibility improvements with respect to the splitting from this package originally from **quanteda**, and adds new data objects.

## Test environments

* local R installation, R 3.6.3
* ubuntu 16.04 (on travis-ci), R 3.6.3
* win-builder (devel, release)

## R CMD check results

No ERRORs, WARNINGS or NOTEs.

## Reverse dependency and other package conflicts

None, according to revdepcheck::revdep_check().
