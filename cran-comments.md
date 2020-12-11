## Purpose

* Fixes Solaris errors
* Fixes errors in r-devel releases caused by changes in how all.equal(f, g) works, as noted in an email on Dec 9, 2020 by Martin Maechler.
* Adds two new features, as noted in NEWS

## Test environments

* local macOS 10.15.7, R 4.0.3
* Ubuntu 18.04 LTS and 18.10, R 4.0.3
* Windows release via devtools::check_win_release()
* Windows devel via devtools::check_win_devel()

## R CMD check results

No ERRORs, WARNINGS or NOTEs.

## Reverse dependency and other package conflicts

None, according to revdepcheck::revdep_check().
