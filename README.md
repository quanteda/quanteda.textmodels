
# quanteda.textmodels

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/quanteda.textmodels)](https://CRAN.R-project.org/package=quanteda.textmodels)
[![](https://img.shields.io/badge/devel%20version-0.9.0-royalblue.svg)](https://github.com/quanteda/quanteda.textmodels)
[![Travis build
status](https://travis-ci.org/quanteda/quanteda.textmodels.svg?branch=master)](https://travis-ci.org/quanteda/quanteda.textmodels)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/5to1am8gcaotww65?svg=true)](https://ci.appveyor.com/project/kbenoit/quanteda-textmodels)
[![codecov](https://codecov.io/gh/quanteda/quanteda.textmodels/branch/master/graph/badge.svg)](https://codecov.io/gh/quanteda/quanteda.textmodels)
[![Lifecycle:
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
<!-- badges: end -->

## About

An R package adding text scaling models and classifiers for
[**quanteda**](https://quanteda.io). Prior to **quanteda** v2, many of
these were part of that package. Early development was supported by the
European Research Council grant ERC-2011-StG 283794-QUANTESS.

For more details, see <https://quanteda.io>.

## How to Install

Once the package is on CRAN (which is it not yet), then you can install
it via the normal way from CRAN, using your R GUI or

``` r
install.packages("quanteda.textmodels") 
```

Or for the latest development version:

``` r
# devtools package required to install quanteda from Github 
devtools::install_github("quanteda/quanteda.textmodels") 
```

Because this compiles some C++ and Fortran source code, you will need to
have installed the appropriate compilers.

**If you are using a Windows platform**, this means you will need also
to install the [Rtools](https://CRAN.R-project.org/bin/windows/Rtools/)
software available from CRAN.

**If you are using macOS**, you should install the [macOS
tools](https://cran.r-project.org/bin/macosx/tools/), namely the Clang
6.x compiler and the GNU Fortran compiler (as **quanteda** requires
gfortran to build). If you are still getting errors related to gfortran,
follow the fixes
[here](https://thecoatlessprofessor.com/programming/rcpp-rcpparmadillo-and-os-x-mavericks--lgfortran-and--lquadmath-error/).

## How to cite

Benoit, Kenneth, Kohei Watanabe, Haiyan Wang, Paul Nulty, Adam Obeng,
Stefan Müller, and Akitaka Matsuo. (2018) “[quanteda: An R package for
the quantitative analysis of textual
data](https://www.theoj.org/joss-papers/joss.00774/10.21105.joss.00774.pdf)”.
*Journal of Open Source Software*. 3(30), 774.
<https://doi.org/10.21105/joss.00774>.

For a BibTeX entry, use the output from `citation(package =
"quanteda")`.

## Leaving Feedback

If you like **quanteda**, please consider leaving [feedback or a
testimonial here](https://github.com/quanteda/quanteda/issues/461).

## Contributing

Contributions in the form of feedback, comments, code, and bug reports
are most welcome. How to contribute:

  - Fork the source code, modify, and issue a [pull
    request](https://help.github.com/articles/creating-a-pull-request-from-a-fork/)
    through the [project GitHub
    page](https://github.com/quanteda/quanteda). See our [Contributor
    Code of
    Conduct](https://github.com/quanteda/quanteda/blob/master/CONDUCT.md)
    and the all-important **quanteda** [Style
    Guide](https://github.com/quanteda/quanteda/wiki/Style-guide).
  - Issues, bug reports, and wish lists: [File a GitHub
    issue](https://github.com/quanteda/quanteda.textmodels/issues).
  - Usage questions: Submit a question on the [**quanteda** channel on
    StackOverflow](https://stackoverflow.com/questions/tagged/quanteda).
  - Contact [the maintainer](mailto:kbenoit@lse.ac.uk) by email.
