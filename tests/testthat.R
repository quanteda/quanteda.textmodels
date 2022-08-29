library("testthat")
library("quanteda")
library("quanteda.textmodels")

# for strong tests for Matrix deprecations
options(Matrix.warnDeprecatedCoerce = 2)

test_check("quanteda.textmodels")
