context("test textmodel_svm")

test_that("the svm model works", {
    ## Example from 13.1 of _An Introduction to Information Retrieval_
    corp <- quanteda::corpus(c(d1 = "Chinese Beijing Chinese",
                     d2 = "Chinese Chinese Shanghai",
                     d3 = "Chinese Macao",
                     d4 = "Tokyo Japan Chinese",
                     d5 = "Chinese Chinese Chinese Tokyo Japan"),
                   docvars = data.frame(train = factor(c("Y", "Y", "Y", "N", NA))))
    dfmat <- quanteda::dfm(corp, tolower = FALSE)
    tmod <- textmodel_svm(dfmat, y = quanteda::docvars(dfmat, "train"), scale = TRUE)

    expect_output(
        print(tmod),
        "Call:"
    )

    expect_equal(
        coef(tmod)[1, 1:3, drop = FALSE],
        matrix(c(0.5535941, 0.1857624, 0.1857624), nrow = 1,
               dimnames = list(NULL, c("Chinese", "Beijing", "Shanghai"))),
        tol = .0000001
    )

    expect_equal(names(summary(tmod)), c("call", "estimated.feature.scores"))
    expect_identical(
        predict(tmod, type = "class"),
        factor(c(d1 = "Y", d2 = "Y", d3 = "Y", d4 = "N", d5 = "Y"))
    )
    set.seed(10)
    expect_equal(
        predict(tmod, type = "probability"),
        matrix(c(.8, .8, .7, .5, .7, .2, .2, .3, .5, .3), ncol = 2,
               dimnames = list(paste0("d", 1:5), c("Y", "N"))),
        tol = .1
    )
})

test_that("the svm model works with different weights", {
    ## Example from 13.1 of _An Introduction to Information Retrieval_
    corp <- quanteda::corpus(c(d1 = "Chinese Beijing Chinese",
                     d2 = "Chinese Chinese Shanghai",
                     d3 = "Chinese Macao",
                     d4 = "Tokyo Japan Chinese",
                     d5 = "Chinese Chinese Chinese Tokyo Japan"),
                   docvars = data.frame(train = factor(c("Y", "Y", "Y", "N", NA))))
    dfmat <- quanteda::dfm(corp, tolower = FALSE)

    tmod <- textmodel_svm(dfmat, y = quanteda::docvars(dfmat, "train"), weight = "docfreq")
    expect_identical(
        predict(tmod, type = "class"),
        factor(c(d1 = "Y", d2 = "Y", d3 = "Y", d4 = "Y", d5 = "Y"), levels = sort(tmod$classnames))
    )
    set.seed(10)
    expect_equal(
        predict(tmod, type = "probability"),
        matrix(c(.8, .8, .7, .5, .8, .2, .2, .3, .4, .2), ncol = 2,
               dimnames = list(paste0("d", 1:5), c("Y", "N"))),
        tol = .1
    )
    tmod <- textmodel_svm(dfmat, y = quanteda::docvars(dfmat, "train"), weight = "termfreq")
    expect_identical(
        predict(tmod, type = "class"),
        factor(c(d1 = "Y", d2 = "Y", d3 = "Y", d4 = "Y", d5 = "Y"), levels = sort(tmod$classnames))
    )
    set.seed(10)
    expect_equal(
        predict(tmod, type = "probability"),
        matrix(c(.8, .8, .7, .6, .8, .2, .2, .3, .4, .2), ncol = 2,
               dimnames = list(paste0("d", 1:5), c("Y", "N"))),
        tol = .1
    )
})

test_that("the svm model works with bias = 0", {
    skip("results are a bit stochastic on this small dataset")
    ## Example from 13.1 of _An Introduction to Information Retrieval_
    set.seed(100)
    corp <- quanteda::corpus(c(d1 = "Chinese Beijing Chinese",
                     d2 = "Chinese Chinese Shanghai",
                     d3 = "Chinese Macao",
                     d4 = "Tokyo Japan Chinese",
                     d5 = "Chinese Chinese Chinese Tokyo Japan"),
                   docvars = data.frame(train = factor(c("Y", "Y", "Y", "N", NA))))
    dfmat <- quanteda::dfm(corp, tolower = FALSE)
    set.seed(10)
    tmod <- textmodel_svm(dfmat, y = docvars(dfmat, "train"), bias = 0)
    expect_identical(
        predict(tmod, type = "class"),
        factor(c(d1 = "Y", d2 = "Y", d3 = "Y", d4 = "N", d5 = "Y"))
    )
    set.seed(10)
    expect_equal(
        predict(tmod, type = "probability"),
        matrix(c(.8, .8, .7, .4, .7, .2, .2, .3, .6, .3), ncol = 2,
               dimnames = list(paste0("d", 1:5), c("Y", "N"))),
        tol = .1
    )
})

test_that("multiclass prediction works", {
    dfmat <- quanteda::dfm(data_corpus_irishbudget2010) %>%
        quanteda::dfm_tfidf()
    tmod2 <- textmodel_svm(dfmat,
                           y = c(rep(NA, 3), "SF", "FF", "FG", NA, "LAB", NA,
                                 NA, "Green", rep(NA, 3)),
                           weight = "docfreq")
    expect_equal(
        head(predict(tmod2, type = "class"), 3),
        factor(c("Lenihan, Brian (FF)" = "FF", "Bruton, Richard (FG)" = "SF",
                 "Burton, Joan (LAB)" = "SF"),
               levels = sort(tmod2$classnames))
    )
})

context("test textmodel_svmlin")

test_that("the svmlin model works", {
    ## Example from 13.1 of _An Introduction to Information Retrieval_
    corp <- quanteda::corpus(c(d1 = "Chinese Beijing Chinese",
                     d2 = "Chinese Chinese Shanghai",
                     d3 = "Chinese Macao",
                     d4 = "Tokyo Japan Chinese",
                     d5 = "Chinese Chinese Chinese Tokyo Japan"),
                   docvars = data.frame(train = factor(c("Y", "Y", "Y", "N", NA))))
    dfmat <- quanteda::dfm(corp, tolower = FALSE) %>%
        quanteda::dfm_tfidf()
    tmod <- textmodel_svmlin(dfmat, y = docvars(dfmat, "train"))

    expect_output(
        print(tmod),
        "Call:"
    )

    expect_equal(
        head(coef(tmod), 3),
        c(intercept = 0.0, Chinese = 0.330, Beijing = 0.330),
        tol = .001
    )

    tmod2 <- textmodel_svmlin(dfmat, y = docvars(dfmat, "train"), intercept = FALSE)
    expect_identical(
        predict(tmod2),
        c(d1 = "Y", d2 = "Y", d3 = "Y", d4 = "N", d5 = "N")
    )
    expect_equal(names(summary(tmod)), c("call", "estimated.feature.scores"))

    expect_identical(
        predict(tmod),
        c(d1 = "Y", d2 = "Y", d3 = "N", d4 = "N", d5 = "N")
    )

    expect_error(
        textmodel_svmlin(dfmat, y = c("Y", "N", "Maybe", NA, NA)),
        "y must contain two values only"
    )
})

test_that("textmodel_svm/svmlin() work with weighted dfm", {
    dfmat <- quanteda::dfm_tfidf(data_dfm_lbgexample)
    expect_silent(
        tmod <- textmodel_svm(dfmat, y = c("N", "N", NA, "Y", "Y", NA))
    )
    expect_silent(
        predict(tmod)
    )
    expect_silent(
        tmod <- textmodel_svmlin(dfmat, y = c("N", "N", NA, "Y", "Y", NA))
    )
    expect_silent(
        predict(tmod)
    )
})
