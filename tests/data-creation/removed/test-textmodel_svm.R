library("quanteda")

test_that("the svm model works", {
    ## Example from 13.1 of _An Introduction to Information Retrieval_
    corp <- corpus(c(d1 = "Chinese Beijing Chinese",
                     d2 = "Chinese Chinese Shanghai",
                     d3 = "Chinese Macao",
                     d4 = "Tokyo Japan Chinese",
                     d5 = "Chinese Chinese Chinese Tokyo Japan"),
                   docvars = data.frame(train = factor(c("Y", "Y", "Y", "N", NA))))
    dfmat <- dfm(tokens(corp), tolower = FALSE)
    set.seed(10)
    tmod <- textmodel_svm(dfmat, y = docvars(dfmat, "train"), scale = TRUE)

    expect_output(
        print(tmod),
        "Call:"
    )

    expect_equal(
        coef(tmod)[1, 1:3, drop = FALSE],
        matrix(c(0.3556985, 0.1370573, 0.1399235), nrow = 1,
               dimnames = list(NULL, c("Chinese", "Beijing", "Shanghai"))),
        tol = .01
    )

    expect_equal(names(summary(tmod)), c("call", "estimated.feature.scores"))
    expect_identical(
        predict(tmod, type = "class"),
        factor(c(d1 = "Y", d2 = "Y", d3 = "Y", d4 = "N", d5 = "N"))
    )
    expect_error(
        predict(tmod, type = "probability"),
        "probability predictions not implemented for this model type"
    )

    # for model = 0 type
    set.seed(10)
    tmod <- textmodel_svm(dfmat, y = docvars(dfmat, "train"), scale = TRUE, type = 0)
    expect_equal(
        predict(tmod, type = "probability"),
        matrix(c(.8, .8, .7, .5, .7, .2, .2, .3, .5, .3), ncol = 2,
               dimnames = list(paste0("d", 1:5), c("Y", "N"))),
        tol = .1
    )
})

test_that("the svm model works with different weights", {
    ## Example from 13.1 of _An Introduction to Information Retrieval_
    corp <- corpus(c(d1 = "Chinese Beijing Chinese",
                     d2 = "Chinese Chinese Shanghai",
                     d3 = "Chinese Macao",
                     d4 = "Tokyo Japan Chinese",
                     d5 = "Chinese Chinese Chinese Tokyo Japan"),
                   docvars = data.frame(train = factor(c("Y", "Y", "Y", "N", NA))))
    dfmat <- dfm(tokens(corp), tolower = FALSE)

    set.seed(10)
    tmod <- textmodel_svm(dfmat, y = docvars(dfmat, "train"), weight = "docfreq")
    expect_identical(
        predict(tmod, type = "class"),
        factor(c(d1 = "Y", d2 = "Y", d3 = "Y", d4 = "N", d5 = "Y"), levels = sort(tmod$classnames))
    )
    set.seed(10)
    tmod <- textmodel_svm(dfmat, y = docvars(dfmat, "train"), weight = "termfreq")
    expect_identical(
        predict(tmod, type = "class"),
        factor(c(d1 = "Y", d2 = "Y", d3 = "Y", d4 = "N", d5 = "Y"), levels = sort(tmod$classnames))
    )
})

test_that("the svm model works with bias = 0", {
    set.seed(100)
    dfmat <- tokens(data_corpus_moviereviews[c(1:100, 1001:1101)]) %>%
        dfm()
    tmod <- textmodel_svm(dfmat, y = dfmat$sentiment, bias = 0)
    expect_identical(
        predict(tmod, newdata = dfm(tokens(data_corpus_moviereviews[1101])), type = "class"),
        factor(c("cv100_11528.txt" = "pos"), levels = c("neg", "pos"))
    )
})

test_that("multiclass prediction works", {
    dfmat <- dfm(tokens(data_corpus_irishbudget2010)) %>%
        dfm_tfidf()
    tmod2 <- textmodel_svm(dfmat,
                           y = c(rep(NA, 3), "SF", "FF", "FG", NA, "LAB", NA,
                                 NA, "Green", rep(NA, 3)),
                           weight = "uniform")
    expect_equal(
        head(predict(tmod2, type = "class"), 3),
        factor(c("Lenihan, Brian (FF)" = "Green", "Bruton, Richard (FG)" = "FG",
                 "Burton, Joan (LAB)" = "FG"),
               levels = sort(tmod2$classnames))
    )
})
