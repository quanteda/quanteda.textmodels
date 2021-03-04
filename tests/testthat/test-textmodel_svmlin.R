library("quanteda")

test_that("the svmlin model works", {
    ## Example from 13.1 of _An Introduction to Information Retrieval_
    corp <- corpus(c(d1 = "Chinese Beijing Chinese",
                     d2 = "Chinese Chinese Shanghai",
                     d3 = "Chinese Macao",
                     d4 = "Tokyo Japan Chinese",
                     d5 = "Chinese Chinese Chinese Tokyo Japan"),
                   docvars = data.frame(train = factor(c("Y", "Y", "Y", "N", NA))))
    dfmat <- dfm(tokens(corp), tolower = FALSE) %>%
        dfm_tfidf()
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
    dfmat <- dfm_tfidf(data_dfm_lbgexample)
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
