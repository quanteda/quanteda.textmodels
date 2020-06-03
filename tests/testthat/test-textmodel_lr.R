context("test textmodel_lr")
test_that("the lr model works with binomal classification", {
    ## Example based on 13.1 of _An Introduction to Information Retrieval_
    corp <- quanteda::corpus(c(d1 = "Chinese Beijing Chinese",
                               d2 = "Chinese Chinese Shanghai",
                               d3 = "Chinese Macao",
                               d4 = "Tokyo Japan Chinese",
                               d5 = "London England Chinese",
                               d6 = "Chinese Chinese Chinese Tokyo Japan"),
                             docvars = data.frame(train = factor(c("Y", "Y", "Y",
                                                                   "N", "N", NA))))
    dfmat <- quanteda::dfm(corp, tolower = FALSE)
    dfmat_test <- dfmat
    #
    set.seed(1)
    dfmat <- quanteda::dfm_sample(dfmat, 100, replace = TRUE)
    tmod <- textmodel_lr(dfmat, y = quanteda::docvars(dfmat, "train"), nfolds = 3)
    expect_output(
        print(tmod),
        "Call:"
    )
    expect_equal(
        as.matrix(coef(tmod)),
        matrix(c(6.60662, 0.577683, 0, 0, 0, -12.042569, -2.236915, 
                 -14.280884, 0), ncol = 1,
               dimnames = list(c("(Intercept)", "Chinese", 
                                 "Beijing", "Shanghai", "Macao", 
                                 "Tokyo", "Japan", "London", "England"
               ), "Y")),
        tol = .0000001
    )

    expect_identical(
        predict(tmod, newdata = dfmat_test, type = "class"),
        factor(c(d1 = "Y", d2 = "Y", d3 = "Y", d4 = "N", d5 = "N", d6 = "N"))
    )
    set.seed(10)
    expect_equal(
        predict(tmod, newdata = dfmat_test, type = "probability"),
        matrix(c(1, 1, 1, 0, 0, 0, 0, 0, 0, 1, 1, 1), ncol = 2,
               dimnames = list(paste0("d", 1:6), c("Y", "N"))),
        tol = .1
    )
})

test_that("the lr model works with multinomial classification", {
    corp <- quanteda::corpus(c(d1 = "Chinese Beijing Chinese",
                               d2 = "Chinese Chinese Shanghai",
                               d3 = "Chinese Macao",
                               d4 = "Tokyo Japan Chinese",
                               d5 = "Japan Japan Sushi",
                               d6 = "Bratwurst German Berlin"),
                             docvars = data.frame(train = factor(c("C", "C", "C",
                                                                   "J", "J", "G"))))
    dfmat <- quanteda::dfm(corp, tolower = FALSE)
    dfmat_test <- dfmat
    #
    set.seed(1)
    dfmat <- quanteda::dfm_sample(dfmat, 100, replace = TRUE)
    tmod <- textmodel_lr(dfmat, y = quanteda::docvars(dfmat, "train"), nfolds = 3)
    expect_output(
        print(tmod),
        "Call:"
    )
    expect_equal(
        as.matrix(coef(tmod)),
        matrix(c(0.535191, 3.589453, 0, 0, 2.768396, -0.283362, 0, 0, 0, 0, 0,
                 -0.356168, 0, 0, 0, 0, 0, 0, 0, 8.08106, 0, 0, -0.179023, 0,
                 0, 0, 0, 6.491737, 4.13242, 0, 0, 0, 0), ncol = 3,
               dimnames = list(c("(Intercept)", "Chinese", "Beijing",
                                 "Shanghai", "Macao", "Tokyo", "Japan",
                                 "Sushi", "Bratwurst", "German", "Berlin"
                   ), c("C", "G", "J"))),
        tol = .0000001
    )

    expect_identical(
        predict(tmod, newdata = dfmat_test, type = "class"),
        factor(c(d1 = "C", d2 = "C", d3 = "C", d4 = "J", d5 = "J", d6 = "G"))
    )
    set.seed(10)
    expect_equal(
        predict(tmod, newdata = dfmat_test, type = "probability"),
        matrix(c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 1, 0),
               ncol = 3, dimnames = list(paste0("d", 1:6), c("C", "G", "J"))),
        tol = .1
    )
})
