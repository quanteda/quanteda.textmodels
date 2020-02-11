context("test textmodel_nnseq")

test_that("the nnseq model works", {
    skip_on_cran()

    set.seed(100)
    corp_train <- corpus_sample(data_corpus_EPcoaldebate, size = 3000, by = "crowd_subsidy_label")
    corp_test <- corpus_sample(data_corpus_EPcoaldebate, size = 10, by = "crowd_subsidy_label")
    dfmat_train <- dfm(corp_train)
    dfmat_test <- dfm(corp_test)

    tmod <- textmodel_nnseq(dfmat_train, y = docvars(dfmat_train, "crowd_subsidy_label"), epoch = 5)

    # label
    pred <- predict(tmod, newdata = dfmat_test, type = "class")
    tab <- table(pred, dfmat_test$crowd_subsidy_label)
    acc <- sum(diag(tab)) / sum(tab)
    expect_gte(acc, .6)

    # predicted prob
    prob <- predict(tmod, newdata = dfmat_test, type = "probability")
    expect_gte(prob["PL_Lamberts_3_3", "Anti-Subsidy"], .95)

    expect_output(
        print(tmod),
        "Call:"
    )

    expect_equal(names(summary(tmod)), c("call", "model structure"))
    set.seed(10)
    pred_max <- apply(prob, 1, function(x) colnames(prob)[which.max(x)])
    expect_equivalent(
        pred_max,
        as.character(pred)
    )
})
