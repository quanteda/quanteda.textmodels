context("test textmodel_cnnlstmemb")

test_that("the cnnlstmemb model works", {
    skip_on_cran()

    corp <- corpus_subset(data_corpus_EPcoaldebate,
                          subset = language == "English") %>%
        corpus_sample(500)

    toks <- tokens(texts(corp))
    label <- ifelse(docvars(corp, "crowd_subsidy_label") == "Pro-Subsidy", 1, 0)
    tmod <- textmodel_cnnlstmemb(toks, y = label, epochs = 8)

    expect_output(
        print(tmod),
        "Call:"
    )
    expect_equal(names(summary(tmod)), c("call", "model structure"))
    con_mat <- table(predict(tmod, type = "class"), label)
    accuracy <- sum(diag(con_mat)) / sum(con_mat)
    expect_equal(
        accuracy,
        0.87,
        tolerance = 0.1
    )
    set.seed(10)
    pred_out <- predict(tmod, type = "probability")
    pred_max <- apply(pred_out, 1, function(x) colnames(pred_out)[which.max(x)])
    con_mat <- table(pred_max, label)
    accuracy <- sum(diag(con_mat)) / sum(con_mat)
    expect_equal(
        accuracy,
        0.87,
        tolerance = 0.1
    )
})

test_that("multiclass prediction works", {
    skip_on_cran()

    toks <- tokens(data_corpus_irishbudget2010)
    y <- docvars(data_corpus_irishbudget2010, "party")
    y[5] <- NA
    tmod2 <- textmodel_cnnlstmemb(toks, y = y)
    expect_equal(
        names(predict(tmod2, type = "class"))[5],
        "Cowen, Brian (FF)"
    )

    probmat <- predict(tmod2, type = "probability")
    expect_equal(dim(probmat), c(14, 5))
    expect_equal(rownames(probmat), docnames(toks))
    expect_equal(colnames(probmat), tmod2$classnames)
    expect_equal(unname(rowSums(probmat)), rep(1, nrow(probmat)), tol = .000001)
})
