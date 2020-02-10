context("test textmodel_nnseq")

test_that("the nnseq model works", {
    skip_on_cran()

    dfmat <- dfm(data_corpus_irishbudget2010)
    y <- test <- docvars(data_corpus_irishbudget2010, "party")
    y[5] <- NA
    tmod <- textmodel_nnseq(dfmat, y = y, epoch = 50)

    # label
    expect_equal(names(predict(tmod, type = "class"))[5], "Cowen, Brian (FF)")
    # prediction
    expect_true(as.character(predict(tmod, type = "class")[5]) %in% c("FF", "Green"))
    
    probmat <- predict(tmod, type = "probability")
    expect_equal(dim(probmat), c(14, 5))
    expect_equal(rownames(probmat), docnames(dfmat))
    expect_equal(colnames(probmat), tmod$classnames)
    expect_equal(unname(rowSums(probmat)), rep(1, nrow(probmat)), tol = .000001)

    expect_output(
        print(tmod),
        "Call:"
    )

    expect_equal(names(summary(tmod)), c("call", "model structure"))
    expect_equivalent(
        as.character(predict(tmod, type = "class")),
        test
    )
    set.seed(10)
    pred_out <- predict(tmod, type = "probability")
    pred_max <- apply(pred_out, 1, function(x) colnames(pred_out)[which.max(x)])
    names(test) <- paste0("text", 1:length(test))
    expect_equivalent(
        pred_max,
        as.character(predict(tmod, type = "class"))
    )
})
