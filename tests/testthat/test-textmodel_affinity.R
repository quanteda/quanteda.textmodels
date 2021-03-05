library("quanteda")

test_that("textmodel_affinity works as expected",  {
    fitted <- textmodel_affinity(data_dfm_lbgexample,
                                 y = c("L", NA, NA, NA, "R", NA))
    predicted <- predict(fitted)
    expect_gte(coef(predicted)["V1", "L"], 0.95)
    expect_equal(rownames(coef(predicted)),
                 c("R1", "R2", "R3", "R4", "R5", "V1"))
})

test_that("textmodel_affinity works for tolower = TRUE dfm objects (#1338)", {
    toks <- tokens(c("A", "B", "C", "a"))
    dfm1 <- dfm(toks, tolower = TRUE)
    dfm2 <- dfm(toks, tolower = FALSE)

    expect_output(
        print(textmodel_affinity(dfm1, y = c("one", "two", NA, NA))),
        "Training documents per class:one: 1, two: 1; total training features: 3"
    )
    expect_output(
        print(textmodel_affinity(dfm2, y = c("one", "two", NA, NA))),
        "Training documents per class:one: 1, two: 1; total training features: 4"
    )
})

test_that("raises error when dfm is empty (#1419)",  {
    mx <- dfm_trim(data_dfm_lbgexample, 1000)
    expect_error(textmodel_affinity(mx, y = c(-1, NA, NA, NA, 1, NA)),
                 quanteda.textmodels:::message_error("dfm_empty"))
})


pdf(file = tempfile(".pdf"), width = 10, height = 10)
test_that("test textmodel_affinity plots", {
    af <- textmodel_affinity(data_dfm_lbgexample, y = c("L", NA, NA, NA, "R", NA))
    afpred <- predict(af)
    expect_silent(textplot_influence(influence(afpred)))
    expect_silent(textplot_influence(summary(influence(afpred))))
})
dev.off()
