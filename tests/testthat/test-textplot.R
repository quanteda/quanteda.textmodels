context("test plots.R")

pdf(file = tempfile(".pdf"), width = 10, height = 10)

test_that("test textmodel_affinity plots", {
    af <- textmodel_affinity(data_dfm_lbgexample, y = c("L", NA, NA, NA, "R", NA))
    afpred <- predict(af)
    expect_silent(textplot_influence(influence(afpred)))
    expect_silent(textplot_influence(summary(influence(afpred))))
})

dev.off()
