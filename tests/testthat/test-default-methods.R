test_that("test default textmodel methods", {
    expect_error(
        textmodel_affinity(TRUE, FALSE),
        "textmodel_affinity\\(\\) only works on dfm.*objects"
    )
    expect_error(
        textmodel_ca(TRUE),
        "textmodel_ca\\(\\) only works on dfm.*objects"
    )
    expect_error(
        textmodel_nb(TRUE),
        "textmodel_nb\\(\\) only works on dfm.*objects"
    )
    # expect_error(
    #     textmodel_svm(TRUE),
    #     "textmodel_svm\\(\\) only works on dfm.*objects"
    # )
    expect_error(
        textmodel_svmlin(TRUE),
        "textmodel_svmlin\\(\\) only works on dfm.*objects"
    )
    expect_error(
        textmodel_wordfish(TRUE),
        "textmodel_wordfish\\(\\) only works on dfm.*objects"
    )
    expect_error(
        textmodel_wordscores(TRUE),
        "textmodel_wordscores\\(\\) only works on dfm.*objects"
    )
})

test_that("test default textplot methods", {
    expect_error(
        textplot_influence(TRUE),
        "textplot_influence\\(\\) only works on influence\\..*textmodel_affinity objects"
    )
})
