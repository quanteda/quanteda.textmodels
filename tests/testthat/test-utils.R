context("test utils")

test_that("force_conformance works", {
    expect_identical(
        suppressWarnings(featnames(quanteda.textmodels:::force_conformance(data_dfm_lbgexample, c("C", "B", "Z")))),
        c("C", "B", "Z")
    )
    suppressWarnings(
        expect_identical(
        as.matrix(quanteda.textmodels:::force_conformance(data_dfm_lbgexample, c("A", "notfound"))),
        matrix(c(2, rep(0, 11)), ncol = 2, dimnames = list(docs = c(paste0("R", 1:5), "V1"),
                                                           features = c("A", "notfound")))
    ))
    expect_warning(
        quanteda.textmodels:::force_conformance(data_dfm_lbgexample, c("C", "B", "Z")),
        "34 features in newdata not used in prediction"
    )
    expect_error(
        quanteda.textmodels:::force_conformance(data_dfm_lbgexample, c("C", "B", "Z"), force = FALSE),
        "newdata's feature set is not conformant to model terms"
    )
})
