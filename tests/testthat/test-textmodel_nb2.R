context("test textmodel_nb")

test_that("textmodel_nb2 is the same as older method", {
    ## Example from 13.1 of _An Introduction to Information Retrieval_
    txt <- c(d1 = "Chinese Beijing Chinese",
             d2 = "Chinese Chinese Shanghai",
             d3 = "Chinese Macao",
             d4 = "Tokyo Japan Chinese",
             d5 = "Chinese Chinese Chinese Tokyo Japan")
    nb_dfm <- dfm(txt, tolower = FALSE)
    nb_class <- factor(c("Y", "Y", "Y", "N", NA), ordered = TRUE)

    tmod_nb <- textmodel_nb(nb_dfm, nb_class, smooth = 1, prior = "docfreq")
    tmod_nb2 <- textmodel_nb2(nb_dfm, nb_class, smooth = 1, prior = "docfreq")

    expect_identical(tmod_nb$PwGc, tmod_nb2$PwGc)
    expect_identical(tmod_nb$PcGw, tmod_nb2$PcGw)
})

test_that("textmodel_nb2 outperforms older method", {
    # performance texts with Large Movie Review Dataset (50k movie reviews)
    skip_if_not_installed("quanteda.classifiers")
    skip_if_not_installed("microbenchmark")

    data(data_corpus_LMRD, package = "quanteda.classifiers")
    dfmat <- dfm(data_corpus_LMRD)

    # uses new quanteda v2 syntax for docvar access
    microbenchmark::microbenchmark(
        tmod_nb = textmodel_nb(dfmat, y = dfmat$polarity, smooth = 1),
        tmod_nb2 = textmodel_nb2(dfmat, y = dfmat$polarity, smooth = 1),
        times = 10
    )

})
