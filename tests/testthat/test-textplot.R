context("test plots.R")

pdf(file = tempfile(".pdf"), width = 10, height = 10)

test_that("test textplot_scale1d wordfish in the most basic way", {
    wf <- textmodel_wordfish(quanteda.core::dfm(data_corpus_irishbudget2010), dir = c(6, 5))
    expect_false(identical(textplot_scale1d(wf, sort = TRUE),
                           textplot_scale1d(wf, sort = FALSE)))
    expect_silent(textplot_scale1d(wf, sort = TRUE,
                                   groups = quanteda.core::docvars(data_corpus_irishbudget2010, "party")))
    expect_silent(textplot_scale1d(wf, sort = FALSE,
                                   groups = quanteda.core::docvars(data_corpus_irishbudget2010, "party")))

    expect_silent(
        textplot_scale1d(wf, doclabels = apply(quanteda.core::docvars(data_corpus_irishbudget2010,
                                                       c("name", "party")),
                                               1, paste, collapse = " "))
    )

    p1 <- textplot_scale1d(wf, margin = "features", sort = TRUE)
    p2 <- textplot_scale1d(wf, margin = "features", sort = FALSE)
    p1$plot_env <- NULL
    p2$plot_env <- NULL
    expect_equivalent(p1, p2)
})

test_that("test textplot_scale1d wordscores in the most basic way", {
    mt <- quanteda.core::dfm(data_corpus_irishbudget2010)
    ws <- textmodel_wordscores(mt, c(rep(NA, 4), -1, 1, rep(NA, 8)))
    pr <- suppressWarnings(predict(ws, mt, force = TRUE))

    expect_false(identical(textplot_scale1d(pr, sort = TRUE),
                           textplot_scale1d(pr, sort = FALSE)))
    expect_silent(textplot_scale1d(pr, sort = TRUE,
                                   groups = quanteda.core::docvars(data_corpus_irishbudget2010, "party")))
    expect_silent(textplot_scale1d(pr, sort = FALSE,
                                   groups = quanteda.core::docvars(data_corpus_irishbudget2010, "party")))

    expect_silent(textplot_scale1d(pr, doclabels = apply(quanteda.core::docvars(data_corpus_irishbudget2010,
                                                                 c("name", "party")),
                                                         1, paste, collapse = " ")))

    p1 <- textplot_scale1d(ws, margin = "features", sort = TRUE)
    p2 <- textplot_scale1d(ws, margin = "features", sort = FALSE)
    p1$plot_env <- NULL
    p2$plot_env <- NULL
    expect_equivalent(p1, p2)

    expect_error(
        textplot_scale1d(ws, margin = "documents"),
        "This margin can only be run on a predicted wordscores object"
    )
    expect_error(
        suppressWarnings(textplot_scale1d(predict(ws), margin = "features")),
        "This margin can only be run on a fitted wordscores object"
    )
})

test_that("test textplot_affinity", {
    af <- textmodel_affinity(data_dfm_lbgexample, y = c("L", NA, NA, NA, "R", NA))
    afpred <- predict(af)
    expect_silent(textplot_influence(influence(afpred)))
    expect_silent(textplot_influence(summary(influence(afpred))))
})

test_that("multiple patterns display correctly in textplot_kwic", {
    skip("For interactive visual inspection only")
    toks <- tokens(c(alpha1 = paste(letters, collapse = " "),
                     alpha2 = paste(LETTERS, collapse = " ")))

    kwic_char_f <- kwic(toks, "f", window = 3)
    kwic_char_u <- kwic(toks, "u", window = 3)
    kwic_char_uf <- kwic(toks, c("u", "f"), window = 3)
    kwic_char_fu <- kwic(toks, c("f", "u"), window = 3)
    kwic_dict_u <- kwic(toks, dictionary(list(ukey = "u")), window = 3)
    kwic_dict_f <- kwic(toks, dictionary(list(fkey = "f")), window = 3)
    kwic_dict_uf <- kwic(toks, dictionary(list(ukey = "u", fkey = "f")), window = 3)
    kwic_dict_fu <- kwic(toks, dictionary(list(fkey = "f", ukey = "u")), window = 3)
    kwic_dict_uf_jm <- kwic(toks, dictionary(list(ufkey = c("u", "f"),
                                                  jmkey = c("j", "m"))), window = 3)

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_char_f, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_char_u, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_char_u, kwic_char_f, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_f, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_u, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_u, kwic_dict_f, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_f, kwic_dict_u, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_uf, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_fu, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_char_uf, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_char_fu, scale = "absolute")

    # warning free: ✓  label order correct: ✓  plot order correct: ✓
    textplot_xray(kwic_dict_uf_jm, scale = "absolute")
})

dev.off()
