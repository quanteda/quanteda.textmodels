context("test tokens2sequences")

test_that("tokens2sequences works", {
    skip_on_cran()

    ## Example from 13.1 of _An Introduction to Information Retrieval_
    text <- c("Chinese Beijing Chinese",
              "Chinese Chinese Shanghai",
              "Chinese Macao",
              "Tokyo Japan",
              "Chinese Chinese Chinese Tokyo Japan")
    text_tokens <- tokens(text)
    seq <- tokens2sequences(text_tokens, maxsenlen = 10, keepn = 5)

    # Check ouptuts for consistency
    expect_equal(dim(seq$matrix), c(5, 10))
    expect_equal(seq$nfeatures, 5)
    expect_equal(max(seq$matrix), 5)
    expect_equal(min(seq$matrix), 0)
    expect_equal(as.integer(apply(seq$matrix, 1, function(x) sum(x != 0))), c(3, 3, 1, 2, 5))

    # Compare with keras's texts_to_sequences function
    tok <- keras::text_tokenizer(filters = "!\"#$%&()*+,-./:;<=>?@[\\]^_`{|}~\t\n\r",lower = T,num_words = 6) %>% # Note: Keras includes 0 as a word. tokens2sequences does not
        keras::fit_text_tokenizer(text)
    tok_mat <- keras::texts_to_sequences(tok,texts = text) %>% keras::pad_sequences(maxlen = 10)
    seq_mat <- unname(seq$matrix)
    expect_equal(seq_mat, tok_mat)
})

test_that("tokens2sequences_conform works", {
    txt1 <- "This is sentence one.  And here is sentence two."
    txt2 <- "This is sentence 3.  Sentence 4!  A fifth and final example."
    toks1 <- corpus(txt1) %>%
        corpus_reshape(to = "sentence") %>%
        tokens() %>%
        tokens_tolower()
    toks2 <- corpus(txt2) %>%
        corpus_reshape(to = "sentence") %>%
        tokens() %>%
        tokens_tolower()

    seqx <- tokens2sequences(toks1, maxsenlen = 4, keepn = 4)
    seqy <- tokens2sequences(toks2, maxsenlen = 4, keepn = 4)

    seqxy <- tokens2sequences_conform(seqx, seqy)
    expect_equal(dim(seqxy$matrix), c(2, 4))
    expect_equal(ncol(seqxy$features), 3)
    expect_output(
        print(seqxy),
        "Ordered feature matrix of: 2 documents, 4 features (12.5% sparse).",
        fixed = TRUE
    )
})
