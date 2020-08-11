context("test textmodel_lda")

require(quanteda)
toks <- tokens(data_corpus_moviereviews[1:20], 
               remove_punct = TRUE, 
               remove_symbols = TRUE,
               remove_number = TRUE)
dfmt <- dfm(toks) %>% 
        dfm_remove(stopwords(), min_nchar = 2) %>% 
        dfm_trim(max_docfreq = 5)

test_that("LDA is working", {
    
    set.seed(1234)
    lda <- textmodel_lda(dfmt, k = 5)
    
    expect_equal(dim(terms(lda, 10)), c(10, 5))
    expect_equal(dim(terms(lda, 20)), c(20, 5))
    expect_identical(
        terms(lda)[1:2,],
        matrix(
            c("joe", "gorilla", "stalked", "obvious", "zero", "alicia", 
              "cool", "baldwin", "mars", "ghosts"), nrow = 2,
            dimnames = list(
                NULL, 
                c("topic1", "topic2", "topic3", "topic4", "topic5")
            )
        )
    )
    expect_identical(
       topics(lda)[1:5],
        c("topic2", "topic5", "topic4", "topic4", "topic2")
    )
    expect_equal(
        ncol(terms(textmodel_lda(dfmt, k = 1))), 1
    )
    expect_equal(
        ncol(terms(textmodel_lda(dfmt, k = 10))), 10
    )
    expect_error(
        textmodel_lda(dfmt, k = 0),
        "k must be larger than zero"
    )
    expect_output(
        print(lda),
        "Topics: 5; 20 documents; 2996 features."
    )
})

test_that("seeded LDA is working", {
    
    dict <- dictionary(list(romance = c("love*", "couple*"), 
                            sifi = c("arean*", "star", "space")))
    
    set.seed(1234)
    lda <- textmodel_seededlda(dfmt, dict, residual = TRUE)
    
    expect_equal(dim(terms(lda, 10)), c(10, 3))
    expect_equal(dim(terms(lda, 20)), c(20, 3))
    expect_identical(
        terms(lda)[1:2,],
        matrix(
            c("couple", "love", "star", "space", "cool", "baldwin"), nrow = 2,
            dimnames = list(
                NULL, 
                c("romance", "sifi", "other")
            )
        )
    )
    expect_identical(
        topics(lda)[1:5],
        c("sifi", "sifi", "other", "other", "sifi")
    )
    expect_equal(
        ncol(terms(textmodel_seededlda(dfmt, dict, residual = FALSE))), 2
    )
    expect_error(
        textmodel_seededlda(dfmt, list("aa", "bb")),
        "dictionary must be a dictionary object"
    )
    expect_error(
        textmodel_seededlda(dfmt, dict, weight = -0.1),
        "weight must be pisitive a value"
    )
    expect_output(
        print(lda),
        "Topics: 3; 20 documents; 2996 features."
    )
})
