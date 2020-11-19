context("test data objects")

test_that("corpus data objects contain expected quantities", {
    expected_user_fields <- c("description", "source", "url", "author", "keywords", "title")
    expected_system_fields <- "summary"

    expect_true(all(expected_user_fields %in% names(quanteda.core::meta(data_corpus_irishbudget2010, type = "user"))))
    expect_true(all(expected_system_fields %in% names(quanteda.core::meta(data_corpus_irishbudget2010, type = "system"))))

    expect_true(all(expected_user_fields %in% names(quanteda.core::meta(data_corpus_dailnoconf1991, type = "user"))))
    expect_true(all(expected_system_fields %in% names(quanteda.core::meta(data_corpus_dailnoconf1991, type = "system"))))

    expect_true(all(expected_user_fields %in% names(quanteda.core::meta(data_corpus_EPcoaldebate, type = "user"))))
    expect_true(all(expected_system_fields %in% names(quanteda.core::meta(data_corpus_EPcoaldebate, type = "system"))))

    expect_true(all(expected_user_fields %in% names(quanteda.core::meta(data_corpus_moviereviews, type = "user"))))
    expect_true(all(expected_system_fields %in% names(quanteda.core::meta(data_corpus_moviereviews, type = "system"))))

    expect_identical(class(data_corpus_irishbudget2010), c("corpus", "character"))
    expect_identical(class(data_corpus_dailnoconf1991), c("corpus", "character"))
    expect_identical(class(data_corpus_EPcoaldebate), c("corpus", "character"))
    expect_identical(class(data_corpus_moviereviews), c("corpus", "character"))
})
