## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  error = TRUE,
  collapse = TRUE,
  comment = "##"
)

## ----setup--------------------------------------------------------------------
library("quanteda")
library("quanteda.textmodels")

## ----echo = FALSE-------------------------------------------------------------
# large movie review database of 50,000 movie reviews
data_corpus_LMRD <- NULL
suppressWarnings(
    try(load(url("https://quanteda.org/data/data_corpus_LMRD.rda")))
)
if (is.null(data_corpus_LMRD)) {
    data_corpus_LMRD <- data_corpus_moviereviews
    data_corpus_LMRD$polarity <- data_corpus_LMRD$sentiment
    data_corpus_LMRD$set <- "train"
    data_corpus_LMRD$set[c(sample(1:1000, size = 200),
                           sample(1001:2000, size = 200))] <- "test"
}

dfmat <- tokens(data_corpus_LMRD) %>%
  dfm()
dfmat_train <- dfm_subset(dfmat, set == "train")
dfmat_test <- dfm_subset(dfmat, set == "test")

## ----eval = FALSE-------------------------------------------------------------
#  # large movie review database of 50,000 movie reviews
#  load(url("https://quanteda.org/data/data_corpus_LMRD.rda"))
#  
#  dfmat <- tokens(data_corpus_LMRD) %>%
#    dfm()
#  dfmat_train <- dfm_subset(dfmat, set == "train")
#  dfmat_test <- dfm_subset(dfmat, set == "test")

## -----------------------------------------------------------------------------
library("microbenchmark")
microbenchmark(
    multi = textmodel_nb(dfmat_train, dfmat_train$polarity, distribution = "multinomial"),
    bern = textmodel_nb(dfmat_train, dfmat_train$polarity, distribution = "Bernoulli"),
    times = 20
)

## -----------------------------------------------------------------------------
microbenchmark(
    multi = predict(textmodel_nb(dfmat_train, dfmat_train$polarity, distribution = "multinomial"),
                    newdata = dfmat_test),
    bern = predict(textmodel_nb(dfmat_train, dfmat_train$polarity, distribution = "Bernoulli"),
                   newdata = dfmat_test),
    times = 20
)

## -----------------------------------------------------------------------------
library("fastNaiveBayes")
library("naivebayes")

microbenchmark(
    textmodels = {
      tmod <-  textmodel_nb(dfmat_train, dfmat_train$polarity, smooth = 1, distribution = "multinomial")
      pred <- predict(tmod, newdata = dfmat_test)
    },
    fastNaiveBayes = { 
      tmod <- fnb.multinomial(as(dfmat_train, "dgCMatrix"), y = dfmat_train$polarity, laplace = 1, sparse = TRUE)
      pred <- predict(tmod, newdata = as(dfmat_test, "dgCMatrix"))
    },
    naivebayes = {
      tmod = multinomial_naive_bayes(as(dfmat_train, "dgCMatrix"), dfmat_train$polarity, laplace = 1)
      pred <- predict(tmod, newdata = as(dfmat_test, "dgCMatrix"))
    },
    times = 20
)

## -----------------------------------------------------------------------------
dfmat_train_bern <- dfm_weight(dfmat_train, scheme = "boolean")
dfmat_test_bern <- dfm_weight(dfmat_test, scheme = "boolean")

microbenchmark(
    textmodel_nb = {
      tmod <-  textmodel_nb(dfmat_train_bern, dfmat_train$polarity, smooth = 1, distribution = "Bernoulli")
      pred <- predict(tmod, newdata = dfmat_test)
    },
    fastNaiveBayes = { 
      tmod <- fnb.bernoulli(as(dfmat_train_bern, "dgCMatrix"), y = dfmat_train$polarity, laplace = 1, sparse = TRUE)
      pred <- predict(tmod, newdata = as(dfmat_test_bern, "dgCMatrix"))
    },
    naivebayes = {
      tmod = bernoulli_naive_bayes(as(dfmat_train_bern, "dgCMatrix"), dfmat_train$polarity, laplace = 1)
      pred <- predict(tmod, newdata = as(dfmat_test_bern, "dgCMatrix"))
    },
    times = 20
)

