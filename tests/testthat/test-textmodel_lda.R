require(quanteda)
require(quanteda.textmodels)

char <- readLines("tests/data/trndocs.txt")
toks <- tokens(char, what = "fastestword")
dfmt <- dfm(toks)

result <- quanteda.textmodels:::qatd_cpp_lda(toks, types(toks), 10, 2000)
dim(result$phi)
colnames(result$phi) <- types(toks)

terms2 <- function(x, n = 6) {
    apply(x$phi, 1, function(x, y, z) head(y[order(x, decreasing = TRUE)], z), colnames(x$phi), n)
}

terms2(result)

require(quanteda.corpora)
corp_news <- download('data_corpus_guardian')
toks_news <- tokens(corp_news, remove_punct = TRUE) %>% 
    tokens_remove(stopwords('en')) %>% 
    tokens_remove(c('*-time', '*-timeUpdated', 'GMT', 'BST'))

lda_news <- quanteda.textmodels:::qatd_cpp_lda(toks_news, types(toks_news), 10, 2000)
colnames(lda_news$phi) <- types(toks_news)
terms2(lda_news)
dim(lda$phi)

require(topicmodels)
dtm <- convert(dfmat_news, to = "topicmodels")
lda_tm <- LDA(dtm, k = 10, method = "Gibbs", control = list(verbose = 1))
terms(lda_tm, 6)
dim(posterior(lda_tm)$terms)

for(i in 1:100) {
    out <- quanteda.textmodels:::qatd_cpp_lda(dfmt, 10, 100)
}
