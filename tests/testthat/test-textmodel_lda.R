require(quanteda)
require(quanteda.textmodels)

char <- readLines("tests/data/trndocs.txt")
toks <- tokens(char, what = "fastestword")
dfmt <- dfm(toks)

result <- quanteda.textmodels:::qatd_cpp_lda(dfmt, 10, 2000)
dim(result$phi)
colnames(result$phi) <- colnames(dfmt)

ge_terms <- function(x, n = 6) {
    apply(x$phi, 1, function(x, y, z) head(y[order(x, decreasing = TRUE)], z), colnames(x$phi), n)
}

require(quanteda.corpora)
corp_news <- download('data_corpus_guardian')
dfmt_news <- dfm(corp_news, remove_punct = TRUE, remove = stopwords('en')) %>% 
    dfm_remove(c('*-time', '*-timeUpdated', 'GMT', 'BST')) %>% 
    dfm_trim(min_termfreq = 0.95, termfreq_type = "quantile", 
             max_docfreq = 0.1, docfreq_type = "prop")
dfmt_news <- dfmt_news[ntoken(dfmt_news) > 0,]
dfmt_news <- dfm_weight(dfmt_news, "boolean")
lda <- quanteda.textmodels:::qatd_cpp_lda(dfmt_news, 10, 2000)
colnames(lda$phi) <- colnames(dfmt_news)
ge_terms(lda)
dim(lda$phi)

require(topicmodels)
dtm <- convert(dfmat_news, to = "topicmodels")
lda_tm <- LDA(dtm, k = 10, method = "Gibbs", control = list(verbose = 1))
terms(lda_tm, 6)
dim(posterior(lda_tm)$terms)

for(i in 1:100) {
    out <- quanteda.textmodels:::qatd_cpp_lda(dfmt, 10, 100)
}
