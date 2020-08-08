require(quanteda)

char <- readLines("tests/data/trndocs.txt")
toks <- tokens(char, what = "fastestword")
head(toks)
dfmt <- dfm(toks)
quanteda.textmodels:::qatd_cpp_lda(dfmt)

