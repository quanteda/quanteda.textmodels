require(quanteda)

char <- readLines("tests/data/trndocs.txt")
toks <- tokens(char, what = "fastestword")
head(toks)
dfmt <- dfm(toks)
out <- quanteda.textmodels:::qatd_cpp_lda(dfmt, 10, "./tests/data/")

