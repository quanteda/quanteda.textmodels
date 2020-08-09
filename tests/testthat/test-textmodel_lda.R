require(quanteda)
require(quanteda.textmodels)

char <- readLines("tests/data/trndocs.txt")
toks <- tokens(char, what = "fastestword")
dfmt <- dfm(toks)
out <- quanteda.textmodels:::qatd_cpp_lda(dfmt, 10, 2000)

for(i in 1:100) {
    out <- quanteda.textmodels:::qatd_cpp_lda(dfmt, 10, 100)
}



