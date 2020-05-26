# create the Pang and Lee dataset

library("quanteda")

download.file("https://www.cs.cornell.edu/people/pabo/movie-review-data/review_polarity.tar.gz",
              destfile = "review_polarity.tar.gz")
untar("review_polarity.tar.gz")

rt <- readtext::readtext("txt_sentoken",
                         docvarsfrom = "filepaths",
                         docvarnames = c("sentiment", "id1", "id2"))
rt$sentiment <- factor(stringi::stri_extract_last_regex(rt$id1, "pos|neg"))
rt$id1 <- stringi::stri_extract_last_regex(rt$id1, "cv\\d+$")
rt$id2 <- stringi::stri_replace_last_fixed(rt$id2, ".txt", "")

data_corpus_moviereviews <- corpus(rt) %>%
    quanteda:::add_summary_metadata()

meta(data_corpus_moviereviews) <- list(
    title = "Movie reviews from Pang, Lee, and Vaithyanathan (2002)",
    description = "A corpus object containing 2,000 movie reviews, one sentence per line, classified by positive or negative sentiment.  Includes a sentiment label (pos or neg).",
    source = 'Pang, B., Lee, L., & Vaithyanathn, S. (2002). "Thumbs Up? Sentiment Classification Using Machine Learning Techniques." Proceedings of the ACL-02 conference on Empirical methods in natural language processing: 79–86.',
    keywords = c("movies", "movie reviews", "sentiment"),
    author = "Pang, B., Lee, L., & Vaithyanathn, S.",
    url = "http://www.cs.cornell.edu/people/pabo/movie-review-data/",
    readme = readtext::readtext("poldata.README.2.0")$text
)

usethis::use_data(data_corpus_moviereviews, overwrite = TRUE)
