#' dfm from data in Table 1 of Laver, Benoit, and Garry (2003)
#'
#' Constructed example data to demonstrate the Wordscores algorithm, from Laver
#' Benoit and Garry (2003), Table 1.
#' @details This is the example word count data from Laver, Benoit and Garry's
#'   (2003) Table 1. Documents R1 to R5 are assumed to have known positions:
#'   -1.5, -0.75, 0, 0.75, 1.5.  Document V1 is assumed unknown, and will have a
#'   raw text score of approximately -0.45 when computed as per LBG (2003).
#' @keywords data
#' @format A [dfm] object with 6 documents and 37 features.
#' @references Laver, M., Benoit, K.R., & Garry, J. (2003).
#'   [Estimating Policy
#'   Positions from Political Text using Words as Data](https://kenbenoit.net/pdfs/WORDSCORESAPSR.pdf). *American
#'   Political Science Review*, 97(2), 311--331.
"data_dfm_lbgexample"


#' Irish budget speeches from 2010
#'
#' Speeches and document-level variables from the debate over the Irish budget
#' of 2010.
#' @details At the time of the debate, Fianna Fáil (FF) and the Greens formed the government
#'   coalition, while Fine Gael (FG), Labour (LAB), and Sinn Féin (SF) were in opposition.
#' @format The corpus object for the 2010 budget speeches, with document-level
#'   variables for year, debate, serial number, first and last name of the
#'   speaker, and the speaker's party.
#' @references Lowe, W. & Benoit, K.R. (2013).
#'   [Validating Estimates of Latent Traits
#'   From Textual Data Using Human Judgment as a Benchmark](https://doi.org/10.1093/pan/mpt002).
#'   *Political Analysis*, 21(3), 298--313.
#' @keywords data
#' @source
#' Dáil Éireann Debate,
#' [Budget Statement 2010.](http://oireachtasdebates.oireachtas.ie/debates\%20authoring/debateswebpack.nsf/takes/dail2009120900022?opendocument)
#' 9 December 2009. vol. 697, no. 3.
#'
#' @examples
#' summary(data_corpus_irishbudget2010)
"data_corpus_irishbudget2010"


#' Confidence debate from 1991 Irish Parliament
#'
#' Texts of speeches from a no-confidence motion debated in the Irish Dáil from
#' 16-18 October 1991 over the future of the Fianna Fail-Progressive Democrat
#' coalition. (See Laver and Benoit 2002 for details.)
#' @format `data_corpus_dailnoconf1991` is a corpus with 58 texts,
#'   including docvars for `name`, `party`, and `position`.
#' @references Laver, M. & Benoit, K.R. (2002).
#'   [Locating
#'   TDs in Policy Spaces: Wordscoring Dáil Speeches](https://kenbenoit.net/pdfs/Laver_Benoit_IPS_2002.pdf). *Irish Political
#'   Studies*, 17(1), 59--73.
#'
#' @references Laver, M., Benoit, K.R., & Garry, J. (2003).
#'   [Estimating Policy
#'   Positions from Political Text using Words as Data](https://kenbenoit.net/pdfs/WORDSCORESAPSR.pdf). *American
#'   Political Science Review*, 97(2), 311--331.
#' @source <https://www.oireachtas.ie/en/debates/debate/dail/1991-10-16/10/>
#' @keywords data
#' @examples
#' \dontrun{
#' library("quanteda")
#' data_dfm_dailnoconf1991 <- dfm(data_corpus_dailnoconf1991, remove_punct = TRUE)
#' tmod <- textmodel_affinity(data_dfm_dailnoconf1991,
#'                            c("Govt", "Opp", "Opp", rep(NA, 55)))
#' (pred <- predict(tmod))
#' dat <-
#'     data.frame(party = as.character(docvars(data_corpus_dailnoconf1991, "party")),
#'                govt = coef(pred)[, "Govt"],
#'                position = as.character(docvars(data_corpus_dailnoconf1991, "position")),
#'                stringsAsFactors = FALSE)
#' bymedian <- with(dat, reorder(paste(party, position), govt, median))
#' par(mar = c(5, 6, 4, 2)+.1)
#' boxplot(govt ~ bymedian, data = dat,
#'         horizontal = TRUE, las = 1,
#'         xlab = "Degree of support for government")
#' abline(h = 7.5, col = "red", lty = "dashed")
#' text(c(0.9, 0.9), c(8.5, 6.5), c("Goverment", "Opposition"))
#' }
"data_corpus_dailnoconf1991"

#' Crowd-labelled sentence corpus from a 2010 EP debate on coal subsidies
#'
#' @description A multilingual text corpus of speeches from a European
#'   Parliament debate on coal subsidies in 2010, with individual crowd codings
#'   as the unit of observation.  The sentences are drawn from officially
#'   translated speeches from a debate over a European Parliament debate
#'   concerning a Commission report proposing an extension to a regulation
#'   permitting state aid to uncompetitive coal mines.
#'
#' @description Each speech is available in six languages: English, German,
#'   Greek, Italian, Polish and Spanish. The unit of observation is the
#'   individual crowd coding of each natural sentence. For more information on
#'   the coding approach see
#'   [Benoit et al. (2016)](https://doi.org/10.1017/S0003055416000058).
#' @format
#'   The corpus consists of 16,806 documents (i.e. codings of a sentence) and includes the following
#'   document-level variables: \describe{
#'   \item{sentence_id}{character; a unique identifier for each sentence}
#'   \item{crowd_subsidy_label}{factor; whether a coder labelled the sentence
#'   as "Pro-Subsidy", "Anti-Subsidy" or "Neutral or inapplicable"}
#'   \item{language}{factor; the language (translation) of the speech}
#'   \item{name_last}{character; speaker's last name}
#'   \item{name_first}{character; speaker's first name}
#'   \item{ep_group}{factor; abbreviation of the EP party group of the speaker}
#'   \item{country}{factor; the speaker's country of origin}
#'   \item{vote}{factor; the speaker's vote on the proposal (For/Against/Abstain/NA)}
#'   \item{coder_id}{character; a unique identifier for each crowd coder}
#'   \item{coder_trust}{numeric; the "trust score" from the Crowdflower platform used to code the
#'    sentences, which can theoretically range between 0 and 1. Only coders with trust scores above
#'    0.8 are included in the corpus.}
#'   }
#' @references Benoit, K., Conway, D., Lauderdale, B.E., Laver, M., & Mikhaylov,
#'   S. (2016). [Crowd-sourced
#'   Text Analysis: Reproducible and Agile Production of Political Data](https://doi.org/10.1017/S0003055416000058).
#'   *American Political Science Review*, 100,(2), 278--295.
#' @format
#'  A [corpus][quanteda::corpus] object.
#' @keywords data
"data_corpus_EPcoaldebate"

#' Movie reviews with polarity from Pang and Lee (2004)
#'
#' A corpus object containing 2,000 movie reviews classified by positive or negative sentiment.
#'
#' For more information, see `cat(meta(data_corpus_moviereviews, "readme"))`.
#'
#' @format
#'   The corpus includes the following document variables: \describe{
#'   \item{sentiment}{factor indicating whether a review was manually classified as
#'   positive `pos` or negative `neg`.}
#'   \item{id1}{Character counting the position in the corpus.}
#'   \item{id2}{Random number for each review.}
#'   }
#'
#' @references
#' Pang, B., Lee, L.  (2004)
#' "[A Sentimental
#' Education: Sentiment Analysis Using Subjectivity Summarization Based on
#' Minimum Cuts.](https://www.cs.cornell.edu/home/llee/papers/cutsent.pdf)", Proceedings of the ACL.
#'
#' @source <https://www.cs.cornell.edu/people/pabo/movie-review-data/>
#' @keywords data
#' @examples
#' # check polarities
#' table(data_corpus_moviereviews$sentiment)
#'
#' # make the data into sentences, because each line is a sentence
#' data_corpus_moviereviewsents <-
#'     quanteda::corpus_segment(data_corpus_moviereviews, "\n", extract_pattern = FALSE)
#' print(data_corpus_moviereviewsents, max_ndoc = 3)
"data_corpus_moviereviews"
