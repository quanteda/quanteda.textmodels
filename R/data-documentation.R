#' dfm from data in Table 1 of Laver, Benoit, and Garry (2003)
#'
#' Constructed example data to demonstrate the Wordscores algorithm, from Laver
#' Benoit and Garry (2003), Table 1.
#' @details This is the example word count data from Laver, Benoit and Garry's
#'   (2003) Table 1. Documents R1 to R5 are assumed to have known positions:
#'   -1.5, -0.75, 0, 0.75, 1.5.  Document V1 is assumed unknown, and will have a
#'   raw text score of approximately -0.45 when computed as per LBG (2003).
#' @keywords data
#' @aliases data_dfm_LBGexample
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

