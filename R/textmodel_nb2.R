#' Naive Bayes classifier for texts
#'
#' Fit a multinomial or Bernoulli Naive Bayes model, given a dfm and some
#' training labels.
#' @param x the [dfm] on which the model will be fit.  Does not need to
#'   contain only the training documents.
#' @param y vector of training labels associated with each document identified
#'   in `train`.  (These will be converted to factors if not already
#'   factors.)
#' @param smooth smoothing parameter for feature counts by class
#' @param prior prior distribution on texts; one of `"uniform"`,
#'   `"docfreq"`, or `"termfreq"`.  See Prior Distributions below.
#' @param distribution count model for text features, can be `multinomial`
#'   or `Bernoulli`.  To fit a "binary multinomial" model, first convert
#'   the dfm to a binary matrix using `[dfm_weight](x, scheme = "boolean")`.
#' @return
#' `textmodel_nb()` returns a list consisting of the following (where
#' \eqn{I} is the total number of documents, \eqn{J} is the total number of
#' features, and \eqn{k} is the total number of training classes):
#' @return \item{call}{original function call}
#' @return \item{PwGc}{\eqn{k \times J}; probability of the word given the class
#'   (empirical likelihood)}
#' @return \item{Pc}{\eqn{k}-length named numeric vector of class prior
#'   probabilities}
#' @return \item{PcGw}{\eqn{k \times J}; posterior class probability given the
#'   word}
#' @return \item{Pw}{\eqn{J \times 1}; baseline probability of the word}
#' @return \item{x}{the \eqn{I \times J} training dfm `x`}
#' @return \item{y}{the \eqn{I}-length `y` training class vector}
#' @return \item{distribution}{the distribution argument}
#' @return \item{prior}{the prior argument}
#' @return \item{smooth}{the value of the smoothing parameter}
#' @section Prior distributions:
#'
#' Prior distributions refer to the prior probabilities assigned to the training
#' classes, and the choice of prior distribution affects the calculation of the
#' fitted probabilities.  The default is uniform priors, which sets the
#' unconditional probability of observing the one class to be the same as
#' observing any other class.
#'
#' "Document frequency" means that the class priors will be taken from the
#' relative proportions of the class documents used in the training set.  This
#' approach is so common that it is assumed in many examples, such as the worked
#' example from Manning, Raghavan, and Schütze (2008) below.  It is not the
#' default in \pkg{quanteda}, however, since there may be nothing informative in
#' the relative numbers of documents used to train a classifier other than the
#' relative availability of the documents.  When training classes are balanced
#' in their number of documents (usually advisable), however, then the
#' empirically computed "docfreq" would be equivalent to "uniform" priors.
#'
#' Setting `prior` to "termfreq" makes the priors equal to the proportions
#' of total feature counts found in the grouped documents in each training
#' class, so that the classes with the largest number of features are assigned
#' the largest priors. If the total count of features in each training class was
#' the same, then "uniform" and "termfreq" would be the same.
#' @references Manning, C.D., Raghavan, P., & Schütze, H. (2008).
#'   *An Introduction to Information Retrieval*. Cambridge: Cambridge University Press
#'   (Chapter 13). Available at <https://nlp.stanford.edu/IR-book/pdf/irbookonlinereading.pdf>.
#'
#'   Jurafsky, D. & Martin, J.H. (2018).
#'   From *Speech and Language Processing: An Introduction to Natural Language
#'   Processing, Computational Linguistics, and Speech Recognition*. Draft of September 23, 2018
#'   (Chapter 6, Naive Bayes). Available at <https://web.stanford.edu/~jurafsky/slp3/>.
#'
#' @seealso [predict.textmodel_nb()]
#' @author Kenneth Benoit
#' @examples
#' ## Example from 13.1 of _An Introduction to Information Retrieval_
#' txt <- c(d1 = "Chinese Beijing Chinese",
#'          d2 = "Chinese Chinese Shanghai",
#'          d3 = "Chinese Macao",
#'          d4 = "Tokyo Japan Chinese",
#'          d5 = "Chinese Chinese Chinese Tokyo Japan")
#' trainingset <- dfm(txt, tolower = FALSE)
#' trainingclass <- factor(c("Y", "Y", "Y", "N", NA), ordered = TRUE)
#'
#' ## replicate IIR p261 prediction for test set (document 5)
#' (tmod1 <- textmodel_nb(trainingset, y = trainingclass, prior = "docfreq"))
#' summary(tmod1)
#' coef(tmod1)
#' predict(tmod1)
#'
#' # contrast with other priors
#' predict(textmodel_nb(trainingset, y = trainingclass, prior = "uniform"))
#' predict(textmodel_nb(trainingset, y = trainingclass, prior = "termfreq"))
#'
#' ## replicate IIR p264 Bernoulli Naive Bayes
#' tmod2 <- textmodel_nb(trainingset, y = trainingclass, distribution = "Bernoulli",
#'                       prior = "docfreq")
#' predict(tmod2, newdata = trainingset[5, ])
#' @export
textmodel_nb2 <- function(x, y, smooth = 1,
                         prior = c("uniform", "docfreq", "termfreq"),
                         distribution = c("multinomial", "Bernoulli")) {
    UseMethod("textmodel_nb")
}

#' @export
textmodel_nb2.default <- function(x, y, smooth = 1,
                                 prior = c("uniform", "docfreq", "termfreq"),
                                 distribution = c("multinomial", "Bernoulli")) {
    stop(friendly_class_undefined_message(class(x), "textmodel_nb"))
}

#' @export
textmodel_nb2.dfm <- function(x, y, smooth = 1,
                             prior = c("uniform", "docfreq", "termfreq"),
                             distribution = c("multinomial", "Bernoulli")) {
    x <- as.dfm(x)
    if (!sum(x)) stop(message_error("dfm_empty"))

    prior <- match.arg(prior)
    distribution <- match.arg(distribution)
    call <- match.call()

    y <- as.factor(y)
    if (stats::var(as.numeric(y), na.rm = TRUE) == 0) stop("y cannot be constant")

    temp <- x[!is.na(y),]
    class <- y[!is.na(y)]

    ## distribution
    if (distribution == "Bernoulli")
        temp <- dfm_weight(temp, "boolean", force = TRUE)

    temp <- dfm_group(temp, class, force = TRUE)

    freq <- rowSums(as.matrix(table(class)))
    if (prior == "uniform") {
        Pc <- rep(1 / ndoc(temp), ndoc(temp))
        names(Pc) <- docnames(temp)
    } else if (prior == "docfreq") {
        Pc <- freq
        Pc <- Pc / sum(Pc)
    } else if (prior == "termfreq") {
        Pc <- rowSums(temp)
        Pc <- Pc / sum(Pc)
    }

    if (distribution == "multinomial") {
        ## (formerly:)
        # PwGc <- dfm_weight(dfm_smooth(temp, smooth), scheme = "prop", force = TRUE)

        ## (experimental: compute as little as possible, as efficiently as possible)
        # copy temp as template, without additional meta stuff
        PwGc <- quanteda:::build_dfm(temp, features = featnames(temp),
                                     docvars = temp@docvars[, "docname_", drop = FALSE])

        # replace the counts with smoothed proportions
        totals <- rowSums(temp) + smooth * nfeat(temp)
        PwGc@x <- temp@x / totals[temp@i + 1]

        # replace the zeros with weight proportions
        PwGc[PwGc == 0] <- smooth / totals

    } else if (distribution == "Bernoulli") {
        # denominator here is same as IIR Fig 13.3 line 8 - see also Eq. 13.7
        PwGc <- (temp + smooth) / (freq + smooth * ndoc(temp))
        PwGc <- as(PwGc, "dgeMatrix")
    }

    # order Pc so that these are the same order as rows of PwGc
    Pc <- Pc[rownames(PwGc)]

    ## posterior: class x words, cols sum to 1
    PcGw <- colNorm(PwGc * base::outer(Pc, rep(1, ncol(PwGc))))

    # rename row dimensions
    names(dimnames(PcGw))[1] <- names(dimnames(PwGc))[1] <- "classes"

    ## P(w)
    Pw <- t(PwGc) %*% as.numeric(Pc)

    result <- list(
        PwGc = as.matrix(PwGc),
        Pc = Pc,
        PcGw = as.matrix(PcGw),
        Pw = as.matrix(Pw),
        x = x, y = y,
        distribution = distribution,
        prior = prior,
        smooth = smooth,
        call = call
    )
    class(result) <- c("textmodel_nb", "textmodel", "list")
    result
}
