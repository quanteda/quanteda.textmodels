#' Naive Bayes classifier for texts
#'
#' Fit a multinomial or Bernoulli Naive Bayes model, given a dfm and some
#' training labels.
#' @param x the [dfm] on which the model will be fit.  Does not need to
#'   contain only the training documents.
#' @param y vector of training labels associated with each document identified
#'   in `train`.  (These will be converted to factors if not already
#'   factors.)
#' @param smooth smoothing parameter for feature counts, added to the
#'   feature frequency totals by training class
#' @param prior prior distribution on texts; one of `"uniform"`,
#'   `"docfreq"`, or `"termfreq"`.  See Prior Distributions below.
#' @param distribution count model for text features, can be `multinomial`
#'   or `Bernoulli`.  To fit a "binary multinomial" model, first convert
#'   the dfm to a binary matrix using `[quanteda::dfm_weight](x, scheme = "boolean")`.
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
#'   Prior distributions refer to the prior probabilities assigned to the
#'   training classes, and the choice of prior distribution affects the
#'   calculation of the fitted probabilities.  The default is uniform priors,
#'   which sets the unconditional probability of observing the one class to be
#'   the same as observing any other class.
#'
#'   "Document frequency" means that the class priors will be taken from the
#'   relative proportions of the class documents used in the training set.  This
#'   approach is so common that it is assumed in many examples, such as the
#'   worked example from Manning, Raghavan, and Schütze (2008) below.  It is not
#'   the default in \pkg{quanteda}, however, since there may be nothing
#'   informative in the relative numbers of documents used to train a classifier
#'   other than the relative availability of the documents.  When training
#'   classes are balanced in their number of documents (usually advisable),
#'   however, then the empirically computed "docfreq" would be equivalent to
#'   "uniform" priors.
#'
#'   Setting `prior` to "termfreq" makes the priors equal to the proportions of
#'   total feature counts found in the grouped documents in each training class,
#'   so that the classes with the largest number of features are assigned the
#'   largest priors. If the total count of features in each training class was
#'   the same, then "uniform" and "termfreq" would be the same.
#' @section Smoothing parameter:
#'
#'   The `smooth` value is added to the feature frequencies, aggregated by
#'   training class, to avoid zero frequencies in any class.  This has the
#'   effect of giving more weight to infrequent term occurrences.
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
#' @importFrom quanteda dfm_weight dfm_group dfm_smooth as.dfm
#' @examples
#' ## Example from 13.1 of _An Introduction to Information Retrieval_
#' txt <- c(d1 = "Chinese Beijing Chinese",
#'          d2 = "Chinese Chinese Shanghai",
#'          d3 = "Chinese Macao",
#'          d4 = "Tokyo Japan Chinese",
#'          d5 = "Chinese Chinese Chinese Tokyo Japan")
#' x <- quanteda::dfm(txt, tolower = FALSE)
#' y <- factor(c("Y", "Y", "Y", "N", NA), ordered = TRUE)
#'
#' ## replicate IIR p261 prediction for test set (document 5)
#' (tmod1 <- textmodel_nb(x, y, prior = "docfreq"))
#' summary(tmod1)
#' coef(tmod1)
#' predict(tmod1, type = "prob")
#' predict(tmod1)
#'
#' # contrast with other priors
#' predict(textmodel_nb(x, y, prior = "uniform"))
#' predict(textmodel_nb(x, y, prior = "termfreq"))
#'
#' ## replicate IIR p264 Bernoulli Naive Bayes
#' tmod2 <- textmodel_nb(x, y, distribution = "Bernoulli", prior = "docfreq")
#' predict(tmod2, newdata = x[5, ], type = "prob")
#' predict(tmod2, newdata = x[5, ])
#' @export
textmodel_nb <- function(x, y, smooth = 1,
                         prior = c("uniform", "docfreq", "termfreq"),
                         distribution = c("multinomial", "Bernoulli")) {
    UseMethod("textmodel_nb")
}

#' @export
textmodel_nb.default <- function(x, y, smooth = 1,
                                 prior = c("uniform", "docfreq", "termfreq"),
                                 distribution = c("multinomial", "Bernoulli")) {
    stop(friendly_class_undefined_message(class(x), "textmodel_nb"))
}

#' @export
#' @importFrom quanteda colSums rowSums dfm_weight
textmodel_nb.dfm <- function(x, y, smooth = 1,
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

    if (distribution == "Bernoulli") {
        temp <- dfm_weight(temp, "boolean", force = TRUE)
    }

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
        PwGc <- dfm_weight(dfm_smooth(temp, smooth), scheme = "prop", force = TRUE)
    } else if (distribution == "Bernoulli") {
        PwGc <- (temp + smooth) / (freq + 2*smooth)
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

# helper methods ----------------

#' Prediction from a fitted textmodel_nb object
#'
#' `predict.textmodel_nb()` implements class predictions from a fitted
#' Naive Bayes model. using trained Naive Bayes examples
#' @param object a fitted Naive Bayes textmodel
#' @param newdata dfm on which prediction should be made
#' @param type the type of predicted values to be returned; see Value
#' @param force make newdata's feature set conformant to the model terms
#' @param ... not used
#' @return `predict.textmodel_nb` returns either a vector of class
#'   predictions for each row of `newdata` (when `type = "class"`), or
#'   a document-by-class matrix of class probabilities (when `type =
#'   "probability"`) or log posterior likelihoods (when `type =
#'   "logposterior"`).
#' @seealso [textmodel_nb()]
#' @examples
#' # application to LBG (2003) example data
#' (tmod <- textmodel_nb(data_dfm_lbgexample, y = c("A", "A", "B", "C", "C", NA)))
#' predict(tmod)
#' predict(tmod, type = "logposterior")
#' @keywords textmodel internal
#' @export
predict.textmodel_nb <- function(object, newdata = NULL,
                                 type = c("class", "probability", "logposterior"),
                                 force = FALSE, ...) {
    unused_dots(...)
    type <- match.arg(type)

    newdata <- if (!is.null(newdata)) as.dfm(newdata) else as.dfm(object$x)
    newdata <- force_conformance(newdata, colnames(object$PwGc), force)

    if (object$distribution == "multinomial") {

        # log P(d|c) class conditional document likelihoods
        loglik <- newdata %*% t(log(object$PwGc))

    } else if (object$distribution == "Bernoulli") {

        newdata <- dfm_weight(newdata, "boolean", force = TRUE)
        Nc <- length(object$Pc)

        present <- log(object$PwGc)
        nonpresent <- log(1 - object$PwGc)
        threshold <- .Machine$double.eps
        present[is.infinite(present)] <- max(-100000, log(threshold))
        nonpresent[is.infinite(nonpresent)] <- max(-100000, log(threshold))

        presence_prob <- newdata %*% t(present)
        nonpresence_prob <- matrix(base::colSums(t(nonpresent)),
                                   nrow = nrow(presence_prob),
                                   ncol = ncol(presence_prob), byrow = TRUE) - newdata %*% t(nonpresent)
        loglik <- presence_prob + nonpresence_prob

    }

    # weight by class priors
    logpos <- t(apply(loglik, 1, "+", log(object$Pc)))

    # predict MAP class
    nb.predicted <- colnames(logpos)[apply(logpos, 1, which.max)]

    if (type == "class") {

        names(nb.predicted) <- docnames(newdata)
        result <- factor(nb.predicted, levels = names(object$Pc))

    } else if (type == "probability") {

        result <- matrix(NA, ncol = ncol(logpos), nrow = nrow(logpos),
                            dimnames = dimnames(logpos))
        for (j in seq_len(ncol(logpos))) {
            base_lpl <- logpos[, j]
            result[, j] <- 1 / (1 + rowSums(exp(logpos[, -j, drop = FALSE] - base_lpl)))
        }

    } else if (type == "logposterior") {
        result <- logpos
    }

    result
}

#' @export
#' @method print textmodel_nb
print.textmodel_nb <- function(x, ...) {
    cat("\nCall:\n")
    print(x$call)
    cat("\n",
        "Distribution: ", x$distribution, "; ",
        "prior: ", x$prior, "; ",
        "smoothing value: ", x$smooth, "; ",
        length(na.omit(x$y)), " training documents; ",
        nfeat(na.omit(x)), " fitted features.",
        "\n",
        sep = "")
}

#' summary method for textmodel_nb objects
#' @param object output from [textmodel_nb()]
#' @param n how many coefficients to print before truncating
#' @param ... additional arguments not used
#' @keywords textmodel internal
#' @method summary textmodel_nb
#' @export
summary.textmodel_nb <- function(object, n = 30, ...) {
    result <- list(
        'call' = object$call,
        'class.priors' = as.coefficients_textmodel(object$Pc),
        'estimated.feature.scores' = as.coefficients_textmodel(head(coef(object), n))
    )
    as.summary.textmodel(result)
}

#' @noRd
#' @method coef textmodel_nb
#' @export
coef.textmodel_nb <- function(object, ...) {
    t(object$PcGw)
}

#' @noRd
#' @method coefficients textmodel_nb
#' @export
coefficients.textmodel_nb <- function(object, ...) {
    UseMethod("coef")
}

## make cols add up to one
colNorm <- function(x) {
    x / outer(rep(1, nrow(x)), colSums(x))
}

#' @export
#' @method print predict.textmodel_nb
print.predict.textmodel_nb <- function(x, ...) {
    print(unclass(x))
}
