#' (faster) Linear SVM classifier for texts
#'
#' Fit a fast linear SVM classifier for texts, using the R interface to the
#' svmlin code by Vikas Sindhwani and S. Sathiya Keerthi for fast linear
#' transductive SVMs. This is passed through to [RSSL::svmlin()] as
#' implemented by the \pkg{RSSL} package.
#'
#' @description This function has been retained for testing purposes only;
#'   we recommend that you use [textmodel_svm()] instead.  That
#'   function is more efficient, and implements prediction for more than
#'   two classes.
#' @param x the [dfm] on which the model will be fit.  Does not need to
#'   contain only the training documents.
#' @param y vector of training labels associated with each document identified
#'   in `train`.  (These will be converted to factors if not already
#'   factors.)
#' @param intercept logical; if `TRUE`, add an intercept to the data
#' @param ... additional arguments passed to [RSSL::svmlin()]
#' @return
#' `textmodel_svmlin()` returns (for now) an object structured as a return
#' object from [RSSL::svmlin()].
#' @references
#' Vikas Sindhwani and S. Sathiya Keerthi (2006).  Large Scale Semi-supervised
#' Linear SVMs. *Proceedings of ACM SIGIR*.
#'
#' V. Sindhwani and S. Sathiya Keerthi (2006).  Newton Methods for Fast Solution of Semi-supervised
#' Linear SVMs. Book Chapter in *Large Scale Kernel Machines*, MIT Press, 2006.
#'
#' @seealso [RSSL::svmlin()], \code{text{textmodel_svm}}
#' @examples
#' # use Lenihan for govt class and Bruton for opposition
#' quanteda::docvars(data_corpus_irishbudget2010, "govtopp") <-
#'     c("Govt", "Opp", rep(NA, 12))
#' dfmat <- quanteda::dfm(data_corpus_irishbudget2010)
#'
#' tmod <- textmodel_svmlin(dfmat, y = quanteda::docvars(dfmat, "govtopp"),
#'                          pos_frac = 5/14)
#' predict(tmod)
#'
#' predict(textmodel_svmlin(dfmat, y = quanteda::docvars(dfmat, "govtopp"),
#'                          intercept = FALSE, pos_frac = 5/14))
#' @importFrom quanteda dfm_group as.dfm
#' @importFrom stats na.omit predict
#' @keywords textmodel internal
#' @export
textmodel_svmlinRSSL <- function(x, y, intercept = TRUE, ...) {
    UseMethod("textmodel_svmlinRSSL")
}

#' @export
textmodel_svmlinRSSL.default <- function(x, y, intercept = TRUE, ...) {
    stop(friendly_class_undefined_message(class(x), "textmodel_svmlin"))
}

#' @export
#' @importFrom RSSL svmlin
#' @importFrom methods as
textmodel_svmlinRSSL.dfm <- function(x, y, intercept = TRUE, ...) {
    x <- as.dfm(x)
    if (!sum(x)) stop(message_error("dfm_empty"))
    call <- match.call()

    y <- factor(y)
    if (length(levels(y)) != 2) stop("y must contain two values only")

    temp <- x[!is.na(y), ]
    class <- y[!is.na(y)]
    temp <- dfm_group(temp, class, force = TRUE)

    svmlinfitted <- RSSL::svmlin(X = as(temp, "dgCMatrix"), X_u = NULL,
                                 y = factor(docnames(temp), levels = docnames(temp)),
                                 intercept = intercept,
                                 ...)
    result <- list(
        x = x, y = y,
        weights = svmlinfitted@weights,
        algorithm = factor(svmlinfitted@algorithm, levels = 0:3,
                           labels = c("Regularized Least Squares Classification",
                                      "SVM",
                                      "Multi-switch Transductive SVM",
                                      "Deterministic Annealing Semi-supervised SVM")),
        classnames = svmlinfitted@classnames,
        intercept = intercept,
        call = call
    )
    weightnames <- featnames(temp)
    if (intercept) weightnames <- c("intercept", weightnames)
    names(result$weights) <- weightnames
    class(result) <- c("textmodel_svmlin", "textmodel", "list")
    result
}
