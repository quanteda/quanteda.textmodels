#' Linear SVM classifier for texts
#'
#' Fit a fast linear SVM classifier for texts, using the R interface to the
#' svmlin code by Vikas Sindhwani and S. Sathiya Keerthi for fast linear
#' transductive SVMs. This is passed through to \code{\link[RSSL]{svmlin}} as
#' implemented by the \pkg{RSSL} package.
#'
#' @description This function has been retained for testing purposes only;
#'   we recommend that you use \code{\link{textmodel_svm}} instead.  That
#'   function is more efficient, and implements prediction for more than
#'   two classes.
#' @param x the \link{dfm} on which the model will be fit.  Does not need to
#'   contain only the training documents.
#' @param y vector of training labels associated with each document identified
#'   in \code{train}.  (These will be converted to factors if not already
#'   factors.)
#' @param intercept logical; if \code{TRUE}, add an intercept to the data
#' @param ... additional arguments passed to \code{\link[RSSL]{svmlin}}
#' @return
#' \code{textmodel_svmlin()} returns (for now) an object structured as a return
#' object from \code{\link[RSSL]{svmlin}}.
#' @references
#' Vikas Sindhwani and S. Sathiya Keerthi (2006).  Large Scale Semi-supervised
#' Linear SVMs. \emph{Proceedings of ACM SIGIR}.
#'
#' V. Sindhwani and S. Sathiya Keerthi (2006).  Newton Methods for Fast Solution of Semi-supervised
#' Linear SVMs. Book Chapter in \emph{Large Scale Kernel Machines}, MIT Press, 2006.
#'
#' @seealso \code{\link[RSSL]{svmlin}}, \code{text{textmodel_svm}}
#' @examples
#' # use Lenihan for govt class and Bruton for opposition
#' docvars(data_corpus_irishbudget2010, "govtopp") <- c("Govt", "Opp", rep(NA, 12))
#' dfmat <- dfm(data_corpus_irishbudget2010)
#'
#' tmod <- textmodel_svmlin(dfmat, y = docvars(dfmat, "govtopp"), pos_frac = 5/14)
#' predict(tmod)
#'
#' predict(textmodel_svmlin(dfmat, y = docvars(dfmat, "govtopp"), intercept = FALSE,
#'                       pos_frac = 5/14))
#' @import quanteda
#' @importFrom stats na.omit predict
#' @keywords textmodel internal
#' @export
textmodel_svmlin <- function(x, y, intercept = TRUE, ...) {
    UseMethod("textmodel_svmlin")
}

#' @export
textmodel_svmlin.default <- function(x, y, intercept = TRUE, ...) {
    stop(quanteda:::friendly_class_undefined_message(class(x), "textmodel_svmlin"))
}

#' @export
#' @importFrom RSSL svmlin
#' @importFrom methods as
textmodel_svmlin.dfm <- function(x, y, intercept = TRUE, ...) {
    x <- as.dfm(x)
    if (!sum(x)) stop(quanteda:::message_error("dfm_empty"))
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

# helper methods ----------------

#' Prediction from a fitted textmodel_svmlin object
#'
#' \code{predict.textmodel_svmlin()} implements class predictions from a fitted
#' linear SVM model.
#' @param object a fitted linear SVM textmodel
#' @param newdata dfm on which prediction should be made
#' @param type the type of predicted values to be returned; see Value
#' @param force make newdata's feature set conformant to the model terms
#' @param ... not used
#' @return \code{predict.textmodel_svmlin} returns either a vector of class
#'   predictions for each row of \code{newdata} (when \code{type = "class"}), or
#'   a document-by-class matrix of class probabilities (when \code{type =
#'   "probability"}).
#' @seealso \code{\link{textmodel_svmlin}}
#' @importFrom stats predict
#' @method predict textmodel_svmlin
#' @keywords textmodel internal
#' @export
predict.textmodel_svmlin <- function(object, newdata = NULL,
                                  type = c("class", "probability"),
                                  force = FALSE, ...) {
    quanteda:::unused_dots(...)

    type <- match.arg(type)

    if (!is.null(newdata)) {
        data <- as.dfm(newdata)
    } else {
        data <- as.dfm(object$x)
    }

    if (object$intercept) {
        data <- cbind(1, data)
        colnames(data)[1] <- "intercept"
    }
    data <- quanteda:::force_conformance(data, names(object$weights), force)

    pred_y <- as.numeric(data %*% object$weights)
    names(pred_y) <- docnames(data)

    if (type == "class") {
        pred_y <- ifelse(pred_y < 0, object$classnames[1], object$classnames[2])
    } else if (type == "probability") {
        stop("probability type not implemented yet")
    }

    pred_y
}

#' @export
#' @method print textmodel_svmlin
print.textmodel_svmlin <- function(x, ...) {
    cat("\nCall:\n")
    print(x$call)
    cat("\n",
        length(na.omit(x$y)), " training documents; ",
        nfeat(na.omit(x)), " fitted features.",
        "\n",
        "Method: ", x$algorithm, "\n",
        sep = "")
}

#' summary method for textmodel_svmlin objects
#' @param object output from \code{\link{textmodel_svmlin}}
#' @param n how many coefficients to print before truncating
#' @param ... additional arguments not used
#' @keywords textmodel internal
#' @method summary textmodel_svmlin
#' @importFrom utils head
#' @export
summary.textmodel_svmlin <- function(object, n = 30, ...) {
    result <- list(
        "call" = object$call,
        "estimated.feature.scores" = as.coefficients_textmodel(head(coef(object), n))
    )
    as.summary.textmodel(result)
}

#' @noRd
#' @method coef textmodel_svmlin
#' @export
coef.textmodel_svmlin <- function(object, ...) {
    object$weights
}

#' @noRd
#' @method coefficients textmodel_svmlin
#' @export
coefficients.textmodel_svmlin <- function(object, ...) {
    UseMethod("coef")
}

#' @export
#' @method print predict.textmodel_svmlin
print.predict.textmodel_svmlin <- function(x, ...) {
    print(unclass(x))
}
