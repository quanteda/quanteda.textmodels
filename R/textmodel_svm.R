#' Linear SVM classifier for texts
#'
#' Fit a fast linear SVM classifier for texts, using the
#' \pkg{LiblineaR} package.
#' @param x the [dfm] on which the model will be fit.  Does not need to
#'   contain only the training documents.
#' @param y vector of training labels associated with each document identified
#'   in `train`.  (These will be converted to factors if not already
#'   factors.)
#' @param weight weights for different classes for imbalanced training sets,
#'   passed to `wi` in [LiblineaR::LiblineaR()]. `"uniform"`
#'   uses default; `"docfreq"` weights by the number of training examples,
#'   and `"termfreq"` by the relative sizes of the training classes in
#'   terms of their total lengths in tokens.
#' @param type argument passed to the `type` argument in
#'   [LiblineaR::LiblineaR()]; default is `1` for L2-regularized L2-loss support
#'   vector classification (dual)
#' @param ... additional arguments passed to [LiblineaR::LiblineaR()]
#' @returns an object of class `textmodel_svm`, a list containing:
#' * `x`, `y`, `weights`, `type`: argument values from the call parameters
#' * `algorithm` character label of the algorithm used in the call to 
#'   [LiblineaR::LiblineaR()]
#' * `classnames` levels of `y`
#' * `bias` the value of `Bias` returned from [LiblineaR::LiblineaR()] 
#' * `svmlinfitted` the fitted model object passed from the call to 
#'   LiblineaR::LiblineaR()]
#' * `call` the model call
#' @references
#' R. E. Fan, K. W. Chang, C. J. Hsieh, X. R. Wang, and C. J. Lin. (2008)
#' LIBLINEAR: A Library for Large Linear Classification.
#' *Journal of Machine Learning Research* 9: 1871-1874.
#' <https://www.csie.ntu.edu.tw/~cjlin/liblinear/>.
#' @seealso [LiblineaR::LiblineaR()] [predict.textmodel_svm()]
#' @examples
#' # use party leaders for govt and opposition classes
#' library("quanteda")
#' docvars(data_corpus_irishbudget2010, "govtopp") <-
#'     c(rep(NA, 4), "Gov", "Opp", NA, "Opp", NA, NA, NA, NA, NA, NA)
#' dfmat <- dfm(tokens(data_corpus_irishbudget2010))
#' tmod <- textmodel_svm(dfmat, y = dfmat$govtopp)
#' predict(tmod)
#'
#' # multiclass problem - all party leaders
#' tmod2 <- textmodel_svm(dfmat,
#'     y = c(rep(NA, 3), "SF", "FF", "FG", NA, "LAB", NA, NA, "Green", rep(NA, 3)))
#' predict(tmod2)
#' @export
textmodel_svm <- function(x, y, weight = c("uniform", "docfreq", "termfreq"), type = 1, ...) {
    UseMethod("textmodel_svm")
}

#' @export
textmodel_svm.default <- function(x, y, weight = c("uniform", "docfreq", "termfreq"), type = 1, ...) {
    stop(check_class(class(x), "textmodel_svm"))
}

#' @importFrom LiblineaR LiblineaR
#' @importFrom SparseM as.matrix.csr
#' @importFrom quanteda dfm_weight dfm_group dfm_trim as.dfm
#' @export
textmodel_svm.dfm <- function(x, y, weight = c("uniform", "docfreq", "termfreq"), type = 1, ...) {
    x <- as.dfm(x)
    if (!sum(x)) stop(message_error("dfm_empty"))
    call <- match.call()
    weight <- match.arg(weight)

    # exclude NA in training labels
    x_train <- suppressWarnings(
        dfm_trim(x[!is.na(y), ], min_termfreq = .0000000001, termfreq_type = "prop")
    )
    y_train <- y[!is.na(y)]
    # remove zero-variance features
    constant_features <- which(apply(x_train, 2, stats::var) == 0)
    if (length(constant_features)) x_train <- x_train[, -constant_features]

    # set wi depending on weight value
    if (weight == "uniform") {
        wi <- NULL
    } else if (weight == "docfreq") {
        wi <- prop.table(table(y_train))
    } else if (weight == "termfreq") {
        wi <- rowSums(dfm_group(x_train, y_train))
        wi <- wi / sum(wi)
    }

    svmlinfitted <- LiblineaR::LiblineaR(as.matrix.csr.dfm(x_train),
                                         target = y_train, wi = wi, type = type, ...)
    colnames(svmlinfitted$W)[seq_along(featnames(x_train))] <- featnames(x_train)
    result <- list(
        x = x, y = y,
        weights = svmlinfitted$W,
        algorithm = svmlinfitted$TypeDetail,
        type = svmlinfitted$Type,
        classnames = svmlinfitted$ClassNames,
        bias = svmlinfitted$Bias,
        svmlinfitted = svmlinfitted,
        call = call
    )
    class(result) <- c("textmodel_svm", "textmodel", "list")
    result
}

# helper methods ----------------

#' Prediction from a fitted textmodel_svm object
#'
#' `predict.textmodel_svm()` implements class predictions from a fitted
#' linear SVM model.
#' @param object a fitted linear SVM textmodel
#' @param newdata dfm on which prediction should be made
#' @param type the type of predicted values to be returned; see Value
#' @param force make newdata's feature set conformant to the model terms
#' @param ... not used
#' @return `predict.textmodel_svm` returns either a vector of class
#'   predictions for each row of `newdata` (when `type = "class"`), or
#'   a document-by-class matrix of class probabilities (when `type =
#'   "probability"`).
#' @seealso [textmodel_svm()]
#' @keywords textmodel internal
#' @importFrom SparseM as.matrix.csr
#' @importFrom stats predict
#' @method predict textmodel_svm
#' @export
predict.textmodel_svm <- function(object, newdata = NULL,
                                  type = c("class", "probability"),
                                  force = TRUE, ...) {
    check_dots(...)

    type <- match.arg(type)

    if (!is.null(newdata)) {
        data <- as.dfm(newdata)
    } else {
        data <- as.dfm(object$x)
    }

    # the seq_along is because this will have an added term "bias" at end if bias > 0
    model_featnames <- colnames(object$weights)
    if (object$bias > 0) model_featnames <- model_featnames[-length(model_featnames)]

    data <- if (is.null(newdata))
        suppressWarnings(force_conformance(data, model_featnames, force))
    else
        force_conformance(data, model_featnames, force)

    if (type == "class") {
        pred_y <- predict(object$svmlinfitted, newx = as.matrix.csr.dfm(data), proba = FALSE)
        pred_y <- pred_y$predictions
        names(pred_y) <- docnames(data)
    } else if (type == "probability") {
        if (object$type != 0)
            stop("probability predictions not implemented for this model type")
        pred_y <- predict(object$svmlinfitted, newx = as.matrix.csr.dfm(data), proba = TRUE)
        pred_y <- pred_y$probabilities
        rownames(pred_y) <- docnames(data)
    }

    pred_y
}

#' @export
#' @method print textmodel_svm
print.textmodel_svm <- function(x, ...) {
    cat("\nCall:\n")
    print(x$call)
    cat("\n",
        format(length(na.omit(x$y)), big.mark = ","), " training documents; ",
        format(length(x$weights), big.mark = ","), " fitted features",
        ".\n",
        "Method: ", x$algorithm, "\n",
        sep = "")
}

#' summary method for textmodel_svm objects
#' @param object output from [textmodel_svm()]
#' @param n how many coefficients to print before truncating
#' @param ... additional arguments not used
#' @returns a `summary.textmodel` classed list containing the call and the
#'   estimated feature scores
#' @keywords textmodel internal
#' @method summary textmodel_svm
#' @export
summary.textmodel_svm <- function(object, n = 30, ...) {
    result <- list(
        "call" = object$call,
        "estimated.feature.scores" = as.coefficients_textmodel(head(coef(object), n))
    )
    as.summary.textmodel(result)
}

#' @noRd
#' @method coef textmodel_svm
#' @importFrom stats coef
#' @export
coef.textmodel_svm <- function(object, ...) {
    object$weights
}

#' @noRd
#' @method coefficients textmodel_svm
#' @importFrom stats coefficients
#' @export
coefficients.textmodel_svm <- function(object, ...) {
    UseMethod("coef")
}

#' @export
#' @method print predict.textmodel_svm
print.predict.textmodel_svm <- function(x, ...) {
    print(unclass(x))
}

#' convert a dfm into a matrix.csr from SparseM package
#'
#' Utility to convert a dfm into a [matrix.csr][SparseM::matrix.csr] from the \pkg{SparseM} package.
#' @param x input [dfm]
#' @param ... unused
#' @importFrom SparseM as.matrix.csr
#' @importFrom methods new
#' @method as.matrix.csr dfm
#' @returns a \pkg{SparseM} object of class [matrix.csr][SparseM::matrix.csr]
#' @keywords internal
as.matrix.csr.dfm <- function(x, ...) {
    # convert first to column sparse format
    as.matrix.csr(new("matrix.csc",
                      ra = x@x,
                      ja = x@i + 1L,
                      ia = x@p + 1L,
                      dimension = x@Dim))
}
