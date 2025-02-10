#' Logistic regression classifier for texts
#'
#' Fits a fast penalized maximum likelihood estimator to predict discrete
#' categories from sparse [dfm][quanteda::dfm] objects. Using the \pkg{glmnet}
#' package, the function computes the regularization path for the lasso or
#' elasticnet penalty at a grid of values for the regularization parameter
#' lambda.  This is done automatically by testing on several folds of the data
#' at estimation time.
#' @param x the [dfm][quanteda::dfm] on which the model will be fit.  Does not
#'   need to contain only the training documents.
#' @param y vector of training labels associated with each document identified
#'   in \code{train}.  (These will be converted to factors if not already
#'   factors.)
#' @param ... additional arguments passed to
#'   [`cv.glmnet()`][glmnet::cv.glmnet()]
#' @returns an object of class `textmodel_lr`, a list containing:
#' * `x`, `y` the input model matrix and input training class labels
#' * `algorithm` character; the type and family of logistic regression model used in calling
#'   [`cv.glmnet()`][glmnet::cv.glmnet()]
#' * `type` the type of associated with `algorithm`
#' * `classnames` the levels of training classes in `y`
#' * `lrfitted` the fitted model object from [`cv.glmnet()`][glmnet::cv.glmnet()]
#' * `call` the model call
#' @seealso [`cv.glmnet()`][glmnet::cv.glmnet()], [predict.textmodel_lr()],
#'   [coef.textmodel_lr()]
#' @references
#' Friedman, J., Hastie, T., & Tibshirani, R. (2010). Regularization Paths for
#' Generalized Linear Models via Coordinate Descent. _Journal of Statistical
#' Software_ 33(1), 1-22.  \doi{10.18637/jss.v033.i01}
#' @examples
#' ## Example from 13.1 of _An Introduction to Information Retrieval_
#' library("quanteda")
#' corp <- corpus(c(d1 = "Chinese Beijing Chinese",
#'                  d2 = "Chinese Chinese Shanghai",
#'                  d3 = "Chinese Macao",
#'                  d4 = "Tokyo Japan Chinese",
#'                  d5 = "London England Chinese",
#'                  d6 = "Chinese Chinese Chinese Tokyo Japan"),
#'                docvars = data.frame(train = factor(c("Y", "Y", "Y", "N", "N", NA))))
#' dfmat <- dfm(tokens(corp), tolower = FALSE)
#'
#' ## simulate bigger sample as classification on small samples is problematic
#' set.seed(1)
#' dfmat <- dfm_sample(dfmat, 50, replace = TRUE)
#'
#' ## train model
#' (tmod1 <- textmodel_lr(dfmat, docvars(dfmat, "train")))
#' summary(tmod1)
#' coef(tmod1)
#'
#' ## predict probability and classes
#' predict(tmod1, type = "prob")
#' predict(tmod1)
#' @export
textmodel_lr <- function(x, y, ...) {
    UseMethod("textmodel_lr")
}

#' @export
textmodel_lr.default <- function(x, y, ...) {
    stop(check_class(class(x), "textmodel_lr"))
}

#' @export
#' @importFrom glmnet cv.glmnet
#' @importFrom quanteda dfm_trim
textmodel_lr.dfm <- function(x, y, ...) {

    x <- as.dfm(x)
    if (!sum(x)) stop(message_error("dfm_empty"))
    call <- match.call()

    # exclude NA in training labels
    x_train <- suppressWarnings(
        dfm_trim(x[!is.na(y), ], min_termfreq = .0000000001,
                 termfreq_type = "prop")
    )
    y_train <- y[!is.na(y)]

    n_class <- if (is.factor(y_train)) {
        length(levels(y_train))
    } else {
        length(unique(y_train))
    }

    family <- if (n_class > 2) {
        "multinomial"
    } else if (n_class > 1) {
        "binomial"
    } else {
        stop("y must at least have two different labels.")
    }

    lrfitted <- glmnet::cv.glmnet(
        x = x_train,
        y = y_train,
        family = family,
        maxit = 10000,
        ...
    )

    if (family == "multinomial") {
      model_feat <- rownames(lrfitted[["glmnet.fit"]][["beta"]][[1]])
    } else {
      model_feat <- rownames(lrfitted[["glmnet.fit"]][["beta"]])
    }
    
    result <- list(
      x = force_conformance(x, model_feat, TRUE), 
      y = y,
      algorithm = paste(family, "logistic regression"),
      type = family,
      classnames = lrfitted[["glmnet.fit"]][["classnames"]],
      lrfitted = lrfitted,
      call = call
    )
    class(result) <- c("textmodel_lr", "textmodel", "list")
    result
}

# helper methods ----------------

#' Prediction from a fitted textmodel_lr object
#'
#' \code{predict.textmodel_lr()} implements class predictions from a fitted
#' logistic regression model.
#' @param object a fitted logistic regression textmodel
#' @param newdata dfm on which prediction should be made
#' @param type the type of predicted values to be returned; see Value
#' @param force make newdata's feature set conformant to the model terms
#' @param ... not used
#' @return `predict.textmodel_lr()` returns either a vector of class
#'   predictions for each row of `newdata` (when `type = "class"`), or
#'   a document-by-class matrix of class probabilities (when `type =
#'   "probability"``).
#' @seealso [textmodel_lr()]
#' @keywords textmodel internal
#' @importFrom stats predict
#' @method predict textmodel_lr
#' @export
predict.textmodel_lr <- function(object, newdata = NULL,
                                 type = c("class", "probability"),
                                 force = TRUE, ...) {
    type <- match.arg(type)
    if (type == "probability") {
        type <- "response"
    }

    if (!is.null(newdata)) {
        data <- as.dfm(newdata)
    } else {
        data <- as.dfm(object$x)
    }

    model_featnames <- colnames(object$x)
    data <- if (is.null(newdata)) {
        suppressWarnings(force_conformance(data, model_featnames, force))
    } else {
        force_conformance(data, model_featnames, force)
    }

    pred_y <- predict(
        object$lrfitted,
        newx = data,
        type = type,
        ...
    )
    if (type == "class") {
        pred_y <- as.factor(pred_y)
        names(pred_y) <-  quanteda::docnames(data)
    } else if (type == "response") {
        if (ncol(pred_y) == 1) {
            pred_y <- cbind(
                pred_y[, 1],
                1 - pred_y[, 1]
            )
            colnames(pred_y) <- rev(object$classnames)
        } else {
            pred_y <- pred_y[, , 1]
        }
    }
    pred_y
}

#' @export
#' @method print textmodel_lr
print.textmodel_lr <- function(x, ...) {
    cat("\nCall:\n")
    print(x$call)
    cat("\n",
        format(quanteda::ndoc(x$x), big.mark = ","), " training documents; ",
        format(quanteda::nfeat(x$x), big.mark = ","), " fitted features",
        ".\n",
        "Method: ", x$algorithm, "\n",
        sep = "")
}

#' @rdname predict.textmodel_lr
#' @method coef textmodel_lr
#' @return `coef.textmodel_lr()` returns a (sparse) matrix of coefficients for
#'   each feature, computed at the value of the penalty parameter fitted in the
#'   model.  For binary outcomes, results are returned only for the class
#'   corresponding to the second level of the factor response; for multinomial
#'   outcomes, these are computed for each class.
#' @importFrom stats coef
#' @export
coef.textmodel_lr <- function(object, ...) {
    if (object$type == "binomial") {
        out <- coef(object$lrfitted)
        colnames(out) <- object$classnames[2]
    } else if (object$type == "multinomial") {
        out <- coef(object$lrfitted)
        out <- do.call(cbind, out)
        colnames(out) <- object$classnames
    }
    out
}

#' @rdname predict.textmodel_lr
#' @method coefficients textmodel_lr
#' @importFrom stats coefficients
#' @export
coefficients.textmodel_lr <- function(object, ...) {
    UseMethod("coef")
}

#' summary method for textmodel_lr objects
#' @param object output from [textmodel_lr()]
#' @param n how many coefficients to print before truncating
#' @param ... additional arguments not used
#' @keywords textmodel internal
#' @returns a `summary.textmodel` classed list containing elements from the 
#' call to `textmodel_lr()`, including the call, statistics for lambda, and
#' the estimated feature scores
#' @method summary textmodel_lr
#' @export
summary.textmodel_lr <- function(object, n = 30, ...) {
    result <- list(
        "call" = object$call,
        # "folds" = object$nfolds,
        "lambda min" = object$lrfitted$lambda.min,
        "lambda 1se" = object$lrfitted$lambda.1se,
        "estimated.feature.scores" = as.matrix(head(coef(object), n))
    )
    as.summary.textmodel(result)
}
