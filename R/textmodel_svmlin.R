#' \[experimental\] Linear SVM classifier for texts
#'
#' Fit a fast linear SVM classifier for sparse text matrices, using svmlin C++
#' code written by Vikas Sindhwani and S. Sathiya Keerthi.  This method
#' implements the modified finite Newton L2-SVM method (L2-SVM-MFN) method
#' described in Sindhwani and Keerthi (2006). Currently,
#' `textmodel_svmlin()` only works for two-class problems.
#'
#' @section Warning:
#' This function is marked experimental since it's not fully working yet in a
#' way that translates into more standard SVM parameters that we understand. Use
#' with caution after reading the Sindhwani and Keerthi (2006) paper.
#'
#' @param x the [dfm] on which the model will be fit.  Does not need to contain
#'   only the training documents.
#' @param y vector of training labels associated with each document identified
#'   in `train`.  (These will be converted to factors if not already factors.)
#' @param intercept logical; if `TRUE`, add an intercept to the data
#' @param lambda numeric; regularization parameter lambda (default 1)
#' @param cp numeric; Relative cost for "positive" examples (the second factor
#'   level)
#' @param cn numeric; Relative cost for "negative" examples (the first factor
#'   level)
#' @param scale	logical; if `TRUE`, normalize the feature counts
#' @param center logical; if `TRUE`, centre the feature counts
#' @return a fitted model object of class `textmodel_svmlin`
#' @references
#' Vikas Sindhwani and S. Sathiya Keerthi (2006).  [Large Scale Semi-supervised
#' Linear SVMs](https://vikas.sindhwani.org/sk_sigir06.pdf). *Proceedings of ACM
#' SIGIR*. August 6–11, 2006, Seattle.
#'
#' V. Sindhwani and S. Sathiya Keerthi (2006).  Newton Methods for Fast Solution
#' of Semi-supervised Linear SVMs. Book Chapter in *Large Scale Kernel
#' Machines*, MIT Press, 2006.
#'
#' @seealso [predict.textmodel_svmlin()]
#' @examples
#' # use Lenihan for govt class and Bruton for opposition
#' library("quanteda")
#' docvars(data_corpus_irishbudget2010, "govtopp") <- c("Govt", "Opp", rep(NA, 12))
#' dfmat <- dfm(tokens(data_corpus_irishbudget2010))
#'
#' tmod <- textmodel_svmlin(dfmat, y = dfmat$govtopp)
#' predict(tmod)
#' @importFrom quanteda dfm_group as.dfm
#' @importFrom stats na.omit predict
#' @keywords textmodel internal experimental
#' @export
textmodel_svmlin <- function(x, y, intercept = TRUE, # x_u = NULL,
                             lambda = 1,
                             # algorithm = 1,
                             # lambda_u = 1, max_switch = 10000, # pos_frac = 0.5,
                             cp = 1, cn = 1,
                             scale = FALSE, center = FALSE) {
    UseMethod("textmodel_svmlin")
}

#' @export
textmodel_svmlin.default <-  function(x, y, intercept = TRUE, # x_u = NULL,
                                      lambda = 1,
                                      # algorithm = 1,
                                      # lambda_u = 1, max_switch = 10000, # pos_frac = 0.5,
                                      cp = 1, cn = 1,
                                      scale = FALSE, center = FALSE) {
  stop(check_class(class(x), "textmodel_svmlin"))
}

#' @export
#' @importFrom methods as
textmodel_svmlin.dfm <-  function(x, y, intercept = TRUE, # x_u = NULL,
                                  lambda = 1,
                                  # algorithm = 1,
                                  # lambda_u = 1, max_switch = 10000, # pos_frac = 0.5,
                                  cp = 1, cn = 1,
                                  scale = FALSE, center = FALSE) {
    x <- as.dfm(x)
    if (!sum(x)) stop(message_error("dfm_empty"))
    call <- match.call()

    y <- factor(y)
    if (length(levels(y)) != 2) stop("y must contain two values only")

    temp <- x[!is.na(y), ]
    class <- y[!is.na(y)]
    temp <- dfm_group(temp, class, force = TRUE)

    svmlinfitted <- svmlin(X = as(temp, "CsparseMatrix"), #X_u = x_u,
                           y = factor(docnames(temp), levels = docnames(temp)),
                           intercept = intercept,
                           algorithm = 1, lambda = lambda,
                           # lambda_u = lambda_u,
                           # max_switch = max_switch,
                           # pos_frac = pos_frac,
                           Cp = cp, Cn = cn, verbose = FALSE,
                           scale = scale, x_center = center)

    result <- list(
        x = x, y = y,
        weights = svmlinfitted$weights,
        # algorithm = factor(svmlinfitted$algorithm, levels = 0:3,
        #                    labels = c("Regularized Least Squares Classification",
        #                               "SVM",
        #                               "Multi-switch Transductive SVM",
        #                               "Deterministic Annealing Semi-supervised SVM")),
        # classnames = svmlinfitted$classnames,
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
#' `predict.textmodel_svmlin()` implements class predictions from a fitted
#' linear SVM model.
#' @param object a fitted linear SVM textmodel
#' @param newdata dfm on which prediction should be made
#' @param type the type of predicted values to be returned; see Value
#' @param force logical, if `TRUE`, make newdata's feature set conformant to the
#'   model terms
#' @param ... not used
#' @return `predict.textmodel_svmlin` returns either a vector of class
#'   predictions for each row of `newdata` (when `type = "class"`), or
#'   a document-by-class matrix of class probabilities (when `type =
#'   "probability"`).
#' @seealso [textmodel_svmlin()]
#' @importFrom stats predict
#' @method predict textmodel_svmlin
#' @keywords textmodel internal
#' @export
predict.textmodel_svmlin <- function(object, newdata = NULL,
                                  type = c("class", "probability"),
                                  force = FALSE, ...) {
    check_dots(...)

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
    data <- force_conformance(data, names(object$weights), force)

    pred_y <- as.numeric(data %*% object$weights)
    names(pred_y) <- docnames(data)

    classnames <- levels(object$y)
    if (type == "class") {
        pred_y <- ifelse(pred_y < 0, classnames[1], classnames[2])
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
#' @param object output from [textmodel_svmlin()]
#' @param n how many coefficients to print before truncating
#' @param ... additional arguments not used
#' @returns a `summary.textmodel` classed list containing the call and the
#'   estimated feature scores
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



# adapted from RSSL -----------------

svmlin <- function(X, y, X_u = NULL, algorithm = 1, lambda = 1, lambda_u = 1, max_switch = 10000,
                   pos_frac = 0.5, Cp = 1.0, Cn = 1.0, verbose = FALSE,
                   intercept = TRUE, scale = FALSE, x_center = FALSE) {

  use_Xu_for_scaling <- TRUE
  classnames <- levels(y)

    if (scale || x_center) {
        if (intercept) {
            cols <- 2:ncol(X) # do not scale the intercept column
        } else {
            cols <- 1:ncol(X)
        }

        if (!is.null(X_u) && use_Xu_for_scaling) {
            Xe <- rbind(X, X_u)
            scaling <- scaleMatrix(Xe[, cols, drop = FALSE], center = TRUE, scale = scale)
            X[, cols] <- predict(scaling, as.matrix(X[, cols, drop=FALSE]))
            X_u[, cols] <- predict(scaling, as.matrix(X_u[, cols, drop=FALSE]))
        } else {
            scaling <- scaleMatrix(X[, cols, drop = FALSE], center = TRUE, scale = scale)
            X[, cols] <- predict(scaling, as.matrix(X[, cols, drop=FALSE]))
            if (!is.null(X_u)) {
                X_u[, cols] <- predict(scaling,as.matrix(X_u[, cols, drop=FALSE]))
            }
        }
    } else {
        scaling = NULL
    }

    y <- as.numeric(y) * 2 - 3

    # Combine feature matrices, add intercept and transpose them to conform to the C++ datastructure
    if (is.null(X_u) || algorithm < 2) {
        if (intercept) {
            X <- cbind(X, 1)
            X_u <- cbind(X_u, 1)
        }
        Xall <- Matrix::t(X)
    } else {
        if (intercept) {
            X <- cbind(X, 1)
            X_u <- cbind(X_u, 1)
        }
        Xall <- Matrix::t(Matrix::rbind2(X, X_u))
        y <- c(y, rep(0,nrow(X_u)))
    }

    # Determine costs
    costs <- rep(1, ncol(Xall))
    if (algorithm < 1) {
        costs[y < 0] <- Cn
        costs[y > 0] <- Cp
    }

    res <- cpp_svmlin(X = Xall, y = y, l = nrow(X), algorithm = algorithm,
                      lambda = lambda, lambda_u = lambda_u, max_switch = max_switch,
                      pos_frac = pos_frac, Cp = Cp, Cn = Cn, costs = costs,
                      verbose = verbose)

    list(classnames = classnames,
         weights = res$Weights,
         algorithm = algorithm,
         scaling = scaling,
         intercept = intercept)
}

setClass("scaleMatrix",
         representation(mean = "ANY", scale = "ANY"))

scaleMatrix <- function(x, center = TRUE, scale = TRUE) {
  if (center) {
    mean <- quanteda::colMeans(x)
    x <- sweep(x, 2, mean)
  } else {
    mean <- NULL
  }

  if (scale) {
    scale <- sqrt(colSums(sweep(x, 2, quanteda::colMeans(x)) ^ 2) / (nrow(x) - 1))
    x <- sweep(x, 2, scale, "/")
  } else {
    scale <- NULL
  }

  object <- new(Class="scaleMatrix", mean = mean, scale = scale)
  return(object)
}

setMethod("predict", signature=c("scaleMatrix"), function(object, newdata, ...) {
    if (!is.matrix(newdata)) stop("Incorrect newdata")
    if (!is.null(object@mean)) {
        newdata <- sweep(newdata, 2, object@mean)
    }
    if (!is.null(object@scale)) {
        newdata <- sweep(newdata, 2, object@scale, "/")
    }
    return(newdata)
})
