#' sequential neural network model for text classification
#'
#' This function is a wrapper for a sequential neural network model with a
#' single hidden layer network with two layers, implemented in the \pkg{keras}
#' package.
#' @inheritParams textmodel_svm
#' @param units The number of network nodes used in the first layer of the
#'   sequential model
#' @param dropout A floating variable bound between 0 and 1. It determines the
#'   rate at which units are dropped for the linear transformation of the
#'   inputs.
#' @param optimizer optimizer used to fit model to training data, see
#'   \code{\link[keras]{compile.keras.engine.training.Model}}
#' @param loss objective loss function, see
#'   \code{\link[keras]{compile.keras.engine.training.Model}}
#' @param metrics metric used to train algorithm, see
#'   \code{\link[keras]{compile.keras.engine.training.Model}}
#' @param ... additional options passed to
#'   \code{\link[keras]{fit.keras.engine.training.Model}}
#' @keywords textmodel
#' @importFrom keras keras_model_sequential to_categorical
#' @importFrom keras layer_dense layer_activation layer_dropout compile fit
#' @seealso [save.textmodel_nnseq()], [load.textmodel_nnseq()]
#' @export
#' @examples
#' \dontrun{
#' # create a dataset with evenly balanced coded and uncoded immigration sentences
#' corpcoded <- corpus_subset(data_corpus_manifestosentsUK, !is.na(crowd_immigration_label))
#' corpuncoded <- data_corpus_manifestosentsUK %>%
#'     corpus_subset(is.na(crowd_immigration_label) & year > 1980) %>%
#'     corpus_sample(size = ndoc(corpcoded))
#' corp <- corpcoded + corpuncoded
#'
#' # form a tf-idf-weighted dfm
#' dfmat <- dfm(corp) %>%
#'     dfm_tfidf()
#'
#' set.seed(1000)
#' tmod <- textmodel_nnseq(dfmat, y = docvars(dfmat, "crowd_immigration_label"),
#'                         epochs = 5, verbose = 1)
#' pred <- predict(tmod, newdata = dfm_subset(dfmat, is.na(crowd_immigration_label)))
#' table(pred)
#' tail(texts(corpuncoded)[pred == "Immigration"], 10)
#' }
textmodel_nnseq <- function(x, y, units = 512, dropout = .2,
                            optimizer = "adam",
                            loss = "categorical_crossentropy",
                            metrics = "categorical_accuracy",
                            ...) {
    UseMethod("textmodel_nnseq")
}

#' @export
textmodel_nnseq.dfm <- function(x, y, units = 512, dropout = .2,
                                optimizer = "adam",
                                loss = "categorical_crossentropy",
                                metrics = "categorical_accuracy", ...) {
    stopifnot(ndoc(x) == length(y))

    x <- as.dfm(x)
    y <- as.factor(y)
    result <- list(x = x, y = y, call = match.call(), classnames = levels(y))

    # trim missings for fitting model
    na_ind <- which(is.na(y))
    if (length(na_ind) > 0) {
        # message(length(na_ind), "observations with the value 'NA' were removed.")
        y <- y[-na_ind]
        x <- x[-na_ind, ]
    }

    # "one-hot" encode y
    y2 <- to_categorical(as.integer(y) - 1, num_classes = nlevels(y))

    # use keras to fit the model
    model <- keras_model_sequential() %>%
        layer_dense(units = units, input_shape = nfeat(x), activation = "relu") %>%
        layer_dropout(rate = dropout) %>%
        layer_dense(units = nlevels(y), activation = "softmax")
    compile(model, loss = loss, optimizer = optimizer, metrics = metrics)
    history <- fit(model, x, y2, ...)

    # compile, class, and return the result
    result <- c(result, nfeatures = nfeat(x), list(seqfitted = model))
    class(result) <- c("textmodel_nnseq", "textmodel", "list")
    return(result)
}

#' Prediction from a fitted textmodel_nnseq object
#'
#' \code{predict.textmodel_nnseq()} implements class predictions from a fitted
#' sequential neural network model.
#' @param object a fitted \link{textmodel_nnseq} model
#' @param newdata dfm on which prediction should be made
#' @param type the type of predicted values to be returned; see Value
#' @param force make \code{newdata}'s feature set conformant to the model terms
#' @param ... not used
#' @return \code{predict.textmodel_nnseq} returns either a vector of class
#'   predictions for each row of \code{newdata} (when \code{type = "class"}), or
#'   a document-by-class matrix of class probabilities (when \code{type =
#'   "probability"}).
#' @seealso \code{\link{textmodel_nnseq}}
#' @keywords textmodel internal
#' @importFrom keras predict_classes predict_proba
#' @export
predict.textmodel_nnseq <- function(object, newdata = NULL,
                                  type = c("class", "probability"),
                                  force = TRUE,
                                  ...) {
    quanteda:::unused_dots(...)

    type <- match.arg(type)

    if (!is.null(newdata)) {
        data <- as.dfm(newdata)
    } else {
        data <- as.dfm(object$x)
    }
    model_featnames <- colnames(object$x)
    data <- if (is.null(newdata)) {
        suppressWarnings(quanteda:::force_conformance(data, model_featnames, force))
    } else {
        quanteda:::force_conformance(data, model_featnames, force)
    }

    if (type == "class") {
        pred_y <- predict_classes(object$seqfitted, x = data)
        pred_y <- factor(pred_y, labels = object$classnames, levels = (seq_along(object$classnames) - 1))
        names(pred_y) <- docnames(data)
    } else if (type == "probability") {
        pred_y <- predict_proba(object$seqfitted, x = data)
        colnames(pred_y) <- object$classnames
        rownames(pred_y) <- docnames(data)
    }

    pred_y
}

#' @export
#' @method print textmodel_nnseq
print.textmodel_nnseq <- function(x, ...) {
    layer_names <- gsub(pattern = "_\\d*", "", lapply(x$seqfitted$layers, function(z) z$name))
    cat("\nCall:\n")
    print(x$call)
    cat("\n",
        format(length(na.omit(x$y)), big.mark = ","), " training documents; ",
        format(length(x$nfeatures), big.mark = ","), " fitted features",
        ".\n",
        "Structure: ", paste(layer_names, collapse = " -> "), "\n",
        sep = "")
}

#' summary method for textmodel_nnseq objects
#' @param object output from \code{\link{textmodel_nnseq}}
#' @param ... additional arguments not used
#' @keywords textmodel internal
#' @method summary textmodel_nnseq
#' @export
summary.textmodel_nnseq <- function(object, ...) {
    layer_names <- gsub(pattern = "_\\d*", "", lapply(object$seqfitted$layers, function(x) x$name))

    result <- list(
        "call" = object$call,
        "model structure" = paste(layer_names, collapse = " -> ")
    )
    as.summary.textmodel(result)
}

#' @export
#' @method print predict.textmodel_nnseq
print.predict.textmodel_nnseq <- function(x, ...) {
    print(unclass(x))
}

#' Load or save keras-based textmodels
#'
#' Functions for loading and saving \pkg{keras}-based models.  Because these are
#' stored as references, they need to be "serialized" prior to saving, or
#' serialized upon loading.  This applies to models fit using
#' [textmodel_cnnlstmemb()] and [textmodel_nnseq()].
#' @param x a \pkg{keras}-based fitted textmodel
#' @param ... additional arguments passed to [save()] or [load()]
#' @importFrom keras serialize_model
#' @keywords internal
#' @export
#' @method save textmodel_nnseq
save.textmodel_nnseq <- function(x, ...) {
    x$seqfitted <- serialize_model(x$seqfitted)
    save(x, ...)
}

#' @rdname save.textmodel_nnseq
#' @importFrom keras unserialize_model
#' @method load textmodel_nnseq
#' @export
load.textmodel_nnseq <- function(x, ...) {
    load(x, ...)
    x$seqfitted <- unserialize_model(x$seqfitted)
}
