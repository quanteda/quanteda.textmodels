#' Latent Dirichlet allocation
#' @keywords textmodel experimental
#' @export
textmodel_lda <- function(
    x, k = 10, max_iter = 2000, seeds = NULL, alpha = NULL, beta = NULL, 
    verbose = quanteda_options("verbose")
) {
    UseMethod("textmodel_lda")
}

#' @export
textmodel_lda.dfm <- function(
    x, k = 10, max_iter = 2000, seeds = NULL, alpha = NULL, beta = NULL,
    verbose = quanteda_options("verbose")
) {
    if (is.null(alpha))
        alpha <- -1.0
    if (is.null(beta))
        beta <- -1.0
    if (is.null(seeds)) {
        seeds <- as(Matrix::sparseMatrix(nfeat(x), k), "dgCMatrix")
        topic <- paste0("topic", seq_len(k))
    } else {
        if (!identical(colnames(x), rownames(seeds)))
            stop("seeds must have the same features")
        k <- ncol(seeds)
        topic <- colnames(seeds)
    }
    result <- qatd_cpp_lda(x, k, max_iter, alpha, beta, seeds, verbose)
    dimnames(result$phi) <- list(topic, colnames(x))
    dimnames(result$theta) <- list(rownames(x), topic)
    result$alpha <- alpha
    result$beta <- beta
    result$beta <- max_iter
    result$data <- x
    class(result) <- c("textmodel_lda")
    return(result)
}

#' Extract most likely terms
#' @param x a fitted LDA model
#' @param k number of terms to be extracted
#' @export
terms <- function(x, k = 10) {
    UseMethod("terms")
}
#' @export
#' @method terms textmodel_lda
terms.textmodel_lda <- function(x, n = 10) {
    apply(x$phi, 1, function(x, y, z) head(y[order(x, decreasing = TRUE), drop = FALSE], z), 
          colnames(x$phi), n)
}

#' Extract most likely topics
#' @export
#' @param x a fitted LDA model
topics <- function(x) {
    UseMethod("topics")
}
#' @export
#' @method topics textmodel_lda
topics.textmodel_lda <- function(x) {
    colnames(x$theta)[max.col(x$theta)]
}

#' Internal function to construct topic-feature matrix
#' @import Matrix quanteda
#' @noRd
tfm <- function(x, dictionary,
                valuetype = c("glob", "regex", "fixed"),
                case_insensitive = TRUE,
                weight = 0.01, residual = TRUE) {
    
    valuetype <- match.arg(valuetype)
    if (weight < 0)
        stop("weight must be pisitive a value")
    id_key <- id_feat <- integer()
    for (i in seq_along(dictionary)) {
        f <- featnames(dfm_select(x, dictionary[i]))
        id_key <- c(id_key, rep(i, length(f)))
        id_feat <- c(id_feat, match(f, featnames(x)))
    }
    count <- rep(floor(sum(x) * weight), length(id_feat))
    key <- names(dictionary)
    if (residual)
        key <- c(key, "other")
    result <- Matrix::sparseMatrix(
        i = id_feat,
        j = id_key,
        x = count,
        dims = c(nfeat(x), length(key)),
        dimnames = list(featnames(x), key)
    )
    as.dfm(result)
}
