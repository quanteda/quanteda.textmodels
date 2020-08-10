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
    
    result <- qatd_cpp_lda(x, k, max_iter, alpha, beta, verbose)
    
    topic <- paste0("topic", seq_len(k))
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
