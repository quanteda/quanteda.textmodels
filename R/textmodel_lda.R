#' Latent Dirichlet allocation
#'
#' `textmodel_lda()` implements Latent Dirichlet allocation (LDA) based on Gibbs
#' sampling. The code adopted from the GibbsLDA++ library (Xuan-Hieu Phan,
#' 2007). `textmodel_seededlda()` allows identification of pre-defined topics by
#' semisupervided learning with a seed word dictionary.
#' @param x the dfm on which the model will be fit
#' @param k the number of topics
#' @param max_iter the maximum number of iteration in Gibbs sampling.
#' @param verbose logical; if `TRUE` print diagnostic information during
#'   fitting.
#' @param alpha the hyper parameter for topic-document distribution
#' @param beta the hyper parameter for topic-word distribution
#' @keywords textmodel experimental
#' @seealso [topicmodels][topicmodels::LDA]
#' @export
textmodel_lda <- function(
    x, k = 10, max_iter = 2000, alpha = NULL, beta = NULL, 
    verbose = quanteda_options("verbose")
) {
    UseMethod("textmodel_lda")
}

#' @export
textmodel_lda.dfm <- function(
    x, k = 10, max_iter = 2000, alpha = NULL, beta = NULL,
    verbose = quanteda_options("verbose")
) {
    
    label <- paste0("topic", seq_len(k))
    lda(x, k, label, max_iter, alpha, beta, NULL, seed, verbose)
}

#' @rdname textmodel_lda
#' @param dictionary a [dictionary][quanteda::dictionary] with seed words as 
#'  examples of topics.
#' @param residual if \code{TRUE} a residual topic (or "garbage topic") will be
#'   added to user-defined topics.
#' @param weight pseudo count given to seed words as a proportion of total
#'   number of words in `x`.
#' @param valuetype see [valuetype][quanteda::valuetype]
#' @param case_insensitive see [valuetype][quanteda::valuetype]
#' @references 
#'   Lu, Bin et al. (2011).
#'   [Multi-aspect Sentiment Analysis with Topic Models](https://dl.acm.org/doi/10.5555/2117693.2119585).
#'   *Proceedings of the 2011 IEEE 11th International Conference on Data Mining Workshops*.
#'   
#'   Watanabe, Kohei & Zhou, Yuan (2020).
#'   [Theory-Driven Analysis of Large Corpora: Semisupervised Topic Classification of the UN Speeches](https://doi.org/10.1177/0894439320907027).
#'   *Social Science Computer Review*.
#' @export
textmodel_seededlda <- function(
    x, dictionary, 
    valuetype = c("glob", "regex", "fixed"),
    case_insensitive = TRUE,
    residual = FALSE, weight = 0.01,
    max_iter = 2000, alpha = NULL, beta = NULL,
    verbose = quanteda_options("verbose")
) {
    UseMethod("textmodel_seededlda")
}

#' @export
textmodel_seededlda.dfm <- function(
    x, dictionary, 
    valuetype = c("glob", "regex", "fixed"),
    case_insensitive = TRUE,
    residual = FALSE, weight = 0.01,
    max_iter = 2000, alpha = NULL, beta = NULL,
    verbose = quanteda_options("verbose")
) {
    
    seeds <- tfm(x, dictionary, weight = weight, residual = residual)
    if (!identical(colnames(x), rownames(seeds)))
        stop("seeds must have the same features")
    k <- ncol(seeds)
    label <- colnames(seeds)
    lda(x, k, label, max_iter, alpha, beta, seeds, seed, verbose)
}

lda <- function(x, k, label, max_iter, alpha, beta, seeds, seed, verbose) {
    
    k <- as.integer(k)
    max_iter <- as.integer(max_iter)
    if (is.null(alpha)) {
        alpha <- -1.0 # default value will be set in C++
    } else {
        alpha <- as.double(alpha)
    }
    if (is.null(beta)) {
        beta <- -1.0 # default value will be set in C++
    } else {
        beta <- as.double(beta)
    }
    verbose <- as.logical(verbose)
    
    if (k < 1)
        stop("k must be larger than zero")
    
    if (is.null(seeds))
        seeds <- as(Matrix::sparseMatrix(nfeat(x), k), "dgCMatrix") # empty seed word matrix
    
    seed <- sample.int(.Machine$integer.max, 1) # seed for random number generation
    result <- qatd_cpp_lda(x, k, max_iter, alpha, beta, seeds, seed, verbose)
    
    dimnames(result$phi) <- list(label, colnames(x))
    dimnames(result$theta) <- list(rownames(x), label)
    result$x <- x
    result$max_iter <- max_iter
    result$call <- match.call()
    class(result) <- c("textmodel_lda", "textmodel", "list")
    return(result)
}

#' print method for a LDA model
#' @param x for print method, the object to be printed
#' @param ... unused
#' @method print textmodel_lda
#' @keywords internal textmodel
#' @export
print.textmodel_lda <- function(x, ...) {
    cat("\nCall:\n")
    print(x$call)
    cat("\n",
        "Topics: ", x$k, "; ",
        ndoc(x$x), " documents; ",
        nfeat(x$x), " features.",
        "\n",
        sep = "")
}

#' Extract most likely terms
#' @param x a fitted LDA model
#' @param n number of terms to be extracted
#' @export
terms <- function(x, n = 10) {
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
#' @noRd
tfm <- function(x, dictionary,
                valuetype = c("glob", "regex", "fixed"),
                case_insensitive = TRUE,
                weight = 0.01, residual = TRUE) {
    
    valuetype <- match.arg(valuetype)
    
    if (!quanteda::is.dictionary(dictionary))
        stop("dictionary must be a dictionary object")
    if (weight < 0)
        stop("weight must be pisitive a value")
    
    id_key <- id_feat <- integer()
    for (i in seq_along(dictionary)) {
        f <- colnames(quanteda::dfm_select(x, dictionary[i]))
        id_key <- c(id_key, rep(i, length(f)))
        id_feat <- c(id_feat, match(f, colnames(x)))
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
        dimnames = list(colnames(x), key)
    )
    quanteda::as.dfm(result)
}
