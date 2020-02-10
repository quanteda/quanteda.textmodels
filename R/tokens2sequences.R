#' Convert quanteda tokens to keras sequences
#'
#' This function converts a \pkg{quanteda} \code{\link[quanteda]{tokens}} object
#' into a tokens sequence object as expected by some functions in the
#' \pkg{keras} package.
#' @param x \code{\link[quanteda]{tokens}} object
#' @param maxsenlen the maximum sentence length kept in output matrix
#' @param keepn the maximum number of features to keep
#' @return \code{\link{tokens2sequences}} The output matrix has a number of rows
#'   which represent each tokenized sentence input into the function and a
#'   number of columns determined by \code{maxsenlen}. The matrix contains a
#'   numeric code for every unique token kept (determined by \code{keepn}) and
#'   they are arranged in the same sequence indicated by the original
#'   \code{\link[quanteda]{tokens}} object.
#' @export
#' @examples
#' corpcoded <- corpus_subset(data_corpus_manifestosentsUK, !is.na(crowd_immigration_label))
#' corpuncoded <- data_corpus_manifestosentsUK %>%
#'     corpus_subset(is.na(crowd_immigration_label) & year > 1980) %>%
#'     corpus_sample(size = ndoc(corpcoded))
#' corp <- corpcoded + corpuncoded
#'
#' corptok <- tokens(texts(corp))
#' print(corp)
#' seqs <- tokens2sequences(corptok, maxsenlen = 200)
#' print(seqs)
tokens2sequences <- function(x, maxsenlen = 40, keepn = NULL) {
    UseMethod("tokens2sequences")
}

#' @export
tokens2sequences.tokens <- function(x, maxsenlen = 40, keepn = NULL) {
    stopifnot(is.tokens(x))
    tfeq <- sort(table(unlist(x)), decreasing = T) # Creates a table of tokens and their frequencies sorted from most common to least
    doc_nam <- docnames(x) # Store docnames from tokens object for future use
    x <- unclass(x) # Convert tokens to integer IDs
    features <- attr(x, "types") # Store feature names
    data <- data.frame(features = features, # Create a dataframe that maps each token to its id and frequency
                       label1 = 1:length(features),
                       freq = as.integer(tfeq[features]),
                       stringsAsFactors = FALSE)
    attributes(x) <- NULL
    data <- data[order(data$freq, decreasing = T), ] # Reorders feature dictionary by frequency
    if (!is.null(keepn)) {
        data$label <- NA
        if (keepn > nrow(data)) keepn <- nrow(data) # Makes sure that we are not attempting to keep more features than exist
        data$label[1:keepn] <- 1:keepn # Subsets tokens to include only the n most common

    } else {
        data$label <- 1:nrow(data)
    }
    data <- data[order(data$label1, decreasing = F), ] # Orders by original numeric labels. This is done to allow 1:1 mapping of dictionary index numbers to original IDs
    x <- lapply(x, function(y) as.integer(na.omit(data$label[y]))) # Assign new, frequency-based IDs to word sequence list
    mat <- do.call("rbind", lapply(x, function(y) # Loops over each word sequence, making them of uniform length and binding them into a matrix
        if (length(y) >= maxsenlen)
            y[1:maxsenlen] # Subsets sentences that are longer than maxsenlen
        else
            c(rep(0L, times = maxsenlen - length(y)), y) # Adds zeros to ensure an even number of rows across word sequences and binds into a single data frame
        ))
    rownames(mat) <- doc_nam # Adds docname to each row of the matrix
    colnames(mat) <- as.character(1:maxsenlen) # Adds a numeric label to each column
    data <- data[!is.na(data$label), ] # Removes words that were not assigned numeric ids from the dictionary
    data <- data[order(data$label, decreasing = FALSE),
                 c("features", "label", "freq")] # selects feature names, ids, and frequency for dictionary and orders by frequency-based ID
    rownames(data) <- NULL # Resets rownames of dictionary
    output <- list(matrix = mat, nfeatures = nrow(data), features = data)
    class(output) <- c("tokens2sequences", "list")
    return(output)
}


#' @seealso \code{\link{tokens2sequences}}
#' @export
#' @method print tokens2sequences
print.tokens2sequences <- function(x, ...) {
    # calculate % sparse
    zeros <- sum(colSums(x$matrix == 0))
    tot <- nrow(x$matrix) * ncol(x$matrix)
    sparse_pct <- round(zeros / tot * 100, 1)

    # determine max number of features to print
    max_n <- ifelse(ncol(x$matrix) > 10, 10, ncol(x$matrix))

    # output
    cat("Ordered feature matrix of: ", format(nrow(x$matrix), big.mark = ","),
        " documents, ", format(x$nfeatures, big.mark = ","), " features ",
        "(", sparse_pct, "% sparse).\n", sep = "")
    cat(nrow(x$matrix), " x ", ncol(x$matrix),
        " Matrix of class \"tokens2sequences\" \n", sep = "")
    head(x$matrix[, 1:max_n], 4)
}

#' Match the feature names of one tokens2sequences object to another
#'
#' Converts the feature names of one tokens2sequences object to those of
#' another.  Useful in aligning training and test sets.
#' @param x \code{\link{tokens2sequences}} object that will be forced to conform
#' @param y \code{\link{tokens2sequences}} object whose feature names will be
#'   used to change token labels for \code{x}
#' @seealso \code{\link{tokens2sequences}}
#' @export
#' @examples
#' corpcoded <- corpus_subset(data_corpus_manifestosentsUK, !is.na(crowd_immigration_label))
#' corpuncoded <- data_corpus_manifestosentsUK %>%
#'     corpus_subset(is.na(crowd_immigration_label) & year > 1980) %>%
#'     corpus_sample(size = ndoc(corpcoded))
#'
#' tokx <- tokens(corpuncoded)
#' toky <- tokens(corpcoded)
#'
#' seqx <- tokens2sequences(tokx, maxsenlen = 50, keepn = 5000)
#' seqy <- tokens2sequences(toky, maxsenlen = 50, keepn = 5000)
#' tokens2sequences_conform(seqx, seqy)
tokens2sequences_conform <- function(x, y) {
    UseMethod("tokens2sequences_conform")
}

#' @export
tokens2sequences_conform.tokens2sequences <- function(x, y) {
    stopifnot(is.tokens2sequences(x) & is.tokens2sequences(y))
    joint_feat <- merge(x$features, y$features[, -3], by = "features",
                        all.x = TRUE)
    joint_feat <- joint_feat[order(joint_feat$label.x, decreasing = FALSE), ]
    mat <- apply(x$matrix, 1,
                 function(x) as.integer(na.omit(joint_feat$label.y[x])))
    mat <- do.call("rbind", lapply(mat, function(y)
        if (length(y) >= ncol(x$matrix))
            y[1:ncol(x$matrix)]
        else
            c(rep(0, times = ncol(x$matrix) - length(y)), y)
    ))
    rownames(mat) <- rownames(x$matrix)
    colnames(mat) <- colnames(x$matrix)
    joint_feat <- joint_feat[, c("features", "label.y", "freq")]
    names(joint_feat)[2] <- "label"
    joint_feat <- joint_feat[order(joint_feat$label, decreasing = F), ]
    joint_feat <- joint_feat[!is.na(joint_feat$label), ]
    rownames(joint_feat) <- NULL

    output <-
        list(matrix = mat, nfeatures = nrow(joint_feat), features = joint_feat)
    class(output) <- c("tokens2sequences", "list")
    return(output)
}

#' Check to see if function is a tokens2sequences type
#'
#'
#' @param x Object that will be checked to see if it is of the type \code{\link{tokens2sequences}}
#' @seealso \code{\link{tokens2sequences}}
#' @export
is.tokens2sequences <- function(x) {
    "tokens2sequences" %in% class(x)
}
