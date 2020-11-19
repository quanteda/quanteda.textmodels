unused_dots <- quanteda.core:::unused_dots

friendly_class_undefined_message <- quanteda.core:::friendly_class_undefined_message

message_error <- quanteda.core:::message_error

catm <- quanteda.core:::catm

#' Internal function to match a dfm features to a target set
#'
#' Takes a dfm and a set of features, and makes them match the features listed
#' in the set.
#' @param x input [dfm]
#' @param features character; a vector of feature names
#' @param force logical; if `TRUE`, make the new dfm conform to the vector of
#'   features, otherwise return an error message
#' @keywords internal dfm
#' @importFrom quanteda.core is.dfm featnames dfm_match
#' @examples
#' quanteda.textmodels:::force_conformance(data_dfm_lbgexample, c("C", "B", "Z"))
force_conformance <- function(x, features, force = TRUE) {
    if (!is.dfm(x))
        stop("x must be a dfm")
    if (force) {
        n <- length(featnames(x)) - length(intersect(featnames(x), features))
        if (n)
            warning(n, " feature", if (n == 1) "" else "s",
                    " in newdata not used in prediction.",
                    call. = FALSE, noBreaks. = TRUE)
        return(dfm_match(x, features))
    } else {
        if (!identical(featnames(x), features))
            stop("newdata's feature set is not conformant to model terms.")
        return(x)
    }
}

## make cols add up to one
colNorm <- function(x) {
    x / outer(rep(1, nrow(x)), colSums(x))
}

## fast way to group by class for sparse matrix
## outputs a dense matrix
group_classes <- function(x, y, smooth = 0) {
    levels <- levels(as.factor(y))
    x <- lapply(levels, function(lev) Matrix::colSums(x[y == lev, , drop = FALSE]) + smooth)
    names(x) <- levels
    do.call("rbind", x)
}
