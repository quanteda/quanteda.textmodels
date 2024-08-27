check_dots <- quanteda:::check_dots
check_class <- quanteda:::check_class
message_error <- quanteda:::message_error
catm <- quanteda:::catm

#' Internal function to match a dfm features to a target set
#'
#' Takes a dfm and a set of features, and makes them match the features listed
#' in the set.
#' @param x input [dfm][quanteda::dfm]
#' @param features character; a vector of feature names
#' @param force logical; if `TRUE`, make the new dfm conform to the vector of
#'   features, otherwise return an error message
#' @returns a [dfm][quanteda::dfm] from the \pkg{quanteda} package containing
#'   only `features` as columns, in the same order as `features`.  A warning message
#'   is printed if some feature names from `features` are not matched in `x`.
#' @keywords internal dfm
#' @importFrom quanteda is.dfm featnames dfm_match
#' @examples
#' quanteda.textmodels:::force_conformance(quanteda::data_dfm_lbgexample, c("C", "B", "Z"))
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

#  Raise warning of unused dots
#  @param ... dots to check
# @keywords internal
# unused_dots <- function(...) {
#     arg <- names(list(...))
#     if (length(arg) == 1) {
#         warning(arg[1], " argument is not used.", call. = FALSE)
#     } else if (length(arg) > 1) {
#         warning(paste0(arg, collapse = ", "), " arguments are not used.", call. = FALSE)
#     }
# }


# #' Print friendly object class not defined message
# #'
# #' Checks valid methods and issues a friendlier error message in case the method is
# #' undefined for the supplied object type.
# #' @param object_class character describing the object class
# #' @param function_name character which is the function name
# #' @keywords internal
# #' @examples
# #' # as.tokens.default <- function(x, concatenator = "", ...) {
# #' #     stop(quanteda:::friendly_class_undefined_message(class(x), "as.tokens"))
# #' # }
# friendly_class_undefined_message <- function(object_class, function_name) {
#     valid_object_types <- as.character(utils::methods(function_name))
#     valid_object_types <- stringi::stri_replace_first_fixed(valid_object_types,
#                                                             paste0(function_name, "."), "")
#     valid_object_types <- valid_object_types[valid_object_types != "default"]
#    paste0(function_name, "() only works on ",
#          paste(valid_object_types, collapse = ", "),
#          " objects.")
#}



# #' Return an error message
# #' @param key type of error message
# #' @keywords internal
# message_error <- function(key = NULL) {
#     msg <- c("dfm_empty" = "dfm must have at least one non-zero value",
#              "fcm_empty" = "fcm must have at least one non-zero value",
#              "fcm_context" = "fcm must be created with a document context",
#              "matrix_mismatch" = "matrix must have the same rownames and colnames",
#              "docnames_mismatch" = "docnames must the the same length as x",
#              "docvars_mismatch" = "data.frame must have the same number of rows as documents",
#              "docvars_invalid" = "document variables cannot begin with the underscore",
#              "docvar_nofield" = "you must supply field name(s)",
#              "docvar_nocolname" = "data.frame must have column names")
#     if (is.null(key) || !key %in% names(msg)) {
#         return("")
#     }
#     return(unname(msg[key]))
# }

# rdname catm
# messages() with some of the same syntax as cat(): takes a sep argument and
# does not append a newline by default
# catm <- function(..., sep = " ", appendLF = FALSE) {
#     message(paste(..., sep = sep), appendLF = appendLF)
# }



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
