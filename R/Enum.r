#' Helper function for creating and locking Enum-like classes.
Enum <- function(...) {
    values <- sapply(match.call(expand.dots = TRUE)[-1L], deparse)
    stopifnot(identical(unique(values), values))
    res <- setNames(seq_along(values), values)
    res <- as.environment(as.list(res))
    lockEnvironment(res, bindings = TRUE)
    return(res)
}
