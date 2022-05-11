
#' Function ot check for date
#'
#' @param x class to check if it is a date
#'
#' @export
is.date <- function(x) {
  return(inherits(x, c("Date", "POSIXt")))
}

propError <- function(name, value, srcFile, srcLoc) {
    val <- toString(value)
    if (nchar(val) > 40) val <- str_trunc(val, 40)
    stop("ERROR: Unallowed property '", val, "' for '", name, "' at ", srcFile, ":", srcLoc)
}
