
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
    # tb <- traceback() ## x = NULL, max.lines = getOption("traceback.max.lines", getOption("deparse.max.lines", -1L)))
    stop("ERROR: Unallowed property '", val, "' for '", name, "' at ", srcFile, ":", srcLoc)
}

## is.sequential <- function(x){
##   return(base::all(base::abs(base::diff(x)) == 1))
## }
##
