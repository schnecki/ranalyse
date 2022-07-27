
#' Function ot check for date
#'
#' @param x class to check if it is a date
#'
#' @export
is.date <- function(x) {
  return(inherits(x, c("Date", "POSIXt")))
}

#' If-then-else
ite <- function(bool, then, elsePart) { if (bool) then else elsePart }

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

# ' Function to check if the model is supported/known. And we can work with it.
## isSupportedModel <- function(x) {
##     return("glm" %in% class(x))
## }


hash <- function(x) {
    return(digest::digest(x, algo = "xxhash32"))
}


#' Return the classname of an R6 object
#'
#' @param x        object    R6 class object
#' @param n.parent numeric   Return n.parent higher superclass. Default 0.
#' @return character Class name
#'
#' @export
getR6ClassName <- function(x, n.parent = 0) {
    return(get(class(x)[[n.parent + 1]], -1)$classname)
}

#' Return the classname of an R6 object
#'
#' @param x        object    R6 class object
#' @param n.parent numeric   Return n.parent higher superclass. Default 0.
#' @return character Class name
#'
#' @export
getR6Class <- function(x, n.parent = 0) {
    return(get(class(x)[[n.parent + 1]], -1))
}


#' Check if function is integer
#'
#' @param x        number
#' @return Bool
#'
#' @export is.integer
is.integer <- function(x) {
    return(base::is.numeric(x) && round(x) == x)
}

`%comp%` <- function(f, g) rhaskell::comp(f, g)
