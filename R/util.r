
#' Function ot check for date
#'
#' @param x class to check if it is a date
#'
#' @export
is.date <- function(x) {
  return(inherits(x, c("Date", "POSIXt")))
}


## is.sequential <- function(x){
##   return(base::all(base::abs(base::diff(x)) == 1))
## }
##
