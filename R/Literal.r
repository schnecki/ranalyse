#' Literal interface.
#'
#' @export Literal
#' @exportClass Literal
Literal <- R6::R6Class(
    classname = "Literal",
    inherit = BoolAlgebra,

    ## Properties
    private = list(
    ),

    ## Methods
    public = list(
        initialize = function(object) {
            private$.input <- list(object)
        },
        getObject = function() {
            return(self$input[[1]])
        },
        toOrAndList = function() {
            return(list(list(self)))
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
    )
)


Literal$mkLiteral <- function(obj) {
    if ("Literal" %in% class(obj)) {
        return(obj)
    } else {
        return(Literal$new(obj))
    }
}
