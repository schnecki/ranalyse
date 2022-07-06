#' Literal interface.
#'
#' @export Literal
#' @exportClass Literal
Literal <- R6::R6Class(
    classname = "Literal",

    ## Properties
    private = list(
        .object = NULL


    ),

    ## Methods
    public = list(
        initialize = function(object) {
            self$object <- object
        },
        getObject = function() {
            return(self$object)
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        object = function(value) {
            if (missing(value)) return(private$.object)
            if (("Literal" %in% class(value)))
                propError("object", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.object <- value
            return(self)
        }

    )
)


Literal$mkLiteral <- function(obj) {
    if ("Literal" %in% class(obj)) {
        return(obj)
    } else {
        return(Literal$new(obj))
    }
}
