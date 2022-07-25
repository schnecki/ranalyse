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
        },
        print = function(...) {
            if (is.null(self$getObject()))
                cat("<EMPTY> (<Literal>)")
            else
                cat(self$getObject(), "(<Literal>)", sep = " ")
            invisible(self)
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
    )
)

Literal$isEmptyLiteral <- function(l) {
    return(("Literal" %in% class(l) && base::is.null(l$getObject())) || is.null(l))
}

Literal$mkLiteral <- function(obj) {
    ## if ("OrAndList" %in% class(obj)) {
    ##     return(obj)
    ## } else
    if ("Literal" %in% class(obj)) {
        return(obj)
    } else if ("BoolAlgebra" %in% class(obj)) {
        return(obj) # OrAndList$new(obj$toOrAndList()))
    } else {
        return(Literal$new(obj))
    }
}


Literal$rmLiteral <- function(obj) {
    if ("Literal" %in% class(obj)) {
        return(obj$getObject())
    } else if ("BoolAlgebra" %in% class(obj)) {
        obj$input <- rhaskell::map(Literal$rmLiteral, obj$input)
    } else if (base::is.list(obj)) {
        obj <- rhaskell::map(Literal$rmLiteral, obj)
    }
    return(obj)
}
