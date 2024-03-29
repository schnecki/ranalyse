' BoolAlgebra interface.
#'
#' @export BoolAlgebra
#' @exportClass BoolAlgebra
BoolAlgebra <- R6::R6Class(
    classname = "BoolAlgebra",

    ## Properties
    private = list(
        .input = NULL # list<Literal>
    ),

    ## Methods
    public = list(
        initialize = function(...) {
            args <- list(...)
            self$input <- rhaskell::map(Literal$mkLiteral, args)
        },
        toOrAndList = function() { # returns a list of disjunctions eich containing conjuntions of literals
            stop("Needs to be implemented")
        }

    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        input = function(value) {
            if (missing(value)) return(private$.input)
            private$.input <- value
            return(self)
        }
    )
)
