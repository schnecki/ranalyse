#' And interface.
#'
#' @export And
#' @exportClass And
And <- R6::R6Class(
    classname = "And",
    inherit = BoolAlgebra,

    ## Properties
    private = list(
    ),

    ## Methods
    public = list(
        initialize = function(...) {
            args <- list(...)
            self$options <- rhaskell::map(Literal$mkLiteral, args)
        },
        toOrList = function() { # returns a list of disjunctions
            return(list(self$input))
        }

    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        options = function(value) {
            if (missing(value)) return(private$.options)
            if (!(base::is.list(value) && rhaskell::all(function(x) "Literal" %in% class(x), value)))
                propError("options", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.options <- value
            return(self)
        }

    )
)
