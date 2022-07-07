#' rOr interface.
#'
#' @export Or
#' @exportClass Or
Or <- R6::R6Class(
    classname = "Or",
    inherit = BoolAlgebra,

    ## Properties
    private = list(
        .options = NULL
    ),

    ## Methods
    public = list(
        initialize = function(...) {
            super$initialize(...)
        },
        toOrAndList = function() { # returns a list of disjunctions
            ##:ess-bp-start::conditional@:##
browser(expr={TRUE})##:ess-bp-end:##
            res <- rhaskell::map(function(x) x$toOrAndList(), self$input)
            return(res)
        }

    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(

    )
)
