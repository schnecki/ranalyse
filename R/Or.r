#' Or interface.
#'
#' @export Or
#' @exportClass Or
Or <- R6::R6Class(
    classname = "Or",
    inherit = BoolAlgebra,

    ## Properties
    private = list(
    ),

    ## Methods
    public = list(
        initialize = function(...) {
            super()$initialize(...)
        },
        toOrList = function() {
            return(rhaskell::map(base::list, self$input))
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
    )
)
