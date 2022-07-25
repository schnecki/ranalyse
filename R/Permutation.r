#' Permutation interface.
#'
#' @export Permutation
#' @exportClass Permutation
Permutation <- R6::R6Class(
    classname = "Permutation",
    ## inherit = Literal, # Every variable is a node

    ## Properties
    private = list(
        .inputs = NULL
    ),

    ## Methods
    public = list(
        initialize = function(...) {
            super$initialize(...)
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
