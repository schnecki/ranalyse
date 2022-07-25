#' Permutation interface. The order of the values does not matter in this implementation!
#'
#' @export Permutation
#' @exportClass Permutation
Permutation <- R6::R6Class(
    classname = "Permutation",
    inherit = BoolAlgebra, # Every variable is a node

    ## Properties
    private = list(
        .inputs = NULL,
        .chooseNr = function(nr) {
            if (nr <= 0) return(list())
            startIdxs <- seq(from = 1, to = length(self$input) - nr + 1)
            return(rhaskell::map(function(start) self$input[start:(start + nr - 1)], startIdxs))
        }
    ),

    ## Methods
    public = list(
        initialize = function(...) {
            super$initialize(...)
        },
        toOrAndList = function() {
            orAnds <- rhaskell::map(function(nr) private$.chooseNr(nr), rhaskell::reverse(seq(1, length(self$input))))
            return(rhaskell::concat(orAnds))
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
