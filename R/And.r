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
            super$initialize(...)
        },
        toOrAndList = function() { # returns a list of disjunctions
            orAnds <- rhaskell::map(function(x) x$toOrAndList(), self$input)
            if (length(orAnds) <= 0) return(list(list()))
            if (length(orAnds) == 1) return(orAnds[[1]])
            fun <- function(acc, ors) {
                res <- rhaskell::concatMap(function(accAnds) rhaskell::map(function(ands) base::append(accAnds, ands), ors), acc)
                return(res)
            }
            return(rhaskell::foldl(fun, rhaskell::head(orAnds), rhaskell::tail(orAnds)))
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
