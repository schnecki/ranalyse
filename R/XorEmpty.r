#' XorEmpty. Select exactly one or no option. The latter can be disabled to get an actual Xor behaviour.
#'
#' @export XorEmpty
#' @exportClass XorEmpty
XorEmpty <- R6::R6Class(
    classname = "XorEmpty",
    inherit = BoolAlgebra,

    ## Properties
    private = list(
        .allowEmpty = TRUE # no selection is allowed too
    ),

    ## Methods
    public = list(
        #' @allowEmpty: allow no selection as well
        initialize = function(..., allowEmpty = TRUE) {
            super$initialize(...)
            self$allowEmpty <- allowEmpty

        },
        toOrAndList = function() {
            xs <- rhaskell::concat(rhaskell::map(function(x) x$toOrAndList(), self$input))
            if (self$allowEmpty && !rhaskell::any(function(ys) rhaskell::all(Literal$isEmptyLiteral, ys), xs))
                return(rhaskell::cons(base::list(Literal$new(NULL)), xs))
            else
                return(xs)
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        allowEmpty = function(value) {
            if (missing(value)) return(private$.allowEmpty)
            if (!(base::is.logical(value)))
                propError("allowEmpty", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.allowEmpty <- value
            return(self)
        }

    )
)
