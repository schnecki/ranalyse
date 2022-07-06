#' EitherOr interface.
#'
#' @export EitherOr
#' @exportClass EitherOr
EitherOr <- R6::R6Class(
    classname = "EitherOr",
    ## inherit = Option,

    ## Properties
    private = list(
        .options = NULL
    ),

    ## Methods
    public = list(
        initialize = function(...) {
            args <- list(...)
            self$options <- rhaskell::map(Option$mkOption, args)
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        options = function(value) {
            if (missing(value)) return(private$.options)
            if (!(base::is.list(value) && rhaskell::all(function(x) "Option" %in% class(x), value)))
                propError("options", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.options <- value
            return(self)
        }

    )
)
