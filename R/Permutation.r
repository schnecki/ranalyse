#' Permutation interface.
#'
#' @export Permutation
#' @exportClass Permutation
Permutation <- R6::R6Class(
    classname = "Permutation",
    ## inherit = Option, # Every variable is a node

    ## Properties
    private = list(
        .options = NULL
    ),

    ## Methods
    public = list(
        initialize = function(...) {
            args <- list(...)
            inputs <- rhaskell::map(Option$mkOption, args)
            ##:ess-bp-start::conditional@:##
browser(expr={TRUE})##:ess-bp-end:##
            if (length(args) < 1) stop("Permutation$new(..) expects at least one input argument!")
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
