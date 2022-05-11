#' Variable that will be present in output tree
#'
#' @export Variable
#' @exportClass Variable
Variable <- R6::R6Class(
    classname = "Variable",
    inherit = Node, # Every variable is a node

    ## Properties
    private = list(
        .vals = NULL,   # vector<numeric>
        .name = NULL    # character
    ),

    ## Methods
    public = list(
        initialize = function(name, vals, desc = NULL) {
            super$initialize(desc)
            self$name <- name
            self$vals <- vals
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        vals = function(value) {
            if (missing(value)) return(private$.vals)
            if (!(base::is.vector(value) && rhaskell::all(base::is.numeric, value)))
                propError(vals, value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.vals <- value
            return(self)
        },
        length = function() length(private$.vals),
        name = function(value) {
            if (missing(value)) return(private$.name)
            if (!(base::is.character(value)))
                propError(name, value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.name <- value
            return(self)
        }
    )

)
