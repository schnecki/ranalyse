#' Variable that will be present in output tree
#'
#' This class defines a basis
#' @export Variable
#' @exportClass Variable
Variable <- R6::R6Class(
    classname = "Variable",
    inherit = Node, # Every variable is a node

    ## Properties
    private = list(
        .vals = NULL,   # vector<numeric>
        .name = NULL   # character
    ),

    ## Methods
    public = list(
        initialize = function(name, vals) {
            self$name <- name
            self$vals <- vals
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        vals = function(value) {
            if (missing(value)) return(private$.vals)
            if (!(is.vector(value) && base::is.numeric(value)))
                stop("ERROR: Unallowed property ", value, " for 'vals' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.vals <- value
            return(self)
        },
        name = function(value) {
            if (missing(value)) return(private$.name)
            if (!(base::is.character(value)))
                stop("ERROR: Unallowed property ", value, " for 'name' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.name <- value
            return(self)
        }
    )

)
