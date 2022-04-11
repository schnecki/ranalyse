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
        .x = NULL,     # vector<numeric>
        .y = NULL,     # vector<numeric>
        .name = NULL   # character
    ),

    ## Methods
    public = list(
        initialize = function(x, y, name) {
            self$x <- x
            self$y <- y
            self$name <- name
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        x = function(value) {
            if (missing(value)) return(private$.x)
            if (!(base::is.numeric(value)))
                stop("ERROR: Unallowed property ", value, " for 'x' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.x <- value
            return(self)
        },
        y = function(value) {
            if (missing(value)) return(private$.y)
            if (!(base::is.numeric(value)))
                stop("ERROR: Unallowed property ", value, " for 'y' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.y <- value
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
