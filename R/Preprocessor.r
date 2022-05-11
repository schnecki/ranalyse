#' Preprocessor interface.
#'
#' This class defines a basis
#' @export Preprocessor
#' @exportClass Preprocessor
Preprocessor <- R6::R6Class(
    classname = "Preprocessor",

    ## Properties
    private = list(
        .input = NULL, # vector<numeric>
        .y = NULL,     # vector<numeric>
        .name = NULL   # character
    ),

    ## Methods
    public = list(
        initialize = function(x, y, name) {
            super$initialize(x, y, name)
        },
        process = function() {
            stop("The function process must be overwritten by the Preprocessor sub-class!")
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        input = function(value) {
            if (missing(value)) return(private$.input)
            if (!(base::is.numeric(value) && base::is.vector(value)))
                propError(input, value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.input <- value
            return(self)
        }

    )

)
