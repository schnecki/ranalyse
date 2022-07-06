#' Option interface.
#'
#' @export Option
#' @exportClass Option
Option <- R6::R6Class(
    classname = "Option",

    ## Properties
    private = list(
        .object = NULL


    ),

    ## Methods
    public = list(
        initialize = function(object) {
            self$object <- object
        },
        getObject = function() {
            return(self$object)
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        object = function(value) {
            if (missing(value)) return(private$.object)
            if (("Option" %in% class(value)))
                propError("object", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.object <- value
            return(self)
        }

    )
)


Option$mkOption <- function(obj) {
    if ("Option" %in% class(obj)) {
        return(obj)
    } else {
        return(Option$new(obj))
    }
}
