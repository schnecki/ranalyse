#' Interface fro GroupBy Aggregation Functions.
#'
#' @export GroupBy
#' @exportClass GroupBy
GroupBy <- R6::R6Class(
    classname = "GroupBy",
    inherit = NodeProcessor, # Every variable is a node

    ## Properties
    private = list(
        .inputName = NULL,  # character
        .outputName = NULL, # character

        #' Aggreate process function. Value that will be in the data set after processing input.
        #' @param inputValues Input values to be aggregated.
        .process = function(xs) {
            stop("Needs to be overwritten by the implementation.")
        },
        #' Default description for aggreage function being applied. Should be overwritten by class implementation.
        .getDefaultDesc = function() {
            stop("Needs to be overwritten by the implementation.")
        }
    ),

    ## Methods
    public = list(
        initialize = function(inputName, as = NULL, desc = NULL) {
            super$initialize(desc)
            self$inputName <- inputName
            self$outputName <- as
        },
        process = function(xs) {
            return(private$.process(xs))
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        inputName = function(value) {
            if (missing(value)) return(private$.inputName)
            if (!(base::is.character(value)))
                propError("inputName", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.inputName <- value
            return(self)
        },
        outputName = function(value) {
            if (missing(value)) return(private$.outputName)
            if (!(base::is.character(value) || base::is.null(value)))
                propError("outputName", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.outputName <- value
            return(self)
        }
    )
)
