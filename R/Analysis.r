#' Analysis class
#'
#' This class defines a set of Analyses for an object of DataSets.
#' @export Analysis
#' @exportClass Analysis
Analysis <- R6::R6Class(
    classname = "Analysis",
    inherit = Node,

    ## Properties
    private = list(
        .name = NULL,       # character
        .dataset = NULL,    # DataSet
        .model = NULL       # Model object

    ),

    ## Methods
    public = list(
        initialize = function(name, dataset, model, desc = NULL) {
            super$initialize(desc)
            self$name <- name
            self$dataset <- dataset
            self$model <- model
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        name = function(value) {
            if (missing(value)) return(private$.name)
            if (!(base::is.character(value)))
                propError("name", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.name <- value
            return(self)
        },
        dataset = function(value) {
            if (missing(value)) return(private$.dataset)
            if (!("DataSet" %in% class(value)))
                propError("dataset", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.dataset <- value
            return(self)
        },
        model = function(value) {
            if (missing(value)) return(private$.model)
            if (!("Fitter" %in% class(value)))
                propError("model", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.model <- value
            return(self)
        }

    )

)
