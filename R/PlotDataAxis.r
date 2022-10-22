#' PlotDataAxis
#'
#' @export PlotDataAxis
#' @exportClass PlotDataAxis
PlotDataAxis <- R6::R6Class(
    classname = "PlotDataAxis",

    ## Properties
    private = list(
        .label = NULL,         # String
        .direction = NULL,     # PlotAxisDirection: Axis (Enum)
        .isContinous = FALSE,  # Bool
        .data = NULL           # Link to data
    ),

    ## Methods
    public = list(
        initialize = function(label, data, direction = Axis$X, isContinous = !base::is.integer(data)) {
            self$label <- label
            self$data <- data
            self$direction <- direction
            self$isContinous <- isContinous
        },
        plot = function() {
            stop("Function PlotDataAxis$plot() must be overriden!")
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        data = function(value) {
            if (missing(value)) return(private$.data)
            if (!(base::is.data.frame(value)))
                propError("data", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.data <- value
            return(self)
        },
        label = function(value) {
            if (missing(value)) return(private$.label)
            if (!(base::is.character(value)))
                propError("xAxis", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.label <- value
            return(self)
        },
        direction = function(value) {
            if (missing(value)) return(private$.direction)
            if (!(value == Axis$X || value == Axis$Y))
                propError("direction", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.direction <- value
            return(self)
        },
        isContinous = function(value) {
            if (missing(value)) return(private$.isContinous)
            if (!(base::is.logical(value)))
                propError("isContinous", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.isContinous <- value
            return(self)
        }
    )
)
