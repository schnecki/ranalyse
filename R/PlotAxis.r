#' PlotAxis
#'
#' @export Plot
#' @exportClass Plot
PlotAxis <- R6::R6Class(
    classname = "PlotAxis",

    ## Properties
    private = list(
        .label = NULL,         # String
        .direction = NULL,     # PlotAxisDirection: Axis (Enum)
        .isContinous = FALSE   # Bool
    ),

    ## Methods
    public = list(
        initialize = function(label, direction = Axis$X, isContinous = FALSE) {
            self$label <- label
            self$direction <- direction
            self$isContinous <- isContinous
        },
        plot = function() {
            # TODO: set data type in plot object
            if (self$direction == Axis$X) {
                return(xlab(title = self$label))
            } else if (self$direction == Axis$Y) {
                return(ylab(title = self$label))
            } else {
                stop("Unknown axis direction '", self$direction, "' in PlotAxis.r")
            }

        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
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
