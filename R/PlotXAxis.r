#' PlotXAxis. X-Axis of a plot. Must hold data.
#'
#' @export Plot
#' @exportClass Plot
PlotXAxis <- R6::R6Class(
    classname = "PlotXAxis",
    inherit = PlotAxis,

    ## Properties
    private = list(
        .data = NULL          # DataFrame
    ),

    ## Methods
    public = list(
        initialize = function(data, label = "x", isContinous = !base::is.integer(data)) {
            super$initialize(label, Axis$X, isContinous)
            self$data <- data
        },
        plot = function() {
            # TODO: set data type in plot object
            return(xlab(title = self$label))
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
        }
    )
)
