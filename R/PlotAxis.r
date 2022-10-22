#' PlotAxis
#'
#' @export PlotAxis
#' @exportClass PlotAxis
PlotAxis <- R6::R6Class(
    classname = "PlotAxis",

    ## Properties
    private = list(
        .title = NULL,         # String
        .direction = NULL,     # PlotAxisDirection: Axis (Enum)
        .isContinous = FALSE,  # Bool
        .plotDataAxes = NULL   # List<PlotDataAxis>
    ),

    ## Methods
    public = list(
        initialize = function(title, plotDataAxes = list(), isContinous = FALSE) {
            if (!base::is.null(plotDataAxes))
            self$label <- label
            self$isContinous <- isContinous
            mapM_(function(x) self$addPlotDataAxis(x), plotDataAxes)
        },
        plot = function() {
            stop("Function PlotDataAxis$plot() must be overriden!")
        },
        addPlotDataAxis = function(axis) {
            if (!"PlotDataAxis" %in% class(axis)) stop("PlotAxis$addPlotDataAxis(): Axis must be a class instance of PlotDataAxis")
            if (rhaskell::null(self$plotDataAxes))
                self$direction <- axis$direction()
            self$plotDataAxes <- base::append(self$plotDataAxes, axis)
            self$isContinous <- self$isContinous() && axis$isContinous()
            if (self$direction() != axis$direction())
                stop("PlotAxis$addPlotDataAxis(): The directions have to be the same when adding a PlotDataAxis!")
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        plotDataAxes = function(value) {
            if (missing(value)) return(private$.plotDataAxes)
            if (!(base::is.list(value) && rhaskell::all))
                propError("plotDataAxes", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.plotDataAxes <- value
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
