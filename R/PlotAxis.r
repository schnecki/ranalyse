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
        initialize = function(title, plotDataAxes = list()) {
            if (!base::is.null(plotDataAxes))
            self$title <- title
            rhaskell::mapM_(function(x) self$addPlotDataAxis(x), as.list(plotDataAxes))
        },
        plot = function() {
            ## stop("Function PlotDataAxis$plot() must be overriden!")
            warning("TODO: PlotAxis$plot(): Enhance axis plotting!")
            if (ranalyse::is.date(self$data[[1]])) {
                return(ggplot2::scale_x_date(title = self$title, date_breaks = "1 year", date_labels = "%Y")) # TODO: enhance
            } else if (self$isDiscrete) {
                return(base::switch(self$direction, ggplot2::scale_x_discrete(name = self$title), ggplot2::scale_y_discrete(name = self$title)))
            } else {
                return(base::switch(self$direction, ggplot2::scale_x_continuous(name = self$title), ggplot2::scale_y_continuous(name = self$title)))
            }
        },
        addPlotDataAxis = function(axis) {
            if (!"PlotDataAxis" %in% class(axis)) stop("PlotAxis$addPlotDataAxis(): Axis must be a class instance of PlotDataAxis")
            if (rhaskell::null(self$plotDataAxes)) {
                self$direction <- axis$direction
                self$isContinous <- axis$isContinous
            }
            self$plotDataAxes <- base::append(self$plotDataAxes, axis)
            self$isContinous <- self$isContinous && axis$isContinous
            if (self$direction != axis$direction)
                stop("PlotAxis$addPlotDataAxis(): The directions have to be the same when adding a PlotDataAxis!")
            return(self)
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        plotDataAxes = function(value) {
            if (missing(value)) return(private$.plotDataAxes)
            if (!(base::is.list(value) && rhaskell::all(function(x) "PlotDataAxis" %in% class(x), value)))
                propError("plotDataAxes", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.plotDataAxes <- value
            return(self)
        },
        title = function(value) {
            if (missing(value)) return(private$.title)
            if (!(base::is.character(value)))
                propError("title", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.title <- value
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
        },
        isDiscrete = function(value) {
            if (missing(value)) return(rhaskell::not(private$.isContinous))
            self$isContinous <- rhaskell::not(value)
            return(self)
        }

    )
)
