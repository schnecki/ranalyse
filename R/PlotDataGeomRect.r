#' PlotDataGeomRect object.
#'
#' @export PlotDataGeomRect
#' @exportClass PlotDataGeomRect
PlotDataGeomRect <- R6::R6Class(
    classname = "PlotDataGeomRect",
    inherit = PlotData,

    ## Properties
    private = list(
        .linetype = NULL, # numeric

        .xMargin = NULL, # float
        .yMargin = NULL # float
    ),

    ## Methods
    public = list(
        initialize = function(name, xMin, xMax, yMin, yMax, xMargin = 0.0, yMargin = 0.1, na.rm = TRUE, colour = NULL, fill = "#D1EEEE33", alpha = 0.5, size = NULL, linetype = NULL) {
            super$initialize(name, base::data.frame(xMin = xMin, xMax = xMax), base::data.frame(yMin = yMin, yMax = yMax)
                           , na.rm = na.rm, colour = colour, fill = fill, size = size, alpha = alpha)
            self$xMargin <- xMargin
            self$yMargin <- yMargin
        },
        #' Returns Function to use for plotting, e.g. one implemention could be `return(ggplot2::geom_line)`.
        getPlotFunction = function() {
            return(ggplot2::geom_rect)
        },
        #' Returns aes mapping
        #'
        #' @param df DataFrame of x and y values
        getMapping = function(df) {
            ## xDiff <- base::abs(df$xMax - df$xMin)
            ## yDiff <- base::abs(df$yMax - df$yMin)
            ## dfMargin <- base::data.frame(xMin = df$xMin - self$xMargin * xDiff, xMax = df$xMax + self$xMargin * xDiff, yMin = df$yMin - self$yMargin * yDiff, yMax = df$yMax + self$yMargin * yDiff)
            ## return(tibble::add_column(self$xVals, self$yVals))

            return(ggplot2::aes(xmin = xMin - self$xMargin * base::abs(xMax - xMin), xmax = xMax + self$xMargin * base::abs(xMax - xMin),
                                ymin = yMin - self$yMargin * base::abs(yMax - yMin), ymax = yMax + self$yMargin * base::abs(yMax - yMin)))
        },
        #' Returns list of arguments for plot function. NULL values are filtered automatically.
        getAddPlotArgs = function() {
            return(list(alpha = self$alpha
                      , colour = self$colour
                      , fill = self$fill
                      , linetype = self$linetype
                      , size = self$size
                      , na.rm = self$na.rm))
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        linetype = function(value) {
            if (missing(value)) return(private$.linetype)
            if (!(base::is.numeric(value) || base::is.null(value)))
                stop("ERROR: Unallowed property ", value, " for 'linetype' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.linetype <- value
            return(self)
        },
        xMargin = function(value) {
            if (missing(value)) return(private$.xMargin)
            if (!(base::is.numeric(value)))
                stop("ERROR: Unallowed property ", value, " for 'xMargin' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.xMargin <- value
            return(self)
        },
        yMargin = function(value) {
            if (missing(value)) return(private$.yMargin)
            if (!(base::is.numeric(value)))
                stop("ERROR: Unallowed property ", value, " for 'yMargin' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.yMargin <- value
            return(self)
        }
    )
)
