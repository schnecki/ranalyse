#' PlotDataGeomLine object.
#'
#' @export PlotDataGeomLine
#' @exportClass PlotDataGeomLine
PlotDataGeomLine <- R6::R6Class(
    classname = "PlotDataGeomLine",
    inherit = PlotData,

    ## Properties
    private = list(
        .linetype = NULL # int
    ),

    ## Methods
    public = list(
        initialize = function(name, xVals, yVals, na.rm = TRUE, colour = "#838B8B", size = 1.0, alpha = 1.0, linetype = NULL) {
            super$initialize(name, xVals, yVals, na.rm = na.rm, colour = colour, size = size, alpha = alpha)
            self$linetype <- linetype
        },
        #' Returns Function to use for plotting, e.g. one implemention could be `return(ggplot2::geom_line)`.
        getPlotFunction = function() {
            return(ggplot2::geom_line)
        },
        #' Returns aes mapping
        #'
        #' @param df DataFrame of x and y values
        getMapping = function(df) {
            xName <- base::attributes(df)$names[[1]]
            yName <- base::attributes(df)$names[[2]]
            return(ggplot2::aes_string(x = xName, y = yName))
        },
        #' Returns list of arguments for plot function. NULL values are filtered automatically.
        getAddPlotArgs = function() {
            return(list(linetype = self$linetype))
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        linetype = function(value) {
            if (missing(value)) return(private$.linetype)
            if (!(ranalyse::is.integer(value) || base::is.null(value)))
                stop("ERROR: Unallowed property ", value, " for 'linetype' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.linetype <- value
            return(self)
        }
    )
)
