#' PlotDataGeomPoint object.
#'
#' @export PlotDataGeomPoint
#' @exportClass PlotDataGeomPoint
PlotDataGeomPoint <- R6::R6Class(
    classname = "PlotDataGeomPoint",
    inherit = PlotData,

    ## Properties
    private = list(
        .shape = NULL, # int or character
        .stroke = NULL # int
    ),

    ## Methods
    public = list(
        initialize = function(name, xVals, yVals, na.rm = TRUE, colour = "#838B8B", fill = NULL, size = 1.0, alpha = 1.0, shape = NULL, stroke = NULL) {
            super$initialize(name, xVals, yVals, na.rm = na.rm, colour = colour, size = size, alpha = alpha)
        },
        #' Returns Function to use for plotting, e.g. one implemention could be `return(ggplot2::geom_line)`.
        getPlotFunction = function() {
            return(ggplot2::geom_point)
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
            return(list(shape = self$shape
                      , stroke = self$stroke
                      ))
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        shape = function(value) {
            if (missing(value)) return(private$.shape)
            if (!(ranalyse::is.integer(value) || base::is.null(value) || base::is.character(value)))
                stop("ERROR: Unallowed property ", value, " for 'shape' at ", getSrcFileshape(function(){}), ":", getSrcLocation(function(){}))
            private$.shape <- value
            return(self)
        },
        stroke = function(value) {
            if (missing(value)) return(private$.stroke)
            if (!(ranalyse::is.integer(value) || base::is.null(value)))
                stop("ERROR: Unallowed property ", value, " for 'stroke' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.stroke <- value
            return(self)
        }

    )
)
