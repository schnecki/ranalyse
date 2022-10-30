#' PlotDataGeomRect object.
#'
#' @export PlotDataGeomRect
#' @exportClass PlotDataGeomRect
PlotDataGeomRect <- R6::R6Class(
    classname = "PlotDataGeomRect",
    inherit = PlotData,

    ## Properties
    private = list(
        .autoDetectType = function() {
            return(PlotDataType$GeomRect)
        }
    ),

    ## Methods
    public = list(
        initialize = function(name, xMin, xMax, yMin, yMax, fill = "#D1EEEE33", alpha = 0.5) {
            super$initialize(name, base::data.frame(xMin = xMin, xMax = xMax), base::data.frame(yMin = yMin, yMax = yMax), plotDataType = PlotDataType$GeomRect, fill = fill, alpha = alpha)
        },
        plot = function() {
            df <- self$asDataFrame()
            ##:ess-bp-start::conditional@:##
browser(expr={TRUE})##:ess-bp-end:##
            return(ggplot2::geom_rect(data = df, aes(xmin = xMin, xmax = xMax, yMin = yMin, ymax = yMax), fill = self$fill, alpha = self$alpha))
            ## return(ggplot2::geom_point(data = df, mapping = ggplot2::aes_string(x = xName, y = yName), na.rm = TRUE))
        },
        #' Create X-Axis Information.
        mkPlotDataGeomRectXAxis = function() {
            return(PlotDataGeomRectXAxis$new(data = self$xVals, label = self$name))
        },
        #' Create Y-Axis Information.
        mkPlotDataGeomRectYAxis = function() {
            return(PlotDataGeomRectYAxis$new(data = self$yVals, label = self$name))
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
    )
)


#' Create a new `PlotDataGeomRect` object of the correct type.
PlotDataGeomRect$fromData <- function(name, xVals, data) {
    ## Convert matrices to tibbles (= data.frames) and check input type
    if (base::is.matrix(data)) data <- tibble::as_tibble(data)
    if (base::is.vector(data)) data <- tibble::as_tibble(data)
    if (base::is.list(data) && base::is.numeric(data[[1]])) data <- tibble::as_tibble(data)
    if (!base::is.data.frame(data)) stop("PlotDataGeomRect$fromData: expected a data.frame, matrix, vector or list of numeric values as input")

    if      (ranalyse::is.date(data[[1]])) return(PlotDataGeomRectDate$new(name, xVals, data))
    else if (base::is.factor(data[[1]]))   return(PlotDataGeomRectFactor$new(name, xVals, data))
    else if (base::is.logical(data[[1]]))  return(PlotDataGeomRectBoolean$new(name, xVals, data))
    else if (base::is.character(data) && base::is.numeric(as.matrix(data)[[1]])) { # is numeric value in character string. convert.
        warning(paste0("Found numeric values as string in PlotDataGeomRect. Converting to numeric values!"))
        return(PlotDataGeomRect$new(name, xVals, as.numeric(data)))
    } else {
        return(PlotDataGeomRect$new(name, xVals, data))
    }
}
