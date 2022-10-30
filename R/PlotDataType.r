
#' Plot Geom Type.
#'
#' @export GeomType
PlotDataType <- Enum(
    ## Values
    GeomPoint,
    GeomLine,
    GeomRect,
    GeomQuantile,
    StatBoxplot

    )


## Functions
#' @export inferPlotDataType
inferPlotDataType <- function(data) {
    if (!base::is.data.frame(data)) stop("GeomType$fromData: Expecting a DataFrame as input.")

    ## idea: points for high variation in y-scale, lines otherwise
    warning("TODO")
    return(PlotDataType$GeomPoint)
}
