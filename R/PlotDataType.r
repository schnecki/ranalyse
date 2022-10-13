
#' Plot Geom Type.
#'
#' @export GeomType
PlotDataType <- Enum(
    ## Values
    GeomPoint,
    GeomLine,
    GeomQuantile,
    StatBoxplot
    )


## Functions
#' @export inferPlotDataType
inferPlotDataType <- function(data) {
    if (!base::is.data.frame(data)) stop("GeomType$fromData: Expecting a DataFrame as input.")

    warning("TODO")
    return(PlotDataType$GeomPoint)
}
