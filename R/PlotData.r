#' PlotData object.
#'
#' @export PlotData
#' @exportClass PlotData
PlotData <- R6::R6Class(
    classname = "PlotData",

    ## Properties
    private = list(
        .name = NULL, # Character
        .xVals = NULL, # DataFrame
        .yVals = NULL, # DataFrame

        #' Function returns type for plotting
        .autoDetectType = function() {
            return(inferPlotDataType(self$yVals))
        }

    ),

    ## Methods
    public = list(
        initialize = function(name, xVals, yVals) {
            self$name <- name
            if (base::nrow(xVals) != base::nrow(yVals))
                stop("PlotData$initialize(..): number of rows for x and y-values have to be equal!")
            self$xVals <- tibble::as.tibble(xVals)
            self$yVals <- tibble::as.tibble(yVals)
        },
        plot = function() {
            tp <- private$.autoDetectType()
            df <- tibble::add_column(self$xVals, self$yVals)
            xName <- attributes(df)$names[[1]]
            yName <- attributes(df)$names[[2]]

            ##:ess-bp-start::conditional@:##
browser(expr={TRUE})##:ess-bp-end:##
            if (tp == PlotDataType$GeomPoint)
                return(ggplot2::geom_point(data = df, mapping = aes(x = xName, y = yName), na.rm = TRUE))
            else if (tp == PlotDataType$GeomLine)
                return(ggplot2::geom_line(data = df, mapping = aes(x = xName, y = yName), na.rm = TRUE))
            else
                stop("Unkown plot type in PlotData.r: ", tp)
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        name = function(value) {
            if (missing(value)) return(private$.name)
            if (!(base::is.character(value)))
                propError("name", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.name <- value
            return(self)
        },
        xVals = function(value) {
            if (missing(value)) return(private$.xVals)
            if (!(base::is.data.frame(value)))
                propError("xVals", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.xVals <- value
            return(self)
        },
        yVals = function(value) {
            if (missing(value)) return(private$.yVals)
            if (!(base::is.data.frame(value)))
                propError("yVals", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.yVals <- value
            return(self)
        }
    )
)


#' Create a new `PlotData` object of the correct type.
PlotData$fromData <- function(name, xVals, data) {
    ## Convert matrices to tibbles (= data.frames) and check input type
    if (base::is.matrix(data)) data <- tibble::as_tibble(data)
    if (base::is.vector(data)) data <- tibble::as_tibble(data)
    if (base::is.list(data) && base::is.numeric(data[[1]])) data <- tibble::as_tibble(data)
    if (!base::is.data.frame(data)) stop("PlotData$fromData: expected a data.frame, matrix, vector or list of numeric values as input")

    if      (ranalyse::is.date(data[[1]])) return(PlotDataDate$new(name, xVals, data))
    else if (base::is.factor(data[[1]]))   return(PlotDataFactor$new(name, xVals, data))
    else if (base::is.logical(data[[1]]))  return(PlotDataBoolean$new(name, xVals, data))
    else if (base::is.character(data) && base::is.numeric(as.matrix(data)[[1]])) { # is numeric value in character string. convert.
        warning(paste0("Found numeric values as string in PlotData. Converting to numeric values!"))
        return(PlotData$new(name, xVals, as.numeric(data)))
    } else {
        return(PlotData$new(name, xVals, data))
    }
}
