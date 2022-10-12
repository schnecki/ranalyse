#' PlotDataFactor: For plotting a Factor type of data.
#'
#' @export PlotDataFactor
#' @exportClass PlotDataFactor
PlotDataFactor <- R6::R6Class(
    classname = "PlotDataFactor",
    inherit = PlotData,

    ## Properties
    private = list(
    ),

    ## Methods
    public = list(
        initialize = function(name, xVals, yVals) {
            super$initialize(name, xVals, yVals)
        },
        plot = function() {
            stop("TODO")
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
    )
)
