#' PlotDataYAxis. Y-Axis of a plot.
#'
#' @export PlotDataYAxis
#' @exportClass PlotDataYAxis
PlotDataYAxis <- R6::R6Class(
    classname = "PlotDataYAxis",
    inherit = PlotDataAxis,

    ## Properties
    private = list(
    ),

    ## Methods
    public = list(
        initialize = function(label, data, isContinous = is.continous(data)) {
            super$initialize(label, data, Axis$Y, isContinous)
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
    )
)
