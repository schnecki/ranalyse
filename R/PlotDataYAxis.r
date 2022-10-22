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
        initialize = function(label, data, isContinous = !base::is.integer(data)) {
            super$initialize(label, data, Axis$Y, isContinous)
        },
        plot = function() {

            warning("TODO: PlotDataYAxis$plot() enhance axis plotting")
            if (ranalyse::is.date(self$data[[1]])) {
                return(ggplot2::scale_y_date(title = self$label, date_breaks = "1 year", date_labels = "%Y")) # TODO: enhance
            } else if (base::is.factor(self$data[[1]]) || base::is.integer(self$data[[1]])) {
                return(ggplot2::scale_y_discrete(self$label))
            } else {
                return(ggplot2::scale_y_continuous(self$label))
            }

        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
    )
)
