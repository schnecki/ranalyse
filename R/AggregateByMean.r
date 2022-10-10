#' AggregateByMean. Aggregation by mean.
#'
#' @export AggregateByMean
#' @exportClass AggregateByMean
AggregateByMean <- R6::R6Class(
    classname = "AggregateByMean",
    inherit = AggregateBy,

    ## Properties
    private = list(
        .inputName = NULL,  # character
        .outputName = NULL, # character
        .process = base::mean,
        .getDefaultFunName = function() {
            return("min")
        }
    ),

    ## Methods
    public = list(
        initialize = function(inputName, as = NULL, columnWise = TRUE, na.rm = TRUE, desc = NULL) {
            super$initialize(inputName, as, columnWise, na.rm, desc)
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
    )
)
