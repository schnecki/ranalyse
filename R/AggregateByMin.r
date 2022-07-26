#' AggregateByMin. Grouping by minimum.
#'
#' @export AggregateByMin
#' @exportClass AggregateByMin
AggregateByMin <- R6::R6Class(
    classname = "AggregateByMin",
    inherit = AggregateBy,

    ## Properties
    private = list(
        .inputName = NULL,  # character
        .outputName = NULL, # character
        .process = min,
        .getDefaultFunName = function() {
            return("min")
        }
    ),

    ## Methods
    public = list(
        initialize = function(inputName, as = NULL, columnWise = TRUE, rm.na = TRUE, desc = NULL) {
            super$initialize(inputName, as, columnWise, rm.na, desc)
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
    )
)
