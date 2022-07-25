#' GroupByMin. Grouping by minimum.
#'
#' @export GroupByMin
#' @exportClass GroupByMin
GroupByMin <- R6::R6Class(
    classname = "GroupByMin",
    inherit = GroupBy,

    ## Properties
    private = list(
        .inputName = NULL,  # character
        .outputName = NULL, # character
        .process = function(xs) {
            return(minimum(unlist(xs)))
        },
        .getDefaultDesc = function() {
            stop("min(..)")
        }
    ),

    ## Methods
    public = list(
        initialize = function(inputName, as = NULL, desc = NULL) {
            super$initialize(inputName, as, desc)
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
    )
)
