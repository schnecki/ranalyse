#' AggregateByMostOccuring. Grouping by minimum.
#'
#' @export AggregateByMostOccuring
#' @exportClass AggregateByMostOccuring
AggregateByMostOccuring <- R6::R6Class(
    classname = "AggregateByMostOccuring",
    inherit = AggregateBy,

    ## Properties
    private = list(
        .inputName = NULL,  # character
        .outputName = NULL, # character
        .process = function(xs) {
            maxVal <- NULL
            maxValStr <- NULL
            maxOcc <- -1
            tbl <- table(xs)
            for (i in length(tbl)) {
                occ <- tbl[[i]]
                if (occ > maxOcc) {
                    maxOcc <- occ
                    maxValStr <- names(tbl)[[i]]
                }
            }
            if (maxOcc < 0) return(NA)
            return(rhaskell::find(rhaskell::pEq(maxValStr) %comp% toString, xs)$fromMaybe(NA))
        },
        .getDefaultFunName = function() {
            return("sum")
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
