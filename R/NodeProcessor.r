#' NodeProcessor that will be present in output tree
#'
#' This class defines a basis
#' @export NodeProcessor
#' @exportClass NodeProcessor
NodeProcessor <- R6::R6Class(
    classname = "NodeProcessor",
    inherit = Node,

    ## Properties
    private = list(
    ),

    ## Methods
    public = list(
        initialize = function(desc) {
            super$initialize(desc)
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        isProcessNodeProcessor = function() TRUE
    )

)
