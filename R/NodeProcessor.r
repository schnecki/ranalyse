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
        .processors = list() # list<Node>
    ),

    ## Methods
    public = list(
        initialize = function(desc) {
            super$initialize(desc)
        },
        addProcessor = function(proc) {
            self$processors <- base::append(self$processors, list(proc))
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        isProcessNodeProcessor = function() TRUE,
        processors = function(value) {
            if (missing(value)) return(private$.processors)
            if (!(base::is.list(value) && rhaskell::all(function(c) "Node" %in% class(c), value)))
                propError("processors", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.processors <- value
            return(self)
        }
    )

)
