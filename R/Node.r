#' Node that will be present in output tree
#'
#' This class defines a basis
#' @export Node
#' @exportClass Node
Node <- R6::R6Class(
    classname = "Node",

    ## Properties
    private = list(
        .parent = NULL,     # Maybe<Node>
        .childs = NULL,     # List<Node>
        .desc = NULL        # Maybe<character>
    ),

    ## Methods
    public = list(
        initialize = function(desc) {
            self$desc <- desc
        },
        addChild = function(child) {
            if (!("Node" %in% class(child)))
                propError(addChild, value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            self.childs <- append(self.childs, list(child))
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        parent = function(value) {
            if (missing(value)) return(private$.parent)
            if (!("Node" %in% class(value)))
                propError(parent, value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.parent <- value
            return(self)
        },
        childs = function(value) {
            if (missing(value)) return(private$.childs)
            if (!("Node" %in% class(value)))
                propError(childs, value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.childs <- value
            return(self)
        },
        desc = function(value) {
            if (missing(value)) return(private$.desc)
            if (!(base::is.character(value) || is.null(value)))
                propError(desc, value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.desc <- value
            return(self)
        }
    )

)
