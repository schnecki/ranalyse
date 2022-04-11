#' Node that will be present in output tree
#'
#' This class defines a basis
#' @export Node
#' @exportClass Node
Node <- R6::R6Class(
    classname = "Node",

    ## Properties
    private = list(
        .prev = NULL,     # Maybe<Node>
        .next = NULL,     # Maybe<Node>
        .desc = NULL      # character
    ),

    ## Methods
    public = list(
        initialize = function(desc, prev) {
            self$desc <- desc
            self$prev <- prev
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        prev = function(value) {
            if (missing(value)) return(private$.prev)
            if (!("Node" %in% class(value)))
                stop("ERROR: Unallowed property ", value, " for 'prev' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.prev <- value
            return(self)
        },
        next = function(value) {
            if (missing(value)) return(private$.next)
            if (!("Node" %in% class(value)))
                stop("ERROR: Unallowed property ", value, " for 'next' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.next <- value
            return(self)
        },
        desc = function(value) {
            if (missing(value)) return(private$.desc)
            if (!(base::is.character(value)))
                stop("ERROR: Unallowed property ", value, " for 'desc' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.desc <- value
            return(self)
        }
    )

)
