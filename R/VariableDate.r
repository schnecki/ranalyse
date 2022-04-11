#' VariableDate is a Variable specialised to x being a Date
#'
#' This class defines a basis
#' @export VariableDate
#' @exportClass VariableDate
VariableDate <- R6::R6Class(
    classname = "VariableDate",
    inherit = Variable, # Is a variable

    ## Properties
    private = list(
        .xDate = NULL,     # vector<numeric>
        .y = NULL,     # vector<numeric>
        .name = NULL   # character
    ),

    ## Methods
    public = list(
        initialize = function(x, y, name) {
            super$initialize(x, y, name)
            ## self$x <- x
            ## self$y <- y
            ## self$name <- name
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        x = function(value) {
            if (missing(value)) return(private$.xDate)
            if (!(base::is.date(value)))
                stop("ERROR: Unallowed property ", value, " for 'xDate' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.xDate <- value
            return(self)
        }
    )

)
