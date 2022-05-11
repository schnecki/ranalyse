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
        .vals = NULL,     # vector<numeric>
        .name = NULL   # character
    ),

    ## Methods
    public = list(
        initialize = function(name, vals, desc = NULL) {
            super$initialize(name, vals, desc)
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        vals = function(value) {
            if (missing(value)) return(private$.vals)
            if (!(rhaskell::all(is.date, value)))
                propError("vals", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.vals <- value
            return(self)
        }
    )

)
