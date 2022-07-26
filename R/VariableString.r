#' VariableString is a Variable specialised to x being a String. See `base::factor`.
#'
#' @export VariableString
#' @exportClass VariableString
VariableString <- R6::R6Class(
    classname = "VariableString",
    inherit = Variable, # Is a variable

    ## Properties
    private = list(
        .vals = NULL,  # vector<factor>
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
            if (!(tibble::is_tibble(value) && rhaskell::all(base::is.character, value)))
                propError("vals", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.vals <- value
            return(self)
        }
    )

)
