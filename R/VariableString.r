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
        initialize = function(name, vals) {
            super$initialize(name, vals)
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        vals = function(value) {
            if (missing(value)) return(private$.vals)
            if (!(is.vector(value) && rhaskell::all(base::is.character, value)))
                stop("ERROR: Unallowed property ", head(value), " for 'vals' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}), ". Variable: ", self$name)
            private$.vals <- value
            return(self)
        }
    )

)
