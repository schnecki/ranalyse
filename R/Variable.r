#' Variable that will be present in output tree
#'
#' @export Variable
#' @exportClass Variable
Variable <- R6::R6Class(
    classname = "Variable",
    inherit = Node, # Every variable is a node

    ## Properties
    private = list(
        .vals = NULL,   # vector<numeric>
        .name = NULL    # character
    ),

    ## Methods
    public = list(
        initialize = function(name, vals, desc = NULL) {
            super$initialize(desc)
            self$name <- name
            self$vals <- vals
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        vals = function(value) {
            if (missing(value)) return(private$.vals)
            if (!(base::is.vector(value) && rhaskell::all(base::is.numeric, value)))
                propError("vals", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.vals <- value
            return(self)
        },
        length = function() length(private$.vals),
        name = function(value) {
            if (missing(value)) return(private$.name)
            if (!(base::is.character(value)))
                propError("name", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.name <- value
            return(self)
        }
    ),
    cloneable = FALSE
)

## Prevent cloning the data. Use a pointer and a new object instead.
Variable$set("public", "clone", function(deep = TRUE) {
    return(Variable$new(self$name, self$vals, self$desc))
})


#' Function used to create a new variable. It checks for the type of data and selects an appropriate
#' Variable class.
#'
#' @param name string Name of variable
#' @param data vector variable data
#' @param desc string Description of variable
#'
#' @export Variable$fromData
Variable$fromData <- function(name, data, desc = NULL) {
    if (is.date(data)) return(VariableDate$new(name, data))
    else if (is.factor(data)) return(VariableFactor$new(name, data))
    else if (is.logical(data)) return(VariableBoolean$new(name, data))
    else if (is.character(data) && is.na(as.numeric(data[[1]]))) return(VariableString$new(name, data))
    else if (is.character(data)) { # is numeric value in character string. convert.
        warning(paste0("Found numeric values variable ", name, ". Converting to numeric values!"))
        return(Variable$new(name, as.numeric(data)))
    } else return(Variable$new(name, as.vector(data)))
}
