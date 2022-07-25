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
            if (!((base::is.vector(value) || base::is.matrix(value)) && rhaskell::all(base::is.numeric, value)))
                propError("vals", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.vals <- value
            return(self)
        },
        ##' Number of values
        length = function() return(self$columns * self$rows),
        ##' Same as length
        columns = function() if (base::is.matrix(private$.vals)) dim(private$.vals)[[2]] else 1,
        ##' Same as length
        rows = function() if (base::is.matrix(private$.vals)) dim(private$.vals)[[1]] else length(private$.vals),
        ## Dimensions
        dim = function() if (base::is.matrix(private$.vals)) dim(private$.vals) else c(length(private$.vals), 1),
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
Variable$fromData <- function(name, data, desc = NULL) {
    if (ranalyse::is.date(data)) return(VariableDate$new(name, data, desc))
    else if (base::is.factor(data)) return(VariableFactor$new(name, data, desc))
    else if (base::is.logical(data)) return(VariableBoolean$new(name, data, desc))
    else if (base::is.character(data) && is.na(as.numeric(data[[1]]))) return(VariableString$new(name, data, desc))
    else if (base::is.matrix(data)) return(Variable$new(name, data, desc))
    else if (base::is.character(data)) { # is numeric value in character string. convert.
        warning(paste0("Found numeric values as string in variable ", name, ". Converting to numeric values!"))
        return(Variable$new(name, as.numeric(data), desc))
    } else {
        return(Variable$new(name, as.vector(data), desc))
    }
}
