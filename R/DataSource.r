#' DataSource interface.
#'
#' This class defines a `DataSource`.
#' @export DataSource
#' @exportClass DataSource
DataSource <- R6::R6Class(
    classname = "DataSource",
    inherit = Node,

    ## Properties
    private = list(
        .xVarName = NULL,     # char
        .variableDesc = NULL, # vector(varName = varDescription)
        .columns = Dict$new(a = NULL)$clear(), # Dict<Char, Vector>
        addColumn = function(name, data) {
            self$columns[name] <- as.vector(data)
        }
    ),

    ## Methods
    public = list(
        initialize = function(xVarName, variableDesc = NULL, desc = NULL) {
            super$initialize(desc)
            if (is.null(variableDesc))
                warning("No `variableDesc` given in `DataSource$initialize(..)`, hence using all available variables with empty description.")
            else
                self$variableDesc <- variableDesc
        },
        #' @param xColName Name of x-column, must be inside `variableDesc`.
        #' @param variableDesc a vector of variable names with descriptions. E.g. `ds$createVariables("x", c("x" = "timeline ...", "y" = "interesting data points"))`
        createDataSet = function() {
            vars <- self$variableDesc
            if (is.null(vars)) vars <- names(self$columns)
            if (!self$columns$has(self$xVarName)) {
                stop("Column '", self$xVarName, "' set as x-axis variable, but cannot be found in set of variables (columns).")
            }
            xVar <-
            ds <- DataSet$new(paste("Dataset, x-Var:", self$xVarName), self$xVarName)
            ds$parent <- self
            return(foldl(function(d, n) ds$addVariableFromData(n, self$columns[[n]]), ds, names(self$columns)))
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        xVarName = function(value) {
            if (missing(value)) return(private$.xVarName)
            if (!(base::is.character(value)))
                propError("xVarName", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.xVarName <- value
            return(self)
        },
        columns = function(value) {
            if (missing(value)) return(private$.columns)
            if (!("Dict" %in% class(value)))
                propError("columns", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.columns <- value
            return(self)
        }


    )

)
