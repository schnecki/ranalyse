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
        .columns = list(),    # Map<Char, Vector>
        addColumn = function(name, data) {
            self$columns[[name]] <- as.vector(data)
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
            ds <- DataSet$new(paste("Dataset, x-Var:", self$xVarName), )
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        xVarName = function(value) {
            if (missing(value)) return(private$.xVarName)
            if (!(base::is.character(value)))
                stop("ERROR: Unallowed property ", value, " for 'xVarName' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.xVarName <- value
            return(self)
        },
        columns = function(value) {
            if (missing(value)) return(private$.columns)
            if (!(base::is.list(value)))
                stop("ERROR: Unallowed property ", value, " for 'columns' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.columns <- value
            return(self)
        }


    )

)
