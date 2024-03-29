#' DataSourceRDS.
#'
#' This class defines a DataSource using a RDS file.
#' @export DataSourceRDS
#' @exportClass DataSourceRDS
DataSourceRDS <- R6::R6Class(
    classname = "DataSourceRDS",
    inherit = DataSource,

    ## Properties
    private = list(
        .filepath = NULL, # Filepath
        .getDefaultDesc = function() return(self$filepath)
    ),

    ## Methods
    public = list(
        #' @param filepath string
        #' @param xVarName string Variable name for x values. If column does not exists in data natural numbers are assumed.
        #' @param variableDesc sets::tuple A tuple of variable names with descriptions. E.g. `sets::tuple(name = "varName", desc = "varirable Description")`
        #' @param nodeDesc string Description for Node in Graph
        initialize = function(filepath, xVarName, variableDesc = NULL, name = NULL, nodeDesc = NULL) {
            if (is.null(nodeDesc)) nodeDesc <- paste("DataSource from RDS file:", filepath)
            super$initialize(xVarName, variableDesc, name, nodeDesc)
            if (!file.exists(filepath)) stop("Filepath '", filepath, "' provided in `DataSourceRDS$new(..)` does not exist!")
            self$filepath <- filepath
            dat <- readRDS(self$filepath)
            for (n in names(dat)) {
                super$addColumn(n, dat[[n]])
            }
        }
    ),


    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        filepath = function(value) {
            if (missing(value)) return(private$.filepath)
            if (!(base::is.character(value)))
                propError("filepath", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.filepath <- value
            return(self)
        }
    )

)
