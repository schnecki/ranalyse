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
        .filepath = NULL # Filepath
    ),

    ## Methods
    public = list(
        initialize = function(filepath, xVarName, nodeDesc = NULL) {
            super$initialize(xVarName, nodeDesc)
            if (!file.exists(filepath)) stop("File path provided in `DataSourceRDS$new(..)` does not exist!")
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
                propError(filepath, value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.filepath <- value
            return(self)
        }
    )

)
