#' DataSet interface.
#'
#' This class defines a basis
#' @export DataSet
#' @exportClass DataSet
DataSet <- R6::R6Class(
    classname = "DataSet",

    ## Properties
    private = list(
        .name = NULL,    # character
        .xVar = NULL, # Variable
        .yVars = list() # List<Vars>
    ),

    ## Methods
    public = list(
        initialize = function(name, xVar, data = NULL) {
            self$name <- name
            self$xVar <- xVar

            if (!base::is.null(data) && "data.frame" %in% class(data)) {
                addFromDataFrame
            } else if (!base::is.null(data)) {
                stop("Unknown input type for data in DataSet$new(..).")
            }

        },
        addFromDataFrame = function(df) {
            if ("data.frame" %in% class(df)) stop("Not a `data.frame` in DataSet$addFromDataFrame(..)")

        }
        ## process = function() {
        ##     stop("The function process must be overwritten by the DataSet sub-class!")
        ## }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        name = function(value) {
            if (missing(value)) return(private$.name)
            if (!(base::is.character))
                stop("ERROR: Unallowed property ", value, " for 'name' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.name <- value
            return(self)
        },
        xVar = function(value) {
            if (missing(value)) return(private$.xVar)
            if (!(base::is.numeric(value) && base::is.vector(value)))
                stop("ERROR: Unallowed property ", value, " for 'xVar' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.xVar <- value
            return(self)
        },
        yVars = function(value) {
            if (missing(value)) return(private$.yVars)
            stop("Cannot set yVars directly")
        }


        ## input = function(value) {
        ##     if (missing(value)) return(private$.input)
        ##     if (!(base::is.numeric(value) && base::is.vector(value)))
        ##         stop("ERROR: Unallowed property ", value, " for 'input' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
        ##     private$.input <- value
        ##     return(self)
        ## }

    )

)
