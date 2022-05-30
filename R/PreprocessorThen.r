#' PreprocessorThen class.
#'
#' It can be used to sum variables in order to create a new variable.
#' @export PreprocessorThen
#' @exportClass PreprocessorThen
PreprocessorThen <- R6::R6Class(
    classname = "PreprocessorThen",
    inherit = Preprocessor,

    ## Properties
    private = list(
        .prepFirst = NULL,  # Preprocessor
        .prepSecond = NULL, # Preprocessor

        ## Processor function
        .process = function(inputValues) {
            output1 <- self$prepFirst(inputValues)
            return(self$prepSecond(output1))
        }
    ),

    ## Methods
    public = list(
        initialize = function(prepFirst, prepSecond, nodeDesc = NULL) {
            if (is.null(nodeDesc)) nodeDesc <- paste0(prepSecond$outputName, " <- ", prepFirst$classname, " >>=", prepSecond$classname)
            super$initialize(prepSecond$outputName, prepFirst$inputNames, prepFirst$deleteInputVars, nodeDesc)
            self$prepFirst <- prepFirst
            self$prepSecond <- prepSecond
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        prepFirst = function(value) {
            if (missing(value)) return(private$.prepFirst)
            if (!("Preprocessor" %in% class(value)))
                stop("ERROR: Unallowed property ", value, " for 'prepFirst' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.prepFirst <- value
            return(self)
        },
        prepSecond = function(value) {
            if (missing(value)) return(private$.prepSecond)
            if (!("Preprocessor" %in% class(value)))
                stop("ERROR: Unallowed property ", value, " for 'prepSecond' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.prepSecond <- value
            return(self)
        }

    )

)
