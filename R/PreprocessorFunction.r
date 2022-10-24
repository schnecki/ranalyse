#' PreprocessorFunction class.
#'
#' Applies a custom function on the whole tibble(s) of the input variables.
#'
#' @export PreprocessorFunction
#' @exportClass PreprocessorFunction
PreprocessorFunction <- R6::R6Class(
    classname = "PreprocessorFunction",
    inherit = Preprocessor,

    ## Properties
    private = list(
        .fun = NULL,     # function to apply
        .funName = NULL, # character

        ## Processor function
        .process = function(inputValues) {
            ## Apply to all rows, one-by-one (needed to be able to handle different data types: Date, Numeric, etc)
            res <- rhaskell::map(function(i) do.call(self$fun, rhaskell::map(function(xs) xs[[i]], inputValues)), base::seq_len(base::length(inputValues[[1]])))
            return(res[[1]])
        },
        .getDefaultDesc = function() {
            return(paste0(self$funName, "(", paste(self$inputNames, collapse = ", "), ")"))
        }
    ),

    ## Methods
    public = list(
        initialize = function(outputName, inputNames, fun, deleteInputVars = FALSE, nodeDesc = NULL) {
            self$inputNames <- inputNames
            self$fun <- fun
            self$funName <- base::deparse(base::substitute(fun))
            if (is.null(nodeDesc)) nodeDesc <- paste0(outputName, " <- ", private$.getDefaultDesc())
            super$initialize(outputName, inputNames, deleteInputVars, nodeDesc)
        }
    ),

    active = list(
        fun = function(value) {
            if (missing(value)) return(private$.fun)
            if (!(base::is.function(value)))
                propError("fun", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            if (!is.null(private$fun))
                stop("You cannot change the function of a PreprocessorFunction object. Create a new one instead!")
            private$.fun <- value
            return(self)
        },
        funName = function(value) {
            if (missing(value)) return(private$.funName)
            if (!(base::is.character(value)))
                propError("funName", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.funName <- value
            return(self)
        }
    )

)
