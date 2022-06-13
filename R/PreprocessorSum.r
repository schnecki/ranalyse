#' PreprocessorSum class.
#'
#' It can be used to sum variables in order to create a new variable.
#' @export PreprocessorSum
#' @exportClass PreprocessorSum
PreprocessorSum <- R6::R6Class(
    classname = "PreprocessorSum",
    inherit = Preprocessor,

    ## Properties
    private = list(

        ## Processor function
        .process = function(inputValues) {
            len <- length(rhaskell::head(inputValues)) # cannot be empty due to check in calling (=parent) class
            return(rhaskell::foldl(function(acc, x) acc + x, base::vector("numeric", len), inputValues))
        },
        .getDefaultDesc = function() {
            return(paste0("Sum(", paste(self$inputNames, collapse = ", "), ")"))
        }
    ),

    ## Methods
    public = list(
        initialize = function(outputName, inputNames, deleteInputVars = FALSE, nodeDesc = NULL) {
            if (is.null(nodeDesc)) nodeDesc <- paste0(outputName, " <- Sum(", paste0(inputNames, collapse = ", "), ")")
            super$initialize(outputName, inputNames, deleteInputVars, nodeDesc)
        }
    )

)
