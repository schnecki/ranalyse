#' PreprocessorApply class.
#'
#' It uses `base::apply` to apply a function on every element of a vector.
#'
#' @export PreprocessorApply
#' @exportClass PreprocessorApply
PreprocessorApply <- R6::R6Class(
    classname = "PreprocessorApply",
    inherit = Preprocessor,

    ## Properties
    private = list(
        .fun = NULL, # function to apply
        .funName = NULL, # character
        .margin = 1, # vector

        ## Processor function
        .process = function(inputValues) {
            return(base::apply(rhaskell::head(inputValues), self$margin, self$fun))
        },
        .getDefaultDesc = function() {
            return(paste0("Apply(", rhaskell::head(self$inputNames), ", ", self$margin, ", ", self$funName, ")"))
        }
    ),

    ## Methods
    public = list(
        initialize = function(outputName, inputNames, fun, margin = 1, deleteInputVars = FALSE, nodeDesc = NULL) {
            if (base::length(inputNames) != 1) stop("PreprocessorApply requires exactly 1 input name")
            self$inputNames <- inputNames
            self$fun <- fun
            self$funName <- base::deparse(base::substitute(fun))
            self$margin <- margin
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
                stop("You cannot change the function of a PreprocessorApply object. Create a new one instead!")
            private$.fun <- value
            return(self)
        },
        funName = function(value) {
            if (missing(value)) return(private$.funName)
            if (!(base::is.character(value)))
                propError("funName", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.funName <- value
            return(self)
        },
        margin = function(value) {
            if (missing(value)) return(private$.margin)
            if (!(base::is.vector(value)))
                propError("margin", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.margin <- value
            return(self)
        }


    )

)
