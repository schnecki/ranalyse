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
        .prepFirst = NULL,          # Preprocessor
        .prepSecond = NULL,         # Preprocessor
        .intermediateResult = NULL, # Variable

        ## Processor function
        .process = function(inputValues) {
            inputValuesFirst <- rhaskell::take(length(self$prepFirst$inputValues), inputValues)
            inputValuesSecond <- rhaskell::drop(length(self$prepFirst$inputValues), inputValues)
            output1 <- self$prepFirst$preprocess(inputValuesFirst)
            self$intermediateResult <- output1
            inputValues <- rhaskell::map(rhaskell::comp(function(v) v$vals, self$getVariable), inputNames)
            return(self$prepSecond$preprocess(output1$vals))
        },
        .getDefaultDesc = function() {
            return(paste0(getR6ClassName(self$prepFirst), "(", paste(self$prepFirst$inputNames, collapse = ", "), ")",
                                   " >>= function(", self$prepFirst$outputName, ") ", getR6ClassName(self$prepSecond), "(", paste(self$prepSecond$inputNames, collapse = ", "), ")"))
        }
    ),

    ## Methods
    public = list(
        initialize = function(prepFirst, prepSecond, nodeDesc = NULL) {
            if (is.null(nodeDesc))
                nodeDesc <- paste0(prepSecond$outputName, " <- ", getR6ClassName(prepFirst), "(", paste(prepFirst$inputNames, collapse = ", "), ")",
                                   " >>= function(", prepFirst$outputName, ") ", getR6ClassName(prepSecond), "(", paste(prepSecond$inputNames, collapse = ", "), ")")
            super$initialize(prepSecond$outputName, base::append(prepFirst$inputNames, prepSecond$inputNames), prepFirst$deleteInputVars, nodeDesc)
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
                propError("prepFirst", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.prepFirst <- value
            return(self)
        },
        prepSecond = function(value) {
            if (missing(value)) return(private$.prepSecond)
            if (!("Preprocessor" %in% class(value)))
                propError("prepSecond", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.prepSecond <- value
            return(self)
        },
        intermediateResult = function(value) {
            if (missing(value)) return(private$.intermediateResult)
            if (!("Variable" %in% class(value)))
                propError("intermediateResult", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.intermediateResult <- value
            return(self)
        }


    )

)
