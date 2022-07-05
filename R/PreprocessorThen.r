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
        .prepFirst = NULL,               # Preprocessor
        .prepSecond = NULL,              # Preprocessor

        ## Processor function
        .process = function(inputValues) {
            self$prepFirst$dataset <- self$dataset
            self$prepSecond$dataset <- self$dataset
            inputValuesFirst <- rhaskell::take(length(self$prepFirst$inputNames), inputValues)
            output1 <- self$prepFirst$preprocess(inputValuesFirst)
            if (!self$prepSecond$deleteInputVars)
                private$.addAdditionalResultVar(output1)
            ## Input to second
            basicInputValuesSecond <- rhaskell::drop(length(self$prepFirst$inputNames), inputValues)
            basicNamesInputValuesSecond <- rhaskell::delete(self$prepFirst$outputName, self$prepSecond$inputNames)
            inputValuesSecond <- list()
            idx <- 1
            for (n in self$prepSecond$inputNames) {
                if (n == self$prepFirst$outputName) inputValuesSecond <- base::append(inputValuesSecond, list(output1$vals))
                else {
                    inputValuesSecond <- base::append(inputValuesSecond, list(basicInputValuesSecond[[idx]]))
                    idx <- idx + 1
                }
            }
            output2 <- self$prepSecond$preprocess(inputValuesSecond)
            return(output2$vals)
        },
        .getDefaultDesc = function() {
            return(paste0(getR6ClassName(self$prepFirst), "(", paste(self$prepFirst$inputNames, collapse = ", "), ")",
                                   " >>= function(", self$prepFirst$outputName, ") ", getR6ClassName(self$prepSecond), "(", paste(self$prepSecond$inputNames, collapse = ", "), ")"))
        }
    ),

    ## Methods
    public = list(
        #' TODO: support multiple first and/or second nodes
        initialize = function(prepFirst, prepSecond, nodeDesc = NULL) {
            inputNames <- base::append(prepFirst$inputNames, rhaskell::delete(prepFirst$outputName, prepSecond$inputNames))
            self$inputNames <- inputNames
            self$prepFirst <- prepFirst
            self$prepSecond <- prepSecond
            if (is.null(nodeDesc)) nodeDesc <- paste0(prepSecond$outputName, " <- ", private$.getDefaultDesc())
            super$initialize(prepSecond$outputName, inputNames, prepFirst$deleteInputVars, nodeDesc)
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
        }
    ),
    cloneable = FALSE
)


## Prevent cloning the parents, otherwise it never stops
PreprocessorThen$set("public", "clone", function(deep = TRUE) {
    self$prepFirst$parent <- NULL
    self$prepSecond$parent <- NULL
    clone <- PreprocessorThen$new(self$prepFirst$clone(deep = deep), self$prepSecond$clone(deep = deep), self$nodeDesc)
    self$prepFirst$parent <- self
    self$prepSecond$parent <- self
    return(clone)
})
