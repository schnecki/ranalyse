#' Preprocessor interface.
#'
#' This class defines a basis
#' @export Preprocessor
#' @exportClass Preprocessor
Preprocessor <- R6::R6Class(
    classname = "Preprocessor",
    inherit = NodeProcessor,

    ## Properties
    private = list(
        .dataset = NULL,                   # reference to origin dataset
        .outputName = NULL,                # character
        .inputNames = NULL,                # list<character>
        .inputValues = NULL,               # list<vector<numeric>>
        .outputValue = NULL,               # vector<numeric>
        .outputVariable = NULL,            # Variable
        .deleteInputVars = FALSE,          # Bool
        .additionalResultVars = list(),    # list<Variable> additional results that can be saved

        #' Preprocessor function. Must return the output @Variable@.
        #' @param inputValues Input values to be processed
        .process = function(inputValues) {
            stop("The private function @.process@ must be overwritten by the @Preprocessor@ sub-class!")
        },
        #' Default description for new @Variable@. Should be overwritten by class implementation.
        .getDefaultDesc = function() {
            return(NULL)
        },
        .addAdditionalResultVar = function(var) {
            self$additionalResultVars <- base::append(self$additionalResultVars, list(var))
        }
    ),

    ## Methods
    public = list(
        initialize = function(outputName, inputNames, deleteInputVars = FALSE, nodeDesc = NULL) {
            if (is.null(nodeDesc)) stop("Parameter @nodeDesc@ must be set by the Preprocessor implementation")
            super$initialize(nodeDesc)
            self$outputName <- outputName
            self$inputNames <- inputNames
            self$deleteInputVars <- deleteInputVars
        },
        then = function(prep) {
            if (!("Preprocessor" %in% class(prep)))
                stop("Expecting a preprocessor as argument in Preprocessor$then(..)")
            then <- PreprocessorThen$new(self, prep)
            then$addChild(self)
            then$addChild(prep)
            return(then)
        },
        #' Execute preprocessor and return new @Variable@.
        preprocess = function(inputValues) {
            if (rhaskell::null(inputValues))
                stop("Empty input to @Preprocessor@")
            if (base::is.null(self$dataset))
                stop("Field `dataset` must be set before calling `preprocess` on objects of class `Preprocessor`")
            self$inputValues <- inputValues
            self$outputValue <- private$.process(inputValues)
            self$outputVariable <- Variable$fromData(self$outputName, self$outputValue, private$.getDefaultDesc())
            return(self$outputVariable)
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        outputName = function(value) {
            if (missing(value)) return(private$.outputName)
            if (!(base::is.character(value)))
                propError("outputName", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.outputName <- value
            return(self)
        },
        inputNames = function(value) {
            if (missing(value)) return(private$.inputNames)
            if (!is.list(value)) value <- list(value)
            if (!(base::is.list(value) && rhaskell::all(base::is.character, value)))
                propError("inputNames", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.inputNames <- value
            return(self)
        },
        inputValues = function(value) {
            if (missing(value)) return(private$.inputValues)
            if (!is.list(value)) value <- list(value)
            if (!(base::is.list(value) && (rhaskell::all(base::is.numeric, value) || rhaskell::all(ranalyse::is.date, value)))) {
                ##:ess-bp-start::conditional@:##
browser(expr={TRUE})##:ess-bp-end:##
                propError("inputValues", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            }
            private$.inputValues <- value
            return(self)
        },
        outputValue = function(value) {
            if (missing(value)) return(private$.outputValue)
            if (!(base::is.numeric(value) || is.date(value)))
                propError("outputValue", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.outputValue <- value
            return(self)
        },
        outputVariable = function(value) {
            if (missing(value)) return(private$.outputVariable)
            if (!("Variable" %in% class(value)))
                propError("outputVariable", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.outputVariable <- value
            return(self)
        },
        deleteInputVars = function(value) {
            if (missing(value)) return(private$.deleteInputVars)
            if (!(base::is.logical(value)))
                propError("deleteInputVars", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.deleteInputVars <- value
            return(self)
        },
        additionalResultVars = function(value) {
            if (missing(value)) return(private$.additionalResultVars)
            if (!is.list(value)) value <- list(value)
            if (!(base::is.list(value) && rhaskell::all(function(c) "Variable" %in% class(c), value)))
                propError("additionalResultVars", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.additionalResultVars <- value
            return(self)
        },
        dataset = function(value) {
            if (missing(value)) return(private$.dataset)
            if (!("DataSet" %in% class(value)))
                propError("dataset", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.dataset <- value
            return(self)
        }


    )

)
