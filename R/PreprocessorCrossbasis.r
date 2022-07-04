#' PreprocessorCrossbasis class.
#'
#' Preprocessor for creating a `dlnm::crossbasis` column/matrix as variable in the dataset.
#'
#' @export PreprocessorCrossbasis
#' @exportClass PreprocessorCrossbasis
PreprocessorCrossbasis <- R6::R6Class(
    classname = "PreprocessorCrossbasis",
    inherit = Preprocessor,

    ## Properties
    private = list(
        .lag = NULL,         # int or vector
        .argvarFun = NULL,   # Function with 1 parameter. Must return a list
        .arglag = NULL,      # arglag. List of arguments
        .groupingFun = NULL, # Function with 1 parameter, the DataSet.

        ## Processor function
        .process = function(inputValues) {
            vals <- rhaskell::head(inputValues) # cannot be empty due to check in calling (=parent) class
            argvar <- self$argvarFun(vals)
            if (base::is.null(self$groupingFun)) grouping <- NULL
            else grouping <- self$groupingFun(self$dataset)
            cb <- dlnm::crossbasis(vals, lag = self$lag, argvar = argvar, arglag = self$arglag, group = grouping)
            return(dlnm::crossbasis(vals, lag = self$lag, argvar = argvar, arglag = self$arglag, group = grouping))
        },
        .getDefaultDesc = function() {
            return(paste0("crossbasis(", rhaskell::head(self$inputNames), ")"))
        }
    ),

    ## Methods
    public = list(
        initialize = function(outputName, inputNames, lag = lag, argvarFun = argvarFun, arglag = arglag, groupingFun = NULL, deleteInputVars = FALSE, nodeDesc = NULL) {
            if (base::length(inputNames) != 1) stop("PreprocessorCrossbasis only takes 1 input name")
            if (!base::is.function(argvarFun)) stop("Parameter 'argvarFun' in PreprocessorCrossbasis$new(..) must be a function with 1 parameter, i.e. a vector of input values.")
            if (!is.null(groupingFun) && !base::is.function(groupingFun)) stop("Parameter 'groupingFun' in PreprocessorCrossbasis$new(..) must be a function with 1 parameter, i.e. the `DataSet` object.")
            self$inputNames <- inputNames
            self$lag <- lag
            self$argvarFun <- argvarFun
            self$arglag <- arglag
            if (is.null(nodeDesc)) nodeDesc <- paste0(outputName, " <- ", private$.getDefaultDesc())
            super$initialize(outputName, inputNames, deleteInputVars, nodeDesc)
        }
    ),

    active = list(
        lag = function(value) {
            if (missing(value)) return(private$.lag)
            if (!(ranalyse::is.integer(value) || (base::is.vector(value) && rhaskell::all(ranalyse::is.integer, value))))
                propError("lag", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.lag <- value
            return(self)
        },
        argvarFun = function(value) {
            if (missing(value)) return(private$.argvarFun)
            if (!(base::is.function(value)))
                propError("argvarFun", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.argvarFun <- value
            return(self)
        },
        arglag = function(value) {
            if (missing(value)) return(private$.arglag)
            if (!(base::is.list(value)))
                propError("arglag", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.arglag <- value
            return(self)
        },
        groupingFun = function(value) {
            if (missing(value)) return(private$.groupingFun)
            if (!(base::is.function(groupingFun)))
                propError("groupingFun", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.groupingFun <- value
            return(self)
        }

    )

)


#' Reexport of function logknots from package dlnm.
#'
#' @export logknots
logknots <- function(...) {
    return(dlnm::logknots(...))
}
