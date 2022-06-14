#' DataSet interface.
#'
#' This class defines a basis
#' @export DataSet
#' @exportClass DataSet
DataSet <- R6::R6Class(
    classname = "DataSet",
    inherit = Node,

    ## Properties
    private = list(
        .name = NULL,  # character
        .xVar = NULL,  # Variable
        .yVars = NULL  # Dict<Vars>, cannot create empty Dict() ^^
    ),

    ## Methods
    public = list(
        initialize = function(name, xVar, desc = NULL) {
            super$initialize(desc)
            self$name <- name
            self$xVar <- xVar
            self$yVars <- Dict$new(a = NULL)$clear()
        },
        addVariablesFromDataFrame = function(df, columns = names(df)) {
            if (!("data.frame" %in% class(df))) stop("Not a `data.frame` in DataSet$addFromDataFrame(..)")
            if (rhaskell::any(function(c) c %notElem% names(df), columns)) stop("Not all column names are part of the data frame that you want to add to the `DataSet`!")
            rhaskell::map(function(c) self$addVariableFromData(c, df[[c]]), columns)
            return(self)
        },
        addVariableFromData = function(name, data, varDesc = NULL) {
            self$addVariable(Variable$fromData(name, data, varDesc))
            return(self)
        },
        addVariable = function(var) {
            if (self$xVar$length != var$length)
                stop(paste0("Number of values from domain (x-axis) and variable to be added to the `DataSet` do not coincide: ", self$xVar$length, " != ", var$length, ". Variable: ", var$name))
            if (self$yVars$has(var$name)) stop(paste0("Variable with name '", var$name, "' already exists in DataSet."))
            self$yVars[var$name] <- var
            return(self)
        },
        getVariable = function(varName) {
            if (varName == self$xVar$name) return(self$xVar)
            else if (!self$yVars$has(varName)) stop(paste0("Variable with name '", varName, "' does not exit in DataSet (anymore)."))
            else return(self$yVars$get(varName))
        },
        removeVariable = function(varName, stopIfExists = TRUE) {
            if (varName == self$xVar$name) stop("Cannot remove x-Variable '", varName, "' from DataSet")
            else if (stopIfExists && !self$yVars$has(varName)) stop(paste0("Variable with name '", varName, "' does not exit in DataSet (anymore)."))
            else if (self$yVars$has(varName)) self$yVars$remove(varName)
        },
        #' @deleteVariable@ is the same as @removeVariable@.
        deleteVariable = function(...) self$removeVariable(...),
        preprocess = function(preprocs) {
            if (!is.list(preprocs) && "Preprocessor" %in% class(preprocs)) preprocs <- list(preprocs)
            if (rhaskell::any(function(x) !("Preprocessor" %in% class(x)), preprocs))
                stop("Expecting a list of @Preprocessor@ objects in Dataset$preprocess(..)")
            oldDs <- self$clone(deep = TRUE) # we create a new node to refer back to the old state. This way the user does not have to change the variable pointing to the @DataSet@.
            preprocsTxt <- paste(rhaskell::map(getR6ClassName, preprocs), collapse = ", ")
            ndProc <- NodeProcessor$new(paste("Preprocessing:", preprocsTxt))
            ndProc$parent <- oldDs
            self$parent <- ndProc
            for (prep in preprocs) {
                prep$parent <- ndProc
                inputNames <- prep$inputNames
                inputValues <- rhaskell::map(rhaskell::comp(function(v) v$vals, self$getVariable), inputNames)
                newVar <- prep$preprocess(inputValues)
                ## Add new variable(s)
                rhaskell::mapM_(self$addVariable, prep$additionalResultVars) # intermediate results
                self$addVariable(newVar)                                     # final variable
                if (prep$deleteInputVars) {
                    rhaskell::mapM_(self$removeVariable, inputNames)
                }
            }
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        name = function(value) {
            if (missing(value)) return(private$.name)
            if (!(base::is.character(value)))
                propError("name", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.name <- value
            return(self)
        },
        xVar = function(value) {
            if (missing(value)) return(private$.xVar)
            if (!("Variable" %in% class(value)))
                propError("xVar", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.xVar <- value
            return(self)
        },
        yVars = function(value) {
            if (missing(value)) return(private$.yVars)
            if (!("Dict" %in% class(value)))
                propError("yVars", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.yVars <- value
            return(self)
        },
        length = function() private$.yVars$length,
        variableNames = function() return(base::append(list(self$xVar$name), self$variableNamesY)),
        variableNamesY = function() return(as.list(self$yVars$keys))
    )

)
