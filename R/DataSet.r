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
        .name = NULL,      # character
        .xVar = NULL,      # Variable
        .yVars = NULL      # Dict<Vars>
    ),

    ## Methods
    public = list(
        initialize = function(name, xVar, desc = NULL) {
            super$initialize(desc)
            self$name <- name
            self$xVar <- xVar
            self$yVars <- Dict::Dict$new(a = NULL)$clear()
        },
        ##' Convert to @data.frame@. Note that matrices will be converted into seperate columns. See also @asEnvironment@.
        asDataFrame = function() {
            yVals <- rhaskell::map(function(y) y$vals, self$yVars$values)
            df <- data.frame(rhaskell::cons(self$xVar$vals, yVals), row.names = 1)
            mkName <- function(n, var) if (var$columns == 1) list(n) else rhaskell::map(function(nr) paste0(n, "_v", nr), seq(1, var$columns))
            names(df) <- unlist(rhaskell::concat(rhaskell::zipWith(mkName, self$yVars$keys, self$yVars$values)))
            return(df)
        },
        ##' Convert to environment.
        asEnvironment = function() {
            e <- rlang::env()
            e[[self$xVar$name]] <- self$xVar$vals
            return(rhaskell::foldl(function(env, var) {
                env[[var$name]] <- var$vals
                return(env)
            }, e, self$yVars$values))
        },
        addVariablesFromDataFrame = function(df, columns = names(df)) {
            if (!("data.frame" %in% class(df))) stop("Not a `data.frame` in DataSet$addFromDataFrame(..)")
            if (rhaskell::any(function(c) c %notElem% names(df), columns)) stop("Not all column names are part of the data frame that you want to add to the `DataSet`!")
            rhaskell::mapM_(function(c) self$addVariableFromData(c, df[[c]]), columns)
            return(self)
        },
        addVariableFromData = function(name, data, varDesc = NULL) {
            self$addVariable(Variable$fromData(name, data, varDesc))
            return(self)
        },
        addVariable = function(var) {
            if (self$xVar$rows != var$rows)
                stop(paste0("Number of values from domain (x-axis) and variable to be added to the `DataSet` do not coincide: ", self$xVar$rows, " != ", var$length, ". Variable: ", var$name))
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
            for (prepObj in preprocs) {
                prep <- prepObj$clone(deep = TRUE) # make a clone in case it is used more than once
                prep$parent <- ndProc
                inputNames <- prep$inputNames
                inputValues <- rhaskell::map(rhaskell::comp(function(v) v$vals, self$getVariable), inputNames)
                prep$dataset <- self
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
        ##' Number of variables.
        length = function() private$.yVars$length,
        ##' Rows of each variable
        rows = function() self$xVar$rows,
        ##' All variables names, i.e. x and y variable Names.
        variableNames = function() return(base::append(list(self$xVar$name), self$variableNamesY)),
        ##' Y variables names (without x-variable).
        variableNamesY = function() return(as.list(self$yVars$keys))
    )

)


## # S3 method for as.data.frame
## DataSet$set("public", "as.data.frame", function() {
##     ## as.data.frame(x, row.names = NULL, optional = FALSE, ...,
##     ##               cut.names = FALSE, col.names = names(x), fix.empty.names = TRUE,
##     ##               stringsAsFactors = default.stringsAsFactors())

## })
