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
            df <- base::data.frame(rhaskell::cons(self$xVar$vals, yVals), row.names = 1)
            mkName <- function(n, var) if (var$columns == 1) list(n) else rhaskell::map(function(nr) paste0(n, "_v", nr), seq(1, var$columns))
            names(df) <- unlist(rhaskell::concat(rhaskell::zipWith(mkName, self$yVars$keys, self$yVars$values)))
            return(df)
        },
        asMatrix = function() {
            yVals <- rhaskell::map(function(y) y$vals, self$yVars$values)
            vals <- rhaskell::cons(self$xVar$vals, yVals)
            mat <- rhaskell::foldl(function(mat, idx) {
                mat[, idx] <- c(vals[[idx]])
                return(mat)
            }, base::matrix(nrow = self$rows, ncol = 1 + self$length), seq(1, base::length(vals)))
            mkName <- function(n, var) if (var$columns == 1) list(n) else rhaskell::map(function(nr) paste0(n, "_v", nr), seq(1, var$columns))
            names(mat) <- unlist(rhaskell::concat(rhaskell::zipWith(mkName, self$yVars$keys, self$yVars$values)))
            return(mat)
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
        groupBy = function(columns, aggregates, xVarName = "t", rm.na = TRUE) {
            if (!base::is.list(columns)) columns <- list(columns)
            if (!base::is.list(aggregates)) aggregates <- list(aggregates)
            if (!rhaskell::all(base::is.character, columns)) stop("Expecting a list of column names to group on")
            if (rhaskell::any(function(x) !("AggregateBy" %in% class(x)), aggregates))
                stop("Expecting a list of @GroupBy*@ aggregate function objects")

            vars <- rhaskell::map(self$getVariable, columns)
            varsNames <- unlist(rhaskell::map(function(var) var$name, vars))
            aggsNames <- unlist(rhaskell::map(function(agg) agg$outputName, aggregates))
            vals <- rhaskell::map(function(var) var$asMatrix(), vars)
            keyVals <- rhaskell::map(unique, vals)
            combs <- rhaskell::foldl(function(acc, x) expand.grid(acc, x), rhaskell::head(keyVals), rhaskell::tail(keyVals))
            ## Order combinations
            orderList <- rhaskell::map(function(i) combs[[i]], base::seq(1, base::length(combs)))
            ##:ess-bp-start::conditional@:##
browser(expr={TRUE})##:ess-bp-end:##
            combs <- combs[do.call("order", orderList), ]
            len <- dim(combs)[[1]]
            ## Create data for each combination
            datas <- rhaskell::foldl(function(mVars, keyIdx) {
                keyVal <- combs[keyIdx, ]
                names(keyVal) <- varsNames
                selector <- rhaskell::foldl(function(acc, x) acc & x, vals[[1]] == keyVal[[1]], rhaskell::zipWith(function(a, b) a == b, rhaskell::tail(vals), rhaskell::tail(keyVal)))
                ## Create aggreated values (can be scalar or vector for each call)
                mkAggregate <- function(agg) {
                    inpName <- agg$inputName
                    outName <- agg$outputName
                    var <- self$getVariable(inpName)
                    newVar <- var$cropRows(selector)
                    if (newVar$rows > 0) { # aggregate
                        newVals <- agg$process(newVar$vals)
                        newVar$vals <- newVals
                    }
                    return(newVar)
                }
                newKeyVars <- rhaskell::zipWith(function(var, kVal) { # Create key variables
                    kVar <- var$clone()
                    kVar$vals <- tibble(kVal)
                    return(kVar)
                }, vars, keyVal)
                newVars <- rhaskell::map(mkAggregate, aggregates) # Create aggreate variables
                if (rhaskell::null(mVars)) # first iteration only
                    return(base::append(newKeyVars, newVars))
                return(rhaskell::zipWith(function(var, newVar) { # combine values
                    ## var$vals <- base::rbind(var$vals, newVar$vals)
                    ## dat <- base::rbind(var$vals, newVar$vals)
                    ## var <- Variable$fromData(var$name, desc = paste0("crop(", var$name, ") w/ ", length(dat), "/", self$rows, " rows"))
                    ## else var$vals <- c(var$vals, newVar$vals)
                    return(var)
                }, mVars, base::append(newKeyVars, newVars)))
            }, list(), seq(1, len))

            ## Create dataset
            xVar <- Variable$fromData(xVarName, seq(1, len), paste0("c(1:", len, ")"))
            dsNew <- DataSet$new(paste0(self$name, " grouped"), xVar, paste0("grouped by ", rhaskell::unlines(rhaskell::intersperse(", ", columns))))
            np <- NodeProcessor$new(paste0("GroupBy(", rhaskell::unlines(rhaskell::intersperse(", ", columns)), ")"))
            procNode <- rhaskell::foldl(function(x, agg) x$addProcessor(agg), np, aggregates)
            self$addChild(procNode)
            procNode$addChild(dsNew)
            return(rhaskell::foldl(function(ds, var) ds$addVariable(var), dsNew, datas))
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
                inputValues <- rhaskell::map(rhaskell::comp(function(v) v$asMatrix(), self$getVariable), inputNames)
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
        ##' Number of y-variables.
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
