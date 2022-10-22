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
        ##' Convert to @tibble@.
        asTibble = function() {
            yVals <- rhaskell::map(function(y) y$vals, self$yVars$values)
            tbbl <- tibble::tibble(self$xVar$vals)
            return(rhaskell::foldl(function(acc, col) tibble::add_column(acc, col), tbbl, yVals))
        },
        #' Convert to @matrix@. Note that matrices can only have one primitive types. If you have a column of type @Date@ this will convert all columns to strings.
        asMatrix = function() {
            yVals <- rhaskell::map(function(y) y$vals, self$yVars$values)
            vals <- rhaskell::cons(self$xVar$vals, yVals)
            return(rhaskell::foldl(function(m, xs) base::cbind(m, base::as.matrix(xs$vals)), base::as.matrix(self$xVar$vals), self$yVars$values))
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
        #' Apply a function `f :: a -> b` to each element of one specific column and replace variable. Returns new DataSet.
        #'
        #' @param fun: function to apply of type `a -> b`.
        #' @param column: column name to apply function to.
        #' @param funDesc: Textual description of function.
        #' @return a new DataSet object.
        map = function(fun, column, funDesc = deparse1(fun)) {
            if (base::is.list(column)) stop("Cannot use multiple columns in function `map`")
            var <- self$getVariable(column)
            varNew <- var$map(fun, funDesc)
            dsNew <- self$clone(deep = TRUE)
            dsNew$replaceVariable(varNew)
            np <- NodeProcessor$new("map")
            procNode <- NodeProcessor$new(paste0("map(", funDesc, ",", column, ")"))
            self$addChild(procNode)
            procNode$addChild(dsNew)
            return(dsNew)
        },
        #' Accumulate one or more variable values to a new variable using a function `f :: [Vector a] -> b`. Adds the new variable to the DataSet.
        #'
        #' @param newVarName: Name of new variable
        #' @param fun: function to apply of type `[Vector a] -> b`. Each element is a scalar or vector (in case of matrix variables) of inputs from the specified columns.
        #' @param columns: columns used as input to the function.
        #' @param funDesc: Textual description of function.
        #' @return a new DataSet object.
        accumTo = function(newVarName, fun, columns, funDesc = deparse1(fun)) {
            if (!base::is.list(columns)) columns <- list(columns)
            vars <- rhaskell::map(self$getVariable, columns)
            varsVals <- rhaskell::map(function(x) x$vals, vars)
            unpackIfOnlyScalars <- function(xs) {
                if (length(xs) == length(rhaskell::concat(xs))) return(unlist(xs))
                else return(xs)
            }
            vals <- unlist(
                rhaskell::map(function(row)
                    fun(unpackIfOnlyScalars(rhaskell::map(function(tibble)
                        unlist(rhaskell::map(function(col) col[[row]], as.list(tibble)))
                      , varsVals)))
                  , seq_len(nrow(varsVals[[1]]))))
            varNew <- Variable$fromData(newVarName, vals, paste(funDesc, "of columns", paste(columns, collapse = ",")))
            rhaskell::mapM_(function(x) x$addChild(varNew), vars)
            dsNew <- self$clone(deep = TRUE)
            dsNew$addVariable(varNew)
            np <- NodeProcessor$new("accumTo")
            procNode <- NodeProcessor$new(paste("accumTo", newVarName, "with", funDesc, "of columns", paste(columns, collapse = ",")))
            self$addChild(procNode)
            procNode$addChild(dsNew)
            return(dsNew)
        },
        #' Group by the specified columns and aggregate using the Aggregate function objects.
        #'
        #' @param columns: Columns to group on. Every tuple of values from these columns will only occur once after grouping.
        #' @param aggregates: Functions to apply on the data that will be aggregated, i.e. if and how other columns that will appear be kept.
        #' @param xVarName: Name of new variable.
        #' @return a new DataSet object.
        groupBy = function(columns, aggregates, xVarName = "t", na.rm = TRUE) {
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
                        newVar$vals <- agg$process(newVar$vals)
                    }
                    newVar$name <- agg$outputName
                    return(newVar)
                }
                newKeyVars <- rhaskell::zipWith(function(var, kVal) { # Create key variables
                    kVar <- var$clone()
                    kVar$vals <- tibble::as_tibble(kVal, ncol = base::ncol(var$vals))
                    names(kVar$vals) <- var$name
                    return(kVar)
                }, vars, keyVal)
                newVars <- rhaskell::map(mkAggregate, aggregates) # Create aggreate variables
                if (rhaskell::null(mVars)) # first iteration only
                    return(base::append(newKeyVars, newVars))
                return(rhaskell::zipWith(function(var, newVar) { # combine values
                    var$vals <- base::rbind(var$vals, newVar$vals)
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
            if (self$yVars$has(var$name)) stop(paste0("Variable with name '", var$name, "' already exists in DataSet. Maybe you want to use `replaceVariable` instead of `addVariable`!"))
            self$yVars[var$name] <- var
            return(self)
        },
        replaceVariable = function(var) {
            if (self$xVar$rows != var$rows)
                stop(paste0("Number of values from domain (x-axis) and variable to be replaced to the `DataSet` do not coincide: ", self$xVar$rows, " != ", var$length, ". Variable: ", var$name))
            if (!self$yVars$has(var$name)) stop(paste0("Variable with name '", var$name, "' does NOT exists in DataSet. Maybe you want to use `addVariable` instead of `replaceVariable`!"))
            self$yVars[var$name] <- var
            return(self)
        },
        getVariable = function(varName) {
            if (varName == self$xVar$name) return(self$xVar)
            else if (!self$yVars$has(varName)) stop(paste0("Variable with name '", varName, "' does not exit in DataSet (anymore)."))
            else return(self$yVars$get(varName))
        },
        removeVariable = function(varName, stopIfNotExists = TRUE) {
            if (varName == self$xVar$name) stop("Cannot remove x-Variable '", varName, "' from DataSet")
            else if (stopIfNotExists && !self$yVars$has(varName)) stop(paste0("Variable with name '", varName, "' does not exit in DataSet (anymore)."))
            else if (self$yVars$has(varName)) { # remove variable
                dsNew <- self$clone(deep = TRUE)
                dsNew$yVars$remove(varName)
                self$addChild(dsNew)
                return(dsNew)
            }
            return(self)
        },
        #' @deleteVariable@ is the same as @removeVariable@.
        deleteVariable = function(...) self$removeVariable(...),
        #' Preprocess data set with given `Preprocessor` objects. All parameters must implement the `Preprocessor` class/interface.
        #'
        #' @param preprocs List of preprocessors, each must implement the `Preprocessor` interface.
        #' @return void
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
                inputValues <- rhaskell::map((function(v) v$asMatrix()) %comp% self$getVariable, inputNames)
                prep$dataset <- self
                newVar <- prep$preprocess(inputValues)
                ## Add new variable(s)
                rhaskell::mapM_(self$addVariable, prep$additionalResultVars) # intermediate results
                self$addVariable(newVar)                                     # final variable
                if (prep$deleteInputVars) {
                    rhaskell::mapM_(self$removeVariable, inputNames)
                }
            }
        },
        #' Plot descriptive information of all variables.
        #' All graphs are written to files.
        #'
        #' @param parentPath Character Parent path (must exist). Default: "."
        #' @param descriptivesFolder Character Folder to place descriptive information into. Default "descriptives"
        #' @param dataSetFolder Bool Create a a subfolder for for each dataset. Default: TRUE
        plotDescriptives = function(parentPath = ".", descriptivesFolder = "descriptives", dataSetFolder = TRUE) {
            path <- paste0(parentPath, "/", descriptivesFolder)
            if (dataSetFolder) path <- paste0(path, "/", self$name)
            if (!dir.exists(path)) dir.create(path, recursive = TRUE)
            ##:ess-bp-start::conditional@:##
browser(expr={TRUE})##:ess-bp-end:##
            xAxis <- self$xVar$mkPlotDataXAxis() # create x values
            mkFileName <- function(var) return(paste0(self$name, "_desc_", var$name))
            vars <- rhaskell::filter(function(x) x$isNumeric, self$variablesY)
            plots <- Plots$new(rhaskell::map(function(v) Plot$new(title = v$name,
                                                                  ##:ess-bp-start::conditional@:##
browser(expr={TRUE})##:ess-bp-end:##
plotData = v$plotData(self$xVar),
                                                                  subtitle = "Descriptive Information", path = path, filename = mkFileName(v)), vars))
            plots$plot()
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
        ##' Number of rows.
        rows = function() self$xVar$rows,
        ##' All variables names, i.e. x and y variable Names.
        variableNames = function() return(base::append(list(self$xVar$name), self$variableNamesY)),
        ##' Y variables names (without x-variable).
        variableNamesY = function() return(as.list(self$yVars$keys)),
        ##' All variables, i.e. x- and y-Variables.
        variables = function() return(base::append(list(self$xVar), as.list(self$yVars$values))),
        ##' All variables, i.e. x- and y-Variables.
        variablesY = function() return(as.list(self$yVars$values))
    )

)


## # S3 method for as.data.frame
## DataSet$set("public", "as.data.frame", function() {
##     ## as.data.frame(x, row.names = NULL, optional = FALSE, ...,
##     ##               cut.names = FALSE, col.names = names(x), fix.empty.names = TRUE,
##     ##               stringsAsFactors = default.stringsAsFactors())

## })
