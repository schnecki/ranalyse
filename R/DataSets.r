#' DataSets class
#'
#' This class defines DataSets, a class holding one or more @DataSet@ objects.
#' @export DataSets
#' @exportClass DataSets
DataSets <- R6::R6Class(
    classname = "DataSets",
    inherit = Node,

    ## Properties
    private = list(
        .name = NULL,     # character
        .datasets = NULL, # list<DataSet>
        .xVar = NULL      # Variable
    ),

    ## Methods
    public = list(
        initialize = function(name, datasets, desc = NULL) {
            if (!is.list(datasets) || length(datasets) < 1) stop("DataSets$new(..) requires a list of datasets, with >= 1 element")
            super$initialize(desc)
            self$name <- name
            self$datasets <- datasets
            ## if (rhaskell::any(rhaskell::pNIdentical(rhaskell::head(datasets)$xVar$vals), rhaskell::map(function(x) x$xVar$vals, rhaskell::tail(datasets))))
            ##     stop("In DataSets$new(..) the xVar values have to be equal for all datasets: ", paste0(rhaskell::map(function(x) x$xVar, datasets)))
            if (rhaskell::any(rhaskell::pNIdentical(rhaskell::head(datasets)$xVar$vals) %comp% (function(x) x$xVar$vals), rhaskell::tail(datasets)))
                stop("In DataSets$new(..) the xVar *values* have to be equal for all datasets")
            if (rhaskell::any(rhaskell::pNeq(rhaskell::head(datasets)$xVar$name) %comp% (function(x) x$xVar$name), rhaskell::tail(datasets)))
                warning("In DataSets$new(..) the xVar *names* are different. Using the first given xVar-name")
            self$xVar <- rhaskell::head(datasets)$xVar
        },
        #' Apply a function `f :: a -> b` to each element of  one specific column and for all DataSets.
        #'
        #' @param fun: function to apply of type `a -> b`.
        #' @param column: column name to apply function to.
        #' @param funDesc: Textual description of function.
        #' @return a new DataSets object.
        map = function(fun, column, funDesc = deparse1(fun)) {
            if (base::is.list(column)) stop("Cannot use multiple columns in function `map`")
            dss <- rhaskell::map(function(ds) ds$map(fun, column, funDesc), self$datasets)
            dsNew <- DataSets$new(paste0(self$name, " mapped"), dss, desc = paste0("mapped ", funDesc, " over ", column))
            self$addChild(dsNew)
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
            dss <- rhaskell::map(function(ds) ds$accumTo(newVarName, fun, columns, funDesc), self$datasets)
            dsNew <- DataSets$new(paste0(self$name, ", accumed"), dss, desc = paste0("accumulated by ", funDesc, " over ", paste0(columns, collapse = ",")))
            self$addChild(dsNew)
            return(dsNew)
        },
        #' Group all datasets by the specified columns and aggregate using the Aggregate function objects.
        #'
        #' @param columns: Columns to group on. Every tuple of values from these columns will only occur once after grouping.
        #' @param aggregates: Functions to apply on the data that will be aggregated, i.e. if and how other columns that will appear be kept.
        #' @param xVarName: Name of new variable.
        #' @return a new DataSet object.
        groupBy = function(columns, aggregates, xVarName = "t", na.rm = TRUE) {
            dss <- rhaskell::map(function(ds) ds$groupBy(columns, aggregates, xVarName, na.rm), self$datasets)
            dsNew <- DataSets$new(paste0(self$name, " grouped"), dss, desc = paste0("grouped by ", rhaskell::unlines(rhaskell::intersperse(", ", columns))))
            self$addChild(dsNew)
            return(dsNew)
        },
        #' Remove a variable from all datasets.
        #'
        #' @param column: Column/Variable name to remove.
        #' @param stopIfNotExists call `stop` if variable does not eixt
        #' @return a new DataSet object.
        removeVariable = function(column, stopIfNotExists = TRUE) {
            dss <- rhaskell::map(function(ds) ds$removeVariable(column, stopIfNotExists), self$datasets)
            dsNew <- DataSets$new(paste0(self$name, " ,removed var"), dss, desc = paste0("removed ", column))
            self$addChild(dsNew)
            return(dsNew)
        },
        createCoreModelsFor = function(outcomes, fitters, formulas, adaptions, selection) {
            if (!base::is.list(outcomes)) outcomes <- list(outcomes)
            if (!base::is.list(fitters))  fitters  <- list(fitters)
            if (!base::is.list(formulas)) formulas <- list(formulas)
            coreModels <- CoreModelSelectors$new(paste0("Possible Core Models for DataSets of '", self$name, "'"), self)

            for (ds in self$datasets) {
                env <- ds$asEnvironment() # create environment
                for (y in outcomes) {
                    selector <- CoreModelSelector$new(paste(ds$name, y), ds)
                    for (fitter in fitters) {
                        for (formula in formulas) {
                            fitter$data <- env                   # set data frame
                            fit <- fitter$fit(paste(y, formula)) # fit model
                            selector$addPossibleCoreModel(fit)   # save as possible model
                        }
                    }
                    if (!selector$hasAnyConvergedModel()){
                        stop("No core model for dataset '", ds$name, "' and '", y, "' converge. Cannot proceed")
                    }
                    coreModels$addCoreModelSelector(y, selector)
                }
            }
            return(coreModels)
        },
        #' Plot descriptive information of all variables and datasets.
        #' All graphs are written to files.
        #'
        #' @param parentPath Character Parent path (must exist). Default: "."
        #' @param resultFolder Character Folder to place results into. Default "results"
        #' @param descriptivesFolder Character Folder to place descriptive information into. Default "descriptives"
        #' @param dataSetFolder Bool Create a a subfolder for for each dataset. Default: TRUE
        plotDescriptives = function(parentPath = ".", resultFolder = "results", descriptivesFolder = "descriptives", dataSetFolder = TRUE) {
            path <- paste0(parentPath, "/", resultFolder)
            if (!dir.exists(path)) dir.create(path, recursive = TRUE)
            rhaskell::mapM_(function(ds) ds$plotDescriptives(path, descriptivesFolder, dataSetFolder), self$datasets)
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
        datasets = function(value) {
            if (missing(value)) return(private$.datasets)
            if (!(base::is.list(value) && length(value) >= 1))
                propError("datasets", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.datasets <- value
            return(self)
        },
        xVar = function(value) {
            if (missing(value)) return(private$.xVar)
            if (!("Variable" %in% class(value)))
                propError("xVar", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.xVar <- value
            return(self)
        }
    )

)
