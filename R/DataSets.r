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
            if (rhaskell::any(rhaskell::comp(rhaskell::pNIdentical(rhaskell::head(datasets)$xVar$vals), function(x) x$xVar$vals), rhaskell::tail(datasets)))
                stop("In DataSets$new(..) the xVar *values* have to be equal for all datasets")
            if (rhaskell::any(rhaskell::comp(rhaskell::pNeq(rhaskell::head(datasets)$xVar$name), function(x) x$xVar$name), rhaskell::tail(datasets)))
                warning("In DataSets$new(..) the xVar *names* are different. Using the first given xVar-name")
            self$xVar <- rhaskell::head(datasets)$xVar
        },
        groupBy = function(columns, aggregates) {
            if (is.null(columns)) stop("Expecting a list of columns to group on")
            if (is.null(aggregates)) stop("Expecting a list of @GroupBy*@ aggregate functions")
            if (!base::is.list(columns)) columns <- list(columns)
            if (!base::is.list(aggregates)) aggregates <- list(aggregates)
            for (ds in self$datasets) {
                vars <- rhaskell::map(ds$getVariable, columns)
                vals <- rhaskell::map(function(var) var$vals, vars)
                keyVals <- rhaskell::map(unique, vals)
                combs <- rhaskell::foldl(function(acc, x) expand.grid(acc, x), rhaskell::head(keyVals), rhaskell::tail(keyVals))
                dict <- Dict::Dict$new(a = NULL)$clear()
                rhaskell::mapM_(function(keyIdx) {
                    keyVals <- combs[keyIdx, ]
                    selector <- rhaskell::foldl(function(acc, x) acc & x, vals[[1]] == keyVals[[1]], rhaskell::zipWith(function(a, b) a == b, rhaskell::tail(vals), rhaskell::tail(keyVals)))
                    dict[toString(keyVals)] <- selector
                    ## idxs <- vals
                    ## if (dict$has(key)) {
                    ##     dict[key] <- rhaskell::cons(idx, dict$get(key))
                    ## } else {
                    ##     dict[key] <- base::list(idx)
                    ## }
                }, seq(1, dim(combs)[[1]]))
                ## df <- ds$asDataFrame()

                ##:ess-bp-start::conditional@:##
browser(expr={TRUE})##:ess-bp-end:##

                stop("Todo: process every element of logicals where at least one field is TRUE")
                dict

            }


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
