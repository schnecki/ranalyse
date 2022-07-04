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
        createCoreModelsFor = function(outcomes, fitters, formulas, adaptions, selection) {
            if (!base::is.list(outcomes)) outcomes <- list(outcomes)
            if (!base::is.list(fitters)) fitters <- list(fitters)
            if (!base::is.list(formulas)) formulas <- list(formulas)
            for (ds in self$datasets) {
                df <- ds$asDataFrame() # create data frame
                for (fitter in fitters) {
                    fitter$data <- df # set data frame
                    for (y in outcomes) {
                        for (formula in formulas) {
                            ##:ess-bp-start::conditional@:##
browser(expr={TRUE})##:ess-bp-end:##
                            fit <- fitter$fit(paste(y, formula)) # fit model
                            ## ds$addCoreModel(y, DataCoreModel$new(...))
                        }
                    }
                }
            }
            stop("TODO: save to set of core models?")
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
