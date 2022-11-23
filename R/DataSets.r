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
        #' Get the dataset by the name.
        #'
        #' @param dsName Name of DataSet
        #' @return rhaskell::Either Character DataSet
        getDataSet = function(dsName) {
            for (ds in self$datasets) if (ds$name == dsName) return(rhaskell::Right(ds))
            return(rhaskell::Left(paste0("Could not find dataset with name '", dsName, "'")))
        },
        #' Apply a function `f :: a -> b` to each element of one specific variable and for all DataSets.
        #'
        #' @param fun: function to apply of type `a -> b`.
        #' @param variable: variable name to apply function to.
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
            dsNew <- DataSets$new(self$name, dss, desc = paste0("'", newVarName, "' created: accumulation with ", funDesc, " of ", paste0(columns, collapse = ",")))
            self$addChild(dsNew)
            return(dsNew)
        },
        #' Rename a variable over all DataSets
        #'
        #' @param newVarName: Name of new variable
        #' @param oldVarName: Old variable name
        #' @return a new DataSet object.
        renameVariableTo = function(newVarName, oldVarName) {
            dss <- rhaskell::map(function(ds) ds$addVariable(ds$getVariable(oldVarName)$rename(newVarName))$removeVariable(oldVarName), self$datasets)
            dsNew <- DataSets$new(self$name, dss, desc = paste0("'", oldVarName, "' renamed to '", newVarName, "'"))
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
        #' Create a core model for each dataset
        #' TODO!!!
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
        #' Create CITS Model
        #' TODO: improve interface to no require 1/0 vectors for periods
        #'
        createCITSModel = function(mainDsName, outcomes, selectionVars, addModelSelection, resultFolder = "results", citsFolder = "CITS") {
            ds <- self$getDataSet(mainDsName)$fromRightOrStop()
            models <- list()
            for (outcome in outcomes) {
                varNamePeriods <- "CITS_periods"
                dssCITS <- self$accumTo(varNamePeriods, sum, selectionVars)
                dsCITS <- dssCITS$getDataSet(mainDsName)$fromRightOrStop()
                ## Fit model: outcome ~  + trend + CITS_periods

                ## TODO: if outcome continous: guassian, otherwise quasipoisson

                fitter <- FitterGLM$new(family = stats::quasipoisson, na.action = "na.exclude")
                fitter$data <- dsCITS$asEnvironment(as_tibble = FALSE)
                ## rhs <- paste(base::append(selectionVars, addModelSelection), collapse = " + ")
                rhs <- paste(base::append(dsCITS$xVar$name, varNamePeriods), collapse = " + ")
                ## rhs <- paste(base::append(base::append(dsCITS$xVar$name, varNamePeriods), addModelSelection), collapse = " + ")


                rhs <- paste(base::append(varNamePeriods, addModelSelection), collapse = " + ")
                fit <- fitter$fit(paste(outcome, "~", rhs))

                ## datanew <- tibble::new_tibble(dsCITS$xVar$asMatrix())
                ## datanew <- tibble::add_column(datanew, dsCITS$getVariable(varNamePeriods)$asMatrix())
                ## datanew <- rhaskell::foldl(function(df, out) tibble::add_column(df, dsCITS$getVariable(out)$asMatrix()), datanew, selectionVars)

                preds <- tibble::as_tibble(stats::predict(fit$model, type = "response", newdata = dsCITS$asEnvironment()))
                env <- dsCITS$asEnvironment()
                dsCITSCF <- dsCITS$map(function(x) 0, varNamePeriods)
                predcf <- tibble::as_tibble(stats::predict(fit$model, type = "response", dsCITSCF$asEnvironment())) # counterfactual
                # dpred <- data.frame(T = datanew$T, pred1 = stats::predict(fit$model, type = "response", newdata = datanew))

                xVals <- dsCITS$xVar$asTibble()
                ## xVals <- dsCITS$getVariable("date")$asTibble()
                yVals <- dsCITS$getVariable(outcome)$asTibble()
                plDs <- rhaskell::map(function(sel) {
                    selVals <- dsCITS$getVariable(sel)$asTibble()
                    xs <- xVals[selVals == 1, ]
                    ys <- yVals[selVals == 1, ]
                    return(PlotDataGeomRect$new(sel
                                              , xMin = base::min(base::as.vector(xs)[[1]], na.rm = TRUE)
                                              , xMax = base::max(base::as.vector(xVals)[[1]], na.rm = TRUE)
                                              , yMin = base::min(base::as.vector(ys)[[1]], na.rm = TRUE)
                                              , yMax = base::max(base::as.vector(yVals)[[1]], na.rm = TRUE)
                                              , alpha = 0.35))
                }, selectionVars)
                plDs <- base::append(plDs, list(PlotDataGeomPoint$new(name = outcome, xVals, yVals, alpha = 0.40)
                                              , PlotDataGeomLine$new(name = "prediction", xVals, preds, colour = "orangered3", linetype = 2, size = 1.0, alpha = 1.0)
                                              , PlotDataGeomLine$new(name = "counterfactual", xVals, predcf, colour = "firebrick1", linetype = 1, size = 1.0, alpha = 1.0)
                                                ))
                plot <- Plot$new(paste0(self$name, ": ", outcome, " ~ ", rhs), plotData = plDs, yAxisTitle = outcome, subtitle = "Only change in level, all periods. Bf diagnosis"
                               , path = paste0(resultFolder, "/", citsFolder), filename = paste0(self$name, "_", "1-level-change_", outcome))
                plot$plot()

                fitModelAndPlot <- function(rhsSel) {
                    rhs <- paste(base::append(rhsSel, addModelSelection), collapse = " + ")
                    fitter <- FitterGLM$new(family = stats::quasipoisson, na.action = "na.exclude")
                    fitter$data <- dsCITS$asEnvironment(as_tibble = FALSE)
                    fit <- fitter$fit(paste(outcome, "~", rhs))
                    preds <- tibble::as_tibble(stats::predict(fit$model, type = "response", newdata = dsCITS$asEnvironment()))
                    dsCITSCF <- dsCITS$map(function(x) 0, rhsSel)
                    predcf <- tibble::as_tibble(stats::predict(fit$model, type = "response", dsCITSCF$asEnvironment())) # counterfactual

                    xVals <- dsCITS$xVar$asTibble()
                    ## xVals <- dsCITS$getVariable("date")$asTibble()
                    yVals <- dsCITS$getVariable(outcome)$asTibble()
                    plDs <- rhaskell::map(function(sel) {
                        selVals <- dsCITS$getVariable(sel)$asTibble()
                        xs <- xVals[selVals == 1, ]
                        ys <- yVals[selVals == 1, ]
                        return(PlotDataGeomRect$new(sel
                                                  , xMin = base::min(base::as.vector(xs)[[1]], na.rm = TRUE)
                                                  , xMax = base::max(base::as.vector(xVals)[[1]], na.rm = TRUE)
                                                  , yMin = base::min(base::as.vector(ys)[[1]], na.rm = TRUE)
                                                  , yMax = base::max(base::as.vector(yVals)[[1]], na.rm = TRUE)
                                                  , alpha = 0.35))
                    }, list(rhsSel))
                    plDs <- base::append(plDs, list(PlotDataGeomPoint$new(name = outcome, xVals, yVals, alpha = 0.40)
                                                  , PlotDataGeomLine$new(name = "prediction", xVals, preds, colour = "orangered3", linetype = 2, size = 1.0, alpha = 1.0)
                                                  , PlotDataGeomLine$new(name = "counterfactual", xVals, predcf, colour = "firebrick1", linetype = 1, size = 1.0, alpha = 1.0)
                                                    ))
                    plot <- Plot$new(paste0(self$name, ": ", outcome, " ~ ", rhs), plotData = plDs, yAxisTitle = outcome, subtitle = paste0("Only change in level, ", rhsSel, ". Bf diagnosis")
                                   , path = paste0(resultFolder, "/", citsFolder), filename = paste0(self$name, "_", "1-level-change_", outcome, "_", rhsSel))

                    plot$plot()
                    return(fit)
                }
                baseModels <- rhaskell::map(fitModelAndPlot, selectionVars)
                ## rhaskell::map(function(mdl) mdl$model$formula, baseModels)
                cis <- rhaskell::map(function(mdl) Epi::ci.lin(mdl$model, Exp = TRUE), baseModels)
                pVals <- rhaskell::zipWith(function(sel, ci) ci[sel, "P"] , selectionVars, cis)
                pVal <- base::min(base::unlist(pVals))
                idx <- rhaskell::find(function(i) (pVals[[i]] == pVal), base::seq_len(base::length(pVals)))$fromJust()
                ## Use
                base::print("Plots have been written. Take a look and decide on the date for futher analysis.")
                rhaskell::void(rhaskell::zipWith3(function(idx, var, pVal) {
                    base::print(paste0("[", idx, "] ", var, " (p-value: ", pVal, ")"))
                }, base::seq_len(base::length(pVals)), selectionVars, pVals))
                v <- base::readline(paste0("Enter value [", idx, "]"))
                if (v != "")
                    idx <- base::as.numeric(v)
                print("idx: ", idx)

                x <- as.numeric(x)

                ## p1<-ggplot() +
                ##   geom_rect(data = data.frame(xmin = 36,xmax = 120, ymin = 50,ymax = 400),aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                ##             fill = grisitot, alpha = 0.5) +
                ##   geom_rect(data = data.frame(xmin = 36+24,xmax = 120, ymin = 50,ymax = 400),aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                ##             fill = grisitot, alpha = 0.5) +
                ##   geom_rect(data = data.frame(xmin = 36+24+16,xmax = 120, ymin = 50,ymax = 400),aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                ##             fill = grisitot, alpha = 0.5) +
                ##   geom_point(data = dSCTFm,aes(T, allrc),colour="azure4",alpha=0.5) +
                ##   scale_x_continuous("month", breaks = 0:9*12, labels = 2008:2017)+
                ##   geom_line(data = dpredcf,aes(T, pred1), colour = "orangered3",linetype=2,size=0.75) +
                ##   geom_line(data = dpred,aes(T, pred1), colour = "firebrick1",size=1) +
                ##   ggtitle("allrc, SCTF, 2008-2017",subtitle = "Only change in level, all periods. Mensual scale. Bf diagnosis")+
                ##   theme_classic()


                ## models <- base::append(models, model)
            }

            ## TODO!!!
            return(models)

            ## if (!base::is.list(outcomes)) outcomes <- list(outcomes)
            ## if (!base::is.list(fitters))  fitters  <- list(fitters)
            ## if (!base::is.list(formulas)) formulas <- list(formulas)
            ## coreModels <- CoreModelSelectors$new(paste0("Possible Core Models for DataSets of '", self$name, "'"), self)

            ## for (ds in self$datasets) {
            ##     env <- ds$asEnvironment() # create environment
            ##     for (y in outcomes) {
            ##         selector <- CoreModelSelector$new(paste(ds$name, y), ds)
            ##         for (fitter in fitters) {
            ##             for (formula in formulas) {
            ##                 fitter$data <- env                   # set data frame
            ##                 fit <- fitter$fit(paste(y, formula)) # fit model
            ##                 selector$addPossibleCoreModel(fit)   # save as possible model
            ##             }
            ##         }
            ##         if (!selector$hasAnyConvergedModel()){
            ##             stop("No core model for dataset '", ds$name, "' and '", y, "' converge. Cannot proceed")
            ##         }
            ##         coreModels$addCoreModelSelector(y, selector)
            ##     }
            ## }
            ## return(coreModels)
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
