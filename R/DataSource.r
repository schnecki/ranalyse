#' @importFrom rhaskell %.%

#' DataSource interface.
#'
#' This class defines a `DataSource`.
#' @export DataSource
#' @exportClass DataSource
DataSource <- R6::R6Class(
    classname = "DataSource",
    inherit = Node,

    ## Properties
    private = list(

        .xVarName = NULL,                           # string
        .variableDesc = NULL,                       # Dict<string, string>
        .columns = NULL,                            # Dict<string, Vector>

        ## Private functions
        addColumn = function(name, data) {
            self$columns[name] <- as.vector(data)
        },
        .getDefaultDesc = function() {
            return("DataSource")
        }
    ),

    ## Methods
    public = list(
        #' @param xVarName character Name of x-column, must be inside `variableDesc`.
        #' @param variableDesc sets::tuple A tuple of variable names with descriptions. E.g. `sets::tuple(name = "varName", desc = "varirable Description")`
        initialize = function(xVarName, variableDesc = NULL, desc = NULL) {
            super$initialize(desc)
            self$xVarName <- xVarName
            self$variableDesc <- Dict$new(a = NULL)$clear()
            self$columns <- Dict$new(a = NULL)$clear()

            if (is.null(variableDesc))
                warning("No `variableDesc` given in `DataSource$initialize(..)`, hence using all available variables with empty description.")
            else if (!rhaskell::all(function(x) length(x) == 2, variableDesc))
                stop("All variable descriptions must have length 2!")
            else
                rhaskell::mapM_(function(tpl) self$variableDesc[tpl[[1]]] <- tpl[[2]], variableDesc)
        },
        #' param filterFun function[char, vector], bool]   Takes as input two parameters (name and data). Must return a TRUE if the variable should be included. E.g. `function(n, dt) n != "person"`.
        createDataSet = function(filterVarsFun = NULL) {
            if (self$columns$length == 0) stop("No data found or empty variable description.")
            vars <- rhaskell::concatMap(function(n) {
                if (n == self$xVarName) return(list())
                if (!is.null(filterVarsFun) && !filterVarsFun(n, self$columns$get(n), tpl[[2]])) return(list())
                return(list(Variable$fromData(n, self$columns$get(n), self$variableDesc$get(n))))
            }, self$columns$keys)
            if (rhaskell::null(vars)) stop("No variables selected in parameter function `filterVarsFun` that are also defined in `variableDesc`")
            lengths <- rhaskell::map(function(x) x$length, vars)
            len <- min(unlist(lengths))
            dataCutTxt <- ""
            if (!rhaskell::all(function(l) l == len, lengths)) {
                dataCutTxt <- "Length of variables do not coincide, cutting of data!"
                warning(dataCutTxt)
                rhaskell::mapM_(function(v) v$vals <- v$vals[1:len], vars)
            }
            data <- self$columns$get(self$xVarName)
            xVarSrc <- self$xVarName
            if (is.null(data)) {
                data <- 1:len
                xVarSrc <- paste0("c(1:", len, ")")
            }
            xVar <- Variable$fromData(self$xVarName, data)
            ## Processing node
            ndProc <- NodeProcessor$new(paste0("Creating DataSource. Y-Vars: ", length(vars), "/", self$columns$length, ". X-Var: ", xVarSrc, ". ", dataCutTxt))
            ndProc$parent <- self
            ## DataSet node
            desc <- paste0(getR6ClassName(self), ": ", private$.getDefaultDesc())
            ds <- DataSet$new(paste("Dataset, x-Var:", self$xVarName), xVar, desc)
            ds$parent <- ndProc
            return(rhaskell::foldl(function(d, v) d$addVariable(v), ds, vars))
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        xVarName = function(value) {
            if (missing(value)) return(private$.xVarName)
            if (!(base::is.character(value)))
                propError("xVarName", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.xVarName <- value
            return(self)
        },
        columns = function(value) {
            if (missing(value)) return(private$.columns)
            if (!("Dict" %in% class(value)))
                propError("columns", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.columns <- value
            return(self)
        },
        variableDesc = function(value) {
            if (missing(value)) return(private$.variableDesc)
            if (!("Dict" %in% class(value)))
                propError("variableDesc", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.variableDesc <- value
            return(self)
        }


    )

)
