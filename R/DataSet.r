library(Dict)
#' DataSet interface.
#'
#' This class defines a basis
#' @export DataSet
#' @exportClass DataSet
DataSet <- R6::R6Class(
    classname = "DataSet",

    ## Properties
    private = list(
        .name = NULL,                       # character
        .xVar = NULL,                       # Variable
        .yVars = Dict$new(a = NULL)$clear() # Dict<Vars>, cannot create empty Dict() ^^
    ),

    ## Methods
    public = list(
        initialize = function(name, xVar, data = NULL, skipColumns = NULL) {
            self$name <- name
            self$xVar <- xVar

            if (!base::is.null(data) && "data.frame" %in% class(data)) {
                cols <- names(data)
                if (!is.null(skipColumns)) cols <- filter(function(x) !(x %in% skipColumns), names(data))
                self$addVariablesFromDataFrame(data, cols)
            } else if (!base::is.null(data)) {
                stop("Unknown input type for data in DataSet$new(..).")
            }
        },
        addVariablesFromDataFrame = function(df, columns = names(df)) {
            if (!("data.frame" %in% class(df))) stop("Not a `data.frame` in DataSet$addFromDataFrame(..)")
            if (rhaskell::any(function(c) c %notElem% names(df), columns)) stop("Not all column names are part of the data frame that you want to add to the `DataSet`!")
            rhaskell::map(function(c) self$addVariableFromData(c, df[[c]]), columns)
        },
        addVariableFromData = function(name, data) {
            if (is.date(data)) var <- VariableDate$new(name, data)
            else if (is.factor(data)) var <- VariableFactor$new(name, data)
            else if (is.logical(data)) var <- VariableBoolean$new(name, data)
            else if (is.character(data) && is.na(as.numeric(data[[1]]))) var <- VariableString$new(name, data)
            else if (is.character(data)) { # is numeric value in character string. convert.
                warning(paste0("Found numeric values variable ", name, ". Converting to numeric values!"))
                var <- Variable$new(name, as.numeric(data))
            } else var <- Variable$new(name, as.vector(data))
            self$addVariable(var)
        },
        addVariable = function(var) {
            if (self$xVar$length != var$length)
                stop(paste0("Number of values from domain (x-axis) and variable to be added to the `DataSet` have does not coincide: ", self$xVar$length, " != ", var$length, ". Variable: ", var$name))
            if (self$yVars$has(var$name)) stop(paste0("Variable with name '", var$name, "' already exists in DataSet."))
            self$yVars[var$name] <- var
        }
        ## process = function() {
        ##     stop("The function process must be overwritten by the DataSet sub-class!")
        ## }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        name = function(value) {
            if (missing(value)) return(private$.name)
            if (!(base::is.character(value)))
                stop("ERROR: Unallowed property ", value, " for 'name' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.name <- value
            return(self)
        },
        xVar = function(value) {
            if (missing(value)) return(private$.xVar)
            if (!("Variable" %in% class(value)))
                stop("ERROR: Unallowed property ", value, " for 'xVar' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.xVar <- value
            return(self)
        },
        yVars = function(value) {
            if (missing(value)) return(private$.yVars)
            if (!("Dict" %in% class(value)))
                stop("ERROR: Unallowed property ", value, " for 'yVars' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.yVars <- value
            return(self)
        },
        length = function() private$.yVars$length,
        variableNames = function() return(self$yVars$keys)


        ## input = function(value) {
        ##     if (missing(value)) return(private$.input)
        ##     if (!(base::is.numeric(value) && base::is.vector(value)))
        ##         stop("ERROR: Unallowed property ", value, " for 'input' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
        ##     private$.input <- value
        ##     return(self)
        ## }

    )

)
