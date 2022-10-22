#' Variable that will be present in output tree
#'
#' @export Variable
#' @exportClass Variable
Variable <- R6::R6Class(
    classname = "Variable",
    inherit = Node, # Every variable is a node

    ## Properties
    private = list(
        .vals = NULL,   # vector<numeric>
        .name = NULL    # character
    ),

    ## Methods
    public = list(
        initialize = function(name, vals, desc = NULL) {
            super$initialize(desc)
            self$name <- name
            if (tibble::is_tibble(vals)) {
                self$vals <- vals
            } else {
                vals <- tibble::tibble(vals)
                names(vals) <- self$name
                self$vals <- vals
            }

        },
        #' Crop rows to selector vector
        #'
        #' @param selector: vector of logicals with length as there are number of rows in data
        #' @return a new variable of same tibble but only selected rows
        cropRows = function(selector) {
            if (!is.logical(selector) || !self$rows == length(selector))
                stop("cropRows exects a vector of logicals with the number of rows as in the variable")
            if (tibble::is_tibble(self$vals)) {
                valsNew <- self$vals[selector, ]
                varNew <- Variable$fromData(self$name, valsNew, desc = paste0("crop(", self$name, ") w/ ", dim(valsNew)[[1]], "/", self$rows, " rows"))
                self$addChild(varNew)
                return(varNew)
            } else {
                stop("EXPECTING TIBBLE!")
                valsNew <- self$vals[selector]
                return(Variable$fromData(self$name, valsNew, desc = paste0("crop(", self$name, ") w/ ", length(valsNew), "/", self$rows, " rows")))
            }
        },
        asMatrix = function() as.matrix(private$.vals),
        #' Apply a function `f :: a -> b` to each element and returns a new variable object with the modified data.
        #'
        #' @param fun: function to apply of type `a -> b`.
        #' @param funDesc: Textual description of function.
        #' @return a new Variable object
        map = function(fun, funDesc = deparse1(fun)) {
            varNew <- Variable$fromData(self$name, base::unlist(rhaskell::map(fun, self$vals)), paste0("map(", funDesc, ",", self$desc, ")"))
            self$addChild(varNew)
            return(varNew)
        },
        #' Plot variable descriptive information
        plotData = function(xVar) {
            xVals <- xVar
            if ("Variable" %in% class(xVar)) xVals <- xVals$vals
            return(PlotData$fromData(self$name, xVals, self$vals))
        },
        #' Convert to PlotDataAxis object.
        mkPlotDataXAxis = function() {
            return(PlotDataXAxis$new(label = self$name, data = self$vals, isContinous = self$isNumeric))
        },
        #' Convert to PlotDataAxis object.
        mkPlotDataYAxis = function() {
            return(PlotDataYAxis$new(label = self$name, data = self$vals, isContinous = self$isNumeric))
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        vals = function(value) {
            if (missing(value)) return(private$.vals)
            if (!((tibble::is_tibble(value)) && rhaskell::all(base::is.numeric, value))) {
                propError("vals", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            }
            private$.vals <- value
            return(self)
        },
        #' Number of values
        length = function() return(self$columns * self$rows),
        #' Same as length
        columns = function() if (tibble::is_tibble(private$.vals)) dim(private$.vals)[[2]] else 1,
        #' Same as length
        rows = function() if (tibble::is_tibble(private$.vals)) dim(private$.vals)[[1]] else length(private$.vals),
        #' Dimensions
        dim = function() if (tibble::is_tibble(private$.vals)) dim(private$.vals) else c(length(private$.vals), 1),
        name = function(value) {
            if (missing(value)) return(private$.name)
            if (!(base::is.character(value)))
                propError("name", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.name <- value
            return(self)
        },
        isDate = function() return(FALSE),
        isFactor = function() return(FALSE),
        isBoolean = function() return(FALSE),
        isString = function() return(FALSE),
        isNumeric = function() return(TRUE)
    ),
    cloneable = FALSE
)

## Prevent cloning the data. Use a pointer and a new object instead.
Variable$set("public", "clone", function(deep = TRUE) {
    return(Variable$new(self$name, self$vals, self$desc))
})


#' Function used to create a new variable. It checks for the type of data and selects an appropriate
#' Variable class.
#'
#' @param name string Name of variable
#' @param data vector variable data
#' @param desc string Description of variable
Variable$fromData <- function(name, data, desc = NULL) {
    if (!rhaskell::all(rhaskell::and %comp% rhaskell::pEq(base::class(data[[1]])) %comp% base::class, data))
        stop("Not all data types of vector/matrix/tibble are equal when creating a new Variable. The must be the same!")

    ## Convert matrices to tibbles (= data.frames)
    if (base::is.matrix(data)) data <- tibble::as_tibble(data)

    if      (ranalyse::is.date(data[[1]]))                                             return(VariableDate$new(name, data, desc))
    else if (base::is.factor(data[[1]]))                                               return(VariableFactor$new(name, data, desc))
    else if (base::is.logical(data[[1]]))                                              return(VariableBoolean$new(name, data, desc))
    else if (base::is.character(data[[1]]) && !base::is.numeric(as.matrix(data)[[1]])) return(VariableString$new(name, data, desc))
    else if (base::is.character(data)) { # is numeric value in character string. convert.
        warning(paste0("Found numeric values as string in variable ", name, ". Converting to numeric values!"))
        return(Variable$new(name, as.numeric(data), desc))
    } else if (base::is.data.frame(data)) { # already a data frame (=matrix/tibble), hence do not convert to vector
        return(Variable$new(name, data, desc))
    } else {
        return(Variable$new(name, as.vector(data), desc))
    }
}
