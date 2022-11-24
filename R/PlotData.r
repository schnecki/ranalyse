#' PlotData object.
#'
#' @export PlotData
#' @exportClass PlotData
PlotData <- R6::R6Class(
    classname = "PlotData",

    ## Properties
    private = list(
        .name = NULL,         # Character
        .xVals = NULL,        # DataFrame
        .yVals = NULL,        # DataFrame
        .na.rm = NULL,        # bool
        .fill = NULL,         # character
        .alpha = NULL,        # float
        .colour = NULL,       # character
        .size = NULL         # float
    ),

    ## Methods
    public = list(
        initialize = function(name, xVals, yVals, na.rm = TRUE, colour = NULL, fill = NULL, size = NULL, alpha = NULL) {
            self$name <- name
            self$xVals <- tibble::as_tibble(xVals)
            self$yVals <- tibble::as_tibble(yVals)
            if (base::nrow(self$xVals) != base::nrow(self$yVals))
                stop("PlotData$initialize(..): number of rows for x and y-values have to be equal!")
            self$na.rm <- na.rm
            self$colour <- colour
            self$fill <- fill
            self$alpha <- alpha
            self$size <- size
        },
        asDataFrame = function() {
            return(tibble::add_column(self$xVals, self$yVals))
        },
        #' Returns Function to use for plotting, e.g. `ggplot2::geom_line`.
        getPlotFunction = function() {
            stop("You need to override getPlotDataType(). Must return a PlotDataType.")
        },
        #' Returns aes mapping
        #'
        #' @param df DataFrame of x and y values
        getMapping = function(df) {
            stop("You need to override getMapping().")
        },
        #' Returns list of additional arguments for plot function. NULL values are filtered automatically.
        getAddPlotArgs = function() {
            stop("You need to override getMapping().")
        },
        #' Builds the function call
        plot = function() {
            df <- self$asDataFrame()
            fun <- self$getPlotFunction()
            mapping <- self$getMapping(df)
            args <- self$getAddPlotArgs()
            defArgs <- list(na.rm = self$na.rm, fill = self$fill, alpha = self$alpha, colour = self$colour, size = self$size)
            defArgs <- rhaskell::filter(function(x) !rhaskell::fst(x) %in% base::names(args), rhaskell::zip(base::names(defArgs), defArgs))
            defArgNames <- rhaskell::map(rhaskell::fst, defArgs)
            defArgVals <- rhaskell::map(rhaskell::snd, defArgs)
            base::names(defArgVals) <- defArgNames
            args <- rhaskell::filter(rhaskell::not %comp% rhaskell::null, base::append(args, defArgVals))
            return(base::do.call(fun, base::append(list(data = df, mapping = mapping), args)))
        },
        #' Create X-Axis Information.
        mkPlotDataXAxis = function() {
            return(PlotDataXAxis$new(data = self$xVals, label = self$name))
        },
        #' Create Y-Axis Information.
        mkPlotDataYAxis = function() {
            return(PlotDataYAxis$new(data = self$yVals, label = self$name))
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
        xVals = function(value) {
            if (missing(value)) return(private$.xVals)
            if (!(base::is.data.frame(value)))
                propError("xVals", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.xVals <- value
            return(self)
        },
        yVals = function(value) {
            if (missing(value)) return(private$.yVals)
            if (!(base::is.data.frame(value)))
                propError("yVals", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.yVals <- value
            return(self)
        },
        na.rm = function(value) {
            if (missing(value)) return(private$.na.rm)
            if (!(base::is.logical(value) || base::is.null(value)))
                stop("ERROR: Unallowed property ", value, " for 'na.rm' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.na.rm <- value
            return(self)
        },
        fill = function(value) {
            if (missing(value)) return(private$.fill)
            if (!(base::is.character(value) || base::is.null(value)))
                stop("ERROR: Unallowed property ", value, " for 'fill' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.fill <- value
            return(self)
        },
        alpha = function(value) {
            if (missing(value)) return(private$.alpha)
            if (!(base::is.numeric(value) || base::is.null(value)))
                stop("ERROR: Unallowed property ", value, " for 'alpha' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.alpha <- value
            return(self)
        },
        colour = function(value) {
            if (missing(value)) return(private$.colour)
            if (!(base::is.character(value) || base::is.null(value)))
                stop("ERROR: Unallowed property ", value, " for 'colour' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.colour <- value
            return(self)
        },
        size = function(value) {
            if (missing(value)) return(private$.size)
            if (!(base::is.numeric(value) || base::is.null(value)))
                stop("ERROR: Unallowed property ", value, " for 'size' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.size <- value
            return(self)
        }
    )
)


#' ## Create a new `PlotData` object of the correct type.
PlotData$fromData <- function(name, xVals, data) {
    ## Convert matrices to tibbles (= data.frames) and check input type
    if (base::is.matrix(data)) data <- tibble::as_tibble(data)
    if (base::is.vector(data)) data <- tibble::as_tibble(data)
    if (base::is.list(data) && base::is.numeric(data[[1]])) data <- tibble::as_tibble(data)
    if (!base::is.data.frame(data)) stop("PlotData$fromData: expected a data.frame, matrix, vector or list of numeric values as input")

    if      (ranalyse::is.date(data[[1]])) return(PlotDataDate$new(name, xVals, data))
    else if (base::is.factor(data[[1]]))   return(PlotDataFactor$new(name, xVals, data))
    else if (base::is.logical(data[[1]]))  return(PlotDataBoolean$new(name, xVals, data))
    else if (base::is.character(data) && base::is.numeric(as.matrix(data)[[1]])) { # is numeric value in character string. convert.
        warning(paste0("Found numeric values as string in PlotData. Converting to numeric values!"))
        return(ranalyse::PlotDataGeomPoint$new(name, xVals, as.numeric(data)))
    } else {
        return(ranalyse::PlotDataGeomPoint$new(name, xVals, data))
    }
}
