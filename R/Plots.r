#' Plots: An object holding multiple plots in order to easily execute functions on all of them.
#'
#' @export Plots
#' @exportClass Plots
Plots <- R6::R6Class(
    classname = "DataSet",

    ## Properties
    private = list(
        .plots = NULL      # character
    ),

    ## Methods
    public = list(
        initialize = function(plots = list()) {
            self$plots <- plots
        },
        #' Add one plot to the Plots object.
        #'
        #' @param plot Plot
        addPlot = function(plot) {
            if (!"Plot" %in% class(plot)) stop("Expected a Plot-object in Plots$addPlot(..), but received ", class(plot))
            self$plots <- base::append(self$plots, plot)
            return(self)
        },
        #' Add multiple plots to the Plots object.
        #'
        #' @param plots List<Plot>
        addPlots = function(plots) {
            if (!(base::is.list(plots) && rhaskell::map(function(x) "Plot" %in% class(x), plots)))
                stop("Expected a list of plot objects in function Plots$addPlots(..), but received ", class(plots))
            self$plots <- base::append(self$plots, plots)
            return(self)
        },
        #' Plot all.
        plot = function() {
            rhaskell::mapM_(function(p) p$plot(), self$plots)
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        plots = function(value) {
            if (missing(value)) return(private$.plots)
            if (!(base::is.list(value) && rhaskell::all(function(x) "Plot" %in% class(x), value)))
                propError("plots", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.plots <- value
            return(self)
        }

    )

    )
