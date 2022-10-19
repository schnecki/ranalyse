#' Plot object.
#'
#' @export Plot
#' @exportClass Plot
Plot <- R6::R6Class(
    classname = "Plot",

    ## Properties
    private = list(
        .title = NULL,    # String
        .subtitle = NULL, # String
        .path = NULL,     # FilePath
        .filename = NULL, # String
        .xAxis = NULL,    # PlotAxis
        .yAxis = NULL,    # PlotAxis
        ## .zAxis = NULL,    # PlotAxis
        .plotData = NULL,      # List<PlotData>

        #' Possibly infer X-Axis from data and generate plot
        mkXAxis = function() {
            if (!base::is.null(self$xAxis)) return(self$xAxis)
            if (base::is.null(self$plotData) || rhaskell::null(self$plotData))
                stop("No data in Plot-object. Cannot generate x-axis from data!")
            ##:ess-bp-start::conditional@:##
browser(expr={TRUE})##:ess-bp-end:##
            xVals <- rhaskell::map(function(dt) dt$xVals, self$plotData)
            ## xVals <- rhaskell::map(function(dt) dt$xVals, self$plotData)
            stop("TODO")
        }

    ),

    ## Methods
    public = list(
        initialize = function(title, plotData = NULL, xAxis = NULL, yAxis = NULL, subtitle = NULL, path = NULL, filename = NULL) {
            self$title <- title
            self$subtitle <- subtitle
            self$path <- path
            self$filename <- filename
            self$xAxis <- Maybe$fromNullable(xAxis).alt(Maybe$fromNullable())
            self$yAxis <- yAxis
            if (!base::is.list(plotData) && "PlotData" %in% class(plotData)) plotData <- list(plotData)
            self$plotData <- plotData
        },
        #' Add a PlotData object to plot.
        #'
        #' @param name character Name of data
        #' @param data dataframe/matrix/vector/list of numericals
        addPlotData = function(name, plotData) {
            self$plotData <- base::append(self$plotData, plotData)
            return(self)
        },
        #' Plot the object. This automatically writes the file.
        plot = function() {
            fn <- Maybe$fromNullable(self$filename)$fromMaybe(self$title) # filename, or use title instead
            formats <- list("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf")
            if (!rhaskell::any(function(fmt) base::grepl(fmt, fn, fixed = TRUE), formats))
                fn <- paste0(fn, ".pdf") # use pdf as default
            path <- Maybe$fromNullable(self$path)$fromMaybe(".")
            file <- paste0(path, "/", fn)
            plot <- ggplot()
            plot <- rhaskell::foldl(function(p, plotData) p + plotData$plot(), plot, self$plotData) # add all data
            ## plot <-


            ## theme_set(theme_grey())
            ## ggplot(, aes(x = periodos, y = so224, fill = periodos)) +
            ##     stat_boxplot(geom = "errorbar",
            ##                  width = 0.2) +
            ##     geom_boxplot(alpha = 0.8,          # Transparencia
            ##                  #colour = "#474747",   # Color del borde
            ##                  outlier.colour = 1) + # Color atípicos
            ##     scale_fill_brewer(palette="Blues")+
            ##     xlab("Period")+
            ##     theme( legend.position = "none" )+
            ##     scale_y_continuous("counts" )+
            ##     facet_wrap(city~., nrow = 2, scales = "free") +
            ##     ggtitle("SO2 by intervention period in each city",subtitle = "time scale: month; 0:pre-intervention; 3: post-intervention")
            ##:ess-bp-start::conditional@:##
browser(expr={TRUE})##:ess-bp-end:##
            ggsave(filename = fn, width = 27, height = 22, dpi = 600, units = "cm")


        }

    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        title = function(value) {
            if (missing(value)) return(private$.title)
            if (!(base::is.character(value)))
                propError("title", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.title <- value
            return(self)
        },
        x = function(value) {
            if (missing(value)) return(private$.x)
            if (!(base::is.data.frame(value)))
                propError("x", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.x <- value
            return(self)
        },

        subtitle = function(value) {
            if (missing(value)) return(private$.subtitle)
            if (!(base::is.character(value)))
                propError("subtitle", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.subtitle <- value
            return(self)
        },
        path = function(value) {
            if (missing(value)) return(private$.path)
            if (!(base::is.character(value)))
                propError("path", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.path <- value
            return(self)
        },
        filename = function(value) {
            if (missing(value)) return(private$.filename)
            if (!(base::is.character(value)))
                propError("filename", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.filename <- value
            return(self)
        },
        xAxis = function(value) {
            if (missing(value)) return(private$.xAxis)
            if (!(base::is.null(value) || "PlotXAxis" %in% class(value)))
                propError("xAxis", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.xAxis <- value
            return(self)
        },
        yAxis = function(value) {
            if (missing(value)) return(private$.yAxis)
            if (!(base::is.null(value) || "PlotAxis" %in% class(value)))
                propError("yAxis", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.yAxis <- value
            return(self)
        },
        ## zAxis = function(value) {
        ##     if (missing(value)) return(private$.zAxis)
        ##     if (!("PlotAxis" %in% class(value)))
        ##         propError("zAxis", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
        ##     private$.zAxis <- value
        ##     return(self)
        ## },
        plotData = function(value) {
            if (missing(value)) return(private$.plotData)
            if (!(base::is.list(value) && rhaskell::all(function(x) "PlotData" %in% class(x), value)))
                propError("plotData", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.plotData <- value
            return(self)
        }


    )

)
