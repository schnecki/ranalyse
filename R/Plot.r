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
        inferXAxis = function() {
            if (!base::is.null(self$xAxis)) return(self$xAxis)
            if (rhaskell::null(self$plotData))
                stop("No data in Plot-object. Cannot generate x-axis from data!")
            axes <- rhaskell::map(function(dt) dt$mkXAxis(), self$plotData)
            return(PlotAxis$new(rhaskell::Maybe$fromNullable(self$xAxis)$bind(function(x) x$title)$fromMaybe("time"), axes))
        },
        #' Infer Y-Axis from data and generate plot
        inferYAxis = function() {
            if (!base::is.null(self$yAxis)) return(self$yAxis)
            if (rhaskell::null(self$plotData))
                stop("No data in Plot-object. Cannot generate y-axis from data!")
            axes <- rhaskell::map(function(dt) dt$mkYAxis(), self$plotData)
            return(PlotAxis$new(rhaskell::Maybe$fromNullable(self$yAxis)$bind(function(x) x$title)$fromMaybe("y"), axes))
        }
    ),

    ## Methods
    public = list(
        initialize = function(title, xAxisTitle = "time", yAxisTitle = "y", plotData = NULL, subtitle = NULL, path = NULL, filename = NULL) {
            self$title <- title
            self$subtitle <- subtitle
            self$path <- path
            self$filename <- filename
            if (base::is.list(plotData)) self$plotData <- plotData
            else self$plotData <- rhaskell::Maybe$fromNullable(plotData)$maybe(base::list(), base::list)
            self$xAxis <- PlotAxis$new(xAxisTitle, rhaskell::map(function(x) x$mkPlotDataXAxis(), self$plotData))
            self$yAxis <- PlotAxis$new(yAxisTitle, rhaskell::map(function(x) x$mkPlotDataYAxis(), self$plotData))
        },
        #' Add a PlotData object to plot.
        #'
        #' @param name character Name of data
        #' @param data dataframe/matrix/vector/list of numericals
        addPlotData = function(name, plotData) {
            self$plotData <- base::append(self$plotData, plotData)
            self$xAxis$addPlotDataAxis(plotData$mkPlotDataXAxis())
            self$yAxis$addPlotDataAxis(plotData$mkPlotDataYAxis())
            return(self)
        },
        #' Plot the object. This automatically writes the file.
        plot = function() {
            if (rhaskell::null(self$plotData))
                stop("Plot$plot(): No data for plotting available")
            fn <- rhaskell::Maybe$fromNullable(self$filename)$fromMaybe(self$title) # filename, or use title instead
            formats <- list("eps", "ps", "tex", "pdf", "jpeg", "tiff", "png", "bmp", "svg", "wmf")
            if (!rhaskell::any(function(fmt) base::grepl(fmt, fn, fixed = TRUE), formats))
                fn <- paste0(fn, ".pdf") # use pdf as default
            path <- rhaskell::Maybe$fromNullable(self$path)$fromMaybe(".")
            if (!dir.exists(path)) dir.create(path, recursive = TRUE)
            file <- paste0(path, "/", fn)
            plot <- ggplot()
            plot <- rhaskell::foldl(function(p, plotData) p + plotData$plot(), plot, self$plotData) # add all data
            plot <- rhaskell::Maybe$fromNullable(self$xAxis)$alt(self$inferXAxis())$fmap(function(x) plot + x$plot())$fromMaybe(plot)
            plot <- rhaskell::Maybe$fromNullable(self$yAxis)$alt(self$inferYAxis())$fmap(function(x) plot + x$plot())$fromMaybe(plot)


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

            plot <- plot + ggplot2::ggtitle(label = self$name, subtitle = self$subtitle)
            ## plot + ggplot2::scale_x_date(## date_breaks = "1 week", date_labels = "%W"
            ##                              )
            plot
            ggsave(filename = file, width = 27, height = 22, dpi = 600, units = "cm")
        }

    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        title = function(value) {
            if (missing(value)) return(private$.title)
            if (!(base::is.character(value) || base::is.null(value)))
                propError("title", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.title <- value
            return(self)
        },
        x = function(value) {
            if (missing(value)) return(private$.x)
            if (!(base::is.data.frame(value) || base::is.null(value)))
                propError("x", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.x <- value
            return(self)
        },

        subtitle = function(value) {
            if (missing(value)) return(private$.subtitle)
            if (!(base::is.character(value) || base::is.null(value)))
                propError("subtitle", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.subtitle <- value
            return(self)
        },
        path = function(value) {
            if (missing(value)) return(private$.path)
            if (!(base::is.character(value) || base::is.null(value)))
                propError("path", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.path <- value
            return(self)
        },
        filename = function(value) {
            if (missing(value)) return(private$.filename)
            if (!(base::is.character(value) || base::is.null(value)))
                propError("filename", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.filename <- value
            return(self)
        },
        xAxis = function(value) {
            if (missing(value)) return(private$.xAxis)
            if (!(base::is.null(value) || "PlotAxis" %in% class(value)))
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
