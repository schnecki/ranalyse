#' Analyses class
#'
#' This class defines a set of Analyses for an object of DataSets.
#' @export Analyses
#' @exportClass Analyses
Analyses <- R6::R6Class(
    classname = "Analyses",
    inherit = Node,

    ## Properties
    private = list(
        .name = NULL,     # character
        .datasets = NULL,  # DataSets
        .analyses = NULL # Dict<DataSet, Dict<outcome, Analysis>>
    ),

    ## Methods
    public = list(
        initialize = function(name, datasets, desc = NULL) {
            super$initialize(desc)
            self$name <- name
            self$datasets <- datasets
            self$analyses <- Dict$new(a = NULL)$clear()

        },
        addAnalysis = function(analysis) {
            if (!"Analysis" %in% class(analysis)) stop("You are attempting to add non-Analysis object to Analyses!")
            stop("Cange to Dict")
            ## if (!rhaskell::any(function(c) base::identical(analysis, c), self$analyses)) {
            ##     self$analyses <- base::append(self$analyses, list(analysis))
            ##     self$addChild(analysis)
            ## }
        },
        analyse = function() {
            stop("TODO")
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
            if (!("DataSets" %in% class(value)))
                propError("datasets", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.datasets <- value
            return(self)
        },
        analyses = function(value) {
            if (missing(value)) return(private$.analyses)
            if (!(base::is.list(value) && rhaskell::all(function(x) "Analysis" %in% class(x), value)))
                propError("analyses", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.analyses <- value
            return(self)
        }

    )

)
