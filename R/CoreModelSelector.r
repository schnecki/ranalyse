#' CoreModelSelector class
#'
#' Defines possible core models, e.g. starting points, of an analysis.
#'
#' @export CoreModelSelector
#' @exportClass CoreModelSelector
CoreModelSelector <- R6::R6Class(
    classname = "CoreModelSelector",
    inherit = Node,

    ## Properties
    private = list(
        .name = NULL,        # character
        .dataset = NULL,     # DataSet
        .coreModels = list() # list<Fitter>. possible core models
    ),

    ## Methods
    public = list(
        initialize = function(name, dataset, desc = NULL) {
            super$initialize(desc)
            self$name <- name
            self$dataset <- dataset
        },
        addPossibleCoreModel = function(fitter) {
            if (!"Fitter" %in% class(fitter)) stop("'addPossibleCoreModel' only takes objects of class 'Fitter'! Saw: ", class(fitter))
            if (!rhaskell::any(function(c) base::identical(fitter, c), self$coreModels)) {
                self$coreModels <- base::append(self$coreModels, list(fitter))
                self$addChild(fitter)
            }
        },
        improveCoreModel = function(analysis) {
            stop("TODO: create new CoreModelSelector class and connect accordingly")
        },
        hasAnyConvergedModel = function() {
            return(rhaskell::any(function(x) x$converged, self$coreModels))
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
        dataset = function(value) {
            if (missing(value)) return(private$.dataset)
            if (!("DataSet" %in% class(value)))
                propError("dataset", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.dataset <- value
            return(self)
        },
        coreModels = function(value) {
            if (missing(value)) return(private$.coreModels)
            if (!(base::is.list(value) && rhaskell::all(function(x) "Fitter" %in% class(x), value)))
                propError("coreModels", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.coreModels <- value
            return(self)
        }

    )

)
