#' CoreModelSelectors class
#'
#' Defines set of 'CoreModelSelector'.
#'
#' @export CoreModelSelectors
#' @exportClass CoreModelSelectors
CoreModelSelectors <- R6::R6Class(
    classname = "CoreModelSelectors",
    inherit = Node,

    ## Properties
    private = list(
        .name = NULL,     # character
        .datasets = NULL,  # DataSets
        .coreModelSelectors = NULL # Dict<DataSet, Dict<outcome, list<CoreModelSelector>>>

    ),

    ## Methods
    public = list(
        initialize = function(name, datasets, desc = NULL) {
            super$initialize(desc)
            self$name <- name
            self$datasets <- datasets
            self$coreModelSelectors <- Dict::Dict$new(a = NULL)$clear()
        },
        addCoreModelSelector = function(outcome, sel) {
            if (!"CoreModelSelector" %in% class(sel)) stop("You are attempting to add non-CoreModelSelector object to CoreModelSelectors!")
            dsHash <- hash(sel$dataset)
            if (self$coreModelSelectors$has(dsHash)) {
                inner <- self$coreModelSelectors$get(dsHash)
                if (inner$has(outcome)) {
                    inner[outcome] <- base::append(inner$get(outcome), list(sel))
                } else {
                    inner[outcome] <- list(sel)
                }
            } else {
                inner <- Dict::Dict$new(a = NULL)$clear()
                inner[outcome] <- list(sel)
                self$coreModelSelectors[dsHash] <- inner
            }
            self$addChild(sel)
        },

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
        coreModelSelectors = function(value) {
            if (missing(value)) return(private$.coreModelSelectors)
            if (!("Dict" %in% class(value) && rhaskell::all(function(x) "Dict" %in% class(x), value$values)))
                propError("coreModelSelectors", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.coreModelSelectors <- value
            return(self)
        }

    )

)
