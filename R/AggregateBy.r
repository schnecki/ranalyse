#' Interface fro AggregateBy Aggregation Functions.
#'
#' @export AggregateBy
#' @exportClass AggregateBy
AggregateBy <- R6::R6Class(
    classname = "AggregateBy",
    inherit = NodeProcessor, # Every variable is a node

    ## Properties
    private = list(
        .inputName = NULL,  # character
        .outputName = NULL, # character
        .rm.na = TRUE,      # logical
        .columnWise = TRUE, # logical

        #' Aggreate process function. Value that will be in the data set after processing input.
        #' @param inputValues Input values to be aggregated.
        .process = function(xs) {
            stop("Needs to be overwritten by the implementation.")
        },
        #' Aggreate Function name, e.g. @"min"@, @"max"@, @"sum"@, @"mean"@, etc.
        .getDefaultFunName = function() {
            stop("Needs to be overwritten by the implementation.")
        },
        #' Default description for aggreage function being applied. Uses @private$.getDefaultFunName@ to create desc.
        .getDefaultDesc = function() {
            f <- private$.getDefaultFunName()
            if (self$inputName == self$outputName)
                return(paste0(f, "(", self$inputName, ")"))
            else
                return(paste0(self$outputName, " <- ", f, "(", self$inputName, ")"))
        }
    ),

    ## Methods
    public = list(
        initialize = function(inputName, as = NULL, columnWise = TRUE, rm.na = TRUE, desc = NULL) {
            super$initialize(desc)
            self$inputName <- inputName
            self$outputName <- ite(is.null(as), inputName, as)
            self$columnWise <- columnWise
            self$rm.na <- rm.na


        },
        process = function(xs) {
            res <- rhaskell::map(function(idx) {
                data <- ite(is.matrix(xs), xs[, idx], xs[idx])
                if (self$rm.na) data <- data[!is.na(data)]
                return(private$.process(unlist(data)))
            }, seq(1, ite(base::is.matrix(xs), dim(xs)[[2]], 1)))
            if (base::is.matrix(xs)) return(matrix(res, ncol = ncol(xs)))
            else return(res)
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        inputName = function(value) {
            if (missing(value)) return(private$.inputName)
            if (!(base::is.character(value)))
                propError("inputName", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.inputName <- value
            return(self)
        },
        outputName = function(value) {
            if (missing(value)) return(private$.outputName)
            if (!(base::is.character(value)))
                propError("outputName", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.outputName <- value
            return(self)
        },
        columnWise = function(value) {
            if (missing(value)) return(private$.columnWise)
            if (!(base::is.logical(value)))
                stop("ERROR: Unallowed property ", value, " for 'columnWise' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.columnWise <- value
            return(self)
        },
        rm.na = function(value) {
            if (missing(value)) return(private$.rm.na)
            if (!(base::is.logical(value)))
                propError("rm.na", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.rm.na <- value
            return(self)
        }
    )
)
