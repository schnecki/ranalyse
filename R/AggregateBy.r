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
        .na.rm = TRUE,      # logical
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
        initialize = function(inputName, as = NULL, columnWise = TRUE, na.rm = TRUE, desc = NULL) {
            super$initialize(desc)
            self$inputName <- inputName
            self$outputName <- ite(base::is.null(as), inputName, as)
            self$columnWise <- columnWise
            self$na.rm <- na.rm
        },
        process = function(xs) {
            if (base::is.matrix(xs)) xs <- tibble::as_tibble(xs)
            if (self$columnWise) {
                res <- rhaskell::map(function(idx) {
                    data <- xs[[idx]]
                    if (base::is.list(data)) {
                        warning("Data should not be stored in a list! Converting with `unlist` which is problematic for `Date` types")
                        data <- unlist(data)
                    }
                    if (self$na.rm) data <- data[!is.na(data)]
                    return(private$.process(data))
                }, seq(1, ite(base::is.data.frame(xs), dim(xs)[[2]], 1)))
                names(res) <- names(xs)
                return(tibble::as.tibble(res, nrow = 1, ncol = length(res)))
            } else {
                if (self$na.rm) data <- tidyr::drop_na(xs)
                res <- private$.process(data)
                if (tibble::is_tibble(res)) return(res)
                if (rhaskell::null(names(res))) names(res) <- names(xs) # suppose same names
                return(tibble::as.tibble(res, nrow = 1, .name_repair = "unique"))

            }
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
        na.rm = function(value) {
            if (missing(value)) return(private$.na.rm)
            if (!(base::is.logical(value)))
                propError("na.rm", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.na.rm <- value
            return(self)
        }
    )
)
