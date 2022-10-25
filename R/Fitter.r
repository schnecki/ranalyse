#' Fitter interface.
#'
#' @export Fitter
#' @exportClass Fitter
Fitter <- R6::R6Class(
    classname = "Fitter",
    inherit = Node, # Every variable is a node

    ## Properties
    private = list(
        .na.action = "na.fail",   # string
        .converged = NULL,        # bool
        .data = NULL,             # data frame, list, or environment
        .model = NULL,            # resulting model
        .akaikeFun = NULL,        # function for computing akaike
        .akaike = NULL,           # akaike value

        ## Private methdos
        .getDefaultDesc = function() {
            return(getR6ClassName(self))
            ## stop("This function must be overwritten by implementing `Fitter` classes")
        },
        .fit = function() {
            ## Override in impelmenting class, ensure to use `data = self$data`.
            stop("This function must be overwritten by implementing `Fitter` classes. Ensure to set `data = self$data`")
        }
    ),

    ## Methods
    public = list(
        initialize = function(na.action = "na.fail", akaikeFun = stats::AIC, desc = NULL) {
            super$initialize(desc)
            self$na.action <- na.action
            self$akaikeFun <- akaikeFun

        },
        fit = function(formula, failOnError = TRUE) {
            ## Do NOT OVERRIDE this method implementing the interface!
            ## fit a model using private `.fit` method.

            if (base::is.null(self$data))
                stop("You must first set the `data` attribute in the calling function of `Fitter*.r`")

            self$model <- tryCatch({
                res <- private$.fit(formula)
                self$converged <- TRUE
                self$akaike <- self$akaikeFun(res)
                res
            }, error = function(cond) {
                self$converged <- FALSE
                warning("Cannot fit model: ", cond)
                if (failOnError) stop(cond)
                return(NULL)
            })

            warning("TODO: add AIFC/anvoa, etc")
            return(self)
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        na.action = function(value) {
            if (missing(value)) return(private$.na.action)
            if (!("na.fail" %in% class(value) || base::is.character(value)))
                propError("na.action", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.na.action <- value
            return(self)
        },
        converged = function(value) {
            if (missing(value)) return(private$.converged)
            if (!(base::is.logical(value)))
                propError("converged", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.converged <- value
            return(self)
        },
        data = function(value) {
            if (missing(value)) return(private$.data)
            if (!(base::is.data.frame(value) || base::is.list(value) || base::is.environment(value)))
                propError("data", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.data <- value
            return(self)
        },
        model = function(value) {
            if (missing(value)) return(private$.model)
            if (!("lm" %in% class(value) || is.null(value)))
                propError("model", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.model <- value
            return(self)
        },
        akaikeFun = function(value) {
            if (missing(value)) return(private$.akaikeFun)
            if (!(base::is.function(value)))
                propError("akaikeFun", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.akaikeFun <- value
            return(self)
        },
        akaike = function(value) {
            if (missing(value)) return(private$.akaike)
            if (!(base::is.numeric(value)))
                propError("akaike", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.akaike <- value
            return(self)
        }

    )
)
