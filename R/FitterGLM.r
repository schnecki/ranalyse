#' FitterGLM interface.
#'
#' @export FitterGLM
#' @exportClass FitterGLM
FitterGLM <- R6::R6Class(
    classname = "FitterGLM",
    inherit = Fitter, # Every variable is a node

    ## Properties
    private = list(
        .family = NULL,   # Function of type stats::family

        ## Private methdos
        .getDefaultDesc = function() {
            return(getR6ClassName(self))
        },
        .fit = function(formula) {
            return(glm(formula, family = self$family, data = self.data, na.action = self$na.action))
        }


    ),

    ## Methods
    public = list(
        initialize = function(family = stats::gaussian, na.action = "na.fail", akaikeFun = stats::AIC, desc = NULL) {
            super$initialize(na.action, akaikeFun, desc)
            self$family <- family
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        ## family = function(value) {
        ##     if (missing(value)) return(private$.family)
        ##     if (!(base::is.function(value) || base::is.character(value)))
        ##         propError("family", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
        ##     private$.family <- value
        ##     return(self)
        ## },
        family = function(value) {
            if (missing(value)) return(private$.family)
            if (!(base::is.function(value)))
                propError("family", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            private$.family <- value
            return(self)
        }


    )
)
