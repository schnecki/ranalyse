#' VariableDate is a Variable specialised to x being a Date
#'
#' @export VariableDate
#' @exportClass VariableDate
VariableDate <- R6::R6Class(
    classname = "VariableDate",
    inherit = Variable, # Is a variable

    ## Properties
    private = list(
        .vals = NULL,     # vector<numeric>
        .name = NULL   # character
    ),

    ## Methods
    public = list(
        initialize = function(name, vals, desc = NULL) {
            super$initialize(name, vals, desc)
        },
        asMatrix = function() {
            x <- as.matrix(private$.vals)
            ## dim(x) <- c(self$rows, self$columns)
            ## dimnames(x) <- list(NULL, base::unlist(rhaskell::replicate(self$columns, self$name)))
            class(x) <- base::append(class(x), list("Date"))
            return(x)
        }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        vals = function(value) {
            if (missing(value)) return(private$.vals)
            if (!(tibble::is_tibble(value) && rhaskell::all(ranalyse::is.date, value))) {
                ##:ess-bp-start::conditional@:##
browser(expr={TRUE})##:ess-bp-end:##
                propError("vals", value, getSrcFilename(function(){}), getSrcLocation(function(){}))
            }
            private$.vals <- value
            return(self)
        },
        isDate = function() return(TRUE),
        isNumeric = function() return(FALSE)
    )

)
