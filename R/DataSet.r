#' DataSet interface.
#'
#' This class defines a basis
#' @export DataSet
#' @exportClass DataSet
DataSet <- R6::R6Class(
    classname = "DataSet",

    ## Properties
    private = list(
        ## .input = NULL, # vector<numeric>
        ## .y = NULL,     # vector<numeric>
        .name = NULL   # character
    ),

    ## Methods
    public = list(
        initialize = function(name) {
            super$initialize(name)
        },
        ## process = function() {
        ##     stop("The function process must be overwritten by the DataSet sub-class!")
        ## }
    ),

    ## Accessable properties. Active bindings look like fields, but each time they are accessed,
    ## they call a function. They are always publicly visible.
    active = list(
        name = function(value) {
            if (missing(value)) return(private$.name)
            if (!(base::is.character))
                stop("ERROR: Unallowed property ", value, " for 'name' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
            private$.name <- value
            return(self)
        }

        ## input = function(value) {
        ##     if (missing(value)) return(private$.input)
        ##     if (!(base::is.numeric(value) && base::is.vector(value)))
        ##         stop("ERROR: Unallowed property ", value, " for 'input' at ", getSrcFilename(function(){}), ":", getSrcLocation(function(){}))
        ##     private$.input <- value
        ##     return(self)
        ## }

    )

)
