

## In case we run in the interpreter, load libraries and files
RUN_IN_INTERPRETER <- !("testthat" %in% .packages())

if (RUN_IN_INTERPRETER) {
    rm(list = ls())
    print("RUNNING IN INTERPRETER: ")

    library(R6)
    library(rhaskell)
    library(Dict)

    source("../../R/util.r", chdir = TRUE)
    source("../../R/Node.r", chdir = TRUE)
    source("../../R/Variable.r", chdir = TRUE)
    source("../../R/VariableDate.r", chdir = TRUE)
    source("../../R/VariableFactor.r", chdir = TRUE)
    source("../../R/VariableBoolean.r", chdir = TRUE)
    source("../../R/DataSet.r", chdir = TRUE)
    expect_true <- function(x) if (!x) stop("expect_true evaluated to ", x)
    expect_equal <- function(x, y) if (!(x == y)) stop("expect_equal not evaluated to TRUE: ", x, y)
    test_that <- function(str, x) x
    testdataPath <- "../testdata.rds"
} else {
    testdataPath <- normalizePath("../testdata.rds")
}
## library(R6)
## library(splines)
## library(mgcv)
## library(usethis)
## library(stringr)
## library(ggplot2)
## library(purrr)
## library(ranalyse)


test_that("VariableDate initialize()", {
    library(Dict)
    print(paste0("Path: ", normalizePath(".")))
    print(testdataPath)
    print(getwd())

    ## Data set for testing
    dat <- readRDS(normalizePath("../testdata.rds"))


    x <- VariableDate$new("date", as.Date(paste0(dat$yy, "-", dat$mm, "-", dat$dd, format = "%Y-%m-%d")))
    expect_true("VariableDate" %in% class(x))
    dataset <- DataSet$new("LPGC", x, dat, list("yy", "mm", "dd")) # datSmall

    expect_equal(dataset$length, 34)
    print(dataset$variableNames)
})


