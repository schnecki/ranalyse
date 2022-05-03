

test_that("VariableDate initialize()", {
    dates <- as.Date(c("2022-06-18", "2022-03-13"))
    y <- c(1.2, 12)
    name <- "test dates"
    var <- VariableDate$new(name, dates)
    expect_equal(var$vals, dates)
    ## expect_equal(var$y, y)
    expect_equal(var$name, name)
    ## expect_error(VariableDate$new(rhaskell::take(1, dates), y, name))
    expect_error(VariableDate$new(dates, name))
})
