context("testing 'toDate'")

test_that("'toDate' works", {
   expect_equal(toDate("2007-04-01"), as.Date("2007-04-01"))
   x <- c("2001-01-01", "2021-09-13", "", "2006-03-12", " ")
   y <- x
   y[c(3,5)] <- NA
   expect_equal(toDate(x), as.Date(y))
   x[5] <- "xkcd"
   expect_equal(toDate(x, not_dates=c("", "xkcd")), as.Date(y))
})
