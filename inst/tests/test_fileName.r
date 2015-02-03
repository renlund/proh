context("testing 'fileName'")

test_that("'fileName' works", {
   x <- c(
      "foo.bar",
      "foo.bar.bar",
      ".foo",
      "foo"
   )
   df <- data.frame(
      name = c("foo", "foo.bar", "", "foo"),
      extension = c(".bar", ".bar", ".foo", "")
   )
   rownames(df) <- x
   expect_equal(fileName(x), df)
})
