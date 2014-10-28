context("'cap_att' and related functions")

test_that("'cap_att' works", {
   opts_proh$restore()
   expect_equal(cap_att("foo", "bar"), "foo")
   opts_proh$set("attach_graph" = TRUE)
   expect_equal(
      cap_att("foo", "bar"),
      "foo \\attachfile{figure/bar-1.pdf}"
      )
   opts_proh$set("graph_dev" = "wmf")
   expect_equal(
      cap_att("foo", "bar"),
      "foo \\attachfile{figure/bar-1.wmf}"
   )
   opts_proh$restore()
   expect_equal(cap_att("foo", "bar"), "foo")
})
test_that("'.dev' works", {
   opts_proh$restore()
   expect_equal(.dev(), "pdf")
   opts_proh$set("graph_dev" = "png")
   expect_equal(.dev(), "png")
})
test_that("'.keep' works", {
   opts_proh$restore()
   expect_equal(.keep(), 'none')
   opts_proh$set("attach_graph" = TRUE)
   expect_equal(.keep(), 'high')
})
