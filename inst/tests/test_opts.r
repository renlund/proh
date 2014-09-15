context("opts is somewhat ok")

test_that("'restore' works", {
   # ls(envir=environment(milieu))
   opts_proh$restore()
   expect_more_than(length(opts_proh$get()), 1)
})

test_that("'proh_get' works", {
   opts_proh$restore()
   expect_equal(opts_proh$get()$attach_graph, FALSE)
   expect_equal(opts_proh$get()$attach_table, FALSE)
   expect_equal(opts_proh$get("attach_table")[[1]], FALSE)
})

test_that("'proh_set' works", {
   opts_proh$restore()
   opts_proh$set("attach_table"=TRUE)
   expect_equal(opts_proh$get("attach_table")[[1]], TRUE)
   opts_proh$set("attach_table" =FALSE, "attach_graph"=TRUE, "dummy"=1:3)
   expect_equal(opts_proh$get()$attach_graph, TRUE)
   expect_equal(opts_proh$get()$attach_table, FALSE)
   expect_null(opts_proh$get()[['dummy']])
})
