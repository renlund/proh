context("opts is somewhat ok")

test_that("'restore' works", {
   # ls(envir=environment(milieu))
   restore()
   expect_more_than(length(ls(envir=environment(milieu))), 1)
})

test_that("'proh_get' works", {
   restore()
   expect_equal(proh_get()$attach_graph, FALSE)
   expect_equal(proh_get()$attach_table, FALSE)
   expect_equal(proh_get("attach_table")[[1]], FALSE)
})

test_that("'proh_set' works", {
   restore()
   proh_set("attach_table"=TRUE)
   expect_equal(proh_get("attach_table")[[1]], TRUE)
   proh_set("attach_table" =FALSE, "attach_graph"=TRUE, "dummy"=1:3)
   expect_equal(proh_get()$attach_graph, TRUE)
   expect_equal(proh_get()$attach_table, FALSE)
   expect_null(proh_get()[['dummy']])
})
