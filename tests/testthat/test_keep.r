context("testing 'keep'")

test_that("'keep' works", {
   dir <- paste0("TMP_DIR__", paste( sample(c(letters,LETTERS,0:9), size=2^6, replace = TRUE), collapse=""))
   dir.create(path = dir)
   old_dir <- setwd(dir)
   dir.create("calc")
   dir.create("calc/autoload")
   test <- 1
   keep("test")
   expect_equal(list.files("calc/"), c("autoload", "test.rdat"))
   a <- "string"
   A <- "another_string"
   keep("a")
   if(Sys.info()['sysname'] == "Windows") expect_error(keep("A"))
   setwd(old_dir)
   unlink(x = dir, recursive = TRUE)
})
