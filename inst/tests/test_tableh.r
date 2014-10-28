if(FALSE){
   # library(xlsx)
   library(Hmisc)
   df <- data.frame(foo = 1:3, bar = letters[3:1], baz = LETTERS[1:3])
   tableh(df, "test_table", write.table, "A caption of interest")
   tableh(df, "test_csv", write.csv, "A caption of interest")
   tableh(df, "test_csv2", write.csv2, "A caption of interest")
   opts_proh$set("attach_table" = TRUE)
   opts_proh$get()
   # tableh(df, "test", write.xlsx, "A caption of interest")
   unlink("table", recursive = TRUE)
}
