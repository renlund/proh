## #' fix_move
## #'
## #' gsub away your problems associated with the move to ~
## #'
## #' @param file
## #'
## #' @return
## #' @export
## #'
## #' @examples
## fix_move <- function(file = ""){
##    X <- readLines(con = file)
##    X0 <- gsub("C:/Projekt", "~/Projekt", X)
##    X1 <- gsub("'C', 'Projekt'", "'~', 'Projekt'")
##    cat(X1, file = file)
## }
