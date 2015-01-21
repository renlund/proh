#'  @title Show available Wes Andersson palettes
#'  @description Show available Wes Andersson palettes
#'  @examples
#'  shoWes()
#'  @export

shoWes <- function(){
   if(require("wesanderson")){
      df <- wesanderson::namelist
      n <- nrow(df)
      N <- max(df$wesnums)
      plot(1,1, xlim=c(0,N+2.5), ylim=c(1,n), type='n', xaxt='n', yaxt='n', bty='n', xlab="", ylab="")
      for(k in 1:n){
         namn <-  df$movies[k]
         wn   <-  df$wesnums[k]
         text(x=1, y=k, namn)
         points(x=3:(3+wn-1), y=rep(k, wn), pch=21, cex=5, bg=wesanderson::wes.palette(n=wn, name=namn))
      }
   } else {
      message("[shoWes] package:wesanderson is not installed")
   }
}
