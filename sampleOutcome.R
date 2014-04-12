################ sample the output and data##############
# sample equal size of passing runs and failing runs, retrun index
sampleOutcome <- function (y) {
  
  y0 <- which(y < 0.5)
  y1 <- which(y > 0.5)
  if (length(y0) < length(y1)) {
    b <- y1[1:length(y0)]
    index <- c(y0, b)
  } else {
    b <- y0[1:length(y1)]
    index <- c(y1, b)
  }
 
  return(index) 
}