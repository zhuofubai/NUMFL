
badFun <- function(a, c) {
  b <- rep(0, length(a));
  for (i in 1:length(a)) {
    b[i] <- a[i] + c[i];
  }
  return(b);
}

