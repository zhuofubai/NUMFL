fillA <- function(c, mu, sigma) {
  a = rep(0, length(c));
  for (i in 1:(length(a) - 1)) {
    if (i %% 2 == 1) {
      a[i] <- c[i];
    } else {
      if (c[i] - 1 != 0) {
        a[i] <- c[i]/(c[i] - 1);
      }
    }
  }
  return(a)
}