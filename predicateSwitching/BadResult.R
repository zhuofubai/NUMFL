
BadResult <- function(niteration, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10,
                      x11, x12, x13, x14, x15, w2, w3, w5, w6, w8, w9) {
  m <- 0;
  n <- niteration;
  #print (length(x1))
  for (i in 1:n){
    if (x1[i] > x2[i]){                    #P1
      if (x3[i] > 0 & w2[i]){      #S1   #P2
        if (x4[i] > 0 & w3[i]){  #S2   #P3
          m <- m+x5[i];       #S3
        }
      }
    }
    if (x6[i] > x7[i]){                    #P4
      if (x8[i] > 0 & w5[i]){      #S4   #P5
        if (x9[i] > 0 & w6[i]){  #S5   #P6 bug
          m <- m+x10[i];      #S6
        }
      }
    }
    if (x11[i] > x12[i]){                  #P7
      if (x13[i] > 0 & w8[i]){     #S7   #P8
        if (x14[i] > 0 & w9[i]){ #S8   #P9
          m <- m+x15[i];      #S9
        }
      }
    }
  }
  return (m);
}