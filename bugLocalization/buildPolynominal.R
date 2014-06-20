
buildPolynominal <- function (subdataX,subdataY,subtruth, ctrl=FALSE) {
  source('C:/Users/zhuofu/RProject/bugLocalization/standardize.R')
  subdataX2<-standardize(subdataX);
  subdataX2 <- subdataX2[, colSums(is.na(subdataX2)) == 0]
  subdataX2<-as.matrix(subdataX2);
  T<-subdataX[,1];
  T2<-subdataX2[,1];
  
  if(ctrl){
  lmfit_ply<-lm(subdataY~I(subdataX2^2))
  lmfit_ply_sc<-lm(subdataY~I(subdataX^2))
  ts<-ncol(subdataX2)
  
  tvalue<-abs(summary(lmfit_ply)$coefficients[2*(ts+1)+2])
  coef<-abs(coefficients(lmfit_ply)[2])
  sdcoef<-abs(coefficients(lmfit_ply_sc)[2]*sd(subdataX[,1])/sd(subdataY))
  
  }
  else{
    lmfit_ply<-lm(subdataY~I(T2^2)+T2)
    lmfit_ply_sc<-lm(subdataY~I(T^2)+T2)
    ts<-2;
    
    tvalue<-abs(summary(lmfit_ply)$coefficients[2*(ts+1)+2])
    coef<-abs(coefficients(lmfit_ply)[2])
    sdcoef<-abs(coefficients(lmfit_ply_sc)[2]*sd(subdataX[,1])/sd(subdataY))
    
  }
  return(list(mod=lmfit_ply,mod_sc=lmfit_ply_sc,Tvalue=tvalue,Coefficient=coef,StandardCoeff=sdcoef));
}