buildDoubleLinear <- function (subdataX,subdataY,subtruth, ctrl=FALSE) {
  source('C:/Users/zhuofu/RProject/bugLocalization/standardize.R')
  source('C:/Users/zhuofu/RProject/bugLocalization/buildlinear.R')
  T<-abs(subdataX[,1]);
  k<-order(T);
  n<-length(k);
  small<-k[1:floor(n/2)];
  large<-k[(floor(n/2)+1):n];
  #print(n)
  ctl=ctrl;
  mod_lm1<-buildlinear(subdataX[small,],subdataY[small],subtruth[small], ctrl)
  mod_lm2<-buildlinear(subdataX[large,],subdataY[large],subtruth[large], ctrl)
  coef<-(mod_lm1$Coefficient+mod_lm2$Coefficient)/2
  sdcoef<-(mod_lm1$StandardCoeff+mod_lm2$StandardCoeff)/2
  tvalue<-(mod_lm1$Tvalue+mod_lm2$Tvalue)/2
  
  return(list(mod1=mod_lm1$mod,mod2=mod_lm2$mod, mod_sc1=mod_lm1$mod_sc, mod_sc2=mod_lm2$mod_sc, Tvalue=tvalue,Coefficient=coef,StandardCoeff=sdcoef));
}