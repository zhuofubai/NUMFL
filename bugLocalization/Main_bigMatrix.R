rm(list = ls())
source('C:/Users/zhuofu/RProject/bugLocalization/IVDModel.R')
clustNum=10;
dir<-"C:/Users/zhuofu/workspace/apacheCommonMath3.1.1/TestBlockRealMatrix/Data";
n=10;
IVDO<- rep(0, n)
ply<- rep(0, n)
dln<-rep(0,n)
IVDOC<- rep(0, n)
IVDOSC<- rep(0, n)
percent_IVDO<- rep(0, n)
percent_ply<- rep(0, n)
percent_dln<-rep(0,n)
for(datafolder in 1:n){
  #datafolder=7;
  result=IVDModel(dir,datafolder,clustNum);
  IVDO[datafolder]=result$IVDO
  IVDOC[datafolder]=result$IVDO_c
  IVDOSC[datafolder]=result$IVDO_sc
  ply[datafolder]=result$ply
  percent_IVDO[datafolder]=result$IVDO/result$n
  percent_ply[datafolder]=result$ply/result$n
  percent_dln[datafolder]=result$dln/result$n
}