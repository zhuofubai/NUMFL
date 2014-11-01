rm(list = ls())
source('C:/Users/zhuofu/RProject/bugLocalization/IVDModel.R')


dir1<-"C:/Users/zhuofu/workspace6/apacheCommonMath3.1.1/TestSymmLQ/Data";
dir2<-"C:/Users/zhuofu/workspace6/apacheCommonMath3.1.1/TestSplineInterpolator/Data";
dir3<-"C:/Users/zhuofu/workspace6/apacheCommonMath3.1.1/TestSimpleRegression/Data";
dir4<-"C:/Users/zhuofu/workspace6/apacheCommonMath3.1.1/TestSchurTransformer/Data";
dir5<-"C:/Users/zhuofu/workspace6/apacheCommonMath3.1.1/TestMillerUpdatingRegression/Data";
dir6<-"C:/Users/zhuofu/workspace6/apacheCommonMath3.1.1/TestHarmonicFitter/Data";
dir7<-"C:/Users/zhuofu/workspace6/apacheCommonMath3.1.1/TestFastSine/Data";
dir8<-"C:/Users/zhuofu/workspace6/apacheCommonMath3.1.1/TestFastCosine/Data";

n1=2;n2=2;n3=2;n4=2;n5=2;n6=2;n7=2;n8=2;

Dirs=c(dir1,dir2,dir3,dir4,dir5,dir6,dir7,dir8)
vers=c(n1,n2,n3,n4,n5,n6,n7,n8)
clustNum=10;
#1:length(Dirs)
for(index in 1:length(Dirs) ){
  ptm<-proc.time();
  dir<-Dirs[index]
  n<-vers[index]
  print(dir)
  print(n)
  IVDO<- rep(0, n)
  ply<- rep(0, n)
  IVDOC<- rep(0, n)
  IVDOSC<- rep(0, n)
  percent_IVDO<- rep(0, n)
  percent_ply<- rep(0, n)
  percent_dln<-rep(0,n)
  
  for(datafolder in 1:n){
    result=IVDModel(dir,datafolder,clustNum);
    IVDO[datafolder]=result$IVDO
    IVDOC[datafolder]=result$IVDO_c
    IVDOSC[datafolder]=result$IVDO_sc
    ply[datafolder]=result$ply
    percent_IVDO[datafolder]=result$IVDO/result$n
    percent_ply[datafolder]=result$ply/result$n
    percent_dln[datafolder]=result$dln/result$n 
  }
#  time=proc.time()-ptm;
#  write(time,file="time3", append=TRUE, ncolumns=3, sep=" ");
#  write(percent_ply, file="ply_small", append=TRUE, ncolumns=n, sep=" ")
  write(percent_dln, file="dln_small_uncontrol", append=TRUE, ncolumns=n, sep=" ")
  
}


# percent_simple
# print(mean(percent_simple))
# percent_complex
# print(mean(percent_complex))
