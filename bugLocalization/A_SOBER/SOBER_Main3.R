rm(list = ls())
source('C:/Users/zhuofu/RProject/bugLocalization/A_SOBER/SOBER.R')


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


for(index in 1:length(Dirs)){
  dir<-Dirs[index]
  n<-vers[index] 
  print(dir)
  print(n)
  sober_score<- rep(0, n)
  percent_sober<- rep(0, n)
  
  for(datafolder in 1:n){
    result<-SOBER(dir,datafolder);
    sober_score[datafolder]<-result$sober;
    percent_sober[datafolder]<-sober_score[datafolder]/result$n 
  }
  write(percent_sober, file="sober_small", append=TRUE, ncolumns=n, sep=" ")
  
}


# percent_simple
# print(mean(percent_simple))
# percent_complex
# print(mean(percent_complex))
