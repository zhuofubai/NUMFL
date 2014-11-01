rm(list = ls())
source('C:/Users/zhuofu/RProject/bugLocalization/A_Conventional/CoverageScore.R')

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
  Tarantula<- rep(0, n)
  Ochiai<- rep(0, n)
  F1<- rep(0, n)
  Dstar<-  rep(0, n)
  
  percent_Tarantula<- rep(0, n)
  percent_Ochiai<- rep(0, n)
  percent_F1<- rep(0, n)
  percent_Dstar<- rep(0, n)
  
  for(datafolder in 1:n){
    result<-CoverageScore(dir,datafolder);
    
    Tarantula[datafolder]<- result$Tarantula;
    Ochiai[datafolder]<- result$Ochiai;
    F1[datafolder]<- result$F1;
    Dstar[datafolder]<-  result$Dstar;
    
    percent_Tarantula[datafolder]<- Tarantula[datafolder]/result$n
    percent_Ochiai[datafolder]<- Ochiai[datafolder]/result$n
    percent_F1[datafolder]<- F1[datafolder]/result$n
    percent_Dstar[datafolder]<- Dstar[datafolder]/result$n
    
  }
  write(percent_Tarantula, file="Tarantula_small", append=TRUE, ncolumns=n, sep=" ")
  write(percent_Ochiai, file="Ochiai_small", append=TRUE, ncolumns=n, sep=" ")
  write(percent_F1, file="F1_small", append=TRUE, ncolumns=n, sep=" ")
  write(percent_Dstar, file="Dstar_small", append=TRUE, ncolumns=n, sep=" ")
}

