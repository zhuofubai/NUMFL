rm(list = ls())
source('C:/Users/zhuofu/RProject/bugLocalization/A_SOBER/SOBER.R')


source('C:/Users/zhuofu/RProject/bugLocalization/IVDModel.R')

dir1<-"C:/Users/zhuofu/workspace6/SciMark1.0/TestScimarkLU/Data";
dir2<-"C:/Users/zhuofu/workspace6/SciMark1.0/TestScimarkFFT/Data";

n1=4;
n2=4;

Dirs=c(dir1,dir2)
vers=c(n1,n2)
clustNum=10;


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
  write(percent_sober, file="sober_sciMark", append=TRUE, ncolumns=n, sep=" ")
  
}


# percent_simple
# print(mean(percent_simple))
# percent_complex
# print(mean(percent_complex))
