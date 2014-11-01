rm(list = ls())
source('C:/Users/zhuofu/RProject/bugLocalization/A_SOBER/SOBER.R')


dir1<-"C:/Users/zhuofu/workspace/ojalgo-33/TestPrimitiveDensityStore/Data";
dir2<-"C:/Users/zhuofu/workspace4/jama/TestJama/Data";

n1=10
n2=10;

Dirs=c(dir1,dir2)
vers=c(n1,n2)



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
  write(percent_sober, file="sober_jama_ojalgo", append=TRUE, ncolumns=n, sep=" ")
  
}


# percent_simple
# print(mean(percent_simple))
# percent_complex
# print(mean(percent_complex))
