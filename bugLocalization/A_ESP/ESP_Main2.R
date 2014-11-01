rm(list = ls())
source('C:/Users/zhuofu/RProject/bugLocalization/A_ESP/ESP.R')

dir1<-"C:/Users/zhuofu/workspace/ojalgo-33/TestPrimitiveDensityStore/Data";
dir2<-"C:/Users/zhuofu/workspace4/jama/TestJama/Data";

n1=10
n2=10;

Dirs=c(dir1,dir2)
vers=c(n1,n2)



for(index in 1:length(Dirs)){
  ptm<-proc.time();
  dir<-Dirs[index]
  n<-vers[index] 
  print(dir)
  print(n)
  simple<- rep(0, n)
  complex<- rep(0, n)
  percent_simple<- rep(0, n)
  percent_complex<- rep(0, n)
  for(datafolder in 1:n){
    result<-ESP(dir,datafolder);
    simple[datafolder]<-result$simple;
    complex[datafolder]<-result$complex;
    percent_simple[datafolder]<-simple[datafolder]/result$n
    percent_complex[datafolder]<-complex[datafolder]/result$n   
  }
  time=proc.time()-ptm;
  write(time,file="time_ESP", append=TRUE, ncolumns=3, sep=" ");
  write(percent_simple, file="simple_EXP_jama_ojalgo", append=TRUE, ncolumns=n, sep=" ")
  write(percent_complex, file="complex_EXP_jama_ojalgo", append=TRUE, ncolumns=n, sep=" ")
}


# percent_simple
# print(mean(percent_simple))
# percent_complex
# print(mean(percent_complex))
