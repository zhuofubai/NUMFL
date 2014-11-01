rm(list = ls())
source('C:/Users/zhuofu/RProject/bugLocalization/A_ESP/ESP.R')

dir1<-"C:/Users/zhuofu/workspace/apacheCommonMath2.1/TestEigenDecomposition/Data";###oldData2
dir2<-"C:/Users/zhuofu/workspace/apacheCommonMath3.1.1/TestDSCompiler/Data";
dir3<-"C:/Users/zhuofu/workspace/apacheCommonMath3.1.1/TestBlockRealMatrix/Data";
dir4<-"C:/Users/zhuofu/workspace2/apacheCommonMath3.1.1/TestRotationAndVector3D/Data";
n1=10;
n2=11;
n3=10
n4=10;

Dirs=c(dir1,dir2,dir3,dir4)
vers=c(n1,n2,n3,n4)


for(index in 1:length(Dirs)){
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
  write(percent_simple, file="simple_EXP", append=TRUE, ncolumns=n, sep=" ")
  write(percent_complex, file="complex_EXP", append=TRUE, ncolumns=n, sep=" ")
}


# percent_simple
# print(mean(percent_simple))
# percent_complex
# print(mean(percent_complex))
