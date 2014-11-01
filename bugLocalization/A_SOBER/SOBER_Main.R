rm(list = ls())
source('C:/Users/zhuofu/RProject/bugLocalization/A_SOBER/SOBER.R')

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
  sober_score<- rep(0, n)
  percent_sober<- rep(0, n)

  for(datafolder in 1:n){
    result<-SOBER(dir,datafolder);
    sober_score[datafolder]<-result$sober;
    percent_sober[datafolder]<-sober_score[datafolder]/result$n 
  }
  write(percent_sober, file="sober", append=TRUE, ncolumns=n, sep=" ")
  
}


# percent_simple
# print(mean(percent_simple))
# percent_complex
# print(mean(percent_complex))
