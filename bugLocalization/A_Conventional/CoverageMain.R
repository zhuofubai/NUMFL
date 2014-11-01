rm(list = ls())
source('C:/Users/zhuofu/RProject/bugLocalization/A_Conventional/CoverageScore.R')

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
  write(percent_Tarantula, file="Tarantula_large", append=TRUE, ncolumns=n, sep=" ")
  write(percent_Ochiai, file="Ochiai_large", append=TRUE, ncolumns=n, sep=" ")
  write(percent_F1, file="F1_large", append=TRUE, ncolumns=n, sep=" ")
  write(percent_Dstar, file="Dstar_large", append=TRUE, ncolumns=n, sep=" ")
}



