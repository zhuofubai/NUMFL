rm(list = ls())
source('C:/Users/zhuofu/RProject/bugLocalization/A_Conventional/CoverageScore.R')

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
  write(percent_Tarantula, file="Tarantula_jama_ojalgo", append=TRUE, ncolumns=n, sep=" ")
  write(percent_Ochiai, file="Ochiai_jama_ojalgo", append=TRUE, ncolumns=n, sep=" ")
  write(percent_F1, file="F1_jama_ojalgo", append=TRUE, ncolumns=n, sep=" ")
  write(percent_Dstar, file="Dstar_jama_ojalgo", append=TRUE, ncolumns=n, sep=" ")
}


