rm(list = ls())
source('C:/Users/zhuofu/RProject/bugLocalization/IVDModel.R')

dir1<-"C:/Users/zhuofu/workspace/ojalgo-33/TestPrimitiveDensityStore/Data";
dir2<-"C:/Users/zhuofu/workspace4/jama/TestJama/Data";

n1=10
n2=10;

Dirs=c(dir1,dir2)
vers=c(n1,n2)
clustNum=10;

for(index in 1:length(Dirs)){
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
  time=proc.time()-ptm;
 # write(time,file="time2", append=TRUE, ncolumns=3, sep=" ");
 # write(percent_ply, file="ply_ojalo_jama2", append=TRUE, ncolumns=n, sep=" ")
  write(percent_dln, file="dln_ojalo_jama_uncontrol", append=TRUE, ncolumns=n, sep=" ")
  
}


# percent_simple
# print(mean(percent_simple))
# percent_complex
# print(mean(percent_complex))
