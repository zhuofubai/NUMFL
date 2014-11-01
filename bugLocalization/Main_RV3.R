rm(list = ls())
source('C:/Users/zhuofu/RProject/bugLocalization/IVDModel.R')
clustNum=10;
dir<-"C:/Users/zhuofu/workspace21/apacheCommonMath3.1.1/TestRotationAndVector3D/Data";
n=10;
IVDO<- rep(0, n)
ply<- rep(0, n)
ply_c<- rep(0, n)
ply_sc<- rep(0, n)

dln<-rep(0,n)
dln_c<-rep(0,n)
dln_sc<-rep(0,n)

percent_IVDO<- rep(0, n)

percent_ply<- rep(0, n)
percent_ply_c<- rep(0, n)
percent_ply_sc<- rep(0, n)

percent_dln<-rep(0,n)
percent_dln_c<-rep(0,n)
percent_dln_sc<-rep(0,n)

for(datafolder in 1:10){
result=IVDModel(dir,datafolder,clustNum);
IVDO[datafolder]=result$IVDO
ply[datafolder]=result$ply
ply_c[datafolder]=result$ply_c
ply_sc[datafolder]=result$ply_sc

dln[datafolder]=result$dln
dln_c[datafolder]=result$dln_c
dln_sc[datafolder]=result$dln_sc

percent_IVDO[datafolder]=result$IVDO/result$n
percent_ply[datafolder]=result$ply/result$n
percent_ply_c[datafolder]=result$ply_c/result$n
percent_ply_sc[datafolder]=result$ply_sc/result$n

percent_dln[datafolder]=result$dln/result$n
percent_dln_c[datafolder]=result$dln_c/result$n
percent_dln_sc[datafolder]=result$dln_sc/result$n

}
percent_ply
print(mean(percent_ply))
percent_ply_c
print(mean(percent_ply_c))
percent_ply_sc
print(mean(percent_ply_sc))

percent_dln
print(mean(percent_dln))
percent_dln_c
print(mean(percent_dln_c))
percent_dln_sc
print(mean(percent_dln_sc))