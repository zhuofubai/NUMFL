source('C:/Users/zhuofu/RProject/bugLocalization/IVDModel.R')
clustNum=10;
dir<-"C:/Users/zhuofu/workspace2/apacheCommonMath3.1.1/TestRotationAndVector3D/Data";
n=10;
IVDO<- rep(0, n)
ply<- rep(0, n)
percent_IVDO<- rep(0, n)
percent_ply<- rep(0, n)
for(datafolder in 1:n){
result=IVDModel(dir,datafolder,clustNum);
IVDO[datafolder]=result$IVDO
ply[datafolder]=result$ply
percent_IVDO[datafolder]=result$IVDO/result$n
percent_ply[datafolder]=result$ply/result$n
}