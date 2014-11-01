ESPScore <- function (l,j,k,mu_x,sigma_x,x,y_raw,data,fp,sp,NumF) {
  source('C:/Users/zhuofu/RProject/bugLocalization/A_ESP/importance.R')
  index1<-which(x<l*sigma_x)
  index2<-which(x>=(mu_x-k*sigma_x)&&x<=(mu_x-j*sigma_x))
  index3<-which(mu_x==x)
  index4<-which(x>=(mu_x+j*sigma_x)&&x<(mu_x+k*sigma_x))
  index5<-which(x>(mu_x+l*sigma_x))
  
  data_1<-data[index1,];
  data_2<-data[index2,];
  data_3<-data[index3,];
  data_4<-data[index4,];
  data_5<-data[index5,];
  
  #   obs_1=intersect(y_raw[,1], data_1[,1]);
  #   y1=y[cover_1];
  #   sp_obs=length(which(y<0.5));
  #   fp_obs=length(which(y>0.5));
  if(length(index1)==0)
  {importance_1<-0;}
  else
  {importance_1<-importance(fp,sp,y_raw, data_1, NumF);}
  #print(1)
 
  if(length(index2)==0)
  {importance_2<-0;}
  else
  {importance_2<-importance(fp,sp,y_raw, data_2, NumF);}
  #print(2)
  
  if(length(index3)==0)
  {importance_3<-0;}
  else
  {importance_3<-importance(fp,sp,y_raw, data_3, NumF);}
  #print(3)
  
  if(length(index4)==0)
  {importance_4<-0;}
  else
  {importance_4<-importance(fp,sp,y_raw, data_4, NumF);}
  #print(4)
  
  if(length(index5)==0)
  {importance_5<-0;}
  else
  {importance_5<-importance(fp,sp,y_raw, data_5, NumF);}
  #print(5)
  
  result<-c(importance_1,importance_2,importance_3,importance_4,importance_5);
  return(result);
}