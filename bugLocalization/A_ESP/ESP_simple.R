ESP_simple <- function (y_raw,data,x,fp,sp) {
  source('C:/Users/zhuofu/RProject/bugLocalization/A_ESP/ESPScore.R')
  y = data.matrix(y_raw[, 2])
  NumF=sum(y);
  
  mu_x<-mean(x);
  sigma_x<-sd(x);
  j=0;k=1;l=3;
  result1 <- ESPScore (l,j,k,mu_x,sigma_x,x,y_raw,data,fp,sp,NumF);
  j=1;k=2;l=3;
  result2 <- ESPScore (l,j,k,mu_x,sigma_x,x,y_raw,data,fp,sp,NumF);
  j=2;k=3;l=3;
  result3 <- ESPScore (l,j,k,mu_x,sigma_x,x,y_raw,data,fp,sp,NumF);
  
  result<-c(result1,result2,result3);
  return (result);
}