importance <- function (fp,sp,y_raw,data, NumF) {
  data<-as.matrix(data);
  common<-intersect(y_raw[,1], data[,1]);
  obsy<-which(y_raw[,1] %in% common);
  obsx<-which(data[,1] %in% common);
  
  y=y_raw[obsy,2];
  sp_obs=length(which(y<0.5));
  fp_obs=length(which(y>0.5));
  
  increase_p<-fp_obs/(sp_obs+fp_obs)-fp/(sp+fp);
  importance_p<-2/(1/increase_p+log(NumF)/log(fp_obs));
  return(importance_p);
}