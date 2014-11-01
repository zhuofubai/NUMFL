ESP_complex <- function (y_raw,data,fp,sp) {
  source('C:/Users/zhuofu/RProject/bugLocalization/A_ESP/ESP_simple.R');
    
  y = data.matrix(y_raw[, 2])
  NumF=sum(y);
  
  data<-as.matrix(data)
  n=ncol(data);
  if(n==2){
    x <- data[, 2];
    result1<-ESP_simple(y_raw,data,x,fp,sp);
    return (as.matrix(result1));
  }
  else if(n>2){
    x<-data[, 2];
    c<-data[, 3:n];
    c<-as.matrix(c);
    c_num<-ncol(c);
    result<-matrix(, nrow = c_num, ncol = 15);
    for(j in 1:c_num)
    {
      diff<-x-c[,j];
      result_temp<-ESP_simple(y_raw,data,diff,fp,sp);
      result[j,]<-result_temp;
    }
    return (result);
  }
  else{
    print("data dimension is incorrect");
    print(n);
    return (0);
  }
  
  
}