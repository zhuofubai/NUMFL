source('C:/Users/zhuofu/RProject/bugLocalization/A_SOBER/SP.R')
SoberScore <- function (y,x) {
  
  thetaF<-which(y>=0.5)
  thetaP<-which(y<0.5)
  m=length(thetaF)
  n=length(thetaP)
  
  xF<-x[thetaF];
  xP<-x[thetaP];
  
  
  k=length(x);
  
  index1_F<-which(xF<0)
  index1_P<-which(xP<0)
  
  index2_F<-which(xF>0)
  index2_P<-which(xP>0)
  
  index3_F<-which(xF==0)
  index3_P<-which(xP==0)
  
  
  if(m==0){
    score_1<-0;
    score_2<-0;
    score_3<-0;
  }else if(n==0){
    score_1<-Inf;
    score_2<-Inf;
    score_3<-Inf;
  }else{
    
    score_1<-SP(index1_F,index1_P,m,n);
    #print(1)
    
    score_2<-SP(index2_F,index2_P,m,n);
    #print(2)
    
    score_3<-SP(index3_F,index3_P,m,n);
       
  }
  result<-c(score_1,score_2,score_3);
  return(result);
}