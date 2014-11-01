SP <- function (index_F,index_P,m,n) {
  
  XFail<-rep(0,m)
  XPass<-rep(0,n)
  
  XFail[index_F]=1;
  XPass[index_P]=1;
  if(sum(XPass)==0){
    if(sum(XFail)==0){
      sober_p=-Inf;
    }else{
      sober_p=Inf;
    }
  }else if(sum(XPass)==n){
    if(sum(XFail)==m){
      sober_p=-Inf;
    }else{
      sober_p=Inf;
    }
  }
  else{
  
  Y=sum(XFail)/m
  
  mu_p=sum(XPass)/n
  sigma_p=var(XPass)
  
  
  Z=(Y-mu_p)/(sigma_p/sqrt(m))
  sober_p=log(sigma_p/(sqrt(m)*dnorm(Z)))
  }
  return(sober_p);
}