buildlinear <- function (subdataX,subdataY,subtruth, ctrl=FALSE) {
  source('C:/Users/zhuofu/RProject/bugLocalization/standardize.R')
  
  subdataX2<-standardize(subdataX);
  subdataX2 <- subdataX2[, colSums(is.na(subdataX2)) == 0]
  subdataX2<-as.matrix(subdataX2);
  if(length(subdataY)<2){
    return(list(mod=0,mod_sc=0,Tvalue=0,Coefficient=0,StandardCoeff=0));
  }
  if(sum(is.na(subdataX2)-1)==0||var(subdataY)==0||var(subdataX[, 1])==0)
  {
    return(list(mod=0,mod_sc=0,Tvalue=0,Coefficient=0,StandardCoeff=0));
    
  }
  
  T<-abs(subdataX[,1]);
  T2<-abs(subdataX2[,1]);
 
  
  if(ctrl){
    
    lmfit_IVDO<-lm(subdataY~subdataX2)
    lmfit_IVDO_sc<-lm(subdataY~subdataX)  
    
    ts<-ncol(subdataX2)
    
    tvalue <- summary(lmfit_IVDO)$coefficients[2*(ts+1)+2]
    tvalue <- abs(tvalue)
    #tempPvalue[j] <- summary(lmfit_IVDO)$coefficients[3*(ts+1)+2]
    
    
    coef <- coefficients(lmfit_IVDO)[2]
    coef <-abs(coef)
    
    sdcoef <- coefficients(lmfit_IVDO_sc)[2]*sd(subdataX[,1])/sd(subdataY)
    sdcoef <- abs(sdcoef)
  }
  else{
    
    lmfit_IVDO<-lm(subdataY~T2+subtruth)
    lmfit_IVDO_sc<-lm(subdataY~T+subtruth)  
    
    ts<-2
    
    tvalue <- summary(lmfit_IVDO)$coefficients[2*(ts+1)+2]
    tvalue <- abs(tvalue)
    #tempPvalue[j] <- summary(lmfit_IVDO)$coefficients[3*(ts+1)+2]
    
    
    coef <- coefficients(lmfit_IVDO)[2]
    coef <-abs(coef)
    
    sdcoef <- coefficients(lmfit_IVDO_sc)[2]*sd(subdataX[,1])/sd(subdataY)
    sdcoef <- abs(sdcoef)
    
  }
  if(is.na(sdcoef)){
    sdcoef=0;
  }
  if(is.na(coef)){
    coef=0;
  }
  if(is.na(tvalue)){
    tvalue=0;
  }
  return(list(mod=lmfit_IVDO,mod_sc=lmfit_IVDO_sc,Tvalue=tvalue,Coefficient=coef,StandardCoeff=sdcoef));
}

