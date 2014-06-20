buildlinear <- function (subdataX,subdataY,subtruth, ctrl=FALSE) {
  source('C:/Users/zhuofu/RProject/bugLocalization/standardize.R')
  
  subdataX2<-standardize(subdataX);
  subdataX2 <- subdataX2[, colSums(is.na(subdataX2)) == 0]
  subdataX2<-as.matrix(subdataX2);
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
  return(list(mod=lmfit_IVDO,mod_sc=lmfit_IVDO_sc,Tvalue=tvalue,Coefficient=coef,StandardCoeff=sdcoef));
}

