####################################################
##use t-value as suspiciousness score
##standardize data/normalize data
##add data preprocessing to handle loop case
####################################################
rm(list = ls())
library(cluster)
library(pls)
source("C:/Users/zhuofu/RProject/bugLocalization/normalize.R")
source("C:/Users/zhuofu/RProject/bugLocalization/standardize.R")
source('C:/Users/zhuofu/RProject/bugLocalization/preprosseData.R')
source('C:/Users/zhuofu/RProject/bugLocalization/buildlinear.R')
source('C:/Users/zhuofu/RProject/bugLocalization/buildPolynominal.R')
source('C:/Users/zhuofu/RProject/bugLocalization/buildDoubleLinear.R')
n <- 504
result <- rep(0, n)

result_IVD_r<-result
result_IVD_r_sc<-result
result_IVDO_r<-result
result_IVDO_r_sc<-result

#result_IVDO2<-result
result_IVDO <- result
result_IVDO_c<-result
result_IVDO_sc<-result

result_ply<-result
result_ply_c<-result
result_ply_sc<-result

result_dln<-result
result_dln_c<-result
result_dln_sc<-result
# result_rsq_ply<-result
# result_rsq_ply_sc<-result
k <- 10;
k2 <- 10;
temp <- rep(1, k)
bugid <-185##bug1 on line 780
dir<-"C:/Users/zhuofu/workspace21/apacheCommonMath3.1.1/TestRotationAndVector3D/Data";
datafolder<-8;
for (i in 1:n) {
  print(i)
  
  y_raw<-read.table(paste(c(dir,as.character(datafolder),"/out.txt"),collapse=""));
  filename<-paste(c(dir,as.character(datafolder),"/",as.character(i), ".txt"),collapse="")
  truth_raw<-read.table(paste(c(dir,as.character(datafolder),"/truth.txt"),collapse=""));
  diff_raw<-read.table(paste(c(dir,as.character(datafolder),"/diff.txt"),collapse=""));
  if (!file.exists(filename)){    
    print("not exist");
    next
  }
  
  data_raw <- read.table(filename, skip = 1,fill=T)## read variables
  
  
  #############preprocess the data############
  data<-preprosseData(data_raw,y_raw,truth_raw,diff_raw);
  X<-data$X;
  y<-data$Y;
  truth<-data$TRUTH;
  diff<-data$DIFFERENCE
  
  if(sum(is.finite(X)-1)<0){
    print("infinite value at")
    next
  }
  #############if treatment is constant  skip,if data size is too small, skip, if y is all 0 or all 1, skip#############
  if (length(y)<30||var(X[, 1])==0||sum(y)==0||sum(y)==length(y))
  {    next }   
  ##############################################################################
  m <- ncol(as.matrix(X))
  
  if (m==1)
  {
    print("no valid covaiate for treatment No "+i);
    next;
  }
  else
  {
    #      T <-standardize( X[, 1])
    #      C <- standardize(X[, 2:m])
    #      C[is.na(C)]=0;
    #      T[is.na(T)]=0;
    T <- X[, 1]
    C <- X[, 2:m]
    ##################################################################################
    ##IVD model
    ## calculate pscore, a linear model between treatment and covariates
    
    ###########################################IVDO########################################
    
    lmfit_ps <- lm(T~C-1);
    coeff <- coefficients(lmfit_ps);
    coeff[is.na(coeff)]=0
    #C2 <- cbind(1,C);
    if(ncol(as.matrix(C))==1)
    {
      pscore_IVD <- C*coeff;
    }else{
      pscore_IVD <- C%*%coeff;
    }
    
    clustindex_IVD<-sort(pscore_IVD,method="sh", index.return=TRUE)$ix;
    binsize<-floor(length(clustindex_IVD)/k2);#Bin the pscore into roughly equal size
    #u_IVD <- pam(pscore_IVD, k)
    ##################################################################################   
    weight_IVDO<-rep(0,k2);
    sum1<-0;
    sum2<-0;
    
    tempresult<-rep(0,k2);
    tempresult_c<-tempresult;
    tempresult_sc<-tempresult;
    
    sum_c<-0;
    sum_sc<-0;
    #########################   
    tempply<-rep(0,k2);
    tempply_c<-rep(0,k2);
    tempply_sc<-rep(0,k2);
    
    
    sum_ply<-0;
    sum_ply_c<-0;
    sum_ply_sc<-0;  
    #########################
    tempdln<-rep(0,k2);
    tempdln_c<-rep(0,k2);
    tempdln_sc<-rep(0,k2);
    
    sum_dln<-0;
    sum_dln_c<-0;
    sum_dln_sc<-0;
    #########################
    for (j in 1: k2)
    {
      if(j==k2){
        subclustindex<-clustindex_IVD[((j-1)*binsize+1):length(clustindex_IVD)];
      }else{
        subclustindex<-clustindex_IVD[((j-1)*binsize+1):(j*binsize)];
      }
      
      subdataY<-y[subclustindex];
      subdiff<-diff[subclustindex];
      subdataX <- X[subclustindex, ];
      subtruth<-truth[subclustindex];
      weight_IVDO[j] <- length(subdataY)/length(y)
      #   sum(subdataY); length(subdataY); cv;
      if(sum(subdataY)==0||sum(subdataY)==length(subdataY)||var(subdataX[, 1])==0)#||abs(cv)>0.10)
      {sum2<-sum2+length(subdataY);
       next;
      }
      
      
      #     mod_lm<-buildlinear(subdataX,subdataY,subtruth, ctrl=FALSE)
      #     mod_ply<-buildPolynominal(subdataX,subdataY,subtruth, ctrl=FALSE)
      mod_lm2<-buildlinear(subdataX,subdiff,subtruth, ctrl=FALSE)
      
      ########
      tempresult[j]<-mod_lm2$Tvalue
      tempresult_c[j]<-mod_lm2$Coefficient
      tempresult_sc[j]<-mod_lm2$StandardCoeff
      
      sum1<-sum1+weight_IVDO[j]*tempresult[j]
      sum_c<-sum_c+weight_IVDO[j]*tempresult_c[j]
      sum_sc<-sum_sc+weight_IVDO[j]*tempresult_sc[j]
      #########
      mod_ply2<-buildPolynominal(subdataX,subdiff,subtruth, ctrl=TRUE)
      
      tempply[j]<-mod_ply2$Tvalue
      tempply_c[j]<-mod_ply2$Coefficient
      tempply_sc[j]<-mod_ply2$StandardCoeff
      
      sum_ply<-sum_ply+weight_IVDO[j]*tempply[j]
      sum_ply_c<-sum_ply_c+weight_IVDO[j]* tempply_c[j]
      sum_ply_sc<-sum_ply_sc+weight_IVDO[j]*tempply_sc[j]
      #########
      mod_dln<-buildDoubleLinear(subdataX,subdiff,subtruth,ctrl=TRUE)
      
      tempdln[j]<-mod_dln$Tvalue
      tempdln_c[j]<-mod_dln$Coefficient
      tempdln_sc[j]<-mod_dln$StandardCoeff
      
      sum_dln<-sum_dln+weight_IVDO[j]*tempdln[j]
      sum_dln_c<-sum_dln_c+weight_IVDO[j]* tempdln_c[j]
      sum_dln_sc<-sum_dln_sc+weight_IVDO[j]*tempdln_sc[j]
      #print(sum_dln)
    }
    total=length(y);
    
    sum1<-sum1*length(y)/(length(y)-sum2)
    sum_c<-sum_c*length(y)/(length(y)-sum2)
    sum_sc<-sum_sc*length(y)/(length(y)-sum2)
    
    
    sum_ply<-sum_ply*length(y)/(length(y)-sum2)
    sum_ply_c<-sum_ply_c*length(y)/(length(y)-sum2)
    sum_ply_sc<-sum_ply_sc*length(y)/(length(y)-sum2)
    
    
    sum_dln<-sum_dln*length(y)/(length(y)-sum2)
    sum_dln_c<-sum_dln_c*length(y)/(length(y)-sum2)
    sum_dln_sc<-sum_dln_sc*length(y)/(length(y)-sum2)
    
    result_IVDO[i] <- sum1
    result_IVDO_c[i]<-sum_c
    result_IVDO_sc[i] <- sum_sc	
    
    result_ply[i]<-sum_ply
    result_ply_c[i]<-sum_ply_c
    result_ply_sc[i]<-sum_ply_sc
    
    result_dln[i]<-sum_dln
    result_dln_c[i]<-sum_dln_c
    result_dln_sc[i]<-sum_dln_sc
    ####################################################
  }
}


which.max(result_IVDO)
which.max(result_IVDO_c)
which.max(result_IVDO_sc)
################

#################
result_IVDO[is.na(result_IVDO)]=0
result_IVDO_c[is.na(result_IVDO_c)]=0
result_IVDO_sc[is.na(result_IVDO_sc)]=0
result_ply[is.na(result_ply)]=0
result_ply_c[is.na(result_ply_c)]=0
result_ply_sc[is.na(result_ply_sc)]=0

n-rank(result_IVDO)[bugid]+1
n-rank(result_IVDO_c)[bugid]+1
n-rank(result_IVDO_sc)[bugid]+1


n-rank(result_ply)[bugid]+1
n-rank(result_ply_c)[bugid]+1
n-rank(result_ply_sc)[bugid]+1

n-rank(result_dln)[bugid]+1
n-rank(result_dln_c)[bugid]+1
n-rank(result_dln_sc)[bugid]+1

