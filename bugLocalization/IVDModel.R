IVDModel <- function (dir,datafolder,clustNum) {
  ####################################################
  ##use t-value as suspiciousness score
  ##standardize data/normalize data
  ##add data preprocessing to handle loop case
  ####################################################
  library(cluster)
  library(pls)
  source("C:/Users/zhuofu/RProject/bugLocalization/normalize.R")
  source("C:/Users/zhuofu/RProject/bugLocalization/standardize.R")
  source('C:/Users/zhuofu/RProject/bugLocalization/preprosseData.R')
  source('C:/Users/zhuofu/RProject/bugLocalization/buildlinear.R')
  source('C:/Users/zhuofu/RProject/bugLocalization/buildPolynominal.R')
  source('C:/Users/zhuofu/RProject/bugLocalization/buildDoubleLinear.R')
  #n <- 404
  y_raw<-read.table(paste(c(dir,as.character(datafolder),"/out.txt"),collapse="")); 
  diff_raw<-read.table(paste(c(dir,as.character(datafolder),"/diff.txt"),collapse=""));
  truth_raw<-read.table(paste(c(dir,as.character(datafolder),"/truth.txt"),collapse=""));
  info<-read.table(paste(c(dir,as.character(datafolder),"/info.txt"),collapse=""))
  info<-as.matrix(info)
  n<-as.numeric(info[1])
  bugid<-as.numeric(info[2])
  
  print(c("n is",n))
  print(c("bugid is",bugid))
  
  result <- rep(0, n)
  
  result_IVDO <- result
  result_IVDO_c<-result
  result_IVDO_sc<-result
  
  result_ply<-result
  result_ply_c<-result
  result_ply_sc<-result
  
  result_dln<-result
  result_dln_c<-result
  result_dln_sc<-result
  
  k2 <- clustNum;
  for (i in 1:n) {
   #  print(i)
    
    
    filename<-paste(c(dir,as.character(datafolder),"/",as.character(i), ".txt"),collapse="")   
    if (!file.exists(filename)){    
      print("not exist");
      next
    }    
    data_raw <- read.table(filename, skip = 1,fill=TRUE)## read variables
    #############preprocess the data############
    data<-preprosseData(data_raw,y_raw,truth_raw,diff_raw);
    X<-data$X;
    y<-data$Y;
    truth<-data$TRUTH;
    diff<-data$DIFFERENCE
    
    ######################
    if(sum(is.finite(X)-1)<0){
      cat("infinite value at file: ", i)
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
      T <- X[, 1]
      C <- X[, 2:m]
      #       T <-standardize( X[, 1])
      #       C <- standardize(X[, 2:m])
      #       C[is.na(C)]=0;
      #       T[is.na(T)]=0;
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
        # print(j)
        if(j==k2){
          subclustindex<-clustindex_IVD[((j-1)*binsize+1):length(clustindex_IVD)];
        }else{
          subclustindex<-clustindex_IVD[((j-1)*binsize+1):(j*binsize)];
        }
        
        subdataY<-y[subclustindex];
        subdiff<-diff[subclustindex];
        subdataX <- X[subclustindex, ];
        subtruth<-truth[subclustindex];
        #   sum(subdataY); length(subdataY); cv;
        weight_IVDO[j] <- length(subdataY)/length(y)
        if(sum(subdataY)==0||sum(subdataY)==length(subdataY)||var(subdataX[, 1])==0)#||abs(cv)>0.10)
        {sum2<-sum2+length(subdataY);
         next;
        }
        #     mod_lm<-buildlinear(subdataX,subdataY,subtruth, ctrl=FALSE)
        #     mod_ply<-buildPolynominal(subdataX,subdataY,subtruth, ctrl=FALSE)
        ################################################################
        
        
        mod_lm2<-buildlinear(subdataX,subdiff,subtruth, ctrl=FALSE)
        
        
        tempresult[j]<-mod_lm2$Tvalue
        tempresult_c[j]<-mod_lm2$Coefficient
        tempresult_sc[j]<-mod_lm2$StandardCoeff
        
        
        ## weighted summation of coefficient
        
        sum1<-sum1+weight_IVDO[j]*tempresult[j]
        sum_c<-sum_c+weight_IVDO[j]*tempresult_c[j]
        sum_sc<-sum_sc+weight_IVDO[j]*tempresult_sc[j]
        ########################################################################       
        mod_ply2<-buildPolynominal(subdataX,subdiff,subtruth, ctrl=TRUE)
        
        tempply[j]<-mod_ply2$Tvalue
        tempply_c[j]<-mod_ply2$Coefficient
        tempply_sc[j]<-mod_ply2$StandardCoeff
        
        
        ## weighted summation of coefficient
#        sum_ply<-max(tempply)
#        sum_ply_c<-max(tempply_c)
#        sum_ply_sc<-max(tempply_sc)
        
         sum_ply<-sum_ply+weight_IVDO[j]*tempply[j]
         sum_ply_c<-sum_ply_c+weight_IVDO[j]* tempply_c[j]
        sum_ply_sc<-sum_ply_sc+weight_IVDO[j]*tempply_sc[j]
        ########################################################################
        
        mod_dln<-buildDoubleLinear(subdataX,subdiff,subtruth,ctrl=FALSE)
        
        tempdln[j]<-mod_dln$Tvalue
        tempdln_c[j]<-mod_dln$Coefficient
        tempdln_sc[j]<-mod_dln$StandardCoeff
        
#sum_dln<-max(tempdln)
#sum_dln_c<-max(tempdln_c)
#sum_dln_sc<-max(tempdln_sc)
         sum_dln<-sum_dln+weight_IVDO[j]*tempdln[j]
         sum_dln_c<-sum_dln_c+weight_IVDO[j]* tempdln_c[j]
         sum_dln_sc<-sum_dln_sc+weight_IVDO[j]*tempdln_sc[j]
        
      }
      total=length(y);
      
#       
       sum1<-sum1*length(y)/(length(y)-sum2)
       sum_c<-sum_c*length(y)/(length(y)-sum2)
       sum_sc<-sum_sc*length(y)/(length(y)-sum2)     
       
       sum_ply<-sum_ply*length(y)/(length(y)-sum2)
       sum_ply_c<-sum_ply_c*length(y)/(length(y)-sum2)
       sum_ply_sc<-sum_ply_sc*length(y)/(length(y)-sum2)
#       
       sum_dln<-sum_dln*length(y)/(length(y)-sum2)
       sum_dln_c<-sum_dln_c*length(y)/(length(y)-sum2)
       sum_dln_sc<-sum_dln_sc*length(y)/(length(y)-sum2)
      ###################################################      
      result_IVDO[i] <- sum1
      result_IVDO_c[i]<-sum_c
      result_IVDO_sc[i] <- sum_sc  
      
      result_ply[i]<-sum_ply
      result_ply_c[i]<-sum_ply_c
      result_ply_sc[i]<-sum_ply_sc
      
      result_dln[i]<-sum_dln
      result_dln_c[i]<-sum_dln_c
      result_dln_sc[i]<-sum_dln_sc
    }
  }
  
  
  #   which.max(result_IVDO)
  #   which.max(result_IVDO_c)
  #   which.max(result_IVDO_sc)
  ################
  
  #################
  result_IVDO[is.na(result_IVDO)]=0
  result_IVDO_c[is.na(result_IVDO_c)]=0
  result_IVDO_sc[is.na(result_IVDO_sc)]=0
  
  result_ply[is.na(result_ply)]=0
  result_ply_c[is.na(result_ply_c)]=0
  result_ply_sc[is.na(result_ply_sc)]=0
  
  result_dln[is.na(result_dln)]=0
  result_dln_c[is.na(result_dln_c)]=0
  result_dln_sc[is.na(result_dln_sc)]=0
  ##################################
  
  if (result_IVDO[bugid]==0){
    IVDO=0;
  }else{
    IVDO= n-rank(result_IVDO)[bugid]+1}
  
  if (result_IVDO_c[bugid]==0)
  {
    IVDO_c=0;
  }else{
    IVDO_c=  n-rank(result_IVDO_c)[bugid]+1
  }
  if(result_IVDO_sc[bugid]==0)
  {IVDO_sc=0}
  else{
    IVDO_sc= n-rank(result_IVDO_sc)[bugid]+1
  }
  
  if(result_ply[bugid]==0)
  {
    ply=0;
  }else{
    ply= n-rank(result_ply)[bugid]+1    
  }
  if(result_ply_c[bugid]==0)
  {
    ply_c=0;
  }else{
    ply_c=n-rank(result_ply_c)[bugid]+1
  }
  if(result_ply_sc[bugid]==0)
  {
    ply_sc=0;
  }else{
    ply_sc= n-rank(result_ply_sc)[bugid]+1
  }
 
  if(result_dln[bugid]==0)
  {
    dln=0;
  }else{
    dln= n-rank(result_dln)[bugid]+1

  }
  if(result_dln_c[bugid]==0)
  {
    dln_c=0;
  }else{
    dln_c=n-rank(result_dln_c)[bugid]+1
    
  }
  if(result_dln_sc[bugid]==0)
  {
    dln_sc=0;
  }else{
    dln_sc= n-rank(result_dln_sc)[bugid]+1
  }
 
  
  return(list(IVDO=IVDO,IVDO_c=IVDO_c,IVDO_sc=IVDO_sc,ply=ply,ply_c=ply_c,ply_sc=ply_sc,dln=dln,dln_c=dln_c,dln_sc=dln_sc,n=n,bugid=bugid));
}


