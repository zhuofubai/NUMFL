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
  
  k2 <- clustNum;
  for (i in 1:n) {
    # print(i)
    
    filename<-paste(c(dir,as.character(datafolder),"/",as.character(i), ".txt"),collapse="")   
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
    diff<-diff^2;//squre the difference
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
      
      tempresult<-rep(0,k2);
      tempresult_c<-tempresult;
      tempresult_sc<-tempresult;
      tempPvalue<-tempresult;
      weight_IVDO<-rep(0,k2);
      RsqO<-rep(0,k2);
      RsqO_sc<-RsqO;
      
      tempply<-rep(0,k2);
      tempply_c<-rep(0,k2);
      tempply_sc<-rep(0,k2);
      RsqPly<-rep(0,k2);
      RsqPly_sc<-rep(0,k2);
      
      pSetO<-0;
      sum1<-0;
      sum2<-0;
      
      sum_c<-0;
      sum_sc<-0;
      sum_r<-0;
      sum_r_sc<-0;
      
      sum_ply<-0;
      sum_ply_c<-0;
      sum_ply_sc<-0;
      
      #     sum_rsq_ply<-0;
      #     sum_rsq_ply_sc<-0;
      #################}
      
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
        #   sum(subdataY); length(subdataY); cv;
        if(sum(subdataY)==0||sum(subdataY)==length(subdataY)||var(subdataX[, 1])==0)#||abs(cv)>0.10)
        {sum2<-sum2+length(subdataY);
         next;
        }
        #     mod_lm<-buildlinear(subdataX,subdataY,subtruth, ctrl=FALSE)
        #     mod_ply<-buildPolynominal(subdataX,subdataY,subtruth, ctrl=FALSE)
        mod_lm2<-buildlinear(subdataX,subdiff,subtruth, ctrl=F)
        mod_ply2<-buildPolynominal(subdataX,subdiff,subtruth, ctrl=F)
        
        tempresult[j]<-mod_lm2$Tvalue
        tempresult_c[j]<-mod_lm2$Coefficient
        tempresult_sc[j]<-mod_lm2$StandardCoeff
        
        tempply[j]<-mod_ply2$Tvalue
        tempply_c[j]<-mod_ply2$Coefficient
        tempply_sc[j]<-mod_ply2$StandardCoeff
        
        ## weighted summation of coefficient
        weight_IVDO[j] <- length(subdataY)/length(y)
        
        sum1<-sum1+weight_IVDO[j]*tempresult[j]
        sum_c<-sum_c+weight_IVDO[j]*tempresult_c[j]
        sum_sc<-sum_sc+weight_IVDO[j]*tempresult_sc[j]
        
        
        sum_ply<-sum_ply+weight_IVDO[j]*tempply[j]
        sum_ply_c<-sum_ply_c+weight_IVDO[j]* tempply_c[j]
        sum_ply_sc<-sum_ply_sc+weight_IVDO[j]*tempply_sc[j]
        
      }
      total=length(y);
      
      
      sum1<-sum1*length(y)/(length(y)-sum2)
      sum_c<-sum_c*length(y)/(length(y)-sum2)
      sum_sc<-sum_sc*length(y)/(length(y)-sum2)     
      
      sum_ply<-sum_ply*length(y)/(length(y)-sum2)
      sum_ply_c<-sum_ply_c*length(y)/(length(y)-sum2)
      sum_ply_sc<-sum_ply_sc*length(y)/(length(y)-sum2)
      
      
      result_IVDO[i] <- sum1
      result_IVDO_c[i]<-sum_c
      result_IVDO_sc[i] <- sum_sc  
      
      result_ply[i]<-sum_ply
      result_ply_c[i]<-sum_ply_c
      result_ply_sc[i]<-sum_ply_sc
      
    }
  }
  
  
  #   which.max(result_IVDO)
  #   which.max(result_IVDO_c)
  #   which.max(result_IVDO_sc)
  ################
  
  #################
  result_ply[is.na(result_IVDO)]=0
  result_ply[is.na(result_IVDO_c)]=0
  result_ply[is.na(result_IVDO_sc)]=0
  result_ply[is.na(result_ply)]=0
  result_ply[is.na(result_ply_c)]=0
  result_ply[is.na(result_ply_sc)]=0
  
  IVDO= n-rank(result_IVDO)[bugid]+1
  IVDO_c=  n-rank(result_IVDO_c)[bugid]+1
  IVDO_sc= n-rank(result_IVDO_sc)[bugid]+1
  
  
  ply= n-rank(result_ply)[bugid]+1
  ply_c=n-rank(result_ply_c)[bugid]+1
  ply_sc= n-rank(result_ply_sc)[bugid]+1
  
  
  return(list(IVDO=IVDO,IVDO_c=IVDO_c,IVDO_sc=IVDO_sc,ply=ply,ply_c=ply_c,ply_sc=ply_sc,n=n,bugid=bugid));
}


