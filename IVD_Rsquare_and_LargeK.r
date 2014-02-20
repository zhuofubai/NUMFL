####################################################
##use t-value as suspiciousness score
##standardize data/normalize data
##add data preprocessing to handle loop case
####################################################
library(cluster)
library(pls)
source("C:/Users/zhuofu/RProject/normalize.R")
source("C:/Users/zhuofu/RProject/standardize.R")
n <- 515
result <- rep(0, n)
result_orig <- result
result_orig_c<-result
result_orig_sc<-result
result_clust <- result
result_clust_c<-result
result_clust_sc<-result
result_clust2<-result
result_IVD2<-result
result_IVD <- result
result_IVD_c<-result
result_IVD_sc<-result
result_bc<-result
result_bc_c<-result
result_bc_sc<-result

result_IVD_r<-result
result_IVD_r_sc<-result
result_IVDO_r<-result
result_IVDO_r_sc<-result

result_IVDO2<-result
result_IVDO <- result
result_IVDO_c<-result
result_IVDO_sc<-result

result_ply<-result
result_ply_c<-result
result_ply_sc<-result
result_rsq_ply<-result
result_rsq_ply_sc<-result
k <- 10;
k2 <- 20;
temp <- rep(1, k)
bugid <-204##bug1 on line 780

for (i in 1:n) {
  y_raw <- read.table("C:/Users/zhuofu/workspace2/apacheCommonMath3.1.1/TestRotationAndVector3D/Data7/out.txt")
  y <- data.matrix(y_raw[, 2])
  print(i)
  filename <- c("C:/Users/zhuofu/workspace2/apacheCommonMath3.1.1/TestRotationAndVector3D/Data7/", as.character(i), ".txt")
  filename <- paste(filename, collapse = "")
  
  if (!file.exists(filename)){
    
    print("not exist");
    next
  }
  
  data_raw <- read.table(filename, skip = 1)## read variables
  #data_raw <- data.matrix(data_raw)
  col <- ncol(data_raw)
  ###############remove the variables has NaN ##############
  naindex<-which(is.na(data_raw[,2]));
  if (length(naindex)!=0){
    data_raw<-data_raw[-naindex,];
  }
  ##filter out the test case which does not cover the instrumented file
  #####################################################################
  common<-intersect(y_raw[,1], data_raw[,1]);
  indsy<-which(y_raw[,1] %in% common);
  indsx<-which(data_raw[,1] %in% common);
  
  y_raw<-y_raw[indsy,];
  y<-y[indsy, ];
  y_index<-y_raw[,1];
  data_raw<-data_raw[indsx,];
  row<-length(y)
  
  ################ sample the output and data##############
  
  y0 <- which(y < 0.5)
  y1 <- which(y > 0.5)
  if (length(y0) < length(y1)) {
    b <- y1[1:length(y0)]
    index <- c(y0, b)
  } else {
    b <- y0[1:length(y1)]
    index <- c(y1, b)
  }
  
  y <- y[index]  
  
  ###skip training oringinal modle if the size of x-y pairs is too small###
  if (length(y)<30){
    next;
  }
  ##################or y is all 0 or 1 skip###############
  if(sum(y)==0||sum(y)==length(y))
  {
    next
  }
  
  ##########preprocessing the data to get the last run of a loop####################
  data <- matrix(0, nrow=row, ncol = col)
  
  for(j in 0:row-1)
  {
    subindex<-which(data_raw[,1]==y_index[j+1])
    if (length(subindex)==0){
      
      data[j+1,]=-1;
      next
    }
    subdata<-data_raw[subindex,]
    if(length(subindex)==1)
    {
      lastiter<-1
      data[j+1,]<-as.numeric(subdata);
    }else{
      lastiter<-nrow(subdata)
      data[j+1,]<-as.numeric(subdata[lastiter,]);}
  }
  #data<-data[-which(xp==-1),]
  
  
  X <- data[, 2:ncol(data)]
  X <- as.matrix(X)
  X <- X[index, ]   
  
  m <- ncol(as.matrix(X))
  #############if treatment is constant  skip############
  if (var(X[, 1])==0)
  {
    next
  }
  
  
  
  ################################################################################## 
  
  #############if data size is too small, skip clustering data##########################
  
  
  ##############################################################################
  if (m==1)
  {
    print(i);
    next;
  }
  else
  {
    T <- X[, 1]
    C <- X[, 2:m]
    
    ##################################################################################
    ##IVD model
    ## calculate pscore, a linear model between treatment and covariates
    
    ###########################################IVDO########################################
    #T2<-standardize(T);
    #C2<-standardize(C);
   # if(is.na(T2[1])==TRUE)
   # {T2=rep(0,length(y));}
   # C2<-C2[, colSums(is.na(C2)) == 0]
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
    
    sum_rsq_ply<-0;
    sum_rsq_ply_sc<-0;
    
    for (j in 1: k2)
    {
      if(j==k2){
        subclustindex<-clustindex_IVD[((j-1)*binsize+1):length(clustindex_IVD)];
      }else{
        subclustindex<-clustindex_IVD[((j-1)*binsize+1):(j*binsize)];
      }
      pSetO <- pscore_IVD[subclustindex];
      if(mean(pSetO)==0){
        cv=0;
      }else
      {
        cv=sd(pSetO)/mean(pSetO);
      }
      subdataY<-y[subclustindex];
      sum(subdataY); length(subdataY); cv;
      if(sum(subdataY)==0||sum(subdataY)==length(subdataY)||abs(cv)<0.10)
      {sum2<-sum2+length(subdataY);
       next;
      }
      
      subdataX <- X[subclustindex, ];
      if (var(subdataX[, 1])==0)
      {sum2<-sum2+length(subdataY);
       next;
      }
      subdata2<-subdataX;
      
      subdataX2<-standardize(subdataX);
      subdataX2 <- subdataX2[, colSums(is.na(subdataX2)) == 0]
      subdataX2<-as.matrix(subdataX2);
      lmfit_IVDO<-lm(subdataY~subdataX2)
      lmfit_IVDO_sc<-lm(subdataY~subdataX)	
      
      
      lmfit_ply<-lm(subdataY~I(subdataX2^2))
      lmfit_ply_sc<-lm(subdataY~I(subdataX^2))
      
      ts<-ncol(subdataX2)
      
      tempresult[j] <- summary(lmfit_IVDO)$coefficients[2*(ts+1)+2]
      tempresult[j] <- abs(tempresult[j])
      tempPvalue[j] <- summary(lmfit_IVDO)$coefficients[3*(ts+1)+2]
      
      
      tempresult_c[j] <- coefficients(lmfit_IVDO)[2]
      tempresult_c[j] <-abs(tempresult_c[j])
      
      tempresult_sc[j] <- coefficients(lmfit_IVDO_sc)[2]*sd(subdataX[,1])/sd(subdataY)
      tempresult_sc[j] <- abs(tempresult_sc[j])
      
      RsqO[j] <- summary(lmfit_IVDO)$adj.r.squared;
      RsqO_sc[j] <- summary(lmfit_IVDO_sc)$adj.r.squared;
      
      tempply[j]<-abs(summary(lmfit_ply)$coefficients[2*(ts+1)+2])
      tempply_c[j]<-abs(coefficients(lmfit_ply)[2])
      tempply_sc[j]<-abs(coefficients(lmfit_ply_sc)[2]*sd(subdataX[,1])/sd(subdataY))
      RsqPly[j]<-summary(lmfit_ply)$adj.r.squared;
      RsqPly_sc[j]<-summary(lmfit_ply_sc)$adj.r.squared;
      
      
      
      ## weighted summation of coefficient
      weight_IVDO[j] <- length(subdataY)/length(y)
      sum1<-sum1+weight_IVDO[j]*tempresult[j]
      sum_c<-sum_c+weight_IVDO[j]*tempresult_c[j]
      sum_sc<-sum_sc+weight_IVDO[j]*tempresult_sc[j]
      sum_r<-sum_r+weight_IVDO[j]*RsqO[j];
      sum_r_sc<-sum_r_sc+weight_IVDO[j]*RsqO_sc[j];
      
      sum_ply<-sum_ply+weight_IVDO[j]*tempply[j]
      sum_ply_c<-sum_ply_c+weight_IVDO[j]* tempply_c[j]
      sum_ply_sc<-sum_ply_sc+weight_IVDO[j]*tempply_sc[j]
      
      sum_rsq_ply<-sum_rsq_ply+weight_IVDO[j]*RsqPly[j]
      sum_rsq_ply_sc<-sum_rsq_ply_sc+weight_IVDO[j]*RsqPly_sc[j]
    }
    
    sum1<-sum1*length(y)/(length(y)-sum2)
    sum_c<-sum_c*length(y)/(length(y)-sum2)
    sum_sc<-sum_sc*length(y)/(length(y)-sum2)
    sum_r<-sum_r*length(y)/(length(y)-sum2)
    sum_r_sc<-sum_r_sc*length(y)/(length(y)-sum2)
    
    
    sum_ply<-sum_ply*length(y)/(length(y)-sum2)
    sum_ply_c<-sum_ply_c*length(y)/(length(y)-sum2)
    sum_ply_sc<-sum_ply_sc*length(y)/(length(y)-sum2)
    
    sum_rsq_ply<-sum_rsq_ply*length(y)/(length(y)-sum2)
    sum_rsq_ply_sc<-sum_rsq_ply_sc*length(y)/(length(y)-sum2)
    
    result_IVDO2[i] <- max(tempresult)	
    result_IVDO[i] <- sum1
    result_IVDO_c[i]<-sum_c
    result_IVDO_sc[i] <- sum_sc	
    result_IVDO_r[i]<-sum_r
    result_IVDO_r_sc[i]<-sum_r_sc
    
    result_ply[i]<-sum_ply
    result_ply_c[i]<-sum_ply_c
    result_ply_sc[i]<-sum_ply_sc
    result_rsq_ply[i]<-sum_rsq_ply
    result_rsq_ply_sc[i]<-sum_rsq_ply_sc
  }
}


which.max(result_IVDO_r_sc)
which.max(result_IVDO_r)
which.max(result_IVDO)
which.max(result_IVDO_c)
which.max(result_IVDO_sc)
################

#################


n-rank(result_IVDO)[bugid]+1

n-rank(result_IVDO_c)[bugid]+1
n-rank(result_IVDO_sc)[bugid]+1


n-rank(result_IVDO_r)[bugid]+1
n-rank(result_IVDO_r_sc)[bugid]+1

n-rank(result_ply)[bugid]+1
n-rank(result_ply_c)[bugid]+1
n-rank(result_ply_sc)[bugid]+1
n-rank(abs(result_rsq_ply))[bugid]+1
n-rank(abs(result_rsq_ply_sc))[bugid]+1