####################################################
##use t-value as suspiciousness score
##standardize data/normalize data
##add data preprocessing to handle loop case
####################################################
library(cluster)
library(pls)
source("C:/Users/zhuofu/RProject/normalize.R")
source("C:/Users/zhuofu/RProject/standardize.R")
n <- 9
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

y <- read.table("C:/Users/zhuofu/workspace/apacheCommonMath3.1.1/TestPascalDistributio/Data2/out.txt")
y <- data.matrix(y[, 2])
row<-nrow(y)
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
k <- 5
temp <- rep(1, k)
bugid <- 8
for (i in 1:n) {
	
    print(i)
    filename <- c("C:/Users/zhuofu/workspace/apacheCommonMath3.1.1/TestPascalDistributio/Data2/", as.character(i), ".txt")
    filename <- paste(filename, collapse = "")
    if (!file.exists(filename)){
    next
	}

    data_raw <- read.table(filename, skip = 1)
    data_raw <- data.matrix(data_raw)
    col <- ncol(data_raw)
   
##preprocessing the data to get the last run
data <- matrix(0, nrow=row, ncol = col)


   
    X <- data_raw[, 2:ncol(data_raw)]
    X <- as.matrix(X)
    X <- X[index, ]   
   
    m <- ncol(as.matrix(X))

################################################################################## 
    ## original model
    X2 <- standardize(X)
    X2 <- X2[, colSums(is.na(X2)) == 0]
    lmfit_orig <- lm(y ~ X2)
    lmfit_orig2 <-lm(y ~ X)
	#lmfit_orig <- glm(y~X2, family = binomial(link = "probit"));
	result_orig_c[i] <- coefficients(lmfit_orig)[2]
	ts=ncol(as.matrix(X2))
	result_orig[i]<-summary(lmfit_orig)$coefficients[2*(ts+1)+2]
	if(m==1)
	{
	result_orig_sc[i]<-coefficients(lmfit_orig2)[2]*sd(X)/sd(y)
	}else
	{
	result_orig_sc[i]<-coefficients(lmfit_orig2)[2]*sd(X[,1])/sd(y)
	}
	result_orig <- abs(result_orig)
      result_orig_c<-abs(result_orig_c)
	result_orig_sc<-abs(result_orig_sc)  
#############################################################################

 if (m==1)
	{
	#X2 <- standardize(X)
    	#X2 <- X2[, colSums(is.na(X2)) == 0]
    	#lmfit_orig <- lm(y ~ X2)
	#lmfit <- glm(y~X2, family = binomial(link = "probit"));
	# result[i] <- coefficients(lmfit_orig)[2]
	#ts=ncol(as.matrix(X2))
	##result_orig[i]<-summary(lmfit_orig)$coefficients[2*(ts+1)+2]

      #result_orig <- abs(result_orig)

	result_bc[i]<-abs(result_orig[i])
	result_clust[i]<-abs(result_orig[i])
	result_IVD[i]<-abs(result_orig[i])

	result_bc_c[i]<-abs(result_orig_c[i])
	result_clust_c[i]<-abs(result_orig_c[i])
	result_IVD_c[i]<-abs(result_orig_c[i])
	
	result_bc_sc[i]<-abs(result_orig_sc[i])
	result_clust_sc[i]<-abs(result_orig_sc[i])
	result_IVD_sc[i]<-abs(result_orig_sc[i])
	}
	else
	{
      T <- X[, 1]
	C <- X[, 2:m]
###################################################################
     ## binary cluster model
    ##C0=normalize(C)

    ##cluster the data based on covariate
    temp2=temp
    T0=standardize(T)                               
    u <- pam(C, k)             
    clustindex <- u$clustering 
    clustMat <- array(0, dim = c(nrow(X), k))
    for (j in 1:nrow(X)) {
        clustMat[j, clustindex[j]] <- 1
    }
    
    ## create the membership clustering matrix
    pmodel <- lm(T0 ~ clustMat - 1)
    pcoeff <- coefficients(pmodel)
    #pcoeff[is.na(pcoeff)] <- 0
    
    pmodel2<-lm(T~clustMat-1)
    pcoeff2<-coefficients(pmodel2)
    for (j in 1:k) {
        temp[j] <- pcoeff[j]
	  temp2[j]<-pcoeff2[j]
    }
    
    pscore <- clustMat %*% temp
    pscore2 <- clustMat %*% temp2
      
   ## pscore <- scale(pscore)
    
    lmfit_bc <- lm(y ~ T0 + pscore)
   #lmfit_bc<-lm(y ~ T0 + pscore,family = binomial(link = "probit"))
    lmfit_bc2<-lm(y~T+pscore2)
        
		
    result_bc_c[i] <- coefficients(lmfit_bc)[2]
    result_bc[i]<-summary(lmfit_bc)$coefficients[8]
    result_bc_sc[i] <- coefficients(lmfit_bc2)[2]*sd(T)/sd(y)
        
    ## HI model

################################################################################## 
   lmfit_ps<-lm(T~C);
	coeff<-coefficients(lmfit_ps);
	coeff[is.na(coeff)]<-0
	C2=cbind(1,C);
	pscore_IVD=C2%*%coeff;
      #pscore_IVD[is.na(pscore_IVD)] <- 0

	u_IVD <- pam(pscore_IVD, k)
      clustindex_IVD <- u_IVD$clustering

	tempresult<-rep(0,k)
	tempresult_c<-tempresult;
	tempresult_sc<-tempresult;
	tempPvalue=tempresult;
	weight_IVD<-rep(0,k)
	sum=0;
	sum2=0;
	sum_c=0;
	sum_sc=0;
	for (j in 1: k)
	{	
	subclustindex=which(clustindex_IVD == j);
	subdataY<-y[subclustindex];
	if(sum(subdataY)==0||sum(subdataY)==length(subdataY))
	{sum2=sum2+length(subdataY);
	next;
       }

	subdataX <- X[subclustindex, ];
	subdataX2=standardize(subdataX);
	subdataX2 <- subdataX2[, colSums(is.na(subdataX2)) == 0]
	lmfit_IVD<-lm(subdataY~subdataX2)
	lmfit_IVD_sc<-lm(subdataY~subdataX)
	lmfit_IVD2<-glm(subdataY~subdataX2,family = binomial(link = "probit"))
	
	ts=ncol(subdataX2)
	tempresult[j]<-summary(lmfit_IVD)$coefficients[2*(ts+1)+2]
	tempresult[j] <- abs(tempresult[j])
	tempPvalue[j]<-summary(lmfit_IVD)$coefficients[3*(ts+1)+2]
    	

	tempresult_c[j] <- coefficients(lmfit_IVD)[2]
	tempresult_c[j] <-abs(tempresult_c[j])

	tempresult_sc[j]<-coefficients(lmfit_IVD)[2]*sd(subdataX[,1])/sd(subdataY)
	tempresult_sc[j]<-abs(tempresult_sc[j])

## weighted summation of coefficient
	weight_IVD[j]<-length(subdataY)/length(y)
	sum=sum+weight_IVD[j]*tempresult[j]
	sum_c=sum_c+weight_IVD[j]*tempresult_c[j]
	sum_sc=sum_sc+weight_IVD[j]*tempresult_sc[j]
	}

	sum=sum*length(y)/(length(y)-sum2)
	sum_c=sum_c*length(y)/(length(y)-sum2)
	sum_sc=sum_sc*length(y)/(length(y)-sum2)

	result_IVD2[i]=max(tempresult)	
	result_IVD[i]=sum
	result_IVD_c[i]=sum_c
	result_IVD_sc[i]=sum_sc

###################################################################################
    ##cluster model
	
	## within each cluster, train model & standardized the data
	tempresult2<-rep(0,k)
	tempPvalue2=tempresult2;
	tempresult2_c<-tempresult2;
	tempresult2_sc<-tempresult2;
	weight2<-rep(0,k)
	sum=0;
	sum2=0;
	sum_c=0;
	sum_sc=0;
	for (j in 1: k)
	{	
	subclustindex=which(clustindex == j);
	subdataY<-y[subclustindex];
	if(sum(subdataY)==0||sum(subdataY)==length(subdataY))
	{sum2=sum2+length(subdataY);
	next;
       }

	subdataX <- X[subclustindex, ];
	subdataT<-T[subclustindex];
	subdataX2<-standardize(subdataX);
	subdataT2<-standardize(subdataT);
	subdataX2 <- subdataX2[, colSums(is.na(subdataX2)) == 0]
	#lmfit_clust<-lm(subdataY~subdataX2)
	lmfit_clust<-lm(subdataY~subdataT2-1)
#lmfit_clust<-lm(subdataY~subdataX2,family = binomial(link = "probit"))
	lmfit_clust_sc<-lm(subdataY~subdataT-1);
	
	tempresult2_c[j] <- coefficients(lmfit_clust)[1]
	tempresult2_sc[j]<- coefficients(lmfit_clust_sc)[1]*sd(subdataT)/sd(subdataY)

	tempresult2[j]<-summary(lmfit_clust)$coefficients[3]
	tempPvalue2[j]<-summary(lmfit_clust)$coefficients[4]

	tempresult2_sc[j]<-abs(tempresult2_sc[j])
	tempresult2_c[j]<-abs(tempresult2_c[j])
    	tempresult2[j] <- abs(tempresult2[j])
	## weighted summation of coefficient
	weight2[j]<-length(subdataY)/length(y)
	sum=sum+weight2[j]*tempresult2[j]
	sum_c=sum_c+weight2[j]*tempresult2_c[j]
	sum_sc=sum_sc+weight2[j]*tempresult2_sc[j]

	}
	sum=sum*length(y)/(length(y)-sum2)
	sum_c=sum_c*length(y)/(length(y)-sum2)
	result_clust2[i]=max(tempresult2)	
	result_clust[i]=sum
	result_clust_c[i]=sum_c
	result_clust_sc[i]=sum_sc
	
	
 }
}
which.max(result_bc)
which.max(result_orig)
which.max(result_clust)
which.max(result_clust2)
which.max(result_IVD)
which.max(result_IVD2)
which.max(result_bc_c)
which.max(result_orig_c)
which.max(result_IVD_c)
which.max(result_clust_c)
which.max(result_bc_sc)
which.max(result_clust_sc)
which.max(result_orig_sc)
which.max(result_IVD_sc)
result_bc
result_orig
result_clust
result_clust2
result_IVD


result_bc
result_orig
result_clust
result_clust2
result_IVD
result_IVD2
result_bc_c
result_orig_c
result_IVD_c
result_clust_c
result_bc_sc
result_clust_sc
result_orig_sc
result_IVD_sc

tempPvalue
tempPvalue2