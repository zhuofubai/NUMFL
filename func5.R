##########################################################
##use adjusted coefficients as suspiciousness score
##every variable is standardized excepet binary indicator
##add data preprocessing to handle loop case
##########################################################
library(cluster)
library(pls)
#source("C:/Users/zhuofu/RProject/normalize.R")
source("C:/Users/zhuofu/RProject/standardize.R")
n <- 19
result <- rep(0, n)
result_orig <- result
result_clust <- result
result_clust2<-result
result_IVD2<-result
result_IVD <- result
result_bc<-result
y <- read.table("C:/Users/zhuofu/workspace/Test3/TestCosineTransformData/Data1/out.txt")
y <- data.matrix(y[, 2])
row<-length(y)
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
bugid <- 5
for (i in 1:n) {
	
    print(i)
    filename <- c("C:/Users/zhuofu/workspace/Test3/TestCosineTransformData/Data1/", as.character(i), ".txt")
    filename <- paste(filename, collapse = "")
    if (!file.exists(filename)){
    next
	}

    data_raw <- read.table(filename, skip = 1)
    data_raw <- data.matrix(data_raw)
    col <- ncol(data_raw)
   
##preprocessing the data to get the last run
data <- matrix(0, nrow = row, ncol = col)

for(j in 0:row-1)
{
  subindex<-which(data_raw[,1]==j)
  subdata<-data_raw[subindex,]
  if(length(subindex)==1)
  {
   lastiter=1
   data[j+1,]<-subdata;
  }else{
  lastiter<-nrow(subdata)
  data[j+1,]<-as.numeric(subdata[lastiter,]);}
}




    X <- data[, 2:ncol(data)]
    X <- as.matrix(X)
    X <- X[index, ]   
   
    m <- ncol(as.matrix(X))
 if (m==1)
	{
	X2 <- standardize(X)
    	X2 <- X2[, colSums(is.na(X2)) == 0]
    	lmfit <- lm(y ~ X2)
	#lmfit <- glm(y~X2, family = binomial(link = "probit"));
	 result[i] <- coefficients(lmfit_org)[2]
	
      result_orig <- abs(result_orig)

	result_bc[i]<-abs(result_orig[i])
	result_clust[i]<-abs(result_orig[i])
	result_IVD[i]<-abs(result_orig[i])
	}else
{
     T <- X[, 1]
	C <- X[, 2:m]
###################################################################
    ## binary cluster model
    ##C0=normalize(C)
    T0=standardize(T);
    T0 <- T0[, colSums(is.na(T0)) == 0];
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
    
    for (j in 1:k) {
        temp[j] <- pcoeff[j]
    }
    
    pscore <- clustMat %*% temp
    
    if (i == bugid) {
        nc <- ncol(C)
        r3 <- rep(1, nc)
        for (j in 1:nc) {
            
            cm1 <- lm(C[, j] ~ T + pscore)
            ce1 <- coefficients(cm1)[2]
            cm2 <- lm(C[, j] ~ T)
            ce2 <- coefficients(cm2)[2]
            abs(ce2) - abs(ce1)
        }
    }
    
   ## pscore <- scale(pscore)
    
    lmfit_bc <- lm(y ~ T0 + pscore)
   #lmfit_bc<-lm(y ~ T0 + pscore,family = binomial(link = "probit"))


    result_bc[i] <- coefficients(lmfit_bc)[2]#*var(T0)/var(y)

    result_bc <- abs(result_bc)
################################################################################## 
    ## original model
    X2 <- standardize(X)
    X2 <- X2[, colSums(is.na(X2)) == 0]
    lmfit_org <- lm(y ~ X2)
	#lmfit_org <- glm(y~X2, family = binomial(link = "probit"));
	 result_orig[i] <- coefficients(lmfit_org)[2]#*var(T0)/var(y)
	
    result_orig <- abs(result_orig)
        
    ## HI model
##################################################################################
    ##IVD model
    ## calculate pscore, a linear model between treatment and covariates

      lmfit_ps<-lm(T~C);
	coeff<-coefficients(lmfit_ps);
	coeff[is.na(coeff)]<-0
	C2=cbind(1,C);
	pscore_IVD=C2%*%coeff;
      #pscore_IVD[is.na(pscore_IVD)] <- 0

	u_IVD <- pam(pscore_IVD, k)
      clustindex_IVD <- u_IVD$clustering

	tempresult_IVD<-rep(0,k)
	weight_IVD<-rep(0,k)
	sum=0;
	for (j in 1: k)
	{	
	subclustindex=which(clustindex_IVD == j);
	subdataY<-y[subclustindex];
	subdataX <- X[subclustindex, ];
	subdataX2=standardize(subdataX);
	subdataX2 <- subdataX2[, colSums(is.na(subdataX2)) == 0]
	lmfit_IVD<-lm(subdataY~subdataX2)
	#lmfit_IVD<-lm(subdataY~subdataX2,family = binomial(link = "probit"))
	tempresult_IVD[j] <- coefficients(lmfit_IVD)[2]#*var(subdataX[,1])/var(subdataY)

	
    	tempresult_IVD[j] <- abs(tempresult_IVD[j])
	## weighted summation of coefficient
	weight_IVD[j]<-length(subdataY)/length(y)
	sum=sum+weight_IVD[j]*tempresult_IVD[j]
	
	}
	result_IVD2[i]=max(tempresult_IVD)	
	result_IVD[i]=sum
###################################################################################
    ##cluster model
	
	## within each cluster, train model & standardized the data
	tempresult<-rep(0,k)
	weight<-rep(0,k)
	sum=0;
	for (j in 1: k)
	{	
	subclustindex=which(clustindex == j);
	subdataY<-y[subclustindex];
	subdataX <- X[subclustindex, ];
	subdataT<-T[subclustindex];
	subdataX2=standardize(subdataX);
	subdataT2<-standardize(subdataT);
	subdataX2 <- subdataX2[, colSums(is.na(subdataX2)) == 0]
	#lmfit_clust<-lm(subdataY~subdataX2)
	lmfit_clust<-lm(subdataY~subdataT2-1)
#lmfit_clust<-lm(subdataY~subdataX2,family = binomial(link = "probit"))


	tempresult[j] <- coefficients(lmfit_clust)[1]#*var(subdataX[,1])/var(subdataY)	
    	tempresult[j] <- abs(tempresult[j])
	## weighted summation of coefficient
	weight[j]<-length(subdataY)/length(y)
	sum=sum+weight[j]*tempresult[j]
	
	}

	result_clust2[i]=max(tempresult)	
	result_clust[i]=sum
	
	}
  }

which.max(result_bc)
which.max(result_orig)
which.max(result_clust)
which.max(result_clust2)
which.max(result_IVD)
which.max(result_IVD2)
