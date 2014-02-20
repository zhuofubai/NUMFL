####################################################
##use t-value as suspiciousness score
##standardize data/normalize data
##add data preprocessing to handle loop case
####################################################
library(cluster)
library(pls)
source("C:/Users/zhuofu/RProject/normalize.R")
source("C:/Users/zhuofu/RProject/standardize.R")
n = 411
result = rep(0, n)
result_orig = result
result_orig_c=result
result_orig_sc=result
result_clust = result
result_clust_c=result
result_clust_sc=result
result_clust2=result
result_IVD2=result
result_IVD = result
result_IVD_c=result
result_IVD_sc=result
result_bc=result
result_bc_c=result
result_bc_sc=result

result_IVDO2=result
result_IVDO = result
result_IVDO_c=result
result_IVDO_sc=result

k = 10
temp = rep(1, k)
bugid =394##bug1 on line 780

for (i in 1:n) {
	y_raw = read.table("C:/Users/zhuofu/workspace2/apacheCommonMath3.1.1/TestRotationAndVector3D/Data9/out.txt")
	y = data.matrix(y_raw[, 2])
    print(i)
    filename = c("C:/Users/zhuofu/workspace2/apacheCommonMath3.1.1/TestRotationAndVector3D/Data9/", as.character(i), ".txt")
    filename = paste(filename, collapse = "")

    if (!file.exists(filename)){
	
	print("not exist");
    		next
	}

    data_raw = read.table(filename, skip = 1)## read variables
    #data_raw = data.matrix(data_raw)
    col = ncol(data_raw)
###############remove the variables has NaN ##############
naindex=which(is.na(data_raw[,2]));
if (length(naindex)!=0){
	data_raw=data_raw[-naindex,];
}
##filter out the test case which does not cover the instrumented file
#####################################################################
common=intersect(y_raw[,1], data_raw[,1]);
indsy=which(y_raw[,1] %in% common);
indsx=which(data_raw[,1] %in% common);

y_raw=y_raw[indsy,];
y=y[indsy, ];
y_index=y_raw[,1];
data_raw=data_raw[indsx,];
row=length(y)

################ sample the output and data##############

y0 = which(y < 0.5)
y1 = which(y > 0.5)
if (length(y0) < length(y1)) {
    b = y1[1:length(y0)]
    index = c(y0, b)
} else {
    b = y0[1:length(y1)]
    index = c(y1, b)
}

y = y[index]  

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
data = matrix(0, nrow=row, ncol = col)

for(j in 0:row-1)
{
  subindex=which(data_raw[,1]==y_index[j+1])
  if (length(subindex)==0){
     
     data[j+1,]=-1;
  	next
  }
  subdata=data_raw[subindex,]
  if(length(subindex)==1)
  {
   lastiter=1
   data[j+1,]=as.numeric(subdata);
  }else{
  lastiter=nrow(subdata)
  data[j+1,]=as.numeric(subdata[lastiter,]);}
}
#data=data[-which(xp==-1),]
 
  
    X = data[, 2:ncol(data)]
    X = as.matrix(X)
    X = X[index, ]   
   
    m = ncol(as.matrix(X))
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
    	T = X[, 1]
	C = X[, 2:m]
###################################################################
  
    ## HI model
##################################################################################
    ##IVD model
    ## calculate pscore, a linear model between treatment and covariates

    	lmfit_ps=lm(T~C-1);
	coeff=coefficients(lmfit_ps);
	coeff[is.na(coeff)]=0
	C2=cbind(1,C);
	if(ncol(as.matrix(C))==1)
	{
	pscore_IVD=C*coeff;
	}else{
	pscore_IVD=C%*%coeff;
      }

      #pscore_IVD[is.na(pscore_IVD)] = 0

	u_IVD = pam(pscore_IVD, k)
      clustindex_IVD = u_IVD$clustering

	tempresult=rep(0,k)
	tempresult_c=tempresult;
	tempresult_sc=tempresult;
	tempPvalue=tempresult;
	weight_IVD=rep(0,k)
	sum=0;
	sum2=0;
	sum_c=0;
	sum_sc=0;
	for (j in 1: k)
	{	
	subclustindex=which(clustindex_IVD == j);
	subdataY=y[subclustindex];
	if(sum(subdataY)==0||sum(subdataY)==length(subdataY))
	{sum2=sum2+length(subdataY);
	next;
       }

	subdataX = X[subclustindex, ];
	if (var(subdataX[, 1])==0)
	{next}
	subdataX2=standardize(subdataX);
	
	subdataX2 = subdataX2[, colSums(is.na(subdataX2)) == 0]
	lmfit_IVD=lm(subdataY~subdataX2)
	lmfit_IVD_sc=lm(subdataY~subdataX)
	##lmfit_IVD2=glm(subdataY~subdataX2,family = binomial(link = "probit"))
	
	ts=ncol(subdataX2)
	tempresult[j]=summary(lmfit_IVD)$coefficients[2*(ts+1)+2]
	tempresult[j] = abs(tempresult[j])
	tempPvalue[j]=summary(lmfit_IVD)$coefficients[3*(ts+1)+2]
    	

	tempresult_c[j] = coefficients(lmfit_IVD)[2]
	tempresult_c[j] =abs(tempresult_c[j])

	tempresult_sc[j]=coefficients(lmfit_IVD_sc)[2]*sd(subdataX[,1])/sd(subdataY)
	tempresult_sc[j]=abs(tempresult_sc[j])

## weighted summation of coefficient
	weight_IVD[j]=length(subdataY)/length(y)
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
    	############################################
	
	clustindex_IVD=sort(pscore_IVD,method="sh", index.return=TRUE)$ix;
	binsize=round(length(clustindex_IVD)/k);#Bin the pscore into roughly equal size
	#u_IVD = pam(pscore_IVD, k)

	tempresult=rep(0,k);
	tempresult_c=tempresult;
	tempresult_sc=tempresult;
	tempPvalue=tempresult;
	weight_IVDO=rep(0,k)
	sum=0;
	sum2=0;
	sum_c=0;
	sum_sc=0;
	for (j in 1: k)
	{
	if(j==k){
	subclustindex=clustindex_IVD[((j-1)*binsize+1):length(clustindex_IVD)];
	}else{
	subclustindex=clustindex_IVD[((j-1)*binsize+1):(j*binsize)];
	}
	subdataY=y[subclustindex];
	if(sum(subdataY)==0||sum(subdataY)==length(subdataY))
	{sum2=sum2+length(subdataY);
	next;
       }

	subdataX = X[subclustindex, ];
	if (var(subdataX[, 1])==0)
	{next}
	subdataX2=standardize(subdataX);
	
	subdataX2 = subdataX2[, colSums(is.na(subdataX2)) == 0]
	lmfit_IVDO=lm(subdataY~subdataX2)
	lmfit_IVDO_sc=lm(subdataY~subdataX)
	
	ts=ncol(subdataX2)
	tempresult[j]=summary(lmfit_IVDO)$coefficients[2*(ts+1)+2]
	tempresult[j] = abs(tempresult[j])
	tempPvalue[j]=summary(lmfit_IVDO)$coefficients[3*(ts+1)+2]
    	

	tempresult_c[j] = coefficients(lmfit_IVDO)[2]
	tempresult_c[j] =abs(tempresult_c[j])

	tempresult_sc[j]=coefficients(lmfit_IVDO_sc)[2]*sd(subdataX[,1])/sd(subdataY)
	tempresult_sc[j]=abs(tempresult_sc[j])

## weighted summation of coefficient
	weight_IVDO[j]=length(subdataY)/length(y)
	sum=sum+weight_IVDO[j]*tempresult[j]
	sum_c=sum_c+weight_IVDO[j]*tempresult_c[j]
	sum_sc=sum_sc+weight_IVDO[j]*tempresult_sc[j]
	}

	sum=sum*length(y)/(length(y)-sum2)
	sum_c=sum_c*length(y)/(length(y)-sum2)
	sum_sc=sum_sc*length(y)/(length(y)-sum2)

	result_IVDO2[i]=max(tempresult)	
	result_IVDO[i]=sum
	result_IVDO_c[i]=sum_c
	result_IVDO_sc[i]=sum_sc	

 }
}
which.max(result_bc)
which.max(result_orig)
which.max(result_clust)
which.max(result_IVD)
which.max(result_clust2)
which.max(result_IVD2)
which.max(result_bc_c)
which.max(result_orig_c)
which.max(result_IVD_c)
which.max(result_clust_c)
which.max(result_bc_sc)
which.max(result_clust_sc)
which.max(result_orig_sc)
which.max(result_IVD_sc)
################
result_bc
result_orig
result_clust
result_IVD
result_clust2
result_IVD2
result_bc_c
result_orig_c
result_IVD_c
result_clust_c
result_bc_sc
result_clust_sc
result_orig_sc
result_IVD_sc
#################

n-rank(result_IVD)[bugid]+1
n-rank(result_IVD2)[bugid]+1

n-rank(result_IVD_c)[bugid]+1




n-rank(result_IVD_sc)[bugid]+1
n-rank(result_IVDO)[bugid]+1
n-rank(result_IVDO2)[bugid]+1
n-rank(result_IVDO_c)[bugid]+1
n-rank(result_IVDO_sc)[bugid]+1
