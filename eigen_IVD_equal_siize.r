####################################################
##use t-value as suspiciousness score
##standardize data/normalize data
##add data preprocessing to handle loop case
####################################################
library(cluster)
library(pls)
source("C:/Users/zhuofu/RProject/normalize.R")
source("C:/Users/zhuofu/RProject/standardize.R")
n = 450
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



k = 5
temp = rep(1, k)
bugid =99##bug1 on line 780

for (i in 1:n) {
	y_raw = read.table("C:/Users/zhuofu/workspace/apacheCommonMath2.1/TestEigenDecomposition/Data9/out.txt")##read outout
	y = data.matrix(y_raw[, 2])
    print(i)
    filename = c("C:/Users/zhuofu/workspace/apacheCommonMath2.1/TestEigenDecomposition/Data9/", as.character(i), ".txt")
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
    ## original model
  
#############if data size is too small, skip clustering data##########################
	if (length(y)<10*k) 
	{
	next;
	}
	
##############################################################################
 if (m==1)
	{
	message("one column data: ", i);	
	}
	else
	{
    T = X[, 1]
	C = X[, 2:m]

##################################################################################
    ##IVD model
    ## calculate pscore, a linear model between treatment and covariates

    lmfit_ps=lm(T~C);
	coeff=coefficients(lmfit_ps);
	coeff[is.na(coeff)]=0
	C2=cbind(1,C);
	pscore_IVD=C2%*%coeff;
      
	clustindex_IVD=sort(pscore_IVD,method="sh", index.return=TRUE)$ix;
	binsize=round(length(clustindex_IVD)/k);#Bin the pscore into roughly equal size
	#u_IVD = pam(pscore_IVD, k)

	tempresult=rep(0,k);
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
	lmfit_IVD=lm(subdataY~subdataX2)
	lmfit_IVD_sc=lm(subdataY~subdataX)
	
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
 }
}

#################

n-rank(result_IVD)[bugid]+1

n-rank(result_IVD2)[bugid]+1

n-rank(result_IVD_c)[bugid]+1

n-rank(result_IVD_sc)[bugid]+1
