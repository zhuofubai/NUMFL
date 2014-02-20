standardize<-function(x){
x<-data.matrix(x)   
m=ncol(x);
if (m>1)
{
 result<-stdize(x,center=TRUE,scale=TRUE)
	}
else{
 result<-(x  - mean(x)) / (apply(x,2,sd)) 
}
return(result)
}