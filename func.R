n=13;
result<-rep(1,n)
y=read.table("D:/workspace/Test3/TestSolver/Data1/out.txt")
y=data.matrix(y[,2]);
y0=which(y<0.5)
y1=which(y>0.5)
if (length(y0)<length(y1)){
b=y1[1:length(y0)];
index=c(y0,b);
}else{
b=y0[1:length(y1)];
index=c(y1,b);
}
y=y[index];
for (i in 1:n){
	
	print(i)
	filename=c("D:/workspace/Test3/TestSolver/Data1/",as.character(i),".txt")
	filename=paste(filename, collapse = '');
	data=read.table(filename,skip=1);

	data=data.matrix(data)
	n=ncol(data);
	X=data[,2:n];
	X=data.matrix(X)
	
	X=(X-colMeans(X))/apply(X,2,var);
	X=X[index,];
		lmfit = lm( y ~ X );
	result[i]=coefficients(lmfit)[2];
	result2=abs(result);
}   

which.max( result2 )
