dir<-"C:/Users/zhuofu/workspace/apacheCommonMath2.1/TestEigenDecomposition/Data";
datafolder=4;
y_raw<-read.table(paste(c(dir,as.character(datafolder),"/out.txt"),collapse="")); 
l<-length(y_raw[,2])
p=sum(y_raw[,2])
print(l)
print(p)