dir<-"C:/Users/zhuofu/workspace/apacheCommonMath2.1/TestEigenDecomposition/Data";
datafolder=9
y_raw<-read.table(paste(c(dir,as.character(datafolder),"/out.txt"),collapse="")); 
n<-length(y_raw[,1])
index<-seq(1,n,2)
out<-y_raw[index,]
write.table(out,paste(c(dir,as.character(datafolder),"/out2.txt"),collapse=""),row.names = FALSE,col.names = FALSE)



#cat("\014")  