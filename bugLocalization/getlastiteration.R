getlastiteration<-function(data,row)

n=ncol(data);
result <- matrix(0, nrow = r, ncol = n)
n<-row-1;
for(i in 0:n)
{
  subindex<-which(data[,1]==i)
  subdata<-data[subindex,]
  m<-nrow(subdata)
  result[i+1,]<-as.numeric(subdata[m,]);
}

return(result)

