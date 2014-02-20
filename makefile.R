pSetO <- pscore_IVD[subclustindex];
sd(pSetO)

subclustindex<-clustindex_IVD[((j-1)*binsize+1):(j*binsize)];
subdataY=y[subclustindex];    
subdataX = X[subclustindex, ];

pi=which(subdataY==0)
fi=which(subdataY==1)
hist(subdataX[fi,1])


h = hist(subdataX[fi,1])
h$density = h$counts/sum(h$counts)
plot(h,freq=F)


db=seq(from=-0.54, to =-0.4, by=0.01)



h1 = hist(subdataX[pi,1],breaks=db,);
h1$density = h1$counts/sum(h1$counts)


h2 = hist(subdataX[fi,1],breaks=db)
h2$density = h2$counts/sum(h2$counts)

par(mfrow=c(1,2))
plot(h1,freq=F,main="Histogram of m", sub="pass")
plot(h2,freq=F,main="Histogram of m", sub="fail")

#hist(subdataX[pi,1],breaks=db)
#hist(subdataX[fi,1],breaks=db)

4 5 8 9 10

plot(subdataX[fi,c(2,3)], main="covariate distribution", sub="fail execution",
     +      xlab="q", ylab="error",
     +      xlim=c(-0.55, -0.4), ylim=c(-1E-5,1E-5 ))

plot(subdataX[fi,c(2,3)], main="covariate distribution", sub="fail execution",
     +      xlab="q", ylab="error",
     +      xlim=c(-0.55, -0.4), ylim=c(-1E-5,1E-5 ))




for (j in 1: k2)
{  
  if(j==k2){
    subclustindex<-clustindex_IVD[((j-1)*binsize+1):length(clustindex_IVD)];
  }else{
    subclustindex<-clustindex_IVD[((j-1)*binsize+1):(j*binsize)];
  }
  pSetO <- pscore_IVD[subclustindex];
 # print(sd(pSetO))
  cv=sd(pSetO)/mean(pSetO)
  print(cv)
  #subdataY=y[subclustindex];    
  #subdataX = X[subclustindex, ];
  

}