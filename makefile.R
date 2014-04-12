truth <- read.table("C:/Users/zhuofu/workspace2/apacheCommonMath3.1.1/TestRotationAndVector3D/Data9/truth.txt")



pSetO <- pscore_IVD[subclustindex];
sd(pSetO)

subclustindex<-clustindex_IVD[((j-1)*binsize+1):(j*binsize)];
subdataY=y[subclustindex];    
subdataX = X[subclustindex, ];
subtruth=truth[subclustindex];

pi=which(subdataY==0)
fi=which(subdataY==1)
hist(subtruth[fi])
hist(subtruth[pi])

for(j in 1:k2){
  subclustindex<-clustindex_IVD[((j-1)*binsize+1):(j*binsize)];
  subdataY=y[subclustindex];    
  subdataX = X[subclustindex, ];
  
  pi=which(subdataY==0)
  fi=which(subdataY==1)
  print(j);
  print(length(fi));
  print(length(pi));
}


opar <- par()   
par(opar)

db=seq(from=-0.4, to =0.45, by=0.02)

T=subdataX[,1];

h1 = hist(subdataX[pi,1],breaks=db,);
h1$density = h1$counts/sum(h1$counts)


h2 = hist(subdataX[fi,1],breaks=db)
h2$density = h2$counts/sum(h2$counts)

par(mfrow=c(1,2))
plot(h1,freq=F,main="Histogram of m", sub="pass")
plot(h2,freq=F,main="Histogram of m", sub="fail")

#hist(subdataX[pi,1],breaks=db)
#hist(subdataX[fi,1],breaks=db)
par(mfrow=c())
4 5 8 9 10

plot(subdataX[fi,c(2,3)], main="covariate distribution", sub="fail execution",
     +      xlab="q", ylab="error",
     +      xlim=c(-0.55, -0.4), ylim=c(-1E-5,1E-5 ))

plot(subdataX[fi,c(2,3)], main="covariate distribution", sub="fail execution",
     +      xlab="q", ylab="error",
     +      xlim=c(-0.55, -0.4), ylim=c(-1E-5,1E-5 ))


fail<-rep(1,length(fi))
pass<-rep(0,length(pi))
output<-c(fail,pass)
dataf<-rbind(subdataX[fi,],subdataX[pi,])
datan<-cbind(dataf,output)
datacov<-datan[,2:5]
par(mfrow=c())



scatterplot3d(datacov[,1],datacov[,2],datacov[,3],color=c("red","blue")[datacov[,4]+1],    xlab="q1",ylab="q2",zlab="q3",
           main="covariates distribution",sub="pass(red)   fail(blue)")


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