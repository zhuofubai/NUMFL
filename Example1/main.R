
 source('C:/Users/zhuofu/RProject/Example1/goodFun.R')
 source('C:/Users/zhuofu/RProject/Example1/fillA.R')
 source('C:/Users/zhuofu/RProject/Example1/badFun.R')
c <- rnorm(1000, 0, 100)
a <- fillA(c, 0, 100)
goodOut <- goodFun(a, c)
badOut <- badFun(a, c)
fails <- abs(goodOut - badOut)>0.0001
outDiff <- goodOut - badOut
M1 <- lm(fails ~ badOut + a + c)
summary(M1)

M2 <- lm(fails ~ goodOut + a + c)
summary(M2)

M3 <- lm(outDiff ~ badOut + a + c)
summary(M3)

M4 <- lm(outDiff ~ goodOut + a + c)
summary(M4)


M5 <- lm(outDiff ~ badOut) #no adjustment for a and c
summary(M5)
 
 scatterplotMatrix(cbind(fails,outDiff,badOut,goodOut,a,c),pch=19,cex=.5,reg.line=F, lwd.smooth=1.25,
                   spread=F,ellipse=F, col=c('gray60','#2957FF','#FF8000'),
                   col.axis='gray50')
 
 dmelt = melt(d, id=c('Country','Overall'),
              measure=c('Interest','Support','Income','Health','Edu','HDI'))
 
 
 ggplot(aes(x=badOut,y=outDiff),data=dmelt) +
   geom_point(color='#FF8000',alpha=.75) +
   geom_smooth(se=F, method='gam', formula=y~s(x), color='#2957FF') +
   facet_wrap(~variable, scales='free_x')
 opar <- par()  
 par(mfrow=c(2,4))
 
 k=8
 clustindex<-sort(goodOut,method="sh", index.return=TRUE)$ix;
 binsize<-floor(length(clustindex)/k)
 for (j in 1: k){
 if(j==k){
   subclustindex<-clustindex[((j-1)*binsize+1):length(clustindex)];
 }else{
   subclustindex<-clustindex[((j-1)*binsize+1):(j*binsize)];
 }

 sub_outDiff<-outDiff[subclustindex];
 sub_fails<-fails[subclustindex];
 sub_goodOut <- goodOut[subclustindex];
 sub_badOut<-badOut[subclustindex];
 sub_a<-a[subclustindex];
 sub_c<-c[subclustindex];
 M5_1<-lm(sub_outDiff~sub_badOut);
 print(coefficients(M5_1)[2]);
 plot(sub_badOut,sub_fails)
 #print(var(sub_goodOut))
 }
 par(opar)
