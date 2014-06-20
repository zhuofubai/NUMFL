#generate data with c=1
n=100;
n2=sample(20:(n-20),1)
Cs=rep(1,n)
Ts=c(rep(1,n2),rep(0,n-n2))
Y=floor(runif(n, min=0, max=1)*2)


Cs2=c(Cs, rep(0,n))
Ts2=c(Ts,rep(0,n))
yplus=floor(runif(n, min=0, max=1)*2)
Y2=c(Y,yplus)

mod_1=lm(Y~Ts+Cs)
mod_2=lm(Y2~Ts2+Cs2)

beta=sum(Y[1:n2])/n2-sum(Y[(n2+1):n])/(n-n2)
beta




