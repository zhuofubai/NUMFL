normalize<-function(x){
x<-data.matrix(x)   
sweep(x, 2, apply(abs(x), 2, max), "/")
}