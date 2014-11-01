SOBER<-function (dir,datafolder){
 
  source('C:/Users/zhuofu/RProject/bugLocalization/A_SOBER/SoberScore.R')
  
  
  y_raw<-read.table(paste(c(dir,as.character(datafolder),"/out.txt"),collapse=""));
  info<-read.table(paste(c(dir,as.character(datafolder),"/info.txt"),collapse=""))
  info<-as.matrix(info)
  n<-as.numeric(info[1])
  bugid<-as.numeric(info[2])
  susp<- rep(0, n)
  print(c("n is",n))
  print(c("bugid is",bugid))
  
  
  for (i in 1:n) {
    
    filename<-paste(c(dir,as.character(datafolder),"/",as.character(i), ".txt"),collapse="")
    if (!file.exists(filename)){    
      print("not exist");
      next
    }  
    
    data_raw <- read.table(filename, skip = 1,fill=T)## read variables
    naindex<-which(is.na(data_raw[,2]));
    if (length(naindex)!=0){
      data_raw<-data_raw[-naindex,];
    }
    
    finite_index<-which(is.finite(data_raw[,2])==TRUE);
    
    data<-data_raw[finite_index,]  
    
    y <- y_raw[,2];
    x <- data[, 2];
    data<-as.matrix(data);
    
    common<-intersect(y_raw[,1],data[,1]);
    indsy<-which(y_raw[,1] %in% common);
    indsx<-which(data[,1] %in% common);
    
    x<-x[indsx];
    y<-y[indsy];
    
    result<-SoberScore(y,x);
    
    susp[i]<-max(result);
    
  }
  
  
  srank<-n-rank(susp)[bugid]+1;
  return(list(sober=srank,n=n,bugid=bugid));
}
