ESP<-function (dir,datafolder,clustNum){

# source("C:/Users/zhuofu/RProject/bugLocalization/normalize.R")
# source("C:/Users/zhuofu/RProject/bugLocalization/standardize.R")
# source('C:/Users/zhuofu/RProject/bugLocalization/preprosseData.R')
source('C:/Users/zhuofu/RProject/bugLocalization/A_ESP/ESP_simple.R');
source('C:/Users/zhuofu/RProject/bugLocalization/A_ESP/ESP_complex.R');


y_raw<-read.table(paste(c(dir,as.character(datafolder),"/out.txt"),collapse=""));
info<-read.table(paste(c(dir,as.character(datafolder),"/info.txt"),collapse=""))
info<-as.matrix(info)
n<-as.numeric(info[1])
bugid<-as.numeric(info[2])

print(c("n is",n))
print(c("bugid is",bugid))

susp_simple <- rep(0, n)
susp_complex<- rep(0, n)
for (i in 1:n) {
  # print(i)
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
  
  fp<-length(which(y[indsy]>0.5));
  sp<-length(which(y[indsy]<0.5));
  
  if(fp==0||nrow(data)==1)
  {
    susp_simple[i]<-0;
    susp_complex[i]<-0;
    next;
  }
  result_simple<-ESP_simple(y_raw,data,x,fp,sp);
  result_complex<-ESP_complex(y_raw,data,fp,sp);
  
  susp_simple[i]<-max(result_simple);
  susp_complex[i]<-max(result_complex);
}

simple<-n-rank(susp_simple)[bugid]+ 1;
complex<-n-rank(susp_complex)[bugid]+1;
return(list(simple=simple,complex=complex,n=n,bugid=bugid));
}
