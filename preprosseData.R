preprosseData <- function (data_raw,y_raw,truth_raw,diff_raw) {
  y <- data.matrix(y_raw[, 2])
  truth_raw<-data.matrix(truth_raw)
  diff_raw<-data.matrix(diff_raw)
  col <- ncol(data_raw)
  ###############remove the variables has NaN ##############
  naindex<-which(is.na(data_raw[,2]));
  if (length(naindex)!=0){
    data_raw<-data_raw[-naindex,];
  }
  ##filter out the test case which does not cover the instrumented file
  #####################################################################
  common<-intersect(y_raw[,1], data_raw[,1]);
  indsy<-which(y_raw[,1] %in% common);
  indsx<-which(data_raw[,1] %in% common);
  
  y_raw<-y_raw[indsy,];
  
  y<-y[indsy, ];
  truth_raw<-truth_raw[indsy,];
  diff_raw<-diff_raw[indsy,];
  y_index<-y_raw[,1];
  data_raw<-data_raw[indsx,];

  row<-length(y)
  ################ sample the output and data##############
  source('C:/Users/zhuofu/RProject/sampleOutcome.R');
  index<-sampleOutcome(y);
  y <- y[index]; 
  truth_raw<-truth_raw[index]
  diff_raw<-diff_raw[index]
  
  ###skip training oringinal modle if the size of x-y pairs is too small###
  
  
  ##########preprocessing the data to get the last run of a loop####################
  data <- matrix(0, nrow=row, ncol = col)
  
  for(j in 0:row-1)
  {
    subindex<-which(data_raw[,1]==y_index[j+1])
    if (length(subindex)==0){
      
      data[j+1,]=-1;
      next
    }
    subdata<-data_raw[subindex,]
    if(length(subindex)==1)
    {
      lastiter<-1
      data[j+1,]<-as.numeric(subdata);
    }else{
      lastiter<-nrow(subdata)
      data[j+1,]<-as.numeric(subdata[lastiter,]);}
  }
  #data<-data[-which(xp==-1),]
  
  
  x <- data[, 2:ncol(data)]
  x <- as.matrix(x)
  x <- x[index, ]   
   
  return(list(X=x,Y=y,TRUTH=truth_raw,DIFFERENCE=diff_raw))
}