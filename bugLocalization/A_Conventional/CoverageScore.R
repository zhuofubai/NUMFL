CoverageScore <- function (dir,datafolder) {
  
  y_raw<-read.table(paste(c(dir,as.character(datafolder),"/out.txt"),collapse="")); 
  diff_raw<-read.table(paste(c(dir,as.character(datafolder),"/diff.txt"),collapse=""));
  truth_raw<-read.table(paste(c(dir,as.character(datafolder),"/truth.txt"),collapse=""));
  info<-read.table(paste(c(dir,as.character(datafolder),"/info.txt"),collapse=""))
  info<-as.matrix(info)
  n<-as.numeric(info[1])
  bugid<-as.numeric(info[2])
  
  print(c("n is",n))
  print(c("bugid is",bugid))
  
  result = rep(0, n);
  result_T=result;
  result_O=result;
  result_F=result;
  result_D=result;
  
  for (i in 1:n) {
    
    y = data.matrix(y_raw[, 2])
    
    
    filename<-paste(c(dir,as.character(datafolder),"/",as.character(i), ".txt"),collapse="")   
    if (!file.exists(filename)){    
      print("not exist");
      next
    }    
    data_raw <- read.table(filename, skip = 1,fill=T)## read variables
    
    
    index0 = which(y < 0.5)
    index1 = which(y > 0.5)
    
    pass=y_raw[index0,];
    fail=y_raw[index1,];
    mps=intersect(pass[,1], data_raw[,1]);
    mfs=intersect(fail[,1], data_raw[,1]);
    
    ps=length(mps)
    fs=length(mfs)
    ns=ps+fs;
    p=length(pass[,1]);
    f=length(fail[,1]);
    
    ncf=fs;
    nuf=f-fs;
    ncs=ps;
    
    scoreT=(fs/f)/(ps/p+fs/f);
    scoreO=fs/sqrt(f*ns);
    scoreF=2/(1/(fs/f)+1/(fs/ns));
    scoreD=ncf*ncf*ncf/(nuf+ncs);
    
    result_T[i]=scoreT;
    result_O[i]=scoreO;
    result_F[i]=scoreF;
    result_D[i]=scoreD;
  }
  
  
  Tarantula<-n-rank(result_T)[bugid]+1;
  Ochiai<-n-rank(result_O)[bugid]+1;
  F1<-n-rank(result_F)[bugid]+1;
  Dstar<-n-rank(result_D)[bugid]+1;
  return(list(Tarantula=Tarantula,Ochiai=Ochiai,F1=F1,Dstar=Dstar,n=n,bugid=bugid));
}