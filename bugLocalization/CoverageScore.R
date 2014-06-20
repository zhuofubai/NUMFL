
n=203
result = rep(0, n);
result_T=result;
result_O=result;
result_F=result;
bugid =164 ##bug1 on line 780
for (i in 1:n) {
	y_raw = read.table("C:/Users/zhuofu/workspace/apacheCommonMath3.1.1/TestBlockRealMatrix/Data5/out.txt")
	y = data.matrix(y_raw[, 2])
    print(i)
    filename = c("C:/Users/zhuofu/workspace/apacheCommonMath3.1.1/TestBlockRealMatrix/Data5/", as.character(i), ".txt")
    filename = paste(filename, collapse = "")

    if (!file.exists(filename)){
	
	print("not exist");
    		next
	}

    data_raw = read.table(filename, skip = 1)## read variables
	
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
	scoreT=(fs/f)/(ps/p+fs/f);
	scoreO=fs/sqrt(f*ns);
	scoreF=2/(1/(fs/f)+1/(fs/ns));
	result_T[i]=scoreT;
	result_O[i]=scoreO;
	result_F[i]=scoreF;
	
}

result_T
result_O
result_F

which.max(result_T);
which.max(result_O);
which.max(result_F);



n-rank(result_T)[bugid]+1;
n-rank(result_O)[bugid]+1;
n-rank(result_F)[bugid]+1;
