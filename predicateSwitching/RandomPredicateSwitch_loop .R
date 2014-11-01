source('C:/Users/zhuofu/RProject/predicateSwitching/GoodResult.R')
source('C:/Users/zhuofu/RProject/predicateSwitching/BadResult.R')
#======================================================================================================================
statements <- paste('S', 1 : 9, sep = '');
#-----------
nTests <- 8000; #number of observations
niteration<-50
x1_m  <-replicate(niteration,  rnorm(nTests, mean = 0, sd = 10));
x2_m  <-replicate(niteration,  rnorm(nTests, mean = 0, sd = 10));
x3_m  <-replicate(niteration,  rnorm(nTests, mean = 0, sd = 10));
x4_m  <-replicate(niteration,  rnorm(nTests, mean = 0, sd = 10));
x5_m  <-replicate(niteration, sample(c(0, 1, 2), nTests, replace = TRUE));
x6_m  <-replicate(niteration,  rnorm(nTests, mean = 0, sd = 10));
x7_m  <-replicate(niteration,  rnorm(nTests, mean = 0, sd = 10));
x8_m  <-replicate(niteration,  rnorm(nTests, mean = 0, sd = 10));
x9_m  <-replicate(niteration,  rnorm(nTests, mean = 0, sd = 10));
x10_m <-replicate(niteration, sample(c(0, 1, 2), nTests, replace = TRUE));
x11_m <-replicate(niteration,  rnorm(nTests, mean = 0, sd = 10));
x12_m <-replicate(niteration,  rnorm(nTests, mean = 0, sd = 10));
x13_m <-replicate(niteration,  rnorm(nTests, mean = 0, sd = 10));
x14_m <-replicate(niteration,  rnorm(nTests, mean = 0, sd = 10));
x15_m <-replicate(niteration, sample(c(0, 1, 2), nTests, replace = TRUE));
rw_base<-sample(c(0,1), nTests, replace = TRUE);
rw_m <- replicate(niteration, rw_base);  #random predicate switching
ct_m <- replicate(niteration,rep(1, nTests));	

x1<-lapply(seq_len(nrow(x1_m)), function(i) x1_m[i,])
x2<-lapply(seq_len(nrow(x2_m)), function(i) x2_m[i,])
x3<-lapply(seq_len(nrow(x3_m)), function(i) x3_m[i,])
x4<-lapply(seq_len(nrow(x4_m)), function(i) x4_m[i,])
x5<-lapply(seq_len(nrow(x5_m)), function(i) x5_m[i,])
x6<-lapply(seq_len(nrow(x6_m)), function(i) x6_m[i,])
x7<-lapply(seq_len(nrow(x7_m)), function(i) x7_m[i,])
x8<-lapply(seq_len(nrow(x8_m)), function(i) x8_m[i,])
x9<-lapply(seq_len(nrow(x9_m)), function(i) x9_m[i,])
x10<-lapply(seq_len(nrow(x10_m)), function(i) x10_m[i,])
x11<-lapply(seq_len(nrow(x11_m)), function(i) x11_m[i,])
x12<-lapply(seq_len(nrow(x12_m)), function(i) x12_m[i,])
x13<-lapply(seq_len(nrow(x13_m)), function(i) x13_m[i,])
x14<-lapply(seq_len(nrow(x14_m)), function(i) x14_m[i,])
x15<-lapply(seq_len(nrow(x15_m)), function(i) x15_m[i,])
rw<-lapply(seq_len(nrow(rw_m)), function(i) rw_m[i,])
ct<-lapply(seq_len(nrow(ct_m)), function(i) ct_m[i,])
#-------------------------------------------------------------------------------------------
rows <- c(1 : nTests);
cols <- c('Y', statements);
coverage <- matrix(data = 0, nrow = length(rows), ncol = length(cols), dimnames = list(rows, cols));
coverage[,'S1'] <- rowSums(x1_m > x2_m)>0;
coverage[,'S2'] <- (rowSums(x1_m > x2_m)>0) & (rowSums(x3_m > 0)>0);
coverage[,'S3'] <- (rowSums(x1_m > x2_m)>0) & (rowSums(x3_m > 0)>0) & (rowSums(x4_m > 0)>0);
coverage[,'S4'] <- (rowSums(x6_m > x7_m)>0);
coverage[,'S5'] <- (rowSums(x6_m > x7_m)>0) & (rowSums(x8_m > 0)>0);
coverage[,'S6'] <- (rowSums(x6_m > x7_m)>0) & (rowSums(x8_m > 0)>0) & (rowSums(x9_m > 0)>0);
coverage[,'S7'] <- (rowSums(x11_m > x12_m)>0);
coverage[,'S8'] <- (rowSums(x11_m > x12_m)>0) & (rowSums(x13_m > 0)>0);
coverage[,'S9'] <- (rowSums(x11_m > x12_m)>0) & (rowSums(x13_m > 0)>0) & (rowSums(x14_m > 0)>0);
#======================================================================================================================
rows <- c('P2', 'P3', 'P5', 'P6', 'P8', 'P9');
cols <- c('risk1','risk0','diff');
score_rps1 <- as.data.frame(matrix(data = 0, nrow = length(rows), ncol = length(cols), dimnames = list(rows, cols)));

#----
P <- 'P2';
CorRes <- mapply(GoodResult,niteration, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, rw, ct, ct, ct, ct, ct);
ErrRes <- mapply(BadResult, niteration, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, rw, ct, ct, ct, ct, ct);
fails  <- ifelse(CorRes != ErrRes, 1, 0);
predicate <- coverage[,'S1'];
score_rps1[P, 'risk0'] <- sum(fails & predicate &rowSums(!rw_m))/ sum(predicate & rowSums(!rw_m))
score_rps1[P, 'risk1'] <- sum(fails & predicate & rowSums(rw_m)) / sum(predicate & rowSums(rw_m))
score_rps1[P, 'diff' ] <- score_rps1[P, 'risk1'] - score_rps1[P, 'risk0'];

#score_rps1[P, 'baah' ] <- Baah(as.formula(pdg['S1', 'CDG']), coverage);
#----
P <- 'P3';
CorRes <- mapply(GoodResult,niteration, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, ct, rw, ct, ct, ct, ct);
ErrRes <- mapply(BadResult, niteration, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, ct, rw, ct, ct, ct, ct);
fails  <- ifelse(CorRes != ErrRes, 1, 0);
predicate <- coverage[,'S2'];
score_rps1[P, 'risk0'] <- sum(fails & predicate &rowSums(!rw_m))/ sum(predicate & rowSums(!rw_m))
score_rps1[P, 'risk1'] <- sum(fails & predicate & rowSums(rw_m)) / sum(predicate & rowSums(rw_m))
score_rps1[P, 'diff' ] <- score_rps1[P, 'risk1'] - score_rps1[P, 'risk0'];

#----
#**** Bug ****
P <- 'P5';
CorRes <- mapply(GoodResult,niteration, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, ct, ct, rw, ct, ct, ct);
ErrRes <- mapply(BadResult, niteration, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, ct, ct, rw, ct, ct, ct);
fails  <- ifelse(CorRes != ErrRes, 1, 0);
predicate <- coverage[,'S4'];
score_rps1[P, 'risk0'] <- sum(fails & predicate &rowSums(!rw_m))/ sum(predicate & rowSums(!rw_m))
score_rps1[P, 'risk1'] <- sum(fails & predicate & rowSums(rw_m)) / sum(predicate & rowSums(rw_m))
score_rps1[P, 'diff' ] <- score_rps1[P, 'risk1'] - score_rps1[P, 'risk0'];

#----
P <- 'P6';
CorRes <- mapply(GoodResult,niteration, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, ct, ct, ct, rw, ct, ct);
ErrRes <- mapply(BadResult,niteration,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, ct, ct, ct, rw, ct, ct);
fails  <- ifelse(CorRes != ErrRes, 1, 0);
predicate <- coverage[,'S5'];
score_rps1[P, 'risk0'] <- sum(fails & predicate &rowSums(!rw_m))/ sum(predicate & rowSums(!rw_m))
score_rps1[P, 'risk1'] <- sum(fails & predicate & rowSums(rw_m)) / sum(predicate & rowSums(rw_m))
score_rps1[P, 'diff' ] <- score_rps1[P, 'risk1'] - score_rps1[P, 'risk0'];

#----
P <- 'P8';
CorRes <- mapply(GoodResult,niteration, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, ct, ct, ct, ct, rw, ct);
ErrRes <- mapply(BadResult,niteration,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, ct, ct, ct, ct, rw, ct);
fails  <- ifelse(CorRes != ErrRes, 1, 0);
predicate <- coverage[,'S7'];
score_rps1[P, 'risk0'] <- sum(fails & predicate &rowSums(!rw_m))/ sum(predicate & rowSums(!rw_m))
score_rps1[P, 'risk1'] <- sum(fails & predicate & rowSums(rw_m)) / sum(predicate & rowSums(rw_m))
score_rps1[P, 'diff' ] <- score_rps1[P, 'risk1'] - score_rps1[P, 'risk0'];

#----
P <- 'P9';
CorRes <- mapply(GoodResult,niteration, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, ct, ct, ct, ct, ct, rw);
ErrRes <- mapply(BadResult, niteration, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, ct, ct, ct, ct, ct, rw);
fails  <- ifelse(CorRes != ErrRes, 1, 0);
predicate <- coverage[,'S8'];
score_rps1[P, 'risk0'] <- sum(fails & predicate &rowSums(!rw_m))/ sum(predicate & rowSums(!rw_m))
score_rps1[P, 'risk1'] <- sum(fails & predicate & rowSums(rw_m)) / sum(predicate & rowSums(rw_m))
score_rps1[P, 'diff' ] <- score_rps1[P, 'risk1'] - score_rps1[P, 'risk0'];


score_rps1
