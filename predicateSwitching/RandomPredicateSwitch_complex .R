

	#======================================================================================================================
	GoodResult <- function(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, w2, w3, w5, w6, w8, w9) {
		m <- 0;
		if (x1 > x2){                    #P1
			if (x3 > 0 & w2){      #S1   #P2
				if (x4 > 0 & w3){  #S2   #P3
					m <- x5;       #S3
				}
			}
		}
		if (x6 > x7){                    #P4
			if (x8 > 0 & w5){      #S4   #P5
				if (x9 > 5 & w6){  #S5   #P6
					m <- x10;      #S6
				}
			}
		}
		if (x11 > x12){                  #P7
			if (x13 > 0 & w8){     #S7   #P8
				if (x14 > 0 & w9){ #S8   #P9
					m <- x15;      #S9
				}
			}
		}
		return (m);
	}
	BadResult <- function(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, w2, w3, w5, w6, w8, w9) {
		m <- 0;
		if (x1 > x2){                    #P1
			if (x3 > 0 & w2){      #S1   #P2
				if (x4 > 0 & w3){  #S2   #P3
					m <- x5;       #S3
				}
			}
		}
		if (x6 > x7){                    #P4
			if (x8 > 0 & w5){      #S4   #P5<-------------------
				if (x9 > 0 & w6){  #S5   #P6
					m <- x10;      #S6
				}
			}
		}
		if (x11 > x12){                  #P7
			if (x13 > 0 & w8){     #S7   #P8
				if (x14 > 0 & w9){ #S8   #P9
					m <- x15;      #S9
				}
			}
		}
		return (m);
	}
	#======================================================================================================================
	statements <- paste('S', 1 : 9, sep = '');
	#-----------
	nTests <- 100000; #number of observations
	x1  <- rnorm(nTests, mean = 0, sd = 10);
	x2  <- rnorm(nTests, mean = 0, sd = 10);
	x3  <- rnorm(nTests, mean = 0, sd = 10);
	x4  <- rnorm(nTests, mean = 0, sd = 10);
	x5  <- sample(c(0, 1, 2), nTests, replace = TRUE);
	x6  <- rnorm(nTests, mean = 0, sd = 10);
	x7  <- rnorm(nTests, mean = 0, sd = 10);
	x8  <- rnorm(nTests, mean = 0, sd = 10);
	x9  <- rnorm(nTests, mean = 0, sd = 10);
	x10 <- sample(c(0, 1, 2), nTests, replace = TRUE);
	x11 <- rnorm(nTests, mean = 0, sd = 10);
	x12 <- rnorm(nTests, mean = 0, sd = 10);
	x13 <- rnorm(nTests, mean = 0, sd = 10);
	x14 <- rnorm(nTests, mean = 0, sd = 10);
	x15 <- sample(c(0, 1, 2), nTests, replace = TRUE);
	rw <- sample(c(0,1), nTests, replace = TRUE);	#random predicate switching
	ct <- rep(1, nTests);							#contact predicate
	#-------------------------------------------------------------------------------------------
	rows <- c(1 : nTests);
	cols <- c('Y', statements);
	coverage <- matrix(data = 0, nrow = length(rows), ncol = length(cols), dimnames = list(rows, cols));
	coverage[,'S1'] <- (x1 > x2);
	coverage[,'S2'] <- (x1 > x2) & (x3 > 0);
	coverage[,'S3'] <- (x1 > x2) & (x3 > 0) & (x4 > 0);
	coverage[,'S4'] <- (x6 > x7);
	coverage[,'S5'] <- (x6 > x7) & (x8 > 0);
	coverage[,'S6'] <- (x6 > x7) & (x8 > 0) & (x9 > 0);
	coverage[,'S7'] <- (x11 > x12);
	coverage[,'S8'] <- (x11 > x12) & (x13 > 0);
	coverage[,'S9'] <- (x11 > x12) & (x13 > 0) & (x14 > 0);
	#======================================================================================================================
	rows <- c('P2', 'P3', 'P5', 'P6', 'P8', 'P9');
	cols <- c('risk1','risk0','diff');
	score_rps1 <- as.data.frame(matrix(data = 0, nrow = length(rows), ncol = length(cols), dimnames = list(rows, cols)));
	#----
	P <- 'P2';
	CorRes <- mapply(GoodResult, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, rw, ct, ct, ct, ct, ct);
	ErrRes <- mapply(BadResult,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, rw, ct, ct, ct, ct, ct);
	fails  <- ifelse(CorRes != ErrRes, 1, 0);
	predicate <- coverage[,'S1'];
	score_rps1[P, 'risk0'] <- sum(fails & predicate & !rw) / sum(predicate & !rw)
	score_rps1[P, 'risk1'] <- sum(fails & predicate & rw) / sum(predicate & rw)
	score_rps1[P, 'diff' ] <- score_rps1[P, 'risk1'] - score_rps1[P, 'risk0'];
	
	#score_rps1[P, 'baah' ] <- Baah(as.formula(pdg['S1', 'CDG']), coverage);
	#----
	P <- 'P3';
	CorRes <- mapply(GoodResult, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, ct, rw, ct, ct, ct, ct);
	ErrRes <- mapply(BadResult,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, ct, rw, ct, ct, ct, ct);
	fails  <- ifelse(CorRes != ErrRes, 1, 0);
	predicate <- coverage[,'S2'];
	score_rps1[P, 'risk0'] <- sum(fails & predicate & !rw) / sum(predicate & !rw)
	score_rps1[P, 'risk1'] <- sum(fails & predicate & rw) / sum(predicate & rw)
	score_rps1[P, 'diff' ] <- score_rps1[P, 'risk1'] - score_rps1[P, 'risk0'];
	#----
	#**** Bug ****
	P <- 'P5';
	CorRes <- mapply(GoodResult, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, ct, ct, rw, ct, ct, ct);
	ErrRes <- mapply(BadResult,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, ct, ct, rw, ct, ct, ct);
	fails  <- ifelse(CorRes != ErrRes, 1, 0);
	predicate <- coverage[,'S4'];
	score_rps1[P, 'risk0'] <- sum(fails & predicate & !rw) / sum(predicate & !rw)
	score_rps1[P, 'risk1'] <- sum(fails & predicate & rw) / sum(predicate & rw)
	score_rps1[P, 'diff' ] <- score_rps1[P, 'risk1'] - score_rps1[P, 'risk0'];
	#----
	P <- 'P6';
	CorRes <- mapply(GoodResult, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, ct, ct, ct, rw, ct, ct);
	ErrRes <- mapply(BadResult,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, ct, ct, ct, rw, ct, ct);
	fails  <- ifelse(CorRes != ErrRes, 1, 0);
	predicate <- coverage[,'S5'];
	score_rps1[P, 'risk0'] <- sum(fails & predicate & !rw) / sum(predicate & !rw)
	score_rps1[P, 'risk1'] <- sum(fails & predicate & rw) / sum(predicate & rw)
	score_rps1[P, 'diff' ] <- score_rps1[P, 'risk1'] - score_rps1[P, 'risk0'];
	#----
	P <- 'P8';
	CorRes <- mapply(GoodResult, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, ct, ct, ct, ct, rw, ct);
	ErrRes <- mapply(BadResult,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, ct, ct, ct, ct, rw, ct);
	fails  <- ifelse(CorRes != ErrRes, 1, 0);
	predicate <- coverage[,'S7'];
	score_rps1[P, 'risk0'] <- sum(fails & predicate & !rw) / sum(predicate & !rw)
	score_rps1[P, 'risk1'] <- sum(fails & predicate & rw) / sum(predicate & rw)
	score_rps1[P, 'diff' ] <- score_rps1[P, 'risk1'] - score_rps1[P, 'risk0'];
	#----
	P <- 'P9';
	CorRes <- mapply(GoodResult, x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, ct, ct, ct, ct, ct, rw);
	ErrRes <- mapply(BadResult,  x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, ct, ct, ct, ct, ct, rw);
	fails  <- ifelse(CorRes != ErrRes, 1, 0);
	predicate <- coverage[,'S8'];
	score_rps1[P, 'risk0'] <- sum(fails & predicate & !rw) / sum(predicate & !rw)
	score_rps1[P, 'risk1'] <- sum(fails & predicate & rw) / sum(predicate & rw)
	score_rps1[P, 'diff' ] <- score_rps1[P, 'risk1'] - score_rps1[P, 'risk0'];

	
	
score_rps1

