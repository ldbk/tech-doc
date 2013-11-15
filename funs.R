# quant 2 quant
qt2qt <- function(object, id=5, split="-"){
	qt <- object[,id]
	levels(qt) <- unlist(lapply(strsplit(levels(qt), split=split), "[[", 2))
	as.numeric(as.character(qt))
}

# check import and massage
cim <- function(object, n, wt, hrv="missing"){
	v <- object[sample(1:nrow(object), 1),]
	c1 <- c(n[as.character(v$V5),as.character(v$V1),1,as.character(v$V2)]==v$V6)
	c2 <- c(wt[as.character(v$V5),as.character(v$V1),1,as.character(v$V2)]==v$V7)
	if(missing(hrv)){
		c1 + c2 == 2	
	} else {
		c3 <- c(hrv[as.character(v$V5),as.character(v$V1),1,as.character(v$V2)]==v$V8)
		c1 + c2 + c3 == 3	
	}
}

