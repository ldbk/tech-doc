

#==============================================================================
# libraries and constants
#==============================================================================

library(FLa4a)
library(XML)
library(reshape2)
data(rfLen)
data(ple4)
data(ple4.indices)
source("funs.R")



#==============================================================================
# Read files
#==============================================================================

# catch
cth.orig <- read.table("data/catch.len", skip=5)

# stock
stk.orig <- read.table("data/red.len", skip=4)

# surveys
idx.orig <- read.table("data/survey.len", skip=5)
idxJmp.orig <- read.table("data/jump.survey.len", skip=5)
idxTrd.orig <- read.table("data/tend.survey.len", skip=5)

#==============================================================================
# Recode
#==============================================================================

# catch
cth.orig[,5] <- qt2qt(cth.orig)

# stock
stk.orig[,5] <- qt2qt(stk.orig)

# surveys
idx.orig[,5] <- qt2qt(idx.orig)
idxJmp.orig[,5] <- qt2qt(idxJmp.orig)
idxTrd.orig[,5] <- qt2qt(idxTrd.orig)



#==============================================================================
# cast
#==============================================================================

# catch
cth.n <- acast(V5~V1~1~V2~1~1, value.var="V6", data=cth.orig)
cth.wt <- acast(V5~V1~1~V2~1~1, value.var="V7", data=cth.orig)
hrv <- acast(V5~V1~1~V2~1~1, value.var="V8", data=cth.orig)

# stock
stk.n <- acast(V5~V1~1~V2~1~1, value.var="V6", data=stk.orig)
stk.wt <- acast(V5~V1~1~V2~1~1, value.var="V7", data=stk.orig)

# surveys
idx.n <- acast(V5~V1~1~V2~1~1, value.var="V6", data=idx.orig)
idx.wt <- acast(V5~V1~1~V2~1~1, value.var="V7", data=idx.orig)
idx.hrv <- acast(V5~V1~1~V2~1~1, value.var="V8", data=idx.orig)

idxJmp.n <- acast(V5~V1~1~V2~1~1, value.var="V6", data=idxJmp.orig)
idxJmp.wt <- acast(V5~V1~1~V2~1~1, value.var="V7", data=idxJmp.orig)
idxJmp.hrv <- acast(V5~V1~1~V2~1~1, value.var="V8", data=idxJmp.orig)

idxTrd.n <- acast(V5~V1~1~V2~1~1, value.var="V6", data=idxTrd.orig)
idxTrd.wt <- acast(V5~V1~1~V2~1~1, value.var="V7", data=idxTrd.orig)
idxTrd.hrv <- acast(V5~V1~1~V2~1~1, value.var="V8", data=idxTrd.orig)



#==============================================================================
# quants
#==============================================================================

# catch
dnms <- dimnames(cth.n)
names(dnms) <- names(dimnames(FLQuant()))
names(dnms)[1] <- "len"
cth.n <- FLQuant(cth.n, dimnames=dnms)
cth.wt <- FLQuant(cth.wt, dimnames=dnms)
hrv <- FLQuant(hrv, dimnames=dnms)
units(hrv) <- "f"

# stock
dnms <- dimnames(stk.n)
names(dnms) <- names(dimnames(FLQuant()))
names(dnms)[1] <- "len"
stk.n <- FLQuant(stk.n, dimnames=dnms)
stk.wt <- FLQuant(stk.wt, dimnames=dnms)

# stock
dnms <- dimnames(idx.n)
names(dnms) <- names(dimnames(FLQuant()))
names(dnms)[1] <- "len"
idx.n <- FLQuant(idx.n, dimnames=dnms)
idx.wt <- FLQuant(idx.wt, dimnames=dnms)
idx.hrv <- FLQuant(idx.hrv, dimnames=dnms)

dnms <- dimnames(idxJmp.n)
names(dnms) <- names(dimnames(FLQuant()))
names(dnms)[1] <- "len"
idxJmp.n <- FLQuant(idxJmp.n, dimnames=dnms)
idxJmp.wt <- FLQuant(idxJmp.wt, dimnames=dnms)
idxJmp.hrv <- FLQuant(idxJmp.hrv, dimnames=dnms)

dnms <- dimnames(idxTrd.n)
names(dnms) <- names(dimnames(FLQuant()))
names(dnms)[1] <- "len"
idxTrd.n <- FLQuant(idxTrd.n, dimnames=dnms)
idxTrd.wt <- FLQuant(idxTrd.wt, dimnames=dnms)
idxTrd.hrv <- FLQuant(idxTrd.hrv, dimnames=dnms)



#==============================================================================
# check
#==============================================================================

#------------------------------------------------------------------------------
# match original data
#------------------------------------------------------------------------------

# catch
cim(cth.orig, cth.n, cth.wt, hrv)

# stock
cim(stk.orig, stk.n, stk.wt)

# surveys
cim(idx.orig, idx.n, idx.wt, idx.hrv)
cim(idxJmp.orig, idxJmp.n, idxJmp.wt, idxJmp.hrv)
cim(idxTrd.orig, idxTrd.n, idxTrd.wt, idxTrd.hrv)



#==============================================================================
# FLR objects
#==============================================================================

rfLen.stk <- FLStockLen(stock.n=stk.n, stock.wt=stk.wt, stock=quantSums(stk.wt*stk.n), catch.n=cth.n, catch.wt=cth.wt/cth.n, catch=quantSums(cth.wt), harvest=hrv)
m(rfLen.stk)[] <- 0.05
mat(rfLen.stk)[] <- m.spwn(rfLen.stk)[] <- harvest.spwn(rfLen.stk)[] <- 0
mat(rfLen.stk)[38:59,,,3:4] <- 1
  
rfTrawl.idx <- FLIndex(index=idx.n, catch.n=idx.n, catch.wt=idx.wt, sel.pattern=idx.hrv) 
effort(rfTrawl.idx)[] <- 100

rfTrawlJmp.idx <- FLIndex(index=idxJmp.n, catch.n=idxJmp.n, catch.wt=idxJmp.wt, sel.pattern=idxJmp.hrv) 
effort(rfTrawlJmp.idx)[] <- 100

rfTrawlTrd.idx <- FLIndex(index=idxTrd.n, catch.n=idxTrd.n, catch.wt=idxTrd.wt, sel.pattern=idxTrd.hrv) 
effort(rfTrawlTrd.idx)[] <- 100

#------------------------------------------------------------------------------
# save
#------------------------------------------------------------------------------

save(rfLen.stk, rfTrawl.idx, rfTrawlJmp.idx, rfTrawlTrd.idx, file="rfLen.rdata")



showClass("a4aGr")



#------------------------------------------------------------------------------
# a von Bertalanffy model
#------------------------------------------------------------------------------

vbObj <- a4aGr(grMod=~linf*(1-exp(-k*(t-t0))), grInvMod=~t0-1/k*log(1-len/linf), params=FLPar(linf=58.5, k=0.086, t0=0.001, units=c("cm","ano-1","ano")))

# a quick check about the model and it's inverse
lc=20
predict(vbObj, t=predict(vbObj, len=lc))==lc



#------------------------------------------------------------------------------
# predicting ages from lengths and vice-versa
#------------------------------------------------------------------------------
predict(vbObj, len=5:10+0.5)
predict(vbObj, t=5:10+0.5)



# vcov matrix
mm <- matrix(NA, ncol=3, nrow=3)
diag(mm) <- c(100, 0.001,0.001)
mm[upper.tri(mm)] <- mm[lower.tri(mm)] <- c(0.1,0.1,0.0003)
# object
vbObj <- a4aGr(grMod=~linf*(1-exp(-k*(t-t0))), grInvMod=~t0-1/k*log(1-len/linf), params=FLPar(linf=58.5, k=0.086, t0=0.001, units=c("cm","ano-1","ano")), vcov=mm, distr="norm")
# simulate
vbObj <- mvrnorm(100,vbObj)
# predict
predict(vbObj, len=5:10+0.5)



boxplot(t(predict(vbObj, t=0:50+0.5)))



addr <- "http://www.fishbase.org/PopDyn/PopGrowthList.php?ID=501"
tab <- try(readHTMLTable(addr))
linf <- as.numeric(as.character(tab$dataTable[,2]))
k <- as.numeric(as.character(tab$dataTable[,4]))
t0 <- as.numeric(as.character(tab$dataTable[,5]))
vbObj <- a4aGr(grMod=~linf*(1-exp(-k*(t-t0))), 
			   grInvMod=~t0-1/k*log(1-len/linf), 
			   params=FLPar(linf=58.5, k=0.086, t0=0.001, units=c("cm","ano-1","ano")), 
			   vcov=mm)
pars <- list(list(a=min(linf), b=max(linf), c=median(linf)), list(a=min(k), b=max(k), c=median(k)), list(a=median(t0, na.rm=T)-IQR(t0, na.rm=T)/2, b=median(t0, na.rm=T)+IQR(t0, na.rm=T)/2))
vbSim <- mvrtriangle(10000, vbObj, paramMargins=pars)



par(mfrow=c(3,1))
hist(c(params(vbSim)["linf",]), main="linf")
hist(c(params(vbSim)["k",]), main="k", prob=TRUE)
lines(x. <- seq(min(k), max(k), len=100), dnorm(x., mean(k), sd(k)))
hist(c(params(vbSim)["t0",]), main="t0")



splom(data.frame(t(params(vbSim)@.Data)), pch=".")



boxplot(t(predict(vbSim, t=0:20+0.5)))



vbSim <- mvrcop(10000, vbObj, copula="archmCopula", family="clayton", param=2, margins="triangle", paramMargins=pars)



splom(data.frame(t(params(vbSim)@.Data)), pch=".")



boxplot(t(predict(vbSim, t=0:20+0.5)))



#--------------------------------------------------------------------
# growth object
#--------------------------------------------------------------------
vbObj <- a4aGr(grMod=~linf*(1-exp(-k*(t-t0))), grInvMod=~t0-1/k*log(1-len/linf), params=FLPar(linf=58.5, k=0.086, t0=0.001, units=c("cm","ano-1","ano")), vcov=mm, distr="norm")
#--------------------------------------------------------------------
# converte catch-at-length to catch-at-age
#--------------------------------------------------------------------
cth.n <- l2a(catch.n(rfLen.stk), vbObj)



# trick
quant(cth.n) <- "len"
xyplot(data~len|qname, groups=year, data=(FLQuants(len=catch.n(rfLen.stk), age=cth.n)), type="l", xlab="", ylab="numbers")



#--------------------------------------------------------------------
# convertion
#--------------------------------------------------------------------
aStk <- l2a(rfLen.stk, vbObj)
aIdx <- l2a(rfTrawl.idx, vbObj)



showClass("a4aM")



mod2 <- FLModelSim(model=~a, params=FLPar(a=0.2))
m1 <- a4aM(level=mod2)



#--------------------------------------------------------------------
# models or shape and level
#--------------------------------------------------------------------
mod1 <- FLModelSim(model=~exp(-age-0.5))
mod2 <- FLModelSim(model=~1.5*k, params=FLPar(k=0.4))
#--------------------------------------------------------------------
# constructor
#--------------------------------------------------------------------
m2 <- a4aM(shape=mod1, level=mod2)



#--------------------------------------------------------------------
# get NAO
#--------------------------------------------------------------------
nao.orig <- read.table("http://www.cdc.noaa.gov/data/correlation/nao.data", skip=1, nrow=62, na.strings="-99.90")
dnms <- list(quant="nao", year=1948:2009, unit="unique", season=1:12, area="unique")
nao.flq <- FLQuant(unlist(nao.orig[,-1]), dimnames=dnms, units="nao")
# build covar
nao <- seasonMeans(nao.flq[,,,1:3]) 
nao <- nao>0
#--------------------------------------------------------------------
# the trend model M increases 50% if NAO is positive on the first quarter
#--------------------------------------------------------------------
mod3 <- FLModelSim(model=~1+b*nao, params=FLPar(b=0.5))
#--------------------------------------------------------------------
# constructor
#--------------------------------------------------------------------
mod1 <- FLModelSim(model=~exp(-age-0.5))
mod2 <- FLModelSim(model=~1.5*k, params=FLPar(k=0.4))
m3 <- a4aM(shape=mod1, level=mod2, trend=mod3)



#--------------------------------------------------------------------
# the same exponential decay for shape
#--------------------------------------------------------------------
mod1 <- FLModelSim(model=~exp(-age-0.5))
#--------------------------------------------------------------------
# For level we'll use Jensen's third estimator (Kenshington, 2013).
#--------------------------------------------------------------------
mod2 <- FLModelSim(model=~k^0.66*t^0.57, params=FLPar(matrix(c(0.4,10)), dimnames=list(params=c("k","t"), iter=1)), vcov=array(c(0.002, 0.01,0.01, 1), dim=c(2,2)))
#--------------------------------------------------------------------
# and a trend from NAO
#--------------------------------------------------------------------
mod3 <- FLModelSim(model=~1+b*nao, params=FLPar(b=0.5), vcov=matrix(0.02))
#--------------------------------------------------------------------
# create object and simulate
#--------------------------------------------------------------------
m4 <- a4aM(shape=mod1, level=mod2, trend=mod3)
m4 <- mvrnorm(100, m4)



m4 <- a4aM(shape=mod1, level=mvrnorm(100, mod2), trend=mvrnorm(100, mod3))



linf <- 60
k <- 0.4
# vcov matrix
mm <- matrix(NA, ncol=2, nrow=2)
# 10% cv
diag(mm) <- c((linf*0.1)^2, (k*0.1)^2)
# 0.2 correlation
mm[upper.tri(mm)] <- mm[lower.tri(mm)] <- c(0.05)
# a good way to check is using cov2cor
cov2cor(mm)
# create object
mgis2 <- FLModelSim(model=~K*(linf/len)^1.5, params=FLPar(linf=linf, K=k), vcov=mm)

pars <- list(list(55,65), list(a=0.3, b=0.6, c=0.35))
mgis2 <- mvrtriangle(1000, mgis2, paramMargins=pars)



splom(t(params(mgis2)@.Data))



par(mfrow=c(2,1))
hist(c(params(mgis2)["linf",]), main="Linf")
hist(c(params(mgis2)["K",]), main="K")



m5 <- a4aM(shape=mgis2, level=mod2, trend=mod3)
# or
m5 <- m4
level(m5) <- mgis2



# simple
m(m1)
# with ages
rngage(m1) <- c(0,15)
m(m1)
# with ages and years
rngyear(m1) <- c(2000, 2010)
m(m1)



# simple
m(m2)
# with ages
rngage(m2) <- c(0,15)
m(m2)
# with ages and years
rngyear(m2) <- c(2000, 2003)
m(m2)
# note that 
predict(level(m2))
# is similar to 
m(m2)["0"]
# that's because mbar is "0"
rngmbar(m2)
# changing ...
rngmbar(m2)<- c(0,5)
quantMeans(m(m2)[as.character(0:5)])



# simple
m(m3, nao=1)
# with ages
rngage(m3) <- c(0,15)
m(m3, nao=0)
# with ages and years
rngyear(m3) <- c(2000, 2003)
m(m3, nao=as.numeric(nao[,as.character(2000:2003)]))



# simple
m(m4, nao=1)
# with ages
rngage(m4) <- c(0,15)
m(m4, nao=0)
# with ages and years
rngyear(m4) <- c(2000, 2003)
m(m4, nao=as.numeric(nao[,as.character(2000:2003)]))



bwplot(data~factor(quant)|year, data=m(m4, nao=as.numeric(nao[,as.character(2000:2003)])))



plotIters(m(m4, nao=as.numeric(nao[,as.character(2000:2003)])), by = "year")



showClass("a4aFit")



showClass("a4aFitSA")



showClass("SCAPars")
showClass("a4aStkParams")



fmodel <- ~ factor(age) + factor(year)
qmodel <- list(~ factor(age)) 



fit <- a4a(fmodel, qmodel, stock = ple4, indices = ple4.indices[1])



fitstk <- ple4 + fit
plotIters(fbar(fitstk))



stock <- ple4[,paste(1995:2008)]
indices <- ple4.indices[[1]]



fmodel1 <- ~ 1
fmodel2 <- ~ year
fmodel3 <- ~ age


