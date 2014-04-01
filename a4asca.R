library(FLa4a)
library(diagram)
data(ple4)
data(ple4.indices)
source("funs.R")

#####################################
# DATA STRUCTURES
#####################################

showClass("a4aFit")

plotS4("a4aFit", main="a4aFit class", lwd = 1, box.lwd = 2, cex.txt = 0.8, box.size = 0.1, box.type = "square", box.prop = 0.3)

showClass("a4aFitSA")

plotS4("a4aFitSA", main="a4aFitSA class", lwd = 1, box.lwd = 2, cex.txt = 0.8, box.size = 0.1, box.type = "square", box.prop = 0.3)

showClass("SCAPars")

plotS4("SCAPars", main="a4aStkParams class", lwd = 1, box.lwd = 2, cex.txt = 0.8, box.size = 0.1, box.type = "square", box.prop = 0.3)

showClass("a4aStkParams")

plotS4("a4aStkParams", main="a4aStkParams class", lwd = 1, box.lwd = 2, cex.txt = 0.8, box.size = 0.1, box.type = "square", box.prop = 0.3)

#####################################
# FAST AND FURIOUS 1.0
#####################################

fit <- sca(ple4, ple4.indices)
res <- residuals(fit, ple4, ple4.indices)
plot(res)
bubbles(res)
qqmath(res)
stk <- ple4 + fit
plot(stk)
wireframe(data ~ age + year, data = as.data.frame(harvest(stk)), drape = TRUE, main="Fishing mortality", screen = list(x = -90, y=-45))
wireframe(data ~ age + year, data = as.data.frame(stock.n(stk)), drape = TRUE, main="Population", screen = list(x = -90, y=-45))
wireframe(data ~ age + year, data = as.data.frame(catch.n(stk)), drape = TRUE, main="Catches", screen = list(x = -90, y=-45))

#####################################
# Common stock assessment - sca
#####################################

# fishing mortality

qmodel <- list(~ factor(age)) 
fmodel <- ~ factor(age) + factor(year)
fit <- sca(stock = ple4, indices = ple4.indices[1], fmodel=fmodel, qmodel=qmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit)), drape = TRUE, screen = list(x = -90, y=-45))

fmodel <- ~ s(age, k=4) + s(year, k = 20)
fit1 <- sca(ple4, ple4.indices[1], fmodel, qmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit1)), drape = TRUE, screen = list(x = -90, y=-45))

fmodel <- ~ s(age, k=4) + s(year, k = 20) + te(age, year, k = c(3,3))
fit2 <- sca(ple4, ple4.indices[1], fmodel, qmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit2)), drape = TRUE, screen = list(x = -90, y=-45))

fmodel <- ~ te(age, year, k = c(4,20))
fit3 <- sca(ple4, ple4.indices[1], fmodel, qmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit3)), drape = TRUE, screen = list(x = -90, y=-45))

fmodel <- ~ te(age, year, k = c(4,20)) + s(year, k = 5, by = as.numeric(age==1))
fit4 <- sca(ple4, ple4.indices[1], fmodel, qmodel)
wireframe(data ~ age + year, data = as.data.frame(harvest(fit4)), drape = TRUE, screen = list(x = -90, y=-45))

# q

sfrac <- mean(range(ple4.indices[[1]])[c("startf", "endf")])
fmodel <- ~ factor(age) + factor(year)

qmodel <- list(~ factor(age)) 
fit <- sca(ple4, ple4.indices[1], fmodel, qmodel)
Z <- m(ple4) + harvest(fit)*frac
lst <- dimnames(fit@index[[1]])
lst$x <- stock.n(fit)*exp(-Z)
stkn <- do.call("trim", lst)
wireframe(data ~ age + year, data = as.data.frame(index(fit)[[1]]/stkn), drape = TRUE, screen = list(x = -90, y=-45))

qmodel <- list(~ s(age, k=4))
fit1 <- sca(ple4, ple4.indices[1], fmodel, qmodel)
Z <- m(ple4) + harvest(fit1)*frac
lst <- dimnames(fit1@index[[1]])
lst$x <- stock.n(fit1)*exp(-Z)
stkn <- do.call("trim", lst)
wireframe(data ~ age + year, data = as.data.frame(index(fit1)[[1]]/stkn), drape = TRUE, screen = list(x = -90, y=-45))

qmodel <- list(~ te(age, year, k = c(3,40)))
fit2 <- sca(ple4, ple4.indices[1], fmodel, qmodel)
Z <- m(ple4) + harvest(fit2)*frac
lst <- dimnames(fit2@index[[1]])
lst$x <- stock.n(fit2)*exp(-Z)
stkn <- do.call("trim", lst)
wireframe(data ~ age + year, data = as.data.frame(index(fit2)[[1]]/stkn), drape = TRUE, screen = list(x = -90, y=-45))

qmodel <- list( ~ s(age, k=4) + year)
fit3 <- sca(ple4, ple4.indices[1], fmodel, qmodel)
Z <- m(ple4) + harvest(fit3)*frac
lst <- dimnames(fit3@index[[1]])
lst$x <- stock.n(fit3)*exp(-Z)
stkn <- do.call("trim", lst)
wireframe(data ~ age + year, data = as.data.frame(index(fit3)[[1]]/stkn), drape = TRUE, screen = list(x = -90, y=-45))

# S/R

fmodel <- ~ s(age, k=4) + s(year, k = 20)
qmodel <- list(~ s(age, k=4))
srmodel <- ~ factor(year)
fit <- sca(ple4, ple4.indices[1], fmodel=fmodel, qmodel=qmodel, srmodel=srmodel) 

srmodel <- ~ s(year, k=20)
fit1 <- sca(ple4, ple4.indices[1], fmodel, qmodel, srmodel) 

srmodel <- ~ ricker(CV=0.05)
fit2 <- sca(ple4, ple4.indices[1], fmodel, qmodel, srmodel) 

srmodel <- ~ bevholt(CV=0.05)
fit3 <- sca(ple4, ple4.indices[1], fmodel, qmodel, srmodel) 

srmodel <- ~ hockey(CV=0.05)
fit4 <- sca(ple4, ple4.indices[1], fmodel, qmodel, srmodel) 

srmodel <- ~ geomean(CV=0.05)
fit5 <- sca(ple4, ple4.indices[1], fmodel, qmodel, srmodel) 

flqs <- FLQuants(fac=stock.n(fit)[1], smo=stock.n(fit1)[1], ric=stock.n(fit2)[1], bh=stock.n(fit3)[1], hs=stock.n(fit4)[1], gm=stock.n(fit5)[1])
xyplot(data~year, groups=qname, data=flqs, type="l", main="Recruitment models", auto.key=list(points=FALSE, lines=TRUE, columns=3))

#####################################
# Advanced stock assessment - a4aSCA
#####################################

#====================================
# models
#====================================
fmodel <- ~ s(age, k=4) + s(year, k = 20)
qmodel <- list( ~ s(age, k=4) + year)
srmodel <- ~s(year, k=20)
fit <- a4aSCA(ple4, ple4.indices[1], fmodel, qmodel, srmodel) 

#====================================
# N1 model
#====================================
n1model <- ~s(age, k=4)
fit1 <- a4aSCA(ple4, ple4.indices[1], fmodel, qmodel, srmodel, n1model) 
flqs <- FLQuants(smo=stock.n(fit1)[,1], fac=stock.n(fit)[,1])
xyplot(data~age, groups=qname, data=flqs, type="l", main="N1 models", auto.key=list(points=FALSE, lines=TRUE, columns=2))

#====================================
# variance model
#====================================
vmodel <- list(~1, ~1)
fit <- a4aSCA(ple4, ple4.indices[1], fmodel, qmodel, srmodel, n1model, vmodel) 

vmodel <- list(~ s(age, k=4), ~1)
fit1 <- a4aSCA(ple4, ple4.indices[1], fmodel, qmodel, srmodel, n1model, vmodel) 
flqs <- FLQuants(cts=catch.n(fit), smo=catch.n(fit1))
xyplot(data~year|age, groups=qname, data=flqs, type="l", main="Variance models", scales=list(y=list(relation="free")), auto.key=list(points=FALSE, lines=TRUE, columns=2))

#====================================
# weighting the likelihood
#====================================
stk <- ple4
idx <- ple4.indices[1]
# variance of observed catches
varslt <- catch.n(stk)
varslt[] <- 1
catch.n(stk) <- FLQuantDistr(catch.n(stk), varslt)
# variance of observed indices
varslt <- index(idx[[1]])
varslt[] <- 0.05
index.var(idx[[1]]) <- varslt
# run
fit1 <- a4aSCA(stk, idx, fmodel, qmodel, srmodel, n1model, vmodel=list(~1, ~1)) 
flqs <- FLQuants(nowgt=stock.n(fit), extwgt=stock.n(fit1))
xyplot(data~year|age, groups=qname, data=flqs, type="l", main="Likelihood weighting", scales=list(y=list(relation="free")), auto.key=list(points=FALSE, lines=TRUE, columns=2))

#====================================
# assessing ADMB files
#====================================
fit1 <- a4aSCA(stk, idx, fmodel, qmodel, srmodel, n1model, vmodel=list(~1, ~1), wkdir="mytest") 

#####################################
# Predict and simulate
#####################################

fmodel <- ~ s(age, k=4) + s(year, k = 20)
qmodel <- list( ~ s(age, k=4) + year)
srmodel <- ~s(year, k=20)
fit <- a4aSCA(ple4, ple4.indices[1], fmodel, qmodel, srmodel) 

# predict
fit.pred <- predict(fit)
lapply(fit.pred, names)

# simulate
fits <- simulate(fit, 25)
flqs <- FLQuants(sim=iterMedians(stock.n(fits)), det=stock.n(fit))
xyplot(data~year|age, groups=qname, data=flqs, type="l", main="Median simulations VS fit", scales=list(y=list(relation="free")))
stks <- ple4 + fits
plot(stks)

#####################################
# Advanced'issimo stock assessment
#####################################
fmodel <- ~ s(age, k=4) + s(year, k = 20)
qmodel <- list( ~ s(age, k=4) + year)
srmodel <- ~s(year, k=20)
fit <- a4aSCA(ple4, ple4.indices[1], fmodel, qmodel, srmodel) 

# WCSAM exercise - replicating itself
idxs <- ple4.indices[1]
index(idxs[[1]]) <- index(fits)[[1]]

fit1 <- a4aSCA(stks, idxs, fmodel, qmodel, srmodel, fit="MP") 
flqs <- FLQuants(sim=iterMedians(stock.n(fits)), det=stock.n(fit), rep=iterMedians(stock.n(fit1)))
xyplot(data~year|age, groups=qname, data=flqs, type="l", main="Median simulations VS fit", scales=list(y=list(relation="free")))

# working with covariates
nao <- read.table("http://www.cdc.noaa.gov/data/correlation/nao.data", skip=1, nrow=62, na.strings="-99.90")
dnms <- list(quant="nao", year=1948:2009, unit="unique", season=1:12, area="unique")
nao <- FLQuant(unlist(nao[,-1]), dimnames=dnms, units="nao")
nao <- seasonMeans(trim(nao, year=dimnames(stock.n(ple4))$year))
nao <- as.numeric(nao)
srmodel <- ~ nao
fit2 <- a4aSCA(ple4, ple4.indices[1], fmodel, qmodel, srmodel) 
flqs <- FLQuants(fac=stock.n(fit)[1], cvar=stock.n(fit2)[1])
xyplot(data~year, groups=qname, data=flqs, type="l", main="Recruitment models")

srmodel <- ~ ricker(a=~nao, CV=0.1)
fit3 <- a4aSCA(ple4, ple4.indices[1], fmodel, qmodel, srmodel) 
flqs <- FLQuants(fac=stock.n(fit)[1], cvar=stock.n(fit3)[1])
xyplot(data~year, groups=qname, data=flqs, type="l", main="Recruitment models")

srmodel <- ~s(year, k=20)
qmodel <- ~ factor(age) + nao
fit4 <- a4aSCA(ple4, ple4.indices[1], fmodel, qmodel, srmodel) 
flqs <- FLQuants(smo=stock.n(fit), cvar=stock.n(fit4))
xyplot(data~year|age, groups=qname, data=flqs, type="l", main="Recruitment models")

# paralell computing
library(parallel)
options(mc.cores=3)
lst <- mclapply(split(1:100, 1:100), function(x){
	stk <- ple4 + fits[,,,,,as.numeric(x)]
	a4aSCA(stk, ple4.indices[1], fmodel, qmodel, srmodel, fit="MP") 
})




