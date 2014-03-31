

library(FLa4a)
library(diagram)
data(ple4)
data(ple4.indices)

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
wireframe(data ~ year + age, data = as.data.frame(harvest(stk)), drape = TRUE, main="Fishing mortality")
wireframe(data ~ year + age, data = as.data.frame(stock.n(stk)), drape = TRUE, main="Population")
wireframe(data ~ year + age, data = as.data.frame(catch.n(stk)), drape = TRUE, main="Catches")

#####################################
# SUBmodels
#####################################

# fishing mortality

qmodel <- list(~ factor(age)) 
fmodel <- ~ factor(age) + factor(year)
fit <- sca(fmodel, qmodel, stock = ple4, indices = ple4.indices[1])
wireframe(data ~ year + age, data = as.data.frame(harvest(fit)), drape = TRUE)

fmodel <- ~ s(age, k=4) + s(year, k = 20)
fit1 <- sca(fmodel, qmodel, stock = ple4, indices = ple4.indices[1]) 
wireframe(data ~ year + age, data = as.data.frame(harvest(fit1)), drape = TRUE)

fmodel <- ~ s(age, k=4) + s(year, k = 20) + te(age, year, k = c(3,3))
fit2 <- sca(fmodel, qmodel, stock = ple4, indices = ple4.indices[1]) 
wireframe(data ~ year + age, data = as.data.frame(harvest(fit2)), drape = TRUE)

fmodel <- ~ te(age, year, k = c(4,20))
fit3 <- sca(fmodel, qmodel, stock = ple4, indices = ple4.indices[1]) 
wireframe(data ~ year + age, data = as.data.frame(harvest(fit3)), drape = TRUE)

fmodel <- ~ te(age, year, k = c(4,20)) + s(year, k = 5, by = as.numeric(age==1))
fit4 <- sca(fmodel, qmodel, stock = ple4, indices = ple4.indices[1]) 
wireframe(data ~ year + age, data = as.data.frame(harvest(fit4)), drape = TRUE)

# q

fmodel <- ~ factor(age) + factor(year)
qmodel <- list(~ factor(age)) 
fit <- sca(fmodel, qmodel, stock = ple4, indices = ple4.indices[1])
lst <- dimnames(fit@index[[1]])
lst$x <- stock.n(fit)
stkn <- do.call("trim", lst)
wireframe(data ~ year + age, data = as.data.frame(index(fit)[[1]]/stkn), drape = TRUE)

qmodel <- ~ s(age, k=4) + s(year, k = 20)
fit1 <- sca(fmodel, qmodel, stock = ple4, indices = ple4.indices[1]) 
lst <- dimnames(fit1@index[[1]])
lst$x <- stock.n(fit1)
stkn <- do.call("trim", lst)
wireframe(data ~ year + age, data = as.data.frame(index(fit1)[[1]]/stkn), drape = TRUE)

qmodel <- ~ te(age, year, k = c(3,20))
fit2 <- sca(fmodel, qmodel, stock = ple4, indices = ple4.indices[1]) 
lst <- dimnames(fit2@index[[1]])
lst$x <- stock.n(fit2)
stkn <- do.call("trim", lst)
wireframe(data ~ year + age, data = as.data.frame(index(fit2)[[1]]/stkn), drape = TRUE)

qmodel <- list( ~ factor(age) + year)
fit3 <- sca(fmodel, qmodel, stock = ple4, indices = ple4.indices[1]) 
lst <- dimnames(fit3@index[[1]])
lst$x <- stock.n(fit3)
stkn <- do.call("trim", lst)
wireframe(data ~ year + age, data = as.data.frame(index(fit3)[[1]]/stkn), drape = TRUE)




fit. <- a4aSCA(fmodel, qmodel, stock = ple4, indices = ple4.indices[1], wkdir="mydir") 



# data
stk <- ple4
idx <- ple4.indices[1]
# models
fmodel <- ~ s(age, k=4) + s(year, k = 20)
qmodel <- list( ~ s(age, k=4))
# variance of observed catches
varslt <- catch.n(stk)
varslt[] <- 1
catch.n(stk) <- FLQuantDistr(catch.n(stk), varslt)
# variance of observed indices
varslt <- index(idx[[1]])
varslt[] <- 0.1
index(idx[[1]]) <- FLQuantDistr(index(idx[[1]]), varslt)
# run
fit0 <- sca(fmodel, qmodel, stock = ple4, indices = ple4.indices[1]) 
fit. <- sca(fmodel, qmodel, stock = stk, indices = idx) 



# data
stk <- ple4
idx <- ple4.indices[1]
# models
fmodel <- ~ s(age, k=4) + s(year, k = 20)
qmodel <- list( ~ s(age, k=4))
vmodel <- list(~1, ~1)
# run
fit0 <- a4aSCA(fmodel, qmodel, stock = ple4, indices = ple4.indices[1]) 
fit00 <- a4aSCA(fmodel, qmodel, vmodel=vmodel, stock = ple4, indices = ple4.indices[1]) 
all.equal(fit0, fit00)
vmodel <- list(~(age)^2-1, ~1)
fit. <- a4aSCA(fmodel, qmodel, stock = stk, indices = idx) 


