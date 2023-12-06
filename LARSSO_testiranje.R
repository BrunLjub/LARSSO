rm(list = ls())
set.seed(14)

data(diabetes)


X = diabetes$x
y = diabetes$y

n = nrow(X)
p = ncol(X)


L1 = LARSS(X,y,T,T)
LARSS.plot(L1,T,T)
PlotCorrelations(L1,X,y)
PlotRSS(L1,X,y,10000)

ntrain = ceil(0.8*n)
ntest = nrow(X)-ntrain
indtrain = sort(sample(1:nrow(X),ntrain))
indtest = (1:nrow(X))[-indtrain]

Xtrain = X[indtrain,]
ytrain = y[indtrain]

Xtest = X[indtest,]
ytest = y[indtest]

Ltrain = LARSS(Xtrain,ytrain,F,F)
LARSS.plot(Ltrain,t,T)
trgres = PlotRSS(Ltrain,Xtrain,ytrain,10000)
Larsputanja = LARSS(Xtrain,ytrain,F,F)

lambde = seq(0, max(Larsputanja$Lambda),length.out = 10000)

cvgres = KfoldCVLARS(Xtrain,ytrain,6,lambde)
loocvgres = KfoldCVLARS(Xtrain,ytrain,36,lambde)


Larsrjesenje = LARSSO(Larsputanja,Xtest,ytest,lambde[which.min(cvgres)])


testgres = PlotRSS(Larsputanja,Xtest,ytest,10000)
plot(1, type = "n",xlim = c(0,max(lambde)),ylim = range(cbind(testgres,trgres,cvgres)), xlab = "Lambda", ylab = "RMSE")
lines(lambde,testgres,col = "green",lw = 2)
lines(lambde,trgres,col = "red",lw = 2)
lines(lambde, cvgres,col = "blue",lw = 2)
abline(v = Larsrjesenje$lambda, lw = 3, lty = 2)

legend(100,239,c("trening greška", "greška validacije", "testna greška"), fill = c("red","blue","green"))

