
KfoldCVLARS <- function(Xtrain,ytrain,numfolds, lambde){
  ntrain = nrow(Xtrain)
  sve = 1:ntrain
  permsve = sample(sve)
  podjela = cbind(permsve,mod(sve-1,numfolds)+1)
  
  CVRESULTS = matrix(0,numfolds,length(lambde))
  
  for (i in 1:numfolds){
    indunutra = sort(podjela[podjela[,2] != i,1])
    indvan = sve[-indunutra]
    
    Xunutra = cbind(Xtrain[indunutra,])
    yunutra = ytrain[indunutra]
    
    Xvan = cbind(Xtrain[indvan,])
    yvan = ytrain[indvan]
    L = LARSS(Xunutra,yunutra,F,F)
    #rssovi = numeric(length(lambde))
    for (j in 1:length(lambde)){
      CVRESULTS[i,j] = (LARSSO(L,Xvan,yvan,lambde[j])$RSS)^2
    }
  }
  CVGRES = sqrt(apply(CVRESULTS,2,mean))
  return(CVGRES)
}