
LARSSO <- function(L1,X,y, lambda){
  #Vraća lasso rješenje za fiksne X,y i lambda
  #L1... lista koju vraća LARSSO
  
  if(lambda > max(L1$Lambda))lambda = max(L1$Lambda)
  
  L = find_range(L1$Lambda,lambda)
  
  if(L[1] == L[2]){
    Beta = L1$Beta[,L[1]]
    Const = L1$Const[L[1]]
  }
  else{
    D = (L1$Beta[,L[1]] - L1$Beta[,L[2]])/(L1$Lambda[L[1]] - L1$Lambda[L[2]])
    C = L1$Beta[,L[1]] - L1$Lambda[L[1]]*D
    Beta = C + lambda*D
    Const = L1$mu-t(Beta)%*%L1$meanx
  }
  Residual = drop(y) - drop(X%*%Beta) - as.numeric(Const)*rep(1,nrow(X))
  RSS = sqrt(mean(Residual^2))
  
  return( list(Beta = drop(Beta), Const = drop(Const), Residual = drop(Residual), RSS = RSS, lambda = lambda) )
}