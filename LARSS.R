LARSS <- function(X, y, intercept = T, normalise = T){
  #Vraća čvorove neprekidnog i po dijelovima afinog LARS lasso rješenja
  #intercept = T ako artimetičke sredine od X i y nisu 0
  #normalise = T ako norme stupaca matrice nisu jednake 1

  #Beta... niz y koordinata čvorova
  #Lambda... niz x koordinata čvorova
  #Const... ako intercept = T, onda prilagođava i intercept
  #k... broj čvorova
  #EQSET... skupovi ekvikorelacije po čvorovima
  #df... stupnjevi slobode po čvorovima
  #indos... lista vektora - svaki vektor je skup indeksa koji su u trenutnom skupu ekvikorelacije
  #meanx... aritmetička sredina matrice X
  #mu... aritmetička sredina vektora y 


  n = nrow(X)
  p = ncol(X)
  epss = 1e-8 #efektivna nula
  one = rep(1,n)
  
  #Centriraj
  if(intercept){
    meanx = drop(one %*% X)/n 
    X = scale(X,meanx,FALSE)
    mu = mean(y)
    y = drop(y-mu)
  }
  else{
    meanx = rep(0,p)
    mu = 0
    y = drop(y)
  }
  
  if(normalise){
    normx = sqrt(drop(one %*% X^2))
    X = scale(X,FALSE,normx)
  }
  else{
    normx = rep(1,p)
  }
  
  
  skalprod = t(X)%*%y
  E = which.max(abs(skalprod))
  EE = list(E)
  FF = 1
  
  indos = paste0(E)
  
  
  Lambda = abs(skalprod[E])
  LAMBDA = Lambda
  s = sign(skalprod[E])
  
  NE = seq(p)[-E]
  
  BETA = c()
  k = 1
  
  repeat{
    
    XE = cbind(X[,E])
    
    
    XXE = XE
    XXE[,s == -1] = -XXE[,s==-1]
    G = t(XXE)%*%XXE
    Ginv = pinv(G)
    w = Ginv%*%rep(1,length(E))
    w = w/sqrt(sum(w))
    u = XXE %*% w
    
    a = t(X)%*%u
    
    
    XEinv = pinv(cbind(XE))
    
    C = XEinv%*%y;
    D = XEinv%*%(t(XEinv)%*%s)
    
    
    
    BETA = cbind(BETA, rep(0,p))
    
    btemp = C-D*Lambda
    btemp[abs(btemp) < epss] = 0
    BETA[E,k] = btemp
    
    XNE = cbind(X[,NE])
    m1 = length(E)
    m2 = p-m1
    
    if(length(NE) < 1)
      LambdaJoin = 0
    else{
      brojnik = t(XNE)%*%(XE%*%C - y)
      nazivnik0 = t(XNE)%*%(XE%*%D)
      
      tjoin1 = brojnik/(nazivnik0+1)
      tjoin2 = brojnik/(nazivnik0-1)
      
      tjoin1[tjoin1 < epss | tjoin1 >= Lambda - epss] = 0
      tjoin2[tjoin2 < epss | tjoin2 >= Lambda - epss] = 0
      
      tjoin = pmax(tjoin1,tjoin2)
      LambdaJoin = max(tjoin)
      IndJoin = which.max(tjoin)
      SignJoin = sign( t(XNE[,IndJoin]) %*% (y - XE%*%(C - D*LambdaJoin)))
      
    }
    
    tcross = C/D
    tcross[tcross < epss | tcross > Lambda-epss] = 0
    LambdaCross = max(tcross)
    IndCross = which.max(tcross)
    
    Lambda = max(LambdaJoin,LambdaCross)
    
    if (Lambda < epss){
      LAMBDA = c(LAMBDA,0)
      BETA = cbind(BETA,rep(0,p))
      C[abs(C) < 1e-8] = 0
      BETA[E,k+1] = C
      break
    }
    LAMBDA = c(LAMBDA,Lambda)
    
    if (LambdaJoin > LambdaCross){
      E = c(E,NE[IndJoin])
      temp = order(E)
      E = E[temp]
      s = c(s,SignJoin)
      s = s[temp]
      
      XE = X[,E]
      XEinv = pinv(X[,E])
      dtemp = XEinv%*%(t(XEinv)%*%s)
      ctemp = XEinv %*% y
      
      
      indos = c(indos, paste0(NE[IndJoin]))
      NE = setdiff(NE,NE[IndJoin])
    }
    if(LambdaCross > Lambda - 1e-8){
      NE = sort(c(NE,E[IndCross]))
      s = s[-IndCross]
      indos = c(indos, paste0("-",(E[IndCross])))
      E = setdiff(E,E[IndCross])
    }
    k = k+1
    EE[[k]] = E
    FF = c(FF, length(E))
  }
  
  BETA = t(scale(t(BETA),FALSE,normx))
  Const = mu - meanx%*%BETA
  
  k = ncol(BETA)
  
  return( list(Beta = BETA, Lambda = LAMBDA, Const = Const, k = k , EQSET = EE, df = FF, indos = indos , meanx = meanx, mu = mu))
}