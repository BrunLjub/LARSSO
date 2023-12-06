LARSS.plot = function(L1, loglambda = FALSE, axisinfo = FALSE){
  #L1... listu koju vraća LARSS
  #loglambda... trebaju li lambde biti u log skali
  #axisinfo... treba li prikazati koji indeksi ulaze u skup ekvikorelacije u čvorovima

  m1 = nrow(L1$Beta) #p
  m2 = ncol(L1$Beta) #k
  if(isFALSE(loglambda)){
    
    plot(1, type = "n", xlim = 1.05*range(L1$Lambda), ylim = 1.05*range(L1$Beta),xlab="Lambda", ylab = "Koeficijenti")
    for (k in 1:m1){
      lines(L1$Lambda,L1$Beta[k,], col = k)
    }
    lines(c(0,max(L1$Lambda)+5),c(0,0))
    points(L1$Lambda[1:length(L1$Lambda)-1],rep(0,length(L1$Lambda)-1))
     if(axisinfo){
       
       xrr = max(L1$Lambda)
       yrr = max(L1$Beta)
       
       text(L1$Lambda[1:length(L1$Lambda)-1] + 0.015*xrr ,rep(0,length(L1$Lambda)-1)+0.05*yrr, L1$indos)
       axis(3, at = L1$Lambda[1:length(L1$Lambda)-1], labels = L1$df )
     }
  }
  else{
    B = L1$Beta
    
    L = L1$Lambda
    
    b1 = B[,m2-1]
    b0 = B[,m2]
    l = L[m2-1]
    
    rr = log(L[1]) - log(L[m2-1])
    
    plot(1, type = "n", xlim = c( log(l) - 0.15*rr , 1.05*max(log(L))), ylim = 1.05*range(B[-m2]),xlab="log(Lambda)", ylab = "Koeficijenti")
    xes = seq(log(l)-0.15*rr, log(l), length.out = 500)

    for (k in 1:m1){
      for (g in 1:(m2-2)){
        logapsc = seq(log(L[g]), log(L[g+1]),length.out = 1000)
        bb1 = B[k,g]
        bb2 = B[k,g+1]
        ord = bb1 + (bb2-bb1)/(L[g+1]-L[g])*(exp(logapsc)-L[g])
        lines(logapsc,ord,col = k)
      }
      yes = b0[k] + exp(xes) * (b1[k]-b0[k])/l
      lines(xes,yes, col = k )
    }
    
    abline(0,0)
    points(log(L[1:m2-1]),rep(0,length(L1$Lambda)-1))
    
    if(axisinfo){
      xrr = log(max(L1$Lambda))
      yrr = max(L1$Beta)
      
      axis(3, at = log(L1$Lambda[1:length(L1$Lambda)-1]), labels = L1$df )
      text(log(L1$Lambda[1:length(L1$Lambda)-1]) + 0.005*xrr ,rep(0,length(L1$Lambda)-1)+0.05*yrr, L1$indos)
    }
      
  }
}