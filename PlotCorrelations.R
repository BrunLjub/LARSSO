
PlotCorrelations <- function(L1,X,y,textbool = T){
  #Crta grafove korelacija


  p = ncol(X)
  CORRS = abs(t(X)%*% (cbind(y)%*% rep(1,L1$k)-X%*%L1$Beta))
  plot(1, type = "n", xlim = c(1,L1$k), ylim = 1.05*range(CORRS),xlab="Korak", ylab = "Apsolutna korelacija")
  for (i in 1:p){
    lines(1:(L1$k), CORRS[i,], col = i,lw = 3,lty=i )
  }
  CORRMAX = apply(CORRS,2,max)
  lines(1:(L1$k), CORRMAX,col = "purple",lw = 4,lty=1)
  if (textbool){
    text(1:(L1$k-1), CORRMAX[1:length(CORRMAX)-1],labels = L1$indos)
    text(1, CORRS[,1],labels = 1:p)
  }
}