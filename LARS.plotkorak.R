LARSS.plotkorak <- function(L1){
  #L1... lista koju vraća LARSS
  m1 = nrow(L1$Beta) #p
  m2 = ncol(L1$Beta) #k
    plot(1, type = "n", xlim = c(1,L1$k), ylim = 1.05*range(L1$Beta),xlab="Korak", ylab = "Koeficijenti")
    for (k in 1:m1){
      lines(1:L1$k,L1$Beta[k,], col = k)
    }
    abline(0,0)
    xrr = L1$k
    yrr = max(L1$Beta)
      
    text(1:(L1$k-1) + 0.015 ,rep(0,L1$k-1)+0.05*yrr, L1$indos)
    axis(3, at = 1:(L1$k-1), labels = L1$df )
}