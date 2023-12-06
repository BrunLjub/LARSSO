
PlotRSS <- function(L1,X,y, len){
  lambde = seq(0,L1$Lambda[1],length.out = len)
  rssovi = numeric(len)
  for (i in 1:len){
    rssovi[i] = LARSSO(L1,X,y,lambde[i])$RSS
  }
  
  plot(lambde,rssovi,type = "l", xlab = "Lambda",ylab = "RMSE",col = "red")
  return(rssovi)
}