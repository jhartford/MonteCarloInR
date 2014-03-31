pricePaths <- function(start = 10,r = 0.03,time = 1,sigma = 0.2,nSims = 1000,nSteps = 11){
  #Generate asset price paths using geometric brownian motion
  Dt = time/(nSteps)
  mat = matrix(rnorm(nSims*nSteps),nrow=nSteps,ncol=nSims)
  mat = exp((r - sigma^2/2)*Dt + sigma*sqrt(Dt)*mat)
  for (i in c(1:nSims)){
    mat[,i] = cumprod(mat[,i])
  }
  mat <- mat*10
  mat <- rbind(rep(start, nSims),mat)
  return(mat)
}

#plot price paths
matplot(pricePaths(nSims = 1000), type = "l")
