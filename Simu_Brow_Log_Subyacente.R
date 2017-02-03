# MonteCarlo Simulation
# Fri Jan 13 16:31:29 2017
# Simulacion subyacente modelo lognormal

subyacente.mc = function(so, mu, sig, t, nsims, nobs)
{
  dt = t/nobs
  
  e = matrix(data = rnorm(n = nsims*nobs), nrow = nsims)
  
  fila1 = rep(so, nsims)
  
  matriz = exp((mu-0.5*sig^2)*dt + sig*(dt^0.5)* e)
    
  matriz1 = rbind(fila1, matriz)
  
  suby = apply(X = matriz1, MARGIN = 2, FUN = cumprod)
  
}

s = subyacente.mc(so = 10, mu = 0.03, sig = 0.25, t = 1, nsims = 100, nobs = 500)
dim(s)
matplot(s, type = "l")

# La matriz es fxc = nsims x nobs 
# nsims = the number of rows of the matrix, the suby changes at each point
# ncol = the number of paths

