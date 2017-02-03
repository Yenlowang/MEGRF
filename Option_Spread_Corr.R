# Ejemplo de uso de correlaciones
# Opcion spread = payoff = max(st1 - st2 - k, 0)

# Solo se va a hacer una senda por cada observacion por lo tanto no hace falta
# dt, ni nobs,...

# ?pmax
# a = c(1, 3)
# b = c(4, 1)
# pmax(a - b, 0)

mc.spread = function(s1, s2, k, t, r, sig1, sig2, rho, nsims)
{
  
  matrizrho = rbind(c(1, rho), c(rho, 1))
  mu = c(0,0)
  
  e = mvrnorm(n = nsims, mu = mu, Sigma = matrizrho)
  # sti = es un vector
  st1 = s1*exp(x = (r-0.5*sig1^2)*t + sig1*t^0.5*e[,1])
  st2 = s2*exp(x = (r-0.5*sig2^2)*t + sig2*t^0.5*e[,2])
  
  payoff = pmax(st1-st2-k, 0) 
  
  prima = mean(payoff)*exp(-r*t)
  
  return(list(prim = prima, payof = payoff))
  
}

val = mc.spread(s1 = 28, s2 = 20, k = 7, t = 0.5,
                r = 0.03, sig1 = 0.15, sig2 = 0.25, rho = 0.6, nsims = 10)
val

