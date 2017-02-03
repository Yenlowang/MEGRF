# Ejemplo de uso de correlaciones error estandar
# Opcion spread = payoff = max(st1 - st2 - k, 0)
# Incluye ERROR ESTANDAR

# Solo se va a hacer una senda por cada observacion por lo tanto no hace falta
# dt, ni nobs,...

mc.spread_ErrorStandard = function(s1, s2, k, t, r, sig1, sig2, rho, nsims)
{
  
  require(MASS)
  matrizrho = rbind(c(1, rho), c(rho, 1))
  mu = c(0,0)
  
  prima = numeric(length = 100)
  
  for(i in 1:100) # Este valor se multiplica por el nsims
  {
    
    e = mvrnorm(n = nsims, mu = mu, Sigma = matrizrho)
    # sti = es un vector
    st1 = s1*exp(x = (r-0.5*sig1^2)*t + sig1*t^0.5*e[,1])
    st2 = s2*exp(x = (r-0.5*sig2^2)*t + sig2*t^0.5*e[,2])
    
    payoff = pmax(st1-st2-k, 0) 
    
    prima[i] = mean(payoff)*exp(-r*t)
  }
  
  val = mean(prima)
  errorstandard = sd(prima)/sqrt(x = nsims)
  
  return(list(prim = val, errorstand = errorstandard))
  
}

val = mc.spread_ErrorStandard(s1 = 28, s2 = 20, k = 7, t = 0.5,
                r = 0.03, sig1 = 0.15, sig2 = 0.25, rho = 0.6, nsims = 1000)
val

# incluir VARIABLE ANTITETICA