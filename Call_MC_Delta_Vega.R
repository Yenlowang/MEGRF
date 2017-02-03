# MonteCarlo Simulation
# Fri Jan 13 16:57:17 2017
# Valoracion de call europea por MC - devuelve delta y prima

callmc.delta_vega = function(so, strike, r, sig, t, nsims, nobs)
{
  dt = t/nobs
  
  deriva = (r-0.5*sig^2)*dt
  deriva3 = (r-0.5*(1.01*sig)^2)*dt # Pequeño cambio en vola
  
  payoff = numeric(nsims)
  payoff2 = numeric(nsims)
  payoff3 = numeric(nsims)
  
  for (i in 1:nsims)
  {
    e = rnorm(n = nobs) # los aleatorios son los mismos para las dos opciones para el calculo de sensibilidades
                        # con esto reduzco los errores de simulacion
    z = exp(x = deriva + sig*dt^0.5*e) 
    s1 = cumprod(c(so,z))
    s2 = cumprod(c(so*1.01,z))
    
    payoff[i] = max(s1[nobs+1] - strike, 0)
    payoff2[i] = max(s2[nobs+1] - strike, 0)
    
    # Calculo de vega - Es el calculo de una opcion con nuevas evoluciones z3 
    # en las que se incluye la pequeña variacion de la sigma
    z3 = exp(x = deriva3 + 1.01*sig*dt^0.5*e) # Es aqui donde se generan la volatilidad
    s3 = cumprod(c(so,z3))
    payoff3[i] = max(s3[nobs+1] - strike, 0)
  }
  
  prima1 = mean(payoff)*exp(-r*t)
  prima2 = mean(payoff2)*exp(-r*t)
  prima3 = mean(payoff3)*exp(-r*t)
  
  delta = (prima2 - prima1)/(so+1.01-so) # Derivada numerica
  vega = (prima3 - prima1)/(0.01*sig) # Derivada numerica
  
  return(list(c = prima1, d = delta, v = vega))
}

s = callmc.delta_vega(so = 10, strike = 2, r = 0.03, sig = 0.25, t = 1, nsims = 100, nobs = 500)
s
