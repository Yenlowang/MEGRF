# MonteCarlo Simulation
# Fri Jan 13 16:57:17 2017
# Valoracion de opcion asiatica de media aritmetica
# If max(St) < B payoff de call

callmc.asiatica = function(so, strike, r, sig, t, nsims, nobs)
{
  dt = t/nobs
  
  deriva = (r-0.5*sig^2)*dt
  payoffA = numeric(nsims)
  
  for (i in 1:nsims)
  {
    e = rnorm(n = nobs) # lo genero para cada simulacion
    z = exp(x = deriva + sig*dt^0.5*e) #Genero las evoluciones
    s = cumprod(c(so,z)) # Genero los caminos aplicando mi evolucion y partiendo de so
    # Esto es un vector (S0, s1,..... St) vector de nobs + 1
    
    A = mean(s[2:(nobs+1)])
    
    payoffA[i] = max(A-strike, 0)
    
  }
  
  primaA = mean(payoffA)*exp(-r*t)
  standerror = sd(payoffA)/sqrt(nsims)#ERRORRR DEBERIA SER DE LA PRIMA!!! CAMBIAR
  
  return(list(prim = primaA, standerr = standerror))
}

v = callmc.asiatica(so = 8000, strike = 8000, r = 0.03, sig = 0.25, t = 1,
                    nsims = 10000, nobs = 12)
v

# Asiatica reduce el riesgo... pero tambien beneficios
# Call.mc corrrido antes
call.mc(so = 8000, strike = 8000, r = 0.03, sig = 0.25, t = 1,
                nsims = 10000, nobs = 12)