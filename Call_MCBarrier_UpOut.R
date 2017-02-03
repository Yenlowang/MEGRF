# MonteCarlo Simulation
# Fri Jan 13 16:57:17 2017
# Valoracion de call europea con barrera up an out por MC # Strike tiene que estar por debajo de barrera
# If max(St) < B payoff de call

callmc.barrier = function(so, strike, r, sig, t, nsims, nobs, barrier)
{
  dt = t/nobs
  
  deriva = (r-0.5*sig^2)*dt
  payoff = numeric(nsims)
  
  for (i in 1:nsims)
  {
    e = rnorm(n = nobs) # lo genero para cada simulacion
    z = exp(x = deriva + sig*dt^0.5*e) #Genero las evoluciones
    s = cumprod(c(so,z)) # Genero los caminos aplicando mi evolucion y partiendo de so
    # Esto es un vector (S0, s1,..... St) vector de nobs + 1
    print(max(s))
    if(max(s) < barrier){
      payoff[i] = max(s[nobs+1]-strike, 0)
    } 
    else{
      payoff[i] = 0
    }
  }
  
  prima = mean(payoff)*exp(-r*t)
  return(list(prim = prima, payof = payoff))
}

s = callmc.barrier(so = 3, strike = 2, r = 0.03, sig = 0.25, t = 1, 
                 nsims = 100, nobs = 500, barrier = 3.5)
s
