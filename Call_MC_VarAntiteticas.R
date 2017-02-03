# MonteCarlo Simulation
# Fri Jan 13 16:57:17 2017
# Valoracion de call europea por MC Variable antitetica

callmc.va = function(so, strike, r, sig, t, nsims, nobs)
{
  dt = t/nobs
  
  deriva = (r-0.5*sig^2)*dt
  #suma = 0
  payoff = numeric(2*nsims)
  
  for (i in 1:nsims)
  {
    e = rnorm(n = nobs) # lo genero para cada simulacion
    z = exp(x = deriva + sig*dt^0.5*e) #Genero las evoluciones
    s = cumprod(c(so,z)) # Genero los caminos aplicando mi evolucion y partiendo de so
    # Esto es un vector (S0, s1,..... St) vector de nobs + 1
    payoff[i] = max(s[nobs+1]-strike, 0)
    #suma = suma+payoff # en cada una de las sendas obtengo un payoff le calculo la media (pag 6 diapo)
    # print(payoff)
    
    # Variable Antitetica:
    
    z2 = exp(x = deriva + sig*dt^0.5*(-e))
    s2 = cumprod(c(so, z2))
    payoff[i+nsims] = max(s2[nobs+1]-strike, 0)
    
  }
  
  # prima = suma/nsims*exp(-r*t)
  prima = mean(payoff)*exp(-r*t)
}

s = callmc.va(so = 10, strike = 2, r = 0.03, sig = 0.25, t = 1, nsims = 100, nobs = 500)
s

# Check the variance of the price
l = 100
s = numeric(l)
for(i in 1:l){
  s[i] = callmc.va(so = 10, strike = 2, r = 0.03, sig = 0.25, t = 1, 
                 nsims = 10000, nobs = 500)
}
sd(s)
summary(s)
# if changing l, number of times s is calculated - no reduction in variance
# if changing nsims reduction of variance
hist(s)
