# MonteCarlo Simulation
# Fri Jan 13 16:57:17 2017
# Valoracion de call europea por MC
# nsims = sendas
call.mc = function(so, strike, r, sig, t, nsims, nobs)
{
  dt = t/nobs
  
  deriva = (r-0.5*sig^2)*dt
  #suma = 0
  suma = numeric(length = nsims)
  
  for (i in 1:nsims)
  {
    e = rnorm(n = nobs) # lo genero para cada simulacion
    z = exp(x = deriva + sig*dt^0.5*e) #Genero las evoluciones
    s = cumprod(c(so,z)) # Genero los caminos aplicando mi evolucion y partiendo de so
    # Esto es un vector (S0, s1,..... St) vector de nobs + 1
    # St = s0*exp(r-sig^2/2)*dt + sig*dt^0.5*e) e Evoluciones de la normal
    # Plotting one evolution of the S - plot(x = seq(0,nobs), y = s, type = "l")
    payoff = max(s[nobs+1]-strike, 0)
    #suma = suma+payoff # en cada una de las sendas obtengo un payoff le calculo la media (pag 6 diapo)
    suma[i] = payoff
    # print(payoff)
  }
  
  # prima = suma/nsims*exp(-r*t)
  prima = sum(suma)/length(x = suma)*exp(-r*t)
}

s = call.mc(so = 10, strike = 2, r = 0.03, sig = 0.25, t = 0.5, nsims = 10000, nobs = 500)
s
BS_EC(T = 0.5, K = 2, r = 0.03, sigma = 0.25, S0 = 10)
# Check the variance of the price
l = 100
s = numeric(l)
for(i in 1:l){
  s[i] = call.mc(so = 10, strike = 2, r = 0.03, sig = 0.25, t = 0.5, 
                 nsims = 10000, nobs = 500)
}
sd(s)
summary(s)
# if changing l, number of times s is calculated - no reduction in variance
# if changing nsims reduction of variance