# MonteCarlo Simulation
# Fri Jan 13 16:57:17 2017
# Valoracion de opcion asiatica de media aritmetica con reduccion de varianza
# Aplicando formula exacta de la asiatica geometrica
# If max(St) < B payoff de call

callmc.asiatica_VarControl = function(so, strike, r, sig, t, nsims, nobs)
{
  dt = t/nobs
  
  deriva = (r-0.5*sig^2)*dt
  payoffA = numeric(nsims)
  payoffG = numeric(nsims)
  
  for (i in 1:nsims)
  {
    e = rnorm(n = nobs) # lo genero para cada simulacion
    z = exp(x = deriva + sig*dt^0.5*e) #Genero las evoluciones
    s = cumprod(c(so,z)) # Genero los caminos aplicando mi evolucion y partiendo de so
    # Esto es un vector (S0, s1,..... St) vector de nobs + 1
    
    A = mean(s[2:(nobs+1)])
    
    payoffA[i] = max(A-strike, 0)
    
    # Formula de la geometrica exacta pag 20 docum
    G = exp(mean(log(s[2:(nobs+1)])))
    payoffG[i] = max(G - strike, 0)
  
  }
  
  primaA = mean(payoffA)*exp(-r*t)
  primaG = mean(payoffG)*exp(-r*t)
  
  beta = cov(x = payoffA, y = payoffG)/var(payoffG)
  
  # Utilizando la formula exacta para call geometrica
  # Este codigo esta contenido como una funcion en una carpeta
  # la llamamos utilizando source
  source(file = "C:/Users/a4usrforlea/Desktop/20170113_R_Montecarlo/Desiderio_13_Enero/asiat.geomet.r")
  exacta = asiat.geomet(so, strike, r, t, sig, 0, nobs, "c")
  
  prima = primaA - beta*(primaG-exacta)
  
}

v = callmc.asiatica_VarControl(so = 8000, strike = 8000, r = 0.03, sig = 0.25, t = 1,
                    nsims = 10000, nobs = 12)
v

