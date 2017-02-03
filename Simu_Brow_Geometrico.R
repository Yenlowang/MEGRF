# MonteCarlo Simulation
# Fri Jan 13 16:15:19 2017
# Simulacion discreta mov brow Geometrico

nsims = 200
nobs = 500
so = 100
r = 0.03
sigma = 0.25
dt = 1/nobs #Numero de elemntos de cada senda

e = matrix(data = rnorm(n = nsims*nobs), ncol = nsims)
fila1 = rep(x = so, nsims)
matriz = 1+r*dt + sigma*dt^0.5*e 

matriz2 = rbind(fila1, matriz)

suby = apply(X = matriz2, MARGIN = 2, FUN = cumprod)
t = seq(0, 1, len = (nobs + 1))

matplot(x = t, y = suby, type = "l")


