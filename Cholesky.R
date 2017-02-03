# Cholesky

rho = -0.7
corr = rbind(c(1,rho), c(rho,1)) # Matriz de correlaciones
L = chol(corr)
e = matrix(rnorm(n = 2000), ncol = 2)
x = e %*% L # Multiplicacion matricial L = 2x2 e = 1000x2 
            # Multiplicando matrices necesito 1000x2 * 2x2
            # Resultado final = 1000x2
cor(e)
cor(x)


dim(e)
dim(L)
dim(x)

library(MASS)
# Crea por cholesky valores correlacionados segun la matriz corr definida

mu = c(0,0)
y = mvrnorm(n = 1000, mu = mu, Sigma = corr)
cor(y)