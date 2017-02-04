##################
# Vasicek
#       dr = a(b-r(t))dt + vdW(t)
#       Inc(i) = a(b-r(t))Inc(t) + vInc(W)      Inc(W) = sqrt(Inc(t))*rnorm(0,1)
#       r[i+1] = r[i] + Inc(i)
##################

# Bono
c=0.03;tau=1;Tvenc=10;Tcanc=5;

# modelo de Vasicek  dr(t) = a(b-r(t))dt + vdW(t)
r0=0.035;a=1;b=0.04;v=0.02;

# MonteCarlo Parameter
nobs = 300; nsims = 1000


ShortRateVasicek = function(a, b, r0, v, t, nobs, nsims)
{
  
  dt = t/nobs
  
  r = matrix(data = 0, nrow = nobs+1, ncol = nsims)
  r[1,] = r0
  
  for(i in 1:nsims)
  {
    for(j in 1:nobs+1)
    {
      dr = a*(b-r[j-1,i])*dt + v*sqrt(x = dt)*rnorm(n = 1, mean = 0, sd = 1)
      r[j,i] = r[j-1,i] + dr
    }
  }
  
  return(r)
}

##################################################
# Para hacerlo con un solo bucle
# for(i in 2:(m+1)){
#   dr = a*(b-r[i-1,])*dt + v*sqrt(x = dt)*rnorm(n = n, mean = 0, sd = 1)
#   r[i,] = r[i-1,] + dr
# }
##################################################

r = ShortRateVasicek(a = a, b = b, r0 = r0, v = v, t = Tcanc, nobs = nobs, nsims = nsims)

dt = Tcanc/nobs
seqT = seq(from = 0, to = Tcanc, by = dt)

matplot(x = seqT, y = r, type = "l", lty = 1, main = "Short Rate Vasicek Path")

# Para crear el valor de los P(Tc, T)
precioBonoCCVasicek<- function(r, a, b, v, T){
  B <- (1-exp(-T*a))/a
  A <- (T-B)*(b-v^2/(2*a^2))+B^2*v^2/(4*a)
  return(exp(-A-B*r))
}

# P(Tcanc,Tk) = precioBonoCCVasicek( r(Tcanc), a, b, v, Tk-Tcanc )


MatrizDto = matrix(data = 0, nrow = nsims, ncol = (Tvenc-Tcanc)) #nrow = nsims, ncol = Tc-Tvto

# precioBonoCCVasicek(r[(nobs+1),2], a, b, v, 2)
rtcanc = r[(nobs+1),]

# Tcanc = 5
# Tvto = 10

# Creation of P(Tc, Tk) matrix
for(i in 1:length(x = rtcanc))
{
  
  for(j in 1:(Tvenc - Tcanc))
  {
    
    MatrizDto[i,j] = precioBonoCCVasicek(r = rtcanc[i], a = a, b = b, v = v, T = j)
    
  }
  
}

# Creation of coupon payments

CPay = MatrizDto*c*tau

# Nominal payment
N = 1

NomPay = N*MatrizDto[,(Tvenc-Tcanc)]

FinalPay = cbind(CPay, NomPay)

RemTc = rowSums(x = FinalPay) # SeRIA EL REM
# A-  CALCULAR MINIMO THE REM

Pago = pmin(RemTc, 1)
# B- CALCULAR PRECIO DE B SUM DE TIPOS DE INTERES MULTIPLICADOS POR DT
# Este BONO TIENE QUE SER UTILIZANDO EXP(COLSUM)
Bono = exp(x = -colSums(r[2:(nobs+1),]*dt))
mean(Bono)
# mean(Bono) deberia ser igual a VasicekZeroCuponPrecio()
VasicekZeroCuponPrecio(r0 = r0, a = a, b = b, sig = v, T = (Tcanc - Tvenc))
# MEAN(A/B)

mean(Pago/Bono) # Da ya el valor descontado al utilizar el Bono como numerario


# CUPONES UTILIZO VASICEKCC PRECIO DEL BONO P(0,1)*C*TAU + P(0,2)*C*TAU... + P(0,5)*C*TAU
# P(0,1) SE CALCULA UTILIZANDO LA FORMULA DADA

Dto0_5 = numeric(length = (Tcanc-0))

for (i in 1:length(x = Dto0_5))
{
  
  Dto0_5[i] = precioBonoCCVasicek(r = r0, a = a, b = b, v = v, T = (i))
  
}

Cupon0_5 = Dto0_5*c*tau
sum(Cupon0_5) + mean(Pago/Bono)
