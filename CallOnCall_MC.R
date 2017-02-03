############################
# Call On call
# Code for calculating the price of a compound option
# Antonio Ramos Muñoz - Torrero
############################

CallOnCall.mc = function(so, strike, r, sig, t, nsims, nobs, t1, strike1)
{
    dt = t/nobs
    
    deriva = (r-0.5*sig^2)*dt

    suma = numeric(length = nsims)
    
    prima1 = as.numeric(x = nsims)
    prima2 = as.numeric(x = nsims)
    for(j in 1:nsims)
    {
        for (i in 1:nsims)
        {
            e = rnorm(n = nobs) 
            z = exp(x = deriva + sig*dt^0.5*e) 
            s = cumprod(c(so,z)) 
            payoff = max(s[nobs+1]-strike, 0)
            
            suma[i] = payoff
            
        }

        
        prima1[j] = sum(suma)/length(x = suma)*exp(-r*t-t1)
        
        prima2[j] = max((prima1[j] - strike1), 0)
        
        
    }
    
    prima1_pago_0 = mean(prima1)*exp(x = -r*(t))
    prima1_pago_t1 = mean(prima1)
    desv1 = sd(prima1)
    
    # prima2 = pmax(prima2, 0)
    prima2_pago = mean(x = prima2)*exp(-r*t)
    desv2 = sd(prima2)
    
    return(list(prima2_pago = prima2_pago, desv2 = desv2,
                prima1_pago_0 = prima1_pago_0, prima1_pago_t1 = prima1_pago_t1, desv1 = desv1))

}

s = CallOnCall.mc(so = 10, strike = 2, r = 0.03, sig = 0.25, t = 1, t1 = 0.5, strike1 = 6.9, 
                  nsims = 50, nobs = 500)
s

s = CallOnCall.mc(so = 10, strike = 2, r = 0.03, sig = 0.25, t = 1, t1 = 0.5, strike1 = 2.9, 
                  nsims = 500, nobs = 500)
s