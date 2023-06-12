#Cali Wilson
##SI model tracking infection in bold (b) and shy (h) individuals
##This script contains the SI function used in the model

#Write model function
si_3 <- function(time, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    dS_b <- gamma*I_b - theta_b * S_b * (c_b*I_b + ((0.5*(c_h+c_b))*I_h))
    dS_h <- gamma*I_h - theta_h * S_h * (c_h*I_h + ((0.5*(c_h+c_b))*I_b))
    dI_b <- theta_b * S_b * (c_b*I_b + ((0.5*(c_h+c_b))*I_h))-gamma*I_b
    dI_h <- theta_h * S_h * (c_h*I_h + ((0.5*(c_h+c_b))*I_b))-gamma*I_h
    
    return(list(c(dS_b, dS_h, dI_b, dI_h)))
  })
}