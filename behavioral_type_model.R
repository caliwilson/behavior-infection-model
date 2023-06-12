###Cali Wilson
##SI model tracking infection in bold (b) and shy (h) individuals
##This script varies contact rates and theta for 4 supplemental feeding scenarios:
## 1. N=50, 50% BOLD, 50% SHY
## 2. N=100, 50% BOLD, 50% SHY
## 3. N=50, 75% BOLD, 25% SHY
## 4. N=100, 75% BOLD, 25% SHY

##Outcomes of interest= prevalence and relative # infected 

   
#Load Packages:
library(deSolve)
library(tidyverse)
#library(beepr) #play noise when code finishes
library(viridis) #for colors
library(gridExtra) #for multipanel plot

#Pull SI function from other script 
source("SI_function.R")  

##--VARY CONTACT AND THETA----------
###Vary contacts and theta (prob of infection given contact)

#Set baseline values for all models:

# Set run duration 
times <- seq(0, 60, by = 1) # run for 60 days

# Define parameter values to iterate over
c_ratio <- seq(1, 2, 0.05)
theta_ratio <- seq(0, 2, 0.05)

#Set most parameters
parameters <- list(
  c_h = 0.1,
  theta_h = 0.05,
  gamma = 0.2, #approx. 5 days 
  f = 0.0
)

##FOUR PROVISIONING SCENARIOS: 
###--N=50, 50% BOLD, 50% SHY-----
# Set initial values
init <- c(S_b = 24, S_h = 25, I_b = 1, I_h = 0)

# Create an empty data frame to store results
test_c <- data.frame(c_b = numeric(),
                     c_h = numeric(),
                     theta_b = numeric(),
                     theta_h = numeric(),
                     c_ratio = numeric(),
                     theta_ratio = numeric(),
                     S_b = numeric(),
                     S_h = numeric(),
                     I_b = numeric(),
                     I_h = numeric(),
                     sum_I_b = numeric(),
                     sum_I_h = numeric())

# Loop over all combinations of c_b and c_h values
for (cR in c_ratio) {
  for (thetaR in theta_ratio) {
    #calculate c_b and theta_b values:
    c_b <- cR * parameters$c_h
    theta_b <- thetaR * parameters$theta_h
    # Set current c_b and theta_b values
    parameters$c_b <- c_b
    parameters$theta_b <- theta_b
    
    # Solve using ode
    out <- ode(y = init, times = times, func = si_3, parms = parameters)
    
    # Extract final values of S and I
    final_values <- out[nrow(out), c("S_b", "S_h", "I_b", "I_h")]
    
    # Calculate cumulative sum of I_b and I_h
    sum_I_b <- cumsum(out[, "I_b"])
    sum_I_h <- cumsum(out[, "I_h"])
    
    # Store results in data frame
    test_c <- rbind(test_c, data.frame(c_b = c_b,
                                       c_h = parameters$c_h,
                                       theta_b = theta_b,
                                       theta_h = parameters$theta_h,
                                       c_ratio = cR,
                                       theta_ratio = thetaR,
                                       S_b = final_values[1],
                                       S_h = final_values[2],
                                       I_b = final_values[3],
                                       I_h = final_values[4],
                                       sum_I_b = sum_I_b[length(sum_I_b)],
                                       sum_I_h = sum_I_h[length(sum_I_h)]))
  }
}

#Outcomes of interest: prevalence and relative infection

test_c$prev<-(test_c$I_b+test_c$I_h)/(test_c$S_b+test_c$S_h+test_c$I_b+test_c$I_h)
test_c$relative_infected<-test_c$I_b/test_c$I_h

out_N50_25_25<-test_c
out_N50_25_25$scenario<-"N=50, 50% bold, 50% shy"


###--N=100, 50% BOLD, 50% SHY-----
# Set initial values
init <- c(S_b = 49, S_h = 50, I_b = 1, I_h = 0)

# Create an empty data frame to store results
test_c <- data.frame(c_b = numeric(),
                     c_h = numeric(),
                     theta_b = numeric(),
                     theta_h = numeric(),
                     c_ratio = numeric(),
                     theta_ratio = numeric(),
                     S_b = numeric(),
                     S_h = numeric(),
                     I_b = numeric(),
                     I_h = numeric(),
                     sum_I_b = numeric(),
                     sum_I_h = numeric())

# Loop over all combinations of c_b and c_h values
for (cR in c_ratio) {
  for (thetaR in theta_ratio) {
    #calculate c_b and theta_b values:
    c_b <- cR * parameters$c_h
    theta_b <- thetaR * parameters$theta_h
    # Set current c_b and theta_b values
    parameters$c_b <- c_b
    parameters$theta_b <- theta_b
    
    # Solve using ode
    out <- ode(y = init, times = times, func = si_3, parms = parameters)
    
    # Extract final values of S and I
    final_values <- out[nrow(out), c("S_b", "S_h", "I_b", "I_h")]
    
    # Calculate cumulative sum of I_b and I_h
    sum_I_b <- cumsum(out[, "I_b"])
    sum_I_h <- cumsum(out[, "I_h"])
    
    # Store results in data frame
    test_c <- rbind(test_c, data.frame(c_b = c_b,
                                       c_h = parameters$c_h,
                                       theta_b = theta_b,
                                       theta_h = parameters$theta_h,
                                       c_ratio = cR,
                                       theta_ratio = thetaR,
                                       S_b = final_values[1],
                                       S_h = final_values[2],
                                       I_b = final_values[3],
                                       I_h = final_values[4],
                                       sum_I_b = sum_I_b[length(sum_I_b)],
                                       sum_I_h = sum_I_h[length(sum_I_h)]))
  }
}


#Outcomes of interest: prevalence and relative infection

test_c$prev<-(test_c$I_b+test_c$I_h)/(test_c$S_b+test_c$S_h+test_c$I_b+test_c$I_h)
test_c$relative_infected<-test_c$I_b/test_c$I_h

out_N100_50_50<-test_c
out_N100_50_50$scenario<-"N=100, 50% bold, 50% shy"


###--N=100, 75% BOLD, 25% SHY-----
# Set initial values
init <- c(S_b = 74, S_h = 25, I_b = 1, I_h = 0)

# Create an empty data frame to store results
test_c <- data.frame(c_b = numeric(),
                     c_h = numeric(),
                     theta_b = numeric(),
                     theta_h = numeric(),
                     c_ratio = numeric(),
                     theta_ratio = numeric(),
                     S_b = numeric(),
                     S_h = numeric(),
                     I_b = numeric(),
                     I_h = numeric(),
                     sum_I_b = numeric(),
                     sum_I_h = numeric())

# Loop over all combinations of c_b and c_h values
for (cR in c_ratio) {
  for (thetaR in theta_ratio) {
    #calculate c_b and theta_b values:
    c_b <- cR * parameters$c_h
    theta_b <- thetaR * parameters$theta_h
    # Set current c_b and theta_b values
    parameters$c_b <- c_b
    parameters$theta_b <- theta_b
    
    # Solve using ode
    out <- ode(y = init, times = times, func = si_3, parms = parameters)
    
    # Extract final values of S and I
    final_values <- out[nrow(out), c("S_b", "S_h", "I_b", "I_h")]
    
    # Calculate cumulative sum of I_b and I_h
    sum_I_b <- cumsum(out[, "I_b"])
    sum_I_h <- cumsum(out[, "I_h"])
    
    # Store results in data frame
    test_c <- rbind(test_c, data.frame(c_b = c_b,
                                       c_h = parameters$c_h,
                                       theta_b = theta_b,
                                       theta_h = parameters$theta_h,
                                       c_ratio = cR,
                                       theta_ratio = thetaR,
                                       S_b = final_values[1],
                                       S_h = final_values[2],
                                       I_b = final_values[3],
                                       I_h = final_values[4],
                                       sum_I_b = sum_I_b[length(sum_I_b)],
                                       sum_I_h = sum_I_h[length(sum_I_h)]))
  }
}

#Outcomes of interest: prevalence and relative infection

test_c$prev<-(test_c$I_b+test_c$I_h)/(test_c$S_b+test_c$S_h+test_c$I_b+test_c$I_h)
test_c$relative_infected<-test_c$I_b/test_c$I_h

out_N100_75_25<-test_c
out_N100_75_25$scenario<-"N=100, 75% bold, 25% shy"

###--N=50, 75% BOLD, 25% SHY-----
# Set initial values
init <- c(S_b = 36.5, S_h = 12.5, I_b = 1, I_h = 0)

# Create an empty data frame to store results
test_c <- data.frame(c_b = numeric(),
                     c_h = numeric(),
                     theta_b = numeric(),
                     theta_h = numeric(),
                     c_ratio = numeric(),
                     theta_ratio = numeric(),
                     S_b = numeric(),
                     S_h = numeric(),
                     I_b = numeric(),
                     I_h = numeric(),
                     sum_I_b = numeric(),
                     sum_I_h = numeric())

# Loop over all combinations of c_b and c_h values
for (cR in c_ratio) {
  for (thetaR in theta_ratio) {
    #calculate c_b and theta_b values:
    c_b <- cR * parameters$c_h
    theta_b <- thetaR * parameters$theta_h
    # Set current c_b and theta_b values
    parameters$c_b <- c_b
    parameters$theta_b <- theta_b
    
    # Solve using ode
    out <- ode(y = init, times = times, func = si_3, parms = parameters)
    
    # Extract final values of S and I
    final_values <- out[nrow(out), c("S_b", "S_h", "I_b", "I_h")]
    
    # Calculate cumulative sum of I_b and I_h
    sum_I_b <- cumsum(out[, "I_b"])
    sum_I_h <- cumsum(out[, "I_h"])
    
    # Store results in data frame
    test_c <- rbind(test_c, data.frame(c_b = c_b,
                                       c_h = parameters$c_h,
                                       theta_b = theta_b,
                                       theta_h = parameters$theta_h,
                                       c_ratio = cR,
                                       theta_ratio = thetaR,
                                       S_b = final_values[1],
                                       S_h = final_values[2],
                                       I_b = final_values[3],
                                       I_h = final_values[4],
                                       sum_I_b = sum_I_b[length(sum_I_b)],
                                       sum_I_h = sum_I_h[length(sum_I_h)]))
  }
}

#Outcomes of interest: prevalence and relative infection

test_c$prev<-(test_c$I_b+test_c$I_h)/(test_c$S_b+test_c$S_h+test_c$I_b+test_c$I_h)
test_c$relative_infected<-test_c$I_b/test_c$I_h


out_N50_75_25<-test_c
out_N50_75_25$scenario<-"N=50, 75% bold, 25% shy"

df_all<- rbind(out_N50_25_25,out_N50_75_25,out_N100_50_50,out_N100_75_25) #combine all dataframes

df_all$sum_I_total<- df_all$sum_I_b+df_all$sum_I_h
df_all$sum_relative_infected<- df_all$sum_I_b/df_all$sum_I_h




###NULL MODEL SCENARIOS-----

# A) Bold and shy have equal contact rates, N= 50----

# Set run duration 
times <- seq(0, 60, by = 1) # run for 60 days

#Set parameters
parameters <- list(
  c_b=0.1,
  c_h = 0.1,
  theta_b = 0.05,
  theta_h = 0.05,
  gamma = 0.2, #approx. 5 days 
  f = 0.0
)
#rough r0 calculation:
(parameters$theta_h*parameters$c_h*50)/parameters$gamma

# Set initial values
init <- c(S_b = 24, S_h = 25, I_b = 1, I_h = 0)

# Solve using ode
out<- ode(y = init, times = times, func = si_3, parms = parameters)
null<-as.data.frame(out)
null$I_total<- null$I_b+null$I_h
null$sum_I_b<-cumsum(null$I_b)
null$sum_I_h<-cumsum(null$I_h)
null$sum_I_total<-null$sum_I_b+null$sum_I_h
null$prev<-(null$I_b+null$I_h)/(null$S_b+null$S_h+null$I_b+null$I_h)
null$sum_relative_infected<-null$sum_I_b/null$sum_I_h

null_a<-null #create new DF to store these results
null_a$scenario<-"null_a"

# B) Bold has higher susceptibility (2x theta) compared to shy but same contact, N=50----

# Set run duration 
times <- seq(0, 60, by = 1) # run for 60 days

#Set parameters
parameters <- list(
  c_b=0.1,
  c_h = 0.1,
  theta_b = 0.1,
  theta_h = 0.05,
  gamma = 0.2, #approx. 5 days 
  f = 0.0
)
#rough r0 calculation:
(parameters$theta_b*parameters$c_b*50)/parameters$gamma #R0 if all bold
(parameters$theta_h*parameters$c_h*50)/parameters$gamma #R0 if all shy

# Set initial values
init <- c(S_b = 24, S_h = 25, I_b = 1, I_h = 0)

# Solve using ode
out<- ode(y = init, times = times, func = si_3, parms = parameters)
null<-as.data.frame(out)
null$I_total<- null$I_b+null$I_h
null$sum_I_b<-cumsum(null$I_b)
null$sum_I_h<-cumsum(null$I_h)
null$sum_I_total<-null$sum_I_b+null$sum_I_h
null$prev<-(null$I_b+null$I_h)/(null$S_b+null$S_h+null$I_b+null$I_h)
null$sum_relative_infected<-null$sum_I_b/null$sum_I_h


null_b<-null #create new DF to store these results
null_b$scenario<-"null_b"



# C) Shy has higher susceptibility (2x theta) compared to bold but same contact, N=50----

# Set run duration 
times <- seq(0, 60, by = 1) # run for 60 days

#Set parameters
parameters <- list(
  c_b=0.1,
  c_h = 0.1,
  theta_b = 0.05,
  theta_h = 0.1,
  gamma = 0.2, #approx. 5 days 
  f = 0.0
)
#rough r0 calculation:
(parameters$theta_b*parameters$c_b*50)/parameters$gamma #R0 if all bold
(parameters$theta_h*parameters$c_h*50)/parameters$gamma #R0 if all shy

# Set initial values
init <- c(S_b = 24, S_h = 25, I_b = 1, I_h = 0)

# Solve using ode
out<- ode(y = init, times = times, func = si_3, parms = parameters)
null<-as.data.frame(out)
null$I_total<- null$I_b+null$I_h
null$sum_I_b<-cumsum(null$I_b)
null$sum_I_h<-cumsum(null$I_h)
null$sum_I_total<-null$sum_I_b+null$sum_I_h
null$prev<-(null$I_b+null$I_h)/(null$S_b+null$S_h+null$I_b+null$I_h)
null$sum_relative_infected<-null$sum_I_b/null$sum_I_h

null_c<-null #create new DF to store these results
null_c$scenario<-"null_c"


# D) Bold has higher baseline contact (2x contact) compared to shy but same susceptibility, N=50----

# Set run duration 
times <- seq(0, 60, by = 1) # run for 60 days

#Set parameters
parameters <- list(
  c_b=0.2,
  c_h = 0.1,
  theta_b = 0.05,
  theta_h = 0.05,
  gamma = 0.2, #approx. 5 days 
  f = 0.0
)
#rough r0 calculation:
(parameters$theta_b*parameters$c_b*50)/parameters$gamma #R0 if all bold
(parameters$theta_h*parameters$c_h*50)/parameters$gamma #R0 if all shy

# Set initial values
init <- c(S_b = 24, S_h = 25, I_b = 1, I_h = 0)

# Solve using ode
out<- ode(y = init, times = times, func = si_3, parms = parameters)
null<-as.data.frame(out)
null$I_total<- null$I_b+null$I_h
null$sum_I_b<-cumsum(null$I_b)
null$sum_I_h<-cumsum(null$I_h)
null$sum_I_total<-null$sum_I_b+null$sum_I_h
null$prev<-(null$I_b+null$I_h)/(null$S_b+null$S_h+null$I_b+null$I_h)
null$sum_relative_infected<-null$sum_I_b/null$sum_I_h

null_d<-null #create new DF to store these results
null_d$scenario<-"null_d"


# E) Bold has higher baseline contact (2x contact) and higher susceptibility (2x theta), N=50----

# Set run duration 
times <- seq(0, 60, by = 1) # run for 60 days

#Set parameters
parameters <- list(
  c_b=0.2,
  c_h = 0.1,
  theta_b = 0.1,
  theta_h = 0.05,
  gamma = 0.2, #approx. 5 days 
  f = 0.0
)
#rough r0 calculation:
(parameters$theta_b*parameters$c_b*50)/parameters$gamma #R0 if all bold
(parameters$theta_h*parameters$c_h*50)/parameters$gamma #R0 if all shy

# Set initial values
init <- c(S_b = 24, S_h = 25, I_b = 1, I_h = 0)

# Solve using ode
out<- ode(y = init, times = times, func = si_3, parms = parameters)
null<-as.data.frame(out)
null$I_total<- null$I_b+null$I_h
null$sum_I_b<-cumsum(null$I_b)
null$sum_I_h<-cumsum(null$I_h)
null$sum_I_total<-null$sum_I_b+null$sum_I_h
null$prev<-(null$I_b+null$I_h)/(null$S_b+null$S_h+null$I_b+null$I_h)
null$sum_relative_infected<-null$sum_I_b/null$sum_I_h

null_e<-null #create new DF to store these results
null_e$scenario<-"null_e"

# E-2) Bold has higher baseline contact (2x contact) and higher susceptibility (2x theta), N=100; 75% bold, 25% shy----

# Set run duration 
times <- seq(0, 60, by = 1) # run for 60 days

#Set parameters
parameters <- list(
  c_b=0.2,
  c_h = 0.1,
  theta_b = 0.1,
  theta_h = 0.05,
  gamma = 0.2, #approx. 5 days 
  f = 0.0
)
#rough r0 calculation:
(parameters$theta_b*parameters$c_b*50)/parameters$gamma #R0 if all bold
(parameters$theta_h*parameters$c_h*50)/parameters$gamma #R0 if all shy

# Set initial values
init <- c(S_b = 74, S_h = 25, I_b = 1, I_h = 0)

# Solve using ode
out<- ode(y = init, times = times, func = si_3, parms = parameters)
null<-as.data.frame(out)
null$I_total<- null$I_b+null$I_h
null$sum_I_b<-cumsum(null$I_b)
null$sum_I_h<-cumsum(null$I_h)
null$sum_I_total<-null$sum_I_b+null$sum_I_h
null$prev<-(null$I_b+null$I_h)/(null$S_b+null$S_h+null$I_b+null$I_h)
null$sum_relative_infected<-null$sum_I_b/null$sum_I_h

null_e_N100<-null #create new DF to store these results
null_e_N100$scenario<-"null_e_N100"



# F) Bold has higher baseline contact (2x contact) and shy has 2x high  susceptibility (2x theta), N=50----

# Set run duration 
times <- seq(0, 60, by = 1) # run for 60 days

#Set parameters
parameters <- list(
  c_b=0.2,
  c_h = 0.1,
  theta_b = 0.05,
  theta_h = 0.1,
  gamma = 0.2, #approx. 5 days 
  f = 0.0
)
#rough r0 calculation:
(parameters$theta_b*parameters$c_b*50)/parameters$gamma #R0 if all bold
(parameters$theta_h*parameters$c_h*50)/parameters$gamma #R0 if all shy

# Set initial values
init <- c(S_b = 24, S_h = 25, I_b = 1, I_h = 0)

# Solve using ode
out<- ode(y = init, times = times, func = si_3, parms = parameters)
null<-as.data.frame(out)
null$I_total<- null$I_b+null$I_h
null$sum_I_b<-cumsum(null$I_b)
null$sum_I_h<-cumsum(null$I_h)
null$sum_I_total<-null$sum_I_b+null$sum_I_h
null$prev<-(null$I_b+null$I_h)/(null$S_b+null$S_h+null$I_b+null$I_h)
null$sum_relative_infected<-null$sum_I_b/null$sum_I_h

null_f<-null #create new DF to store these results
null_f$scenario<-"null_f"

# N=100F) Bold has higher baseline contact (2x contact) and shy has 2x high  susceptibility (2x theta), N=50----

# Set run duration 
times <- seq(0, 60, by = 1) # run for 60 days

#Set parameters
parameters <- list(
  c_b=0.2,
  c_h = 0.1,
  theta_b = 0.05,
  theta_h = 0.1,
  gamma = 0.2, #approx. 5 days 
  f = 0.0
)
#rough r0 calculation:
(parameters$theta_b*parameters$c_b*50)/parameters$gamma #R0 if all bold
(parameters$theta_h*parameters$c_h*50)/parameters$gamma #R0 if all shy

# Set initial values
init <- c(S_b = 74, S_h = 25, I_b = 1, I_h = 0)

# Solve using ode
out<- ode(y = init, times = times, func = si_3, parms = parameters)
null<-as.data.frame(out)
null$I_total<- null$I_b+null$I_h
null$sum_I_b<-cumsum(null$I_b)
null$sum_I_h<-cumsum(null$I_h)
null$sum_I_total<-null$sum_I_b+null$sum_I_h
null$prev<-(null$I_b+null$I_h)/(null$S_b+null$S_h+null$I_b+null$I_h)
null$sum_relative_infected<-null$sum_I_b/null$sum_I_h

null_f100<-null #create new DF to store these results
null_f100$scenario<-"null_f_N100"

##Modifications to null models + integrate with provisioning scenarios 
null_all<-rbind(null_a,null_b,null_c,null_d,null_e,null_f)
null_t60 <- null_all%>% subset(time=='60')%>%rename(null_scenario=scenario)

null_t60_wide<-null_t60%>%pivot_wider(names_from = null_scenario,
                                      values_from = c(S_b:sum_relative_infected))

#Calculate the difference (null scenario value minus provisioning scenario value) for each variable of interest

df_all1<-df_all

#This works but needs to be updated to include all the variables 
# Create an empty data frame to store the results
results_df <- data.frame(null_scenario = character(),
                         diff_S_b = numeric(),
                         diff_I_b = numeric(),
                         stringsAsFactors = FALSE)

# Loop through each row in null_t60
for (i in 1:nrow(null_t60)) {
  
  # Extract the i-th row from null_t60
  null_scenario <- null_t60[i, ]
  
  
  # Calculate the differences
  diff_S_b <- df_all$S_b - null_scenario$S_b
  diff_I_b <- df_all$I_b - null_scenario$I_b
  
  #include other relevant variables from df_all
  c_b<-df_all$c_b
  
  # Append the result to the data frame
  results_df <- rbind(results_df, data.frame(null_scenario = null_scenario$null_scenario,
                                             diff_S_b = diff_S_b,
                                             diff_I_b = diff_I_b,
                                             c_b=c_b))
}

# Reset the row names of the result data frame
rownames(results_df) <- NULL



#MORE DATA VISUALIZTION STUFF:

#jpeg(file="output/nulls_rel_inf_time.jpeg")
null_all$relative_infected<-null_all$I_b/null_all$I_h
ggplot(data=null_all, aes(x=time, y=relative_infected)) +
  geom_line()+facet_wrap(vars(scenario))
#dev.off()

#jpeg(file="output/nulls_sum_rel_inf_time.jpeg")
ggplot(data=null_all, aes(x=time, y=sum_relative_infected)) +
  geom_line()+facet_wrap(vars(scenario))

#jpeg(file="output/nulls_prev_time.jpeg")
ggplot(data=null_all, aes(x=time, y=prev)) +
  geom_line()+facet_wrap(vars(scenario))

null_all_long1<- null_all%>% select(time,scenario,I_b,I_h,I_total)%>% pivot_longer(I_b:I_total,names_to=c("class"),values_to = "count")

null_all_long2<- null_all%>% select(time,scenario,sum_I_b,sum_I_h,sum_I_total)%>% pivot_longer(sum_I_b:sum_I_total,names_to=c("class"),values_to = "count")

#show each infected class over time + total
#jpeg(file="output/nulls_inf_time.jpeg")
ggplot(data=null_all_long1, aes(x=time, y=count, color=class)) +
  geom_line()+facet_wrap(vars(scenario))+ labs(title="Infected classes through time")

#show each infected class over time + total (cumulative sums)
#jpeg(file="output/nulls_cum_inf_time.jpeg")
ggplot(data=null_all_long2, aes(x=time, y=count, color=class)) + geom_line()+facet_wrap(vars(scenario)) + labs(title="Cumulative sum of infected classes through time")


##subset plots to make new figures for poster:-------


#subset for theta_ratio 0.5, 1, and 2
df_sub <- subset(df_all, theta_ratio==0.5|theta_ratio==1.0|theta_ratio==2.0)




df_sub$theta_ratio<-as.factor(df_sub$theta_ratio)
ggplot(data=df_sub, aes(x=c_ratio, y=prev,group=theta_ratio)) +
  geom_line(aes(color=theta_ratio))+ facet_wrap(vars(scenario))+labs(title="Prevalence")


ggplot(data=df_sub, aes(x=c_ratio, y=sum_I_total,group=theta_ratio)) +
  geom_line(aes(color=theta_ratio))+ facet_wrap(vars(scenario))+labs(title="Cumulative inf")

#now choose 2 of these to show on poster- N=100, 75/25 and n=50, 50/50
df_sub2<- subset(df_sub, scenario=="N=50, 50% bold, 50% shy"|scenario=="N=100, 75% bold, 25% shy")
df_sub2$theta_ratio<-as.factor(df_sub2$theta_ratio)


df_sub2$scenario<- factor(df_sub2$scenario,      # Reordering group factor levels
                          levels = c("N=50, 50% bold, 50% shy","N=100, 75% bold, 25% shy"))
p<-ggplot(data=df_sub2, aes(x=c_ratio, y=prev,group=theta_ratio)) +
  geom_line(aes(color=theta_ratio),linewidth=2) + 
  facet_wrap(vars(scenario)) +
  labs(title = expression("Baseline parameters: shy"~italic(c)~"=0.1, shy "~theta ~ "= 0.05"),
       y="Prevalence",
       x="Ratio of bold"~italic(c)~ "to shy"~italic(c)~" (contact rate)")+
  scale_color_manual(values=c("#8ECAE6","#219EBC","#023047"),name="class")+
  theme_bw() + 
  theme(axis.text.x=element_text(size=rel(1.9),angle=45,hjust = 1,color = "black"),
        axis.text.y=element_text(size=rel(1.9),color = "black"),
        axis.title.x=element_text(size=25,color="black"),
        axis.title.y=element_text(size=25,color="black"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=10),
        strip.text.x = element_text(size = 19),
        plot.title = element_text(size=22),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent'))+ #transparent legend panel)+
  ylim(0,1)
p
ggsave("2_scenarios_prevalence.png", p, bg='transparent',path = "output")

#now same plot but cumulative infections:
p_cumulative_inf<-ggplot(data=df_sub2, aes(x=c_ratio, y=sum_I_total,group=theta_ratio)) +
  geom_line(aes(color=theta_ratio),linewidth=2) + 
  facet_wrap(vars(scenario)) +
  labs(title = expression("Baseline parameters: shy"~italic(c)~"=0.1, shy "~theta ~ "= 0.05"),
       y="Cumulative Infections",
       x="Ratio of bold"~italic(c)~ "to shy"~italic(c)~" (contact rate)")+
  scale_color_manual(values=c("#8ECAE6","#219EBC","#023047"),name="class")+
  theme_bw() + 
  theme(axis.text.x=element_text(size=rel(1.9),angle=45,hjust = 1,color = "black"),
        axis.text.y=element_text(size=rel(1.9),color = "black"),
        axis.title.x=element_text(size=25,color="black"),
        axis.title.y=element_text(size=25,color="black"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=10),
        strip.text.x = element_text(size = 14),
        plot.title = element_text(size=22),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent')) #transparent legend panel)+

p_cumulative_inf

ggsave("2_scenarios_cumulativeinf.png", p_cumulative_inf, bg='transparent',path = "output")

#log cumulative infections:
p_cumulative_inf_log<-ggplot(data=df_sub2, aes(x=c_ratio, y=log10(sum_I_total),group=theta_ratio)) +
  geom_line(aes(color=theta_ratio),linewidth=2) + 
  facet_wrap(vars(scenario)) +
  labs(title = expression("Baseline parameters: shy"~italic(c)~"=0.1, shy "~theta ~ "= 0.05"),
       y="Cumulative Infections (Log 10)",
       x="Ratio of bold"~italic(c)~ "to shy"~italic(c)~" (contact rate)")+
  scale_color_manual(values=c("#8ECAE6","#219EBC","#023047"),name="class")+
  theme_bw() + 
  theme(axis.text.x=element_text(size=rel(1.9),angle=45,hjust = 1,color = "black"),
        axis.text.y=element_text(size=rel(1.9),color = "black"),
        axis.title.x=element_text(size=25,color="black"),
        axis.title.y=element_text(size=25,color="black"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=10),
        strip.text.x = element_text(size = 14),
        plot.title = element_text(size=22),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent')) #transparent legend panel)+

p_cumulative_inf_log
ggsave("2_scenarios_cumulativeinflog10.png", p_cumulative_inf, bg='transparent',path = "output")


#clustered barplot showing final eq infections for each scenario
#look at when bold contact is 2x greater and bold are twice as susceptible 

df_sub3<- subset(df_all, theta_ratio==2 & c_ratio==2)
df_sub3<-df_sub3 %>% select(scenario,I_b,I_h)%>%pivot_longer(cols=I_b:I_h,names_to="class",values_to = "count")

library("stringr") 

df_sub3$scenario<- factor(df_sub3$scenario,      # Reordering group factor levels
                          levels = c("N=50, 50% bold, 50% shy","N=50, 75% bold, 25% shy","N=100, 50% bold, 50% shy","N=100, 75% bold, 25% shy"))

p2<-ggplot(data=df_sub3, aes(x=scenario, y=count, fill=class)) +
  geom_bar(stat="identity")+
  labs(title = expression("Parameters: shy"~italic(c)~"=0.1, shy "~theta ~ "= 0.05, bold"~italic(c)~"=0.2, bold "~theta ~"=0.1"),
       y="# Infected hosts at equilibrium",
       x=NULL)+
  scale_fill_manual(values=c("#FB8500","#219EBC"),name="class")+
  theme_bw() + 
  theme(axis.text.x=element_text(size=rel(1.7),angle=0,,color = "black"),
        axis.text.y=element_text(size=rel(1.9),color = "black"),
        axis.title.x=element_text(size=18,color="black"),
        axis.title.y=element_text(size=18,color="black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        strip.text.x = element_text(size = 19),
        plot.title = element_text(size=15),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent')) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))
p2
ggsave("eq_infections.png", p2, bg='transparent',path = "output")


#what if i made the same plot with cumulative sum of infections?
df_sub4<- subset(df_all, theta_ratio==2 & c_ratio==2)
df_sub4<-df_sub4 %>% select(scenario,sum_I_b,sum_I_h)%>%pivot_longer(cols=sum_I_b:sum_I_h,names_to="class",values_to = "count")



df_sub4$scenario<- factor(df_sub4$scenario,      # Reordering group factor levels
                          levels = c("N=50, 50% bold, 50% shy","N=50, 75% bold, 25% shy","N=100, 50% bold, 50% shy","N=100, 75% bold, 25% shy"))

p3<-ggplot(data=df_sub4, aes(x=scenario, y=count, fill=class)) +
  geom_bar(stat="identity")+
  labs(title = expression("Parameters: shy"~italic(c)~"=0.1, shy "~theta ~ "= 0.05, bold"~italic(c)~"=0.2, bold "~theta ~"=0.1"),
       y="Number of cumulative infections",
       x=NULL)+scale_fill_manual(values=c("#FB8500","#219EBC"),name="class")+
  theme_bw() + 
  theme(axis.text.x=element_text(size=rel(1.7),angle=0,,color = "black"),
        axis.text.y=element_text(size=rel(1.9),color = "black"),
        axis.title.x=element_text(size=18,color="black"),
        axis.title.y=element_text(size=18,color="black"),
        legend.title = element_text(size=12),
        legend.text = element_text(size=10),
        strip.text.x = element_text(size = 19),
        plot.title = element_text(size=15),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent')) + 
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15))
p3

ggsave("eq__cumulative_infections.png", p3, bg='transparent',path = "output")

#what if you look at I_b and I_h through time when bold individuals are more susceptible and more contact
#this is the same as current null E and modified null E modified to have n=100 and 75% 25%
null_Efigures<-rbind(null_e_N100,null_e)
null_Efigures_long1<- null_Efigures%>% select(time,scenario,I_b,I_h,I_total)%>% pivot_longer(I_b:I_total,names_to=c("class"),values_to = "count")

null_E100<-null_Efigures_long1
null_E100[null_E100 == "null_e_N100"] <- "N=100, 75% bold, 25% shy"
null_E100[null_E100 == "null_e"] <- "N=50, 50% bold, 50% shy"

null_E100$scenario<- factor(null_E100$scenario,      # Reordering group factor levels
                            levels = c("N=50, 50% bold, 50% shy","N=100, 75% bold, 25% shy"))
ggplot(data=null_E100, aes(x=time, y=count, color=class)) +
  geom_line()+facet_wrap(vars(scenario))+ labs(title="Infected classes through time")



#now same plot but cumulative infections:
p_classes_time<-ggplot(data=null_E100, aes(x=time, y=count, color=class)) +
  geom_line(aes(color=class),linewidth=2) + 
  facet_wrap(vars(scenario)) +
  labs(title = expression("Parameters: shy"~italic(c)~"=0.1, shy "~theta ~ "= 0.05, bold"~italic(c)~"=0.2, bold "~theta ~"=0.1"),
       y="Number of Infected Hosts",
       x="Time steps")+
  scale_color_manual(values=c("#FB8500","#219EBC","#023047"),name="class")+
  theme_bw() + 
  theme(axis.text.x=element_text(size=rel(1.9),angle=45,hjust = 1,color = "black"),
        axis.text.y=element_text(size=rel(1.9),color = "black"),
        axis.title.x=element_text(size=25,color="black"),
        axis.title.y=element_text(size=25,color="black"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        strip.text.x = element_text(size = 14),
        plot.title = element_text(size=17),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent')) #transparent legend panel)+

p_classes_time
ggsave("classes_time.png", p_classes_time, bg='transparent',path = "output")



##NEW BASELINE--------------------

# BASELINE) Behavior types are identical (contactand susc = the avg of the two contacts and theta in scenario E)  and N = 50 (no aggregation). half bold and half shy----

# Set run duration 
times <- seq(0, 60, by = 1) # run for 60 days

#Set parameters
parameters <- list(
  c_b=0.15,
  c_h = 0.15,
  theta_b = 0.075,
  theta_h = 0.075,
  gamma = 0.2, #approx. 5 days 
  f = 0.0
)
#rough r0 calculation:
(parameters$theta_b*parameters$c_b*50)/parameters$gamma #R0 if all bold
(parameters$theta_h*parameters$c_h*50)/parameters$gamma #R0 if all shy

# Set initial values
init <- c(S_b = 24, S_h = 25, I_b = 1, I_h = 0)

# Solve using ode
out<- ode(y = init, times = times, func = si_3, parms = parameters)
null<-as.data.frame(out)
null$I_total<- null$I_b+null$I_h
null$sum_I_b<-cumsum(null$I_b)
null$sum_I_h<-cumsum(null$I_h)
null$sum_I_total<-null$sum_I_b+null$sum_I_h
null$prev<-(null$I_b+null$I_h)/(null$S_b+null$S_h+null$I_b+null$I_h)
null$sum_relative_infected<-null$sum_I_b/null$sum_I_h

null_baseline50<-null #create new DF to store these results
null_baseline50$scenario<-"null_baseline50"

# BASELINE) Behavior types are identical (contactand susc = the avg of the two contacts and theta in scenario E)  and N = 50 (no aggregation). half bold and half shy----

# Set run duration 
times <- seq(0, 60, by = 1) # run for 60 days

#Set parameters
parameters <- list(
  c_b=0.15,
  c_h = 0.15,
  theta_b = 0.075,
  theta_h = 0.075,
  gamma = 0.2, #approx. 5 days 
  f = 0.0
)
#rough r0 calculation:
(parameters$theta_b*parameters$c_b*50)/parameters$gamma #R0 if all bold
(parameters$theta_h*parameters$c_h*50)/parameters$gamma #R0 if all shy

# Set initial values
init <- c(S_b = 49, S_h = 50, I_b = 1, I_h = 0)

# Solve using ode
out<- ode(y = init, times = times, func = si_3, parms = parameters)
null<-as.data.frame(out)
null$I_total<- null$I_b+null$I_h
null$sum_I_b<-cumsum(null$I_b)
null$sum_I_h<-cumsum(null$I_h)
null$sum_I_total<-null$sum_I_b+null$sum_I_h
null$prev<-(null$I_b+null$I_h)/(null$S_b+null$S_h+null$I_b+null$I_h)
null$sum_relative_infected<-null$sum_I_b/null$sum_I_h

null_baseline100<-null #create new DF to store these results
null_baseline100$scenario<-"null_baseline100"

#this is the same as current null E and modified null E modified to have n=100 and 75% 25%
null_baseline_figures<-rbind(null_e_N100,null_baseline)
null_baseline_figures_long1<- null_baseline_figures%>% select(time,scenario,I_b,I_h,I_total)%>% pivot_longer(I_b:I_total,names_to=c("class"),values_to = "count")

baseline_figs<-null_baseline_figures_long1
baseline_figs[baseline_figs == "null_e_N100"] <- "N=100, 75% bold, 25% shy"
baseline_figs[baseline_figs == "null_baseline"] <- "N=50, no distinct behavioral types"

baseline_figs$scenario<- factor(baseline_figs$scenario,      # Reordering group factor levels
                                levels = c("N=50, no distinct behavioral types","N=100, 75% bold, 25% shy"))

ggplot(data=baseline_figs, aes(x=time, y=count, color=class)) +
  geom_line()+facet_wrap(vars(scenario))+ labs(title="Infected classes through time")

#look at prevalence through time:
p_classes_prev_time_base<-ggplot(data=baseline_figs, aes(x=time, y=count, color=class)) +
  geom_line(aes(color=class),linewidth=2) + 
  facet_wrap(vars(scenario)) +
  labs(title = expression("Parameters: shy"~italic(c)~"=0.1, shy "~theta ~ "= 0.05, bold"~italic(c)~"=0.2, bold "~theta ~"=0.1"),
       y="Prevalence",
       x="Time steps")+
  scale_color_manual(values=c("#FB8500","#219EBC","#023047"),name="class")+
  theme_bw() + 
  theme(axis.text.x=element_text(size=rel(1.9),angle=45,hjust = 1,color = "black"),
        axis.text.y=element_text(size=rel(1.9),color = "black"),
        axis.title.x=element_text(size=25,color="black"),
        axis.title.y=element_text(size=25,color="black"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        strip.text.x = element_text(size = 14),
        plot.title = element_text(size=17),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent')) #transparent legend panel)+

p_classes_time_base
ggsave("classes_time_prev_baseline.png", p_classes_prev_time_base, bg='transparent',path = "output")

###LOOK AT PREVALENCE THROUGH TIME FOR EACH CLASS:
null_base_prev <- null_baseline_figures
null_base_prev$prev_b<- null_base_prev$I_b/((null_base_prev$I_b+null_base_prev$I_h+null_base_prev$S_b+null_base_prev$S_h))
null_base_prev$prev_h<- null_base_prev$I_h/((null_base_prev$I_b+null_base_prev$I_h+null_base_prev$S_b+null_base_prev$S_h))
null_base_prev$prev_check<- null_base_prev$prev_b+null_base_prev$prev_h
null_base_prev$prev_diff<-null_base_prev$prev_check-null_base_prev$prev #I think there's some kind of rounding issue because these are identical

null_base_prev_long1<- null_base_prev%>% select(time,scenario,prev_b,prev_h,prev)%>% pivot_longer(prev_b:prev,names_to=c("class"),values_to = "prevalence")

baseline_prev_figs<-null_base_prev_long1
baseline_prev_figs[baseline_prev_figs == "null_e_N100"] <- "N=100, 75% bold, 25% shy"
baseline_prev_figs[baseline_prev_figs == "null_baseline"] <- "N=50, no distinct behavioral types"

baseline_prev_figs$scenario<- factor(baseline_prev_figs$scenario,      # Reordering group factor levels
                                     levels = c("N=50, no distinct behavioral types","N=100, 75% bold, 25% shy"))

p_classes_prev_time_base<-ggplot(data=baseline_prev_figs, aes(x=time, y=prevalence, color=class)) +
  geom_line(aes(color=class),linewidth=2) + 
  facet_wrap(vars(scenario)) +
  labs(title = expression("Parameters: shy"~italic(c)~"=0.1, shy "~theta ~ "= 0.05, bold"~italic(c)~"=0.2, bold "~theta ~"=0.1"),
       y="Prevalence",
       x="Time steps")+
  scale_color_manual(values=c("#023047","#FB8500","#219EBC"),name="class")+
  theme_bw() + 
  theme(axis.text.x=element_text(size=rel(1.9),angle=45,hjust = 1,color = "black"),
        axis.text.y=element_text(size=rel(1.9),color = "black"),
        axis.title.x=element_text(size=25,color="black"),
        axis.title.y=element_text(size=25,color="black"),
        legend.title = element_text(size=15),
        legend.text = element_text(size=15),
        strip.text.x = element_text(size = 14),
        plot.title = element_text(size=17),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        panel.grid.major = element_blank(), #remove major gridlines
        panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent')) #transparent legend panel)+

p_classes_prev_time_base
ggsave("classes_time_prev_baseline.png",p_classes_prev_time_base, bg='transparent',path = "output")


#WHAT IS THE END PREVALENCE FOR THE BASELINE SCENARIO?
null_baseline$prev[61] #64.4%


##ADD IN SCENARIO WITH BOLD = LESS SUSC. (null_F100)
new_base2<-rbind(null_e_N100,null_f100,null_baseline)
new_base<-new_base2

new_base$prev_b<- new_base$I_b/((new_base$I_b+new_base$I_h+new_base$S_b+new_base$S_h))
new_base$prev_h<- new_base$I_h/((new_base$I_b+new_base$I_h+new_base$S_b+new_base$S_h))
new_base$prev_check<- new_base$prev_b+new_base$prev_h
new_base$prev_diff<-new_base$prev_check-new_base$prev #I think there's some kind of rounding issue because these are identical

new_base_long1<- new_base%>% select(time,scenario,prev_b,prev_h,prev)%>% pivot_longer(prev_b:prev,names_to=c("class"),values_to = "prevalence")

base_df<-new_base_long1
base_df[base_df == "null_e_N100"] <- "Provisioning (bold > susc.)"
base_df[base_df == "null_f_N100"] <- "Provisioning (bold < susc.)"
base_df[base_df == "null_baseline"] <- "Baseline"

base_df$scenario<- factor(base_df$scenario,      # Reordering group factor levels
                          levels = c("Baseline","Provisioning (bold > susc.)","Provisioning (bold < susc.)"))


##want to change this so total prev for baseline scenario is integrated into the other 2 panels:

---------#######delete chunk below this once finished:
  
  ##ADD IN SCENARIO WITH BOLD = LESS SUSC. (null_F100)
  head(scen)
simple_df<- scen%>% select(time,baseline_prev=prev)%>% pivot_longer(baseline_prev,names_to=c("class"),values_to = "prevalence")
simple_df1<-simple_df #create 2 dfs
simple_df2<-simple_df #create 2 dfs
simple_df1$scenario<-"null_e_N100"
simple_df2$scenario<-"null_f_N100"
simple_df3<-rbind(simple_df1,simple_df2)#combine these

#now merge with the 2 feeding scenarios:
scen2<-rbind(null_e_N100,null_f100)
scen<-scen2

scen$prev_b<- scen$I_b/((scen$I_b+scen$I_h+scen$S_b+scen$S_h))
scen$prev_h<- scen$I_h/((scen$I_b+scen$I_h+scen$S_b+scen$S_h))

scen_long<- scen%>% select(time,scenario,prev_b,prev_h,prev)%>% pivot_longer(prev_b:prev,names_to=c("class"),values_to = "prevalence")


head(scen_long)
head(simple_df3)

fin_df<-rbind(scen_long,simple_df3)

fin_df[fin_df == "null_e_N100"] <- "B) Food increases bold susc."
fin_df[fin_df == "null_f_N100"] <- "A) Food decreases bold susc."

fin_df$scenario<- factor(fin_df$scenario,      # Reordering group factor levels
                         levels = c("A) Food decreases bold susc.","B) Food increases bold susc."))




p_prev_time_classes_3<-ggplot(data=fin_df, aes(x=time, y=prevalence, color=class)) +
  geom_line(aes(color=class,linetype=class),linewidth=2) + 
  facet_wrap(vars(scenario)) +
  labs(y="Prevalence",
       x="Time steps")+
  scale_color_manual(values=c("grey","#023047","#FB8500","#219EBC"),name="Prevalence Type",labels=c('Baseline', 'Total','Bold','Shy'))+
  scale_linetype_manual(values=c("dashed", "solid","solid","solid"),name="Prevalence Type",labels=c('Baseline', 'Total','Bold','Shy'))+
  #theme_bw() +
  theme_classic()+
  theme(axis.text.x=element_text(size=rel(1.9),angle=45,hjust = 1,color = "black"),
        axis.text.y=element_text(size=rel(1.9),color = "black"),
        axis.title.x=element_text(size=rel(1.9),color="black"),
        axis.title.y=element_text(size=rel(1.9),color="black"),
        legend.title = element_text(size=rel(1.5)),
        legend.text = element_text(size=rel(1.4)),
        strip.text.x = element_text(size=rel(1.7)),#facet title test
        plot.title = element_text(size=rel(1.9)),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA),
        panel.border = element_blank(), #transparent plot bg
        panel.grid.major = element_blank(), #remove major gridlines
        #panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent')+ylim(0,1)) #transparent legend panel)+

p_prev_time_classes_3
ggsave("classes_time_prev_3scenarios.png",p_prev_time_classes_3, bg='transparent',path = "output",height=5,width=9,units="in")


###---------#######delete chunk above this once finished:
##ADD IN SCENARIO WITH BOLD = LESS SUSC. (null_F100)
#two baseline dataframes = null_baseline100 and null_baseline50
bl100<- null_baseline100
bl50<- null_baseline50
#bl_tot<-rbind(bl100,bl50)
bl100_long<- bl100%>% select(time,baseline100_prev=prev)%>% pivot_longer(baseline100_prev,names_to=c("class"),values_to = "prevalence")
bl50_long<- bl50%>% select(time,baseline50_prev=prev)%>% pivot_longer(baseline50_prev,names_to=c("class"),values_to = "prevalence")
simple_df<-rbind(bl100_long,bl50_long)#combine 2 baseline models
simple_df1<-simple_df #create 2 dfs
simple_df2<-simple_df #create 2 dfs
simple_df1$scenario<-"null_e_N100"
simple_df2$scenario<-"null_f_N100"
simple_df3<-rbind(simple_df1,simple_df2)#combine these

#now merge with the 2 feeding scenarios:
scen2<-rbind(null_e_N100,null_f100)
scen<-scen2

scen$prev_b<- scen$I_b/((scen$I_b+scen$I_h+scen$S_b+scen$S_h))
scen$prev_h<- scen$I_h/((scen$I_b+scen$I_h+scen$S_b+scen$S_h))

scen_long<- scen%>% select(time,scenario,prev_b,prev_h,prev)%>% pivot_longer(prev_b:prev,names_to=c("class"),values_to = "prevalence")


head(scen_long)
head(simple_df3)

fin_df<-rbind(scen_long,simple_df3)

fin_df[fin_df == "null_e_N100"] <- "B) Food increases bold susc."
fin_df[fin_df == "null_f_N100"] <- "A) Food decreases bold susc."

fin_df$scenario<- factor(fin_df$scenario,      # Reordering group factor levels
                         levels = c("A) Food decreases bold susc.","B) Food increases bold susc."))




p_prev_time_classes_4<-ggplot(data=fin_df, aes(x=time, y=prevalence, color=class)) +
  geom_line(aes(color=class,linetype=class),linewidth=2) + 
  facet_wrap(vars(scenario)) +
  labs(y="Prevalence",
       x="Time steps")+
  scale_color_manual(values=c("grey","grey","#023047","#FB8500","#219EBC"),name="Prevalence Type",labels=c('Baseline N=100','Baseline N=50', 'Total','Bold','Shy'))+
  scale_linetype_manual(values=c("dashed", "dotted","solid","solid","solid"),name="Prevalence Type",labels=c('Baseline N=100','Baseline N=50', 'Total','Bold','Shy'))+
  #theme_bw() +
  theme_classic()+
  theme(axis.text.x=element_text(size=rel(1.9),angle=45,hjust = 1,color = "black"),
        axis.text.y=element_text(size=rel(1.9),color = "black"),
        axis.title.x=element_text(size=rel(1.9),color="black"),
        axis.title.y=element_text(size=rel(1.9),color="black"),
        legend.title = element_text(size=rel(1.5)),
        legend.text = element_text(size=rel(1.4)),
        strip.text.x = element_text(size=rel(1.7)),#facet title test
        plot.title = element_text(size=rel(1.9)),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA),
        panel.border = element_blank(), #transparent plot bg
        panel.grid.major = element_blank(), #remove major gridlines
        #panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent')+ylim(0,1)) #transparent legend panel)+

p_prev_time_classes_4
ggsave("classes_time_prev_4scenarios.png",p_prev_time_classes_4, bg='transparent',path = "output",height=5,width=9,units="in")



##new figure that is the same as the one above but with only one baseline (N=50) and bold/shy are split
##ADD IN SCENARIO WITH BOLD = LESS SUSC. (null_F100)
#two baseline dataframes = null_baseline100 and null_baseline50
bl50_2<- null_baseline50


bl50_2$prev_b<- bl50_2$I_b/((bl50_2$I_b+bl50_2$I_h+bl50_2$S_b+bl50_2$S_h))
bl50_2$prev_h<- bl50_2$I_h/((bl50_2$I_b+bl50_2$I_h+bl50_2$S_b+bl50_2$S_h))


bl50_long2<- bl50_2%>% select(time,baseline50_total_prev=prev,baseline50_Ib_prev=prev_b,baseline50_Ih_prev=prev_h)%>% pivot_longer(baseline50_total_prev:baseline50_Ih_prev,names_to=c("class"),values_to = "prevalence")
simple_df<-bl50_long2
simple_df1<-simple_df #create 2 dfs
simple_df2<-simple_df #create 2 dfs
simple_df1$scenario<-"null_e_N100"
simple_df2$scenario<-"null_f_N100"
simple_df3<-rbind(simple_df1,simple_df2)#combine these

#now merge with the 2 feeding scenarios:
scen2<-rbind(null_e_N100,null_f100)
scen<-scen2

scen$prev_b<- scen$I_b/((scen$I_b+scen$I_h+scen$S_b+scen$S_h))
scen$prev_h<- scen$I_h/((scen$I_b+scen$I_h+scen$S_b+scen$S_h))

scen_long<- scen%>% select(time,scenario,prev_b,prev_h,prev)%>% pivot_longer(prev_b:prev,names_to=c("class"),values_to = "prevalence")


head(scen_long)
head(simple_df3)

fin_df<-rbind(scen_long,simple_df3)

fin_df[fin_df == "null_e_N100"] <- "B) Food increases bold susc."
fin_df[fin_df == "null_f_N100"] <- "A) Food decreases bold susc."

fin_df$scenario<- factor(fin_df$scenario,      # Reordering group factor levels
                         levels = c("A) Food decreases bold susc.","B) Food increases bold susc."))

fin_df$class <- factor(fin_df$class, levels = c('prev','prev_b','prev_h','baseline50_total_prev','baseline50_Ib_prev','baseline50_Ih_prev')) #reorder classes 


p_prev_time_classes_5<-ggplot(data=fin_df, aes(x=time, y=prevalence, color=class)) +
  geom_line(aes(color=class,linetype=class),linewidth=1.6) + 
  facet_wrap(vars(scenario)) +
  labs(y="Prevalence",
       x="Time steps")+
  scale_color_manual(values=c("#023047","#FB8500","#219EBC","grey","grey","grey"),name="Prevalence Type",labels=c('Total','Bold','Shy','Baseline Total','Baseline Bold','Baseline Shy'))+
  scale_linetype_manual(values=c("solid","solid","solid","dashed","dotted","dotted"),name="Prevalence Type",labels=c('Total','Bold','Shy','Baseline Total','Baseline Bold','Baseline Shy'))+
  #theme_bw() +
  theme_classic()+
  theme(axis.text.x=element_text(size=rel(1.9),angle=45,hjust = 1,color = "black"),
        axis.text.y=element_text(size=rel(1.9),color = "black"),
        axis.title.x=element_text(size=rel(1.9),color="black"),
        axis.title.y=element_text(size=rel(1.9),color="black"),
        legend.title = element_text(size=rel(1.5)),
        legend.text = element_text(size=rel(1.4)),
        strip.text.x = element_text(size=rel(1.7)),#facet title test
        plot.title = element_text(size=rel(1.9)),
        panel.background = element_rect(fill='transparent'), #transparent panel bg
        plot.background = element_rect(fill='transparent', color=NA),
        panel.border = element_blank(), #transparent plot bg
        panel.grid.major = element_blank(), #remove major gridlines
        #panel.grid.minor = element_blank(), #remove minor gridlines
        legend.background = element_rect(fill='transparent'), #transparent legend bg
        legend.box.background = element_rect(fill='transparent')+ylim(0,1)) #transparent legend panel)+

p_prev_time_classes_5
ggsave("classes_time_prev_5scenarios.png",p_prev_time_classes_5, bg='transparent',path = "output",height=6,width=9.7,units="in")







