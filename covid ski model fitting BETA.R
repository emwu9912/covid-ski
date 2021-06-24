# set file paths
path <- "/Users/emilywu883/Documents/CU Anschutz/COVID-19/"

# load packages
library(pacman)
p_load(coda, deSolve, FME, rootSolve, stringr)

# import early regional hospitalization data
region_hosp_early <- read.csv(str_c(path, "Thesis/Data/region_hosp_early.csv"))
region_hosp_early$date <- as.Date(region_hosp_early$date, "%m/%d/%y")

# read in visit and prevalence data
pop_travel <- read.csv(str_c(path, "/Thesis/Data/visit_prev_data.csv"))

# plot early hospitalizations
plot(region_hosp_early$Iht, lwd=6, type='h', xlab="Date", ylab="Active Hospitalizations",
     main="Regional Hospital Census, Early Outbreak", col='orange', axes=FALSE)
     axis(1, seq(0, 140, 20))
     axis(2, seq(0, 40, 5))

# identify historical parameters (either established from literature or estimated previously)
n <- 205382 # total population of the six counties put together
n1 <- 45228 # population 0-19
n2 <- 62369 # population 20-39
n3 <- 67646 # population 40-64
n4 <- 30139 # population 65+
alpha <- 4.2 # latent period (incubation period - 1)
gamma <- 1/9 # recovery rate (1/average length of infection)
lambda <- 1.395 # ratio of infectiousness (symptomatic/asymptomatic)
sigma1 <- 0.1100230677 # proportion of infectious individuals symptomatic age group 1
sigma2 <- 0.3570515767 # 2
sigma3 <- 0.5612057201 # 3
sigma4 <- 0.7748797315 # 4
theta <- 0 # proportion of infections detected
tau <- 0
kappa <- 0
hosp1 <- 0.01743 # percent of symptomatic cases hospitalized age group 1
hosp2 <- 0.02909127 # 2
hosp3 <- 0.032122 # 3
hosp4 <- 0.0501384 # 4
cc1 <- 0.0052754 # percent of symptomatic cases requiring critical care age group 1
cc2a <- 0.007474 # 2
cc2b <- 0.00838
cc2c <- 0.00591
cc3a <- 0.01866 # 3
cc3b <- 0.01785
cc3c <- 0.012226
cc4a <- 0.02885 # 4
cc4b <- 0.02234
cc4c <- 0.0202
hlos1a <- 4.05
hlos1b <- 5.0
hlos2a <- 5.49
hlos2b <- 3.6
hlos3a <- 7.36
hlos3b <- 5.3
hlos4a <- 10.02
hlos4b <- 7.6
clos1a <- 7.00
clos1b <- 4
clos2a <- 9.91
clos2b <- 4.4
clos3a <- 13.47
clos3b <- 8.5
clos4a <- 10.82
clos4b <- 8
dh1 <- 0
dh2 <- 0
dh3 <-  0.01
dh3_2 <- 0.00347
dh4 <- 0.14106
dh4_2 <- 0.06086
dc1 <- 0.0408
dc2 <- 0.0573
dc3 <- 0.16498
dc3_2 <- 0.1101
dc4 <- 0.39066
dc4_2 <- 0.30275
t2 <- 50
t3 <- 64

delta1 <- 0

# specify initial values
inits <- c(S1=45228-1, E1=0, I1=1, A1=0, II1=0, Ih1=0, R1=0, D1=0,
           S2=62369,     E2=0, I2=0, A2=0, II2=0, Ih2=0, R2=0, D2=0,
           S3=67646,     E3=0, I3=0, A3=0, II3=0, Ih3=0, R3=0, D3=0,
           S4=30139,     E4=0, I4=0, A4=0, II4=0, Ih4=0, R4=0, D4=0)

# set up the vector for daily hospitalizations, which we need for optimization
Hosp <- region_hosp_early[c("date", "Iht")]
hosp.vec <- as.vector(Hosp$Iht)

# set up the incrementing time vector for the SEIR model
Day <- 1:length(region_hosp_early$Iht)

# check to make sure the time and hospitalization vectors are the same length
length(hosp.vec)
length(Day)

# list the parameters to be estimated
params1 <- list(
  beta=0.4931,
  delta2=0.5868,
  delta3=0.99)

# build the SEIR model as an R function
seir_ski1 <- function(t, state, parameters) {
  
  with(as.list(c(state, parameters)), {
  
# age-structure the  entry and exit parameters by multiplying by the proportionate age distribution
    mu1 <- pop_travel$mu[[t]]*(45228/205382)
    mu2 <- pop_travel$mu[[t]]*(62369/205382)
    mu3 <- pop_travel$mu[[t]]*(67646/205382)
    mu4 <- pop_travel$mu[[t]]*(30139/205382) 
    eta1 <- pop_travel$eta[[t]]*(45228/205382)
    eta2 <- pop_travel$eta[[t]]*(62369/205382)
    eta3 <- pop_travel$eta[[t]]*(67646/205382)
    eta4 <- pop_travel$eta[[t]]*(30139/205382)
    
# national prevalence of COVID-19 (using NYT past data and covid19sim projections)
    epsilon <- pop_travel$epsilon[[t]] 

# population estimate
    n  <- 205382 # total population of Summit, Grand, Eagle, Pitkin, Routt, and Garfield Counties
    n1 <- 45228 # population age group 1
    n2 <- 62369 # 2
    n3 <- 67646 # 3
    n4 <- 30139 # 4
    
# time-varying transmission control (TC) parameters, represented by delta
    delta <- ifelse(t<t2, delta1, delta2)
    
# toggle CT/CI on or off after December 12th, 2020
    kappa <- ifelse(t<316, 0, kappa)
    
# proportion of symptomatic cases in each age group that end up hospitalized (including ICU)
# critical care needs change over time for age groups 2, 3, and 4   
    cc2 <- ifelse(t < 147, cc2a, ifelse(t < 234, cc2b, cc2c))
    cc3 <- ifelse(t < 147, cc3a, ifelse(t < 234, cc3b, cc3c))
    cc4 <- ifelse(t < 147, cc4a, ifelse(t < 234, cc4b, cc4c))
    
# collapse hospital and ICU into grand total hospitalizations
    phi1 <- hosp1 + cc1
    phi2 <- hosp2 + cc2
    phi3 <- hosp3 + cc3
    phi4 <- hosp4 + cc4
    
# length of stay in the general hospital for each age group
# determined by the weighted average of lengths of stay in non-ICU and ICU

# length of stay in hospital and ICU changes for each age group over time
    hlos1 <- ifelse(t<99, hlos1a, hlos1b)    
    clos1 <- ifelse(t<99, clos1a, clos1b)
    hlos2 <- ifelse(t<99, hlos2a, hlos2b)
    clos2 <- ifelse(t<99, clos2a, clos2b)
    hlos3 <- ifelse(t<99, hlos3a, hlos3b)
    clos3 <- ifelse(t<99, clos3a, clos3b)
    hlos4 <- ifelse(t<99, hlos4a, hlos4b)
    clos4 <- ifelse(t<99, clos4a, clos4b)
# collapse into total length of stay for any hospitalization
    rho1 <- (hlos1*hosp1 + clos1*cc1)/phi1
    rho2 <- (hlos2*hosp2 + clos2*cc2)/phi2
    rho3 <- (hlos3*hosp3 + clos3*cc3)/phi3
    rho4 <- (hlos4*hosp4 + clos4*cc4)/phi4
    
# death fraction for each age group
# determined by the weighted average of deaths in non-ICU and ICU
# death fraction for age groups 3 and 4 change over time
    dh3 <- ifelse(t<160, dh3, dh3_2)
    dh4 <- ifelse(t<160, dh4, dh4_2)
    dc3 <- ifelse(t<160, dc3, dc3_2)
    dc4 <- ifelse(t<160, dc4, dc4_2)
# collapse into all deaths in the hospital (ICU and non-ICU)
    psi1 <- (hosp1*dh1 + cc1*dc1)/phi1
    psi2 <- (hosp2*dh2 + cc2*dc2)/phi2
    psi3 <- (hosp3*dh3 + cc3*dc3)/phi3
    psi4 <- (hosp4*dh4 + cc4*dc4)/phi4
    
# population of groups entering and leaving the system
    N1  <- S1+E1+I1+A1+R1
    N2  <- S2+E2+I2+A2+R2
    N3  <- S3+E3+I3+A3+R3
    N4  <- S4+E4+I4+A4+R4

# total population (sum up all people in all compartments)    
    N   <- S1+S2+S3+S4+E1+E2+E3+E4+I1+I2+I3+I4+A1+A2+A3+A4+II1+II2+II3+II4+Ih1+Ih2+Ih3+Ih4+R1+R2+R3+R4+D1+D2+D3+D4
    
# change equations
    dS1  <- -(I1+I2+I3+I4)*(beta*lambda*S1*(1-delta))/N - (beta*S1*(A1+A2+A3+A4)*(1-delta))/N + (1-epsilon)*mu1 - (S1/N1)*eta1
    dE1  <- -E1/alpha + (I1+I2+I3+I4)*(beta*lambda*S1*(1-delta))/N + (beta*S1*(A1+A2+A3+A4)*(1-delta))/N + ((4/13)*epsilon)*mu1 - (E1/N1)*eta1
    dI1  <- (E1*sigma1)/alpha - I1*theta*kappa*tau - I1*gamma + ((4.5/13)*epsilon)*mu1 - (I1/N1)*eta1                 
    dA1  <- E1/alpha - (E1*sigma1)/alpha - A1*theta*kappa*tau - A1*gamma + ((4.5/13)*epsilon)*mu1 - (A1/N1)*eta1
    dII1 <- -II1*sigma1*gamma + I1*theta*kappa*tau + A1*theta*kappa*tau                            
    dIh1 <- I1*phi1*gamma + II1*sigma1*phi1*gamma - Ih1/rho1
    dR1  <- II1*sigma1*gamma - I1*phi1*gamma - II1*sigma1*phi1*gamma + I1*gamma + A1*gamma + Ih1/rho1 - (psi1*Ih1)/rho1 - (R1/N1)*eta1
    dD1  <- (psi1*Ih1)/rho1
      
    dS2  <- -(I1+I2+I3+I4)*(beta*lambda*S2*(1-delta))/N - (beta*S2*(A1+A2+A3+A4)*(1-delta))/N + (1-epsilon)*mu2 - (S2/N2)*eta2
    dE2  <- -E2/alpha + (I1+I2+I3+I4)*(beta*lambda*S2*(1-delta))/N + (beta*S2*(A1+A2+A3+A4)*(1-delta))/N + ((4/13)*epsilon)*mu2 - (E2/N2)*eta2 
    dI2  <- (E2*sigma2)/alpha - I2*theta*kappa*tau - I2*gamma + ((4.5/13)*epsilon)*mu2 - (I2/N2)*eta2                    
    dA2  <- E2/alpha - (E2*sigma2)/alpha - A2*theta*kappa*tau - A2*gamma + ((4.5/13)*epsilon)*mu2 - (A2/N2)*eta2     
    dII2 <- -II2*sigma2*gamma + I2*theta*kappa*tau + A2*theta*kappa*tau                            
    dIh2 <- I2*phi2*gamma + II2*sigma2*phi2*gamma - Ih2/rho2
    dR2  <- II2*sigma2*gamma - I2*phi2*gamma - II2*sigma2*phi2*gamma + I2*gamma + A2*gamma + Ih2/rho2 - (psi2*Ih2)/rho2 - (R2/N2)*eta2
    dD2  <- (psi2*Ih2)/rho2
    
    dS3  <- -(I1+I2+I3+I4)*(beta*lambda*S3*(1-delta))/N - (beta*S3*(A1+A2+A3+A4)*(1-delta))/N + (1-epsilon)*mu3 - (S3/N3)*eta3
    dE3  <- -E3/alpha + (I1+I2+I3+I4)*(beta*lambda*S3*(1-delta))/N + (beta*S3*(A1+A2+A3+A4)*(1-delta))/N + ((4/13)*epsilon)*mu3 - (E3/N3)*eta3 
    dI3  <- (E3*sigma3)/alpha - I3*theta*kappa*tau - I3*gamma + ((4.5/13)*epsilon)*mu3 - (I3/N3)*eta3               
    dA3  <- E3/alpha - (E3*sigma3)/alpha - A3*theta*kappa*tau - A3*gamma + ((4.5/13)*epsilon)*mu3 - (A3/N3)*eta3 
    dII3 <- -II3*sigma3*gamma + I3*theta*kappa*tau + A3*theta*kappa*tau                            
    dIh3 <- I3*phi3*gamma + II3*sigma3*phi3*gamma - Ih3/rho3
    dR3  <- II3*sigma3*gamma - I3*phi3*gamma - II3*sigma3*phi3*gamma + I3*gamma + A3*gamma + Ih3/rho3 - (psi3*Ih3)/rho3 - (R3/N3)*eta3
    dD3  <- (psi3*Ih3)/rho3
    
    dS4  <- -(I1+I2+I3+I4)*(beta*lambda*S4*(1-delta))/N - (beta*S4*(A1+A2+A3+A4)*(1-delta))/N + (1-epsilon)*mu4 - (S4/N4)*eta4
    dE4  <- -E4/alpha + (I1+I2+I3+I4)*(beta*lambda*S4*(1-delta))/N + (beta*S4*(A1+A2+A3+A4)*(1-delta))/N + ((4/13)*epsilon)*mu4 - (E4/N4)*eta4 
    dI4  <- (E4*sigma4)/alpha - I4*theta*kappa*tau - I4*gamma + ((4.5/13)*epsilon)*mu4 - (I4/N4)*eta4               
    dA4  <- E4/alpha - (E4*sigma4)/alpha - A4*theta*kappa*tau - A4*gamma + ((4.5/13)*epsilon)*mu4 - (A4/N4)*eta4   
    dII4 <- -II4*sigma4*gamma + I4*theta*kappa*tau + A4*theta*kappa*tau                            
    dIh4 <- I4*phi4*gamma + II4*sigma4*phi4*gamma - Ih4/rho4
    dR4  <- II4*sigma4*gamma - I4*phi4*gamma - II4*sigma4*phi4*gamma + I4*gamma + A4*gamma + Ih4/rho4 - (psi4*Ih4)/rho4 - (R4/N4)*eta4
    dD4  <- (psi4*Ih4)/rho4

# differential equation output obtained from solver    
    der <- c(dS1, dE1, dI1, dA1, dII1, dIh1, dR1, dD1,
             dS2, dE2, dI2, dA1, dII2, dIh2, dR2, dD2,
             dS3, dE3, dI3, dA3, dII3, dIh3, dR3, dD3,
             dS4, dE4, dI4, dA4, dII4, dIh4, dR4, dD4)

# function outputs, including additional requested outputs    
    list(der,
         inc=(I1+I2+I3+I4+A1+A2+A3+A4)/9,
         Itotal=I1+I2+I3+I4+A1+A2+A3+A4,
         Iht=Ih1+Ih2+Ih3+Ih4,
         N1=S1+E1+I1+A1+R1,
         N2=S2+E2+I2+A2+R2,
         N3=S3+E3+I3+A3+R3,
         N4=S4+E4+I4+A4+R4,
         Ntotal=S1+S2+S3+S4+E1+E2+E3+E4+I1+I2+I3+I4+A1+A2+A3+A4+II1+II2+II3+II4+Ih1+Ih2+Ih3+Ih4+R1+R2+R3+R4+D1+D2+D3+D4, N) # should be identical to N defined at top
  })
}

# obtain model output as a data frame
output1_early <- as.data.frame(lsoda(y=inits, times=Day, func=seir_ski1, parms=params1))

# test fitted curve
plot(region_hosp_early$Iht, lwd=6,  type='h', col='orange', ann=FALSE, axes=FALSE)
lines(output1_early$Iht, lwd=2, type='l', col='darkblue')

# define a function to calculate the residual sum of squares (RSS), passing in the parameters
# that are to be optimized for the best fit to the daily hospitalization data
RSS1 <- function(params1) {
  names(params1) <- c("beta", "delta2", "delta3")
  output1_early <- lsoda(y=inits, times=Day, func=seir_ski1, parms=params1)
  fit <- output1_early[, "Iht"]
  sum((hosp.vec - fit)^2)
}

# use the optim command and establish upper and lower bounds for parameter estimates
opt <- optim(par=c(0.4472, 0.3091, 0.99),
               fn=RSS1,
               method="L-BFGS-B",
               lower=c(0.4, 0.4, 0.8), 
               upper =c(0.6, 0.7, 0.99))


# check for convergence
opt$message
opt$value

# now examine the fitted values for the parameters of interest
opt_par <- setNames(opt$par, c("beta", "delta2", "delta3"))
opt_par

# if the parameters make sense, proceed to run the model a second time
params2 <- list(
  beta=0.4931,
  delta2=0.5868,
  delta3=0.99)
 
seir_ski2 <- function(t, state, parameters) {
  
  with(as.list(c(state, parameters)), {
  
# age-structure the  entry and exit parameters by multiplying by the proportionate age distribution
    mu1 <- pop_travel$mu[[t]]*(45228/205382)
    mu2 <- pop_travel$mu[[t]]*(62369/205382)
    mu3 <- pop_travel$mu[[t]]*(67646/205382)
    mu4 <- pop_travel$mu[[t]]*(30139/205382) 
    eta1 <- pop_travel$eta[[t]]*(45228/205382)
    eta2 <- pop_travel$eta[[t]]*(62369/205382)
    eta3 <- pop_travel$eta[[t]]*(67646/205382)
    eta4 <- pop_travel$eta[[t]]*(30139/205382)
    
# national prevalence of COVID-19 (using NYT past data and covid19sim projections)
    epsilon <- pop_travel$epsilon[[t]] 

# population estimate
    n  <- 205382 # total population of Summit, Grand, Eagle, Pitkin, Routt, and Garfield Counties
    n1 <- 45228 # population age group 1
    n2 <- 62369 # 2
    n3 <- 67646 # 3
    n4 <- 30139 # 4
    
# time-varying transmission control (TC) parameters, represented by delta
    delta <- ifelse(t<t2, delta1, delta2)
    
# toggle CT/CI on or off after December 12th, 2020
    kappa <- ifelse(t<316, 0, kappa)
    
# proportion of symptomatic cases in each age group that end up hospitalized (including ICU)
# critical care needs change over time for age groups 2, 3, and 4   
    cc2 <- ifelse(t < 147, cc2a, ifelse(t < 234, cc2b, cc2c))
    cc3 <- ifelse(t < 147, cc3a, ifelse(t < 234, cc3b, cc3c))
    cc4 <- ifelse(t < 147, cc4a, ifelse(t < 234, cc4b, cc4c))
    
# collapse hospital and ICU into grand total hospitalizations
    phi1 <- hosp1 + cc1
    phi2 <- hosp2 + cc2
    phi3 <- hosp3 + cc3
    phi4 <- hosp4 + cc4
    
# length of stay in the general hospital for each age group
# determined by the weighted average of lengths of stay in non-ICU and ICU

# length of stay in hospital and ICU changes for each age group over time
    hlos1 <- ifelse(t<99, hlos1a, hlos1b)    
    clos1 <- ifelse(t<99, clos1a, clos1b)
    hlos2 <- ifelse(t<99, hlos2a, hlos2b)
    clos2 <- ifelse(t<99, clos2a, clos2b)
    hlos3 <- ifelse(t<99, hlos3a, hlos3b)
    clos3 <- ifelse(t<99, clos3a, clos3b)
    hlos4 <- ifelse(t<99, hlos4a, hlos4b)
    clos4 <- ifelse(t<99, clos4a, clos4b)
# collapse into total length of stay for any hospitalization
    rho1 <- (hlos1*hosp1 + clos1*cc1)/phi1
    rho2 <- (hlos2*hosp2 + clos2*cc2)/phi2
    rho3 <- (hlos3*hosp3 + clos3*cc3)/phi3
    rho4 <- (hlos4*hosp4 + clos4*cc4)/phi4
    
# death fraction for each age group
# determined by the weighted average of deaths in non-ICU and ICU
# death fraction for age groups 3 and 4 change over time
    dh3 <- ifelse(t<160, dh3, dh3_2)
    dh4 <- ifelse(t<160, dh4, dh4_2)
    dc3 <- ifelse(t<160, dc3, dc3_2)
    dc4 <- ifelse(t<160, dc4, dc4_2)
# collapse into all deaths in the hospital (ICU and non-ICU)
    psi1 <- (hosp1*dh1 + cc1*dc1)/phi1
    psi2 <- (hosp2*dh2 + cc2*dc2)/phi2
    psi3 <- (hosp3*dh3 + cc3*dc3)/phi3
    psi4 <- (hosp4*dh4 + cc4*dc4)/phi4
    
# population of groups entering and leaving the system
    N1  <- S1+E1+I1+A1+R1
    N2  <- S2+E2+I2+A2+R2
    N3  <- S3+E3+I3+A3+R3
    N4  <- S4+E4+I4+A4+R4

# total population (sum up all people in all compartments)    
    N   <- S1+S2+S3+S4+E1+E2+E3+E4+I1+I2+I3+I4+A1+A2+A3+A4+II1+II2+II3+II4+Ih1+Ih2+Ih3+Ih4+R1+R2+R3+R4+D1+D2+D3+D4
    
# change equations
    dS1  <- -(I1+I2+I3+I4)*(beta*lambda*S1*(1-delta))/N - (beta*S1*(A1+A2+A3+A4)*(1-delta))/N + (1-epsilon)*mu1 - (S1/N1)*eta1
    dE1  <- -E1/alpha + (I1+I2+I3+I4)*(beta*lambda*S1*(1-delta))/N + (beta*S1*(A1+A2+A3+A4)*(1-delta))/N + ((4/13)*epsilon)*mu1 - (E1/N1)*eta1
    dI1  <- (E1*sigma1)/alpha - I1*theta*kappa*tau - I1*gamma + ((4.5/13)*epsilon)*mu1 - (I1/N1)*eta1                 
    dA1  <- E1/alpha - (E1*sigma1)/alpha - A1*theta*kappa*tau - A1*gamma + ((4.5/13)*epsilon)*mu1 - (A1/N1)*eta1
    dII1 <- -II1*sigma1*gamma + I1*theta*kappa*tau + A1*theta*kappa*tau                            
    dIh1 <- I1*phi1*gamma + II1*sigma1*phi1*gamma - Ih1/rho1
    dR1  <- II1*sigma1*gamma - I1*phi1*gamma - II1*sigma1*phi1*gamma + I1*gamma + A1*gamma + Ih1/rho1 - (psi1*Ih1)/rho1 - (R1/N1)*eta1
    dD1  <- (psi1*Ih1)/rho1
      
    dS2  <- -(I1+I2+I3+I4)*(beta*lambda*S2*(1-delta))/N - (beta*S2*(A1+A2+A3+A4)*(1-delta))/N + (1-epsilon)*mu2 - (S2/N2)*eta2
    dE2  <- -E2/alpha + (I1+I2+I3+I4)*(beta*lambda*S2*(1-delta))/N + (beta*S2*(A1+A2+A3+A4)*(1-delta))/N + ((4/13)*epsilon)*mu2 - (E2/N2)*eta2 
    dI2  <- (E2*sigma2)/alpha - I2*theta*kappa*tau - I2*gamma + ((4.5/13)*epsilon)*mu2 - (I2/N2)*eta2                    
    dA2  <- E2/alpha - (E2*sigma2)/alpha - A2*theta*kappa*tau - A2*gamma + ((4.5/13)*epsilon)*mu2 - (A2/N2)*eta2     
    dII2 <- -II2*sigma2*gamma + I2*theta*kappa*tau + A2*theta*kappa*tau                            
    dIh2 <- I2*phi2*gamma + II2*sigma2*phi2*gamma - Ih2/rho2
    dR2  <- II2*sigma2*gamma - I2*phi2*gamma - II2*sigma2*phi2*gamma + I2*gamma + A2*gamma + Ih2/rho2 - (psi2*Ih2)/rho2 - (R2/N2)*eta2
    dD2  <- (psi2*Ih2)/rho2
    
    dS3  <- -(I1+I2+I3+I4)*(beta*lambda*S3*(1-delta))/N - (beta*S3*(A1+A2+A3+A4)*(1-delta))/N + (1-epsilon)*mu3 - (S3/N3)*eta3
    dE3  <- -E3/alpha + (I1+I2+I3+I4)*(beta*lambda*S3*(1-delta))/N + (beta*S3*(A1+A2+A3+A4)*(1-delta))/N + ((4/13)*epsilon)*mu3 - (E3/N3)*eta3 
    dI3  <- (E3*sigma3)/alpha - I3*theta*kappa*tau - I3*gamma + ((4.5/13)*epsilon)*mu3 - (I3/N3)*eta3               
    dA3  <- E3/alpha - (E3*sigma3)/alpha - A3*theta*kappa*tau - A3*gamma + ((4.5/13)*epsilon)*mu3 - (A3/N3)*eta3 
    dII3 <- -II3*sigma3*gamma + I3*theta*kappa*tau + A3*theta*kappa*tau                            
    dIh3 <- I3*phi3*gamma + II3*sigma3*phi3*gamma - Ih3/rho3
    dR3  <- II3*sigma3*gamma - I3*phi3*gamma - II3*sigma3*phi3*gamma + I3*gamma + A3*gamma + Ih3/rho3 - (psi3*Ih3)/rho3 - (R3/N3)*eta3
    dD3  <- (psi3*Ih3)/rho3
    
    dS4  <- -(I1+I2+I3+I4)*(beta*lambda*S4*(1-delta))/N - (beta*S4*(A1+A2+A3+A4)*(1-delta))/N + (1-epsilon)*mu4 - (S4/N4)*eta4
    dE4  <- -E4/alpha + (I1+I2+I3+I4)*(beta*lambda*S4*(1-delta))/N + (beta*S4*(A1+A2+A3+A4)*(1-delta))/N + ((4/13)*epsilon)*mu4 - (E4/N4)*eta4 
    dI4  <- (E4*sigma4)/alpha - I4*theta*kappa*tau - I4*gamma + ((4.5/13)*epsilon)*mu4 - (I4/N4)*eta4               
    dA4  <- E4/alpha - (E4*sigma4)/alpha - A4*theta*kappa*tau - A4*gamma + ((4.5/13)*epsilon)*mu4 - (A4/N4)*eta4   
    dII4 <- -II4*sigma4*gamma + I4*theta*kappa*tau + A4*theta*kappa*tau                            
    dIh4 <- I4*phi4*gamma + II4*sigma4*phi4*gamma - Ih4/rho4
    dR4  <- II4*sigma4*gamma - I4*phi4*gamma - II4*sigma4*phi4*gamma + I4*gamma + A4*gamma + Ih4/rho4 - (psi4*Ih4)/rho4 - (R4/N4)*eta4
    dD4  <- (psi4*Ih4)/rho4

# differential equation output obtained from solver    
    der <- c(dS1, dE1, dI1, dA1, dII1, dIh1, dR1, dD1,
             dS2, dE2, dI2, dA1, dII2, dIh2, dR2, dD2,
             dS3, dE3, dI3, dA3, dII3, dIh3, dR3, dD3,
             dS4, dE4, dI4, dA4, dII4, dIh4, dR4, dD4)

# function outputs, including additional requested outputs    
    list(der,
         inc=(I1+I2+I3+I4+A1+A2+A3+A4)/9,
         Itotal=I1+I2+I3+I4+A1+A2+A3+A4,
         Iht=Ih1+Ih2+Ih3+Ih4,
         N1=S1+E1+I1+A1+R1,
         N2=S2+E2+I2+A2+R2,
         N3=S3+E3+I3+A3+R3,
         N4=S4+E4+I4+A4+R4,
         Ntotal=S1+S2+S3+S4+E1+E2+E3+E4+I1+I2+I3+I4+A1+A2+A3+A4+II1+II2+II3+II4+Ih1+Ih2+Ih3+Ih4+R1+R2+R3+R4+D1+D2+D3+D4, N) # should be identical to N defined at top
  })
}
# obtain final model output and create date variable
output2_early <- as.data.frame(lsoda(y=inits, times=Day, func=seir_ski2, parms=params2))
output2_early$date <- as.Date(output2_early$time, format="%m/%d/%Y", origin="01/24/2020")

# export the final output dataset
write.csv(output2_early, str_c(path, "Thesis/Data/output2_early.csv", row.names=F))

# report final parameter estimates
round(opt_par, 4)

# fitted curve after estimating early outbreak parameters

plot(region_hosp_early$Iht, lwd=8,  type='h', xlim=c(39, 76), ylim=c(0, 40), xlab="Date", ylab="Active Hospitalizations", axes=FALSE,
     main="Curve Fit to COVID-19 Regional Hospital Census\nEarly Outbreak in Eagle, Garfield, Grand, Pitkin, Routt, and Summit Counties",
     col='orange', cex.lab=1.2, cex.main=1.5)
lines(output2_early$Iht, lwd=3, type='l', col='darkblue')
axis(1, at=c(-14,  39,      46,     53,       60,      67,       74,     81,      88,      95,     102,      109), 
     labels=c('',"03/01", "03/08", "03/15", "03/22", "03/29", "04/05", "04/12", "04/19", "04/26", "05/03", "05/10"), col="black", col.axis="black", cex.axis=1.2)
axis(2, at=c( -5,  0, 5, 10, 15, 20, 25, 30, 35, 40), col="black", col.axis="black", cex.axis=1.2)
legend(42, 25, c("Reported Hospitalizations", "Fitted Curve"), fill=c("orange", "darkblue", "darkblue"), bty="n", text.col=c("black"),cex=1.5 )

