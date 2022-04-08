# turn off scientific notation
options(scipen=999)

# designate filepath
path <- "C:/Users/Emma J. Wu/Documents/Writing Material/Manuscripts/COVID-19 Ski Paper/"

# load libraries
library(pacman)
p_load(coda, deSolve, dplyr, FME, ggplot2, Hmisc, rootSolve, scales, stringr)

n  <- 205382 # total population of Summit, Grand, Eagle, Pitkin, Routt, and Garfield Counties
n1 <- 45228 # population age group 1
n2 <- 62369 # 2
n3 <- 67646 # 3
n4 <- 30139 # 4

# base parameter list (includes everything except kappa and tc21)
params1 <- list(beta=0.4931, # infection rate, which is a product of transmission-relevant contact rate and probability of transmission given that contact
                gamma=1/9, # recovery rate, which is 1/average length of infection
                alpha=4.2, # latent period, which 1 day shorter than the incubation period
                lambda=1.395, # ratio of infectiousness (symptomatic vs. asymptomatic)
                sigma1=0.1100230677, sigma2=0.3570515767, sigma3=0.5612057201, sigma4=0.7748797315, # proportion of cases that are symptomatic for each age group 
                theta=0.3, # proportion of total infections detected
                tau=0.049, # contact tracing collapsed into a single parameter (see Appendix C)
                hosp1=0.01743, hosp2=0.02909127, hosp3=0.032122, hosp4=0.0501384, # proportion of sympomatic cases in each age group that end up hospitalized
                cc1=0.0052754, cc2a=0.007474, cc2b=0.00838, cc2c=0.00591, cc3a=0.01866, cc3b=0.01785, cc3c=0.012226,
                cc4a=0.02885, cc4b=0.02234, cc4c=0.0202, # proportion sympomatics in each age group that end up in the ICU
                hlos1a=4.05, hlos1b=5.0, hlos2a=5.49, hlos2b=3.6, hlos3a=7.36, hlos3b=5.3, hlos4a=10.02, hlos4b=7.6, # length of stay (non-ICU) for each age group
                clos1a=7.00, clos1b=4, clos2a=9.91, clos2b=4.4, clos3a=13.47, clos3b=8.5, clos4a=10.82, clos4b=8, # length of stay (ICU) for each age group
                dh1=0, dh2=0, dh3a=0.01, dh3b=0.00347, dh4a=0.14106, dh4b=0.06086, # death fraction (non-ICU) for each age group
                dc1=0.0408, dc2=0.0573, dc3a=0.16498, dc3b=0.1101, dc4a=0.39066, dc4b=0.30275, # death fraction (ICU) for each age group
                t2=50, t3=64, t4=78, t5=92, t6=106, t7=120, t8=134, t9=148, t10=162, t11=176, t12=190,
                t13=204, t14=218, t15=232, t16=246, t17=260, t18=274, t19=288, t20=302, t21=316, # biweekly time triggers
                tc1=0, tc2=0.5868, tc3=0.9900, tc4=0.7524, tc5=0.8496, tc6=0.6452,
                tc7=0.7832,  tc8=0.7717, tc9=0.8031, tc10=0.6642, tc11=0.7959, tc12=0.9696,
                tc13=0.9177, tc14=0.8150, tc15=0.7721, tc16=0.7465, tc17=0.4958, tc18=0.7434, tc19=0.5136,
                tc20=0.7146, kappa=0) # time-varying transmission control parameters (fitted)# time-varying transmission control parameters (fitted)


# restrict pertinent datasets to 13 days after the last time trigger
regionhosp_ski1 <- regionhosp_ski[1:330,]

# plot hospitalizations
plot(regionhosp_ski1$totalhosp, lwd=6, type='h', xlab="Date", ylab="Active Hospitalizations",
     main="Regional Hospital Census", col='orange', axes=FALSE)
axis(1, seq(0, 340, 20))
axis(2, seq(0, 150, 5))

# specify initial values
inits <- c(S1=45228-1, E1=0, I1=1, A1=0, II1=0, Ih1=0, R1=0, D1=0,
           S2=62369,   E2=0, I2=0, A2=0, II2=0, Ih2=0, R2=0, D2=0,
           S3=67646,   E3=0, I3=0, A3=0, II3=0, Ih3=0, R3=0, D3=0,
           S4=30139,   E4=0, I4=0, A4=0, II4=0, Ih4=0, R4=0, D4=0)

# set up the vector for daily hospitalizations, which we need for optimization
Hosp <- regionhosp_ski1[c("date", "totalhosp")]
hosp.vec <- as.vector(Hosp$totalhosp)

# set up the incrementing time vector for the SEIR model
Day <- 1:(length(regionhosp_ski1$totalhosp))

# check to make sure the time and hospitalization vectors are the same length

length(hosp.vec)
length(Day)

# build the SEIR model as an R function

seir_ski <- function(t, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    # age-structure the  entry and exit parameters by multiplying by the proportionate age distribution
    entry1 <- skivisits$entry[[t]]*(n1/n)
    entry2 <- skivisits$entry[[t]]*(n2/n)
    entry3 <- skivisits$entry[[t]]*(n3/n)
    entry4 <- skivisits$entry[[t]]*(n4/n) 
    exit1 <- skivisits$exit[[t]]*(n1/n)
    exit2 <- skivisits$exit[[t]]*(n2/n)
    exit3 <- skivisits$exit[[t]]*(n3/n)
    exit4 <- skivisits$exit[[t]]*(n4/n)
    
    # probability of viral import from visitors
    import <- skivisits$import[[t]]
    
    # time-varying transmission control (TC) parameters, represented by tc
    tc <- ifelse(t<t2, tc1, ifelse(t<t3, tc2, ifelse(t<t4, tc3, ifelse(t<t5, tc4, ifelse(t<t6, tc5,
          ifelse(t<t7, tc6, ifelse(t<t8, tc7, ifelse(t<t9, tc8, ifelse(t<t10, tc9, ifelse(t<t11, tc10,
          ifelse(t<t12, tc11, ifelse(t<t13, tc12, ifelse(t<t14, tc13, ifelse(t<t15, tc14, ifelse(t<t16,
          tc15, ifelse(t<t17, tc16, ifelse(t<t18, tc17, ifelse(t<t19, tc18, ifelse(t<t20, tc19, tc20)))))))))))))))))))
    
    # toggle CT/CI on or off after December 12th, 2020
    kappa <- ifelse(t<316, 0, kappa)
    
    # proportion of symptomatic cases in each age group that end up hospitalized (including ICU)
    # critical care needs change over time for age groups 2, 3, and 4   
    cc2 <- ifelse(t<147, cc2a, ifelse(t<234, cc2b, cc2c))
    cc3 <- ifelse(t<147, cc3a, ifelse(t<234, cc3b, cc3c))
    cc4 <- ifelse(t<147, cc4a, ifelse(t<234, cc4b, cc4c))
    
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
    dh3 <- ifelse(t<160, dh3a, dh3b)
    dh4 <- ifelse(t<160, dh4a, dh4b)
    dc3 <- ifelse(t<160, dc3a, dc3b)
    dc4 <- ifelse(t<160, dc4a, dc4b)
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
    dS1  <- -(I1+I2+I3+I4)*(beta*lambda*S1*(1-tc))/N - (beta*S1*(A1+A2+A3+A4)*(1-tc))/N + (1-import)*entry1 - (S1/N1)*exit1
    dE1  <- -E1/alpha + (I1+I2+I3+I4)*(beta*lambda*S1*(1-tc))/N + (beta*S1*(A1+A2+A3+A4)*(1-tc))/N + ((4/13)*import)*entry1 - (E1/N1)*exit1
    dI1  <- (E1*sigma1)/alpha - I1*theta*kappa*tau - I1*gamma + ((4.5/13)*import)*entry1 - (I1/N1)*exit1                 
    dA1  <- E1/alpha - (E1*sigma1)/alpha - A1*theta*kappa*tau - A1*gamma + ((4.5/13)*import)*entry1 - (A1/N1)*exit1
    dII1 <- -II1*sigma1*gamma + I1*theta*kappa*tau + A1*theta*kappa*tau                            
    dIh1 <- I1*phi1*gamma + II1*sigma1*phi1*gamma - Ih1/rho1
    dR1  <- II1*sigma1*gamma - I1*phi1*gamma - II1*sigma1*phi1*gamma + I1*gamma + A1*gamma + Ih1/rho1 - (psi1*Ih1)/rho1 - (R1/N1)*exit1
    dD1  <- (psi1*Ih1)/rho1
    
    dS2  <- -(I1+I2+I3+I4)*(beta*lambda*S2*(1-tc))/N - (beta*S2*(A1+A2+A3+A4)*(1-tc))/N + (1-import)*entry2 - (S2/N2)*exit2
    dE2  <- -E2/alpha + (I1+I2+I3+I4)*(beta*lambda*S2*(1-tc))/N + (beta*S2*(A1+A2+A3+A4)*(1-tc))/N + ((4/13)*import)*entry2 - (E2/N2)*exit2 
    dI2  <- (E2*sigma2)/alpha - I2*theta*kappa*tau - I2*gamma + ((4.5/13)*import)*entry2 - (I2/N2)*exit2                    
    dA2  <- E2/alpha - (E2*sigma2)/alpha - A2*theta*kappa*tau - A2*gamma + ((4.5/13)*import)*entry2 - (A2/N2)*exit2     
    dII2 <- -II2*sigma2*gamma + I2*theta*kappa*tau + A2*theta*kappa*tau                            
    dIh2 <- I2*phi2*gamma + II2*sigma2*phi2*gamma - Ih2/rho2
    dR2  <- II2*sigma2*gamma - I2*phi2*gamma - II2*sigma2*phi2*gamma + I2*gamma + A2*gamma + Ih2/rho2 - (psi2*Ih2)/rho2 - (R2/N2)*exit2
    dD2  <- (psi2*Ih2)/rho2
    
    dS3  <- -(I1+I2+I3+I4)*(beta*lambda*S3*(1-tc))/N - (beta*S3*(A1+A2+A3+A4)*(1-tc))/N + (1-import)*entry3 - (S3/N3)*exit3
    dE3  <- -E3/alpha + (I1+I2+I3+I4)*(beta*lambda*S3*(1-tc))/N + (beta*S3*(A1+A2+A3+A4)*(1-tc))/N + ((4/13)*import)*entry3 - (E3/N3)*exit3 
    dI3  <- (E3*sigma3)/alpha - I3*theta*kappa*tau - I3*gamma + ((4.5/13)*import)*entry3 - (I3/N3)*exit3               
    dA3  <- E3/alpha - (E3*sigma3)/alpha - A3*theta*kappa*tau - A3*gamma + ((4.5/13)*import)*entry3 - (A3/N3)*exit3 
    dII3 <- -II3*sigma3*gamma + I3*theta*kappa*tau + A3*theta*kappa*tau                            
    dIh3 <- I3*phi3*gamma + II3*sigma3*phi3*gamma - Ih3/rho3
    dR3  <- II3*sigma3*gamma - I3*phi3*gamma - II3*sigma3*phi3*gamma + I3*gamma + A3*gamma + Ih3/rho3 - (psi3*Ih3)/rho3 - (R3/N3)*exit3
    dD3  <- (psi3*Ih3)/rho3
    
    dS4  <- -(I1+I2+I3+I4)*(beta*lambda*S4*(1-tc))/N - (beta*S4*(A1+A2+A3+A4)*(1-tc))/N + (1-import)*entry4 - (S4/N4)*exit4
    dE4  <- -E4/alpha + (I1+I2+I3+I4)*(beta*lambda*S4*(1-tc))/N + (beta*S4*(A1+A2+A3+A4)*(1-tc))/N + ((4/13)*import)*entry4 - (E4/N4)*exit4 
    dI4  <- (E4*sigma4)/alpha - I4*theta*kappa*tau - I4*gamma + ((4.5/13)*import)*entry4 - (I4/N4)*exit4               
    dA4  <- E4/alpha - (E4*sigma4)/alpha - A4*theta*kappa*tau - A4*gamma + ((4.5/13)*import)*entry4 - (A4/N4)*exit4   
    dII4 <- -II4*sigma4*gamma + I4*theta*kappa*tau + A4*theta*kappa*tau                            
    dIh4 <- I4*phi4*gamma + II4*sigma4*phi4*gamma - Ih4/rho4
    dR4  <- II4*sigma4*gamma - I4*phi4*gamma - II4*sigma4*phi4*gamma + I4*gamma + A4*gamma + Ih4/rho4 - (psi4*Ih4)/rho4 - (R4/N4)*exit4
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
out1 <- as.data.frame(lsoda(y=inits, times=Day, func=seir_ski, parms=params1))
out1$date <- as.Date(out1$time, format="%m/%d/%Y", origin="01/23/2020")

# test fitted curve
plot(regionhosp_ski1$totalhosp, lwd=6,  type='h', col='orange', ann=FALSE, axes=FALSE)
lines(out1$Iht, lwd=2, type='l', col='darkblue')

# define a function to calculate the residual sum of squares (RSS), passing in the parameters
# that are to be optimized for the best fit to the daily hospitalization data
RSS1 <- function(params1) {
  names(params1) <- c("tc19", "tc20")
  out1 <- lsoda(y=inits, times=Day, func=seir_ski, parms=params1)
  fit <- out1[, "Iht"]
  sum((hosp.vec - fit)^2)
}

# use the optim command and establish upper and lower bounds for parameter estimates
opt <- optim(par=c(0.4132, 0.7540),
               fn=RSS1,
               method="L-BFGS-B",
               lower=c(0.3, 0.3),
               upper =c(0.99, 0.99))

opt$message
opt$value
# now examine the fitted values for the parameters of interest
opt_par <- setNames(opt$par,  c("tc19", "tc20"))
opt_par

round(opt_par, 4)

# export modeling output as csv
write.csv(out1, str_c(path, "Data/out1_final.csv"))

# subset and merge reported to estimated hospitalizations
out1_finalhosp <- out1[c("date", "Iht")]
curvefit <- merge(out1_finalhosp, regionhosp_ski1, "date")

# plot hospitalization curve fit
curvefit_plot <- ggplot(data=curvefit) +
  geom_col(aes(x=date, y=totalhosp, fill="fill1"), size=1.5, alpha=0.9) +
  geom_line(aes(x=date, y=Iht, color="color1"), size=1.5) +
  labs(x="Date", y="Count of Patients in Hospital") +
  ggtitle("Curve Fit to COVID-19 Active Hospitalizations in Summit, Eagle,\nGrand, Pitkin, Routt, and Garfield Counties, Colorado, USA") + 
  scale_x_date(date_labels="%m/%d/%y", date_breaks="3 weeks", limits=c(as.Date("2020-03-01"), as.Date("2020-12-21"))) +
  scale_y_continuous(limits=c(0, 50), breaks=seq(0, 50, 10)) +
  scale_color_manual(labels="Fitted Curve",
                     values="firebrick1") +
  scale_fill_manual(labels="Reported Hospitalizations",
                    values="grey3") +
  theme(panel.grid.minor=element_blank(),
        plot.margin=unit(c(5, 30, 10, 10), "pt"),
        plot.title=element_text(size=26, face="bold", hjust=0.5, margin=margin(10, 0, 10, 0, "pt")),
        plot.subtitle=element_text(size=22, hjust=0.5, margin=margin(0, 0, 10, 0)),
        legend.title=element_blank(),
        legend.background=element_blank(),
        legend.key=element_rect(color=NA, fill=NA),
        legend.position="bottom",
        legend.key.size=unit(1.2, "cm"),
        legend.text=element_text(size=20, margin=margin(0, 20, 0, 0, "pt")),
        axis.title.x=element_text(size=20),
        axis.title.y=element_text(size=20, margin=margin(0, 20, 0, 0, "pt")),
        axis.text.x=element_text(size=18, angle=30, margin=margin(20, 0, 0, 0, "pt")),
        axis.text.y=element_text(size=18, margin=margin(0, 10, 0, 0, "pt")),
        axis.ticks.x=element_line(color="grey"),
        axis.ticks.y=element_blank(),
        axis.ticks.length.x=unit(0.25, "cm"))
curvefit_plot

ggsave(str_c(path, "Figures/curvefit.png"), height=8, width=15, plot=curvefit_plot)
