##### AGE-STRUCTURED SEIR TRANSMISSION CONTROL (TC) MODEL FOR SIX-COUNTY SKI REGION #####

# turn off scientific notation
options(scipen=999)

# set file paths
path <- "/Users/emilywu883/Documents/CU Anschutz/COVID-19/"

# load packages
library(pacman)
p_load(data.table, deSolve, dplyr, ggplot2, ggpubr, scales, zoo)

# read in visitor data
visit <- read.csv(str_c(path, "Thesis/Data/visit_prev_data.csv"))
visit$date <- as.Date(visit$date, "%m/%d/%y")

# plot high and low visitation
mu_high <- ggplot(data=visit, aes(x=date, y=mu_high)) +
  geom_line(color="steelblue4", size=1.3) +
  ggtitle("Without Visitor Restriction") +
  xlab("Date") + ylab("Count") +
  scale_x_date(limits=c(as.Date("2020-01-01"), as.Date("2021-04-30")), date_breaks="2 months", date_labels=(date_format="%b %Y")) +
  scale_y_continuous(limits=c(0, 125000), breaks=seq(0, 125000, 25000), labels=scales::comma) +
  theme(panel.grid.minor=element_blank(),
        plot.margin=unit(c(5, 30, 10, 10), "pt"),
        plot.title=element_text(size=18, face="bold", hjust=0.5, margin=margin(10, 0, 10, 0, "pt")),
        plot.subtitle=element_text(size=22, hjust=0.5, margin=margin(0, 0, 10, 0)),
        legend.text=element_text(size=20, margin=margin(0, 20, 0, 0, "pt")),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=20, margin=margin(0, 20, 0, 0, "pt")),
        axis.text.x=element_text(size=18, angle=30, margin=margin(20, 0, 0, 0, "pt")),
        axis.text.y=element_text(size=18, margin=margin(0, 10, 0, 0, "pt")),
        axis.ticks.x=element_line(color="grey"),
        axis.ticks.y=element_blank(),
        axis.ticks.length.x=unit(0.25, "cm"))

mu_low <- ggplot(data=visit, aes(x=date, y=mu_low)) +
  geom_line(color="darkgreen", size=1.3) +
  ggtitle("With Visitor Restriction") +
  xlab("Date") + ylab("Count") +
  scale_x_date(limits=c(as.Date("2020-01-01"), as.Date("2021-04-30")), date_breaks="2 months", date_labels=(date_format="%b %Y")) +
  scale_y_continuous(limits=c(0, 125000), breaks=seq(0, 125000, 25000), labels=scales::comma) +
  theme(panel.grid.minor=element_blank(),
        plot.margin=unit(c(5, 30, 10, 10), "pt"),
        plot.title=element_text(size=18, face="bold", hjust=0.5, margin=margin(10, 0, 10, 0, "pt")),
        plot.subtitle=element_text(size=22, hjust=0.5, margin=margin(0, 0, 10, 0)),
        legend.text=element_text(size=20, margin=margin(0, 20, 0, 0, "pt")),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=20, margin=margin(0, 20, 0, 0, "pt")),
        axis.text.x=element_text(size=18, angle=30, margin=margin(20, 0, 0, 0, "pt")),
        axis.text.y=element_text(size=18, margin=margin(0, 10, 0, 0, "pt")),
        axis.ticks.x=element_line(color="grey"),
        axis.ticks.y=element_blank(),
        axis.ticks.length.x=unit(0.25, "cm"))

visitors <- annotate_figure(ggarrange(mu_high, mu_low, legend="none"),
                            top=text_grob("Estimated Daily Number of Outside Visitors to Summit, Eagle\nGrand, Pitkin, Routt, and Garfield Counties, January 2020 to April 2021",
                                            size=26, face="bold"))

ggsave(str_c(path, "Thesis/Figures/visitors.png"), height=7, width=18, plot=visitors)

epsilon <- ggplot(data=visit)+
  geom_bar(aes(x=date, y=epsilon_high,  fill="color1", color="color1"),alpha=0.8, stat="identity")+
  geom_bar(aes(x=date, y=epsilon_med,  fill="color2", color="color2"), alpha=0.8, stat="identity")+
  geom_bar(aes(x=date, y=epsilon_low,  fill="color3", color="color3"), alpha=0.8, stat="identity")+
  ggtitle("Estimated Prevalence of Active SARS-CoV-2 Infection in the United\nStates Under High, Medium, and Low Viral Importation Risk") +
  xlab("Date") + ylab("Percent") +
  scale_x_date(limits=c(as.Date("2020-12-04"), as.Date("2021-04-17")), date_breaks="3 weeks", date_labels=(date_format="%m/%d/%Y")) +
  scale_y_continuous(limits=c(0, 0.02), breaks=seq(0, 0.02, 0.005), labels=scales::percent) +
  scale_fill_manual(labels=c("High", "Medium", "Low"), values=c("firebrick3", "darkmagenta", "steelblue4"))+
    scale_color_manual(labels=c("High", "Medium", "Low"), values=c("firebrick3", "darkmagenta", "steelblue4"))+
    guides(color=FALSE, fill=guide_legend(override.aes=list(size=8), nrow=1, byrow=TRUE))+
theme(plot.margin=unit(c(5, 30, 10, 10), "pt"),
           plot.title=element_text(size=26, face="bold", hjust=0.5, margin=margin(10, 0, 10, 0, "pt")),
           plot.subtitle=element_text(size=22, hjust=0.5, margin=margin(0, 0, 10, 0)),
           legend.title=element_blank(), legend.background=element_blank(),
           legend.key=element_rect(color=NA, fill=NA), legend.position="bottom",
           legend.key.size=unit(1.2, "cm"), legend.text=element_text(size=20, margin=margin(0, 20, 0, 0, "pt")),
           axis.title.x=element_blank(),
           axis.title.y=element_text(size=20, margin=margin(0, 20, 0, 0, "pt")),
           axis.text.x=element_text(size=18, angle=30, margin=margin(20, 0, 0, 0, "pt")),
           axis.text.y=element_text(size=18, margin=margin(0, 10, 0, 0, "pt")),
           axis.ticks.x=element_line(color="grey"),
           axis.ticks.y=element_blank(), axis.ticks.length.x=unit(0.25, "cm"))

ggsave(str_c(path, "Thesis/Figures/epsilon.png"), height=7, width=13, plot=epsilon)

# SCENARIO 5: LOW VIRAL IMPORTATION, WITHOUT VISITOR RESTRICTION
seir_ski5 <- function(t, state, parameters) {
  
  with(as.list(c(state, parameters)), {
  
# age-structure the entry and exit parameters by multiplying by the proportionate age distribution
    mu1 <- visit$mu_high[[t]]*(45228/205382)
    mu2 <- visit$mu_high[[t]]*(62369/205382)
    mu3 <- visit$mu_high[[t]]*(67646/205382)
    mu4 <- visit$mu_high[[t]]*(30139/205382) 
    eta1 <- visit$eta_high[[t]]*(45228/205382)
    eta2 <- visit$eta_high[[t]]*(62369/205382)
    eta3 <- visit$eta_high[[t]]*(67646/205382)
    eta4 <- visit$eta_high[[t]]*(30139/205382)
    
# national prevalence (using covid19sim projections)
    epsilon <- visit$epsilon_low[[t]] 

# population estimate
    n  <- 205382 # total population of Summit, Grand, Eagle, Pitkin, Routt, and Garfield Counties
    n1 <- 45228 # population age group 1
    n2 <- 62369 # 2
    n3 <- 67646 # 3
    n4 <- 30139 # 4
    
# time-varying transmission control (TC) parameters, represented by delta
    delta <- ifelse(t<t2, delta1, ifelse(t<t3, delta2, ifelse(t<t4, delta3, ifelse(t<t5, delta4,
             ifelse(t<t6, delta5, ifelse(t<t7, delta6, ifelse(t<t8, delta7, ifelse(t<t9, delta8,
             ifelse(t<t10, delta9, ifelse(t<t11, delta10, ifelse(t<t12, delta11, ifelse(t<t13,
             delta12, ifelse(t<t14, delta13, ifelse(t<t15, delta14, ifelse(t<t16, delta15,
             ifelse(t<t17, delta16, ifelse(t<t18, delta17, ifelse(t<t19, delta18, ifelse(t<t20,
             delta19, ifelse(t<t21, delta20, delta21))))))))))))))))))))
    
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

# SCENARIO 6: LOW VIRAL IMPORTATION, WITH VISITOR RESTRICTION
seir_ski6 <- function(t, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    # age-structure the  entry and exit parameters by multiplying by the proportionate age distribution
    mu1 <- visit$mu_low[[t]]*(45228/205382)
    mu2 <- visit$mu_low[[t]]*(62369/205382)
    mu3 <- visit$mu_low[[t]]*(67646/205382)
    mu4 <- visit$mu_low[[t]]*(30139/205382) 
    eta1 <- visit$eta_low[[t]]*(45228/205382)
    eta2 <- visit$eta_low[[t]]*(62369/205382)
    eta3 <- visit$eta_low[[t]]*(67646/205382)
    eta4 <- visit$eta_low[[t]]*(30139/205382)
    
    # national prevalence of COVID-19 (using NYT past data and covid19sim projections)
    epsilon <- visit$epsilon_low[[t]]
    
    # population estimate
    n  <- 205382 # total population of Summit, Grand, Eagle, Pitkin, Routt, and Garfield Counties
    n1 <- 45228 # population age group 1
    n2 <- 62369 # 2
    n3 <- 67646 # 3
    n4 <- 30139 # 4
    
    # time-varying transmission control (TC) parameters, represented by delta
    delta <- ifelse(t<t2, delta1, ifelse(t<t3, delta2, ifelse(t<t4, delta3, ifelse(t<t5, delta4,
             ifelse(t<t6, delta5, ifelse(t<t7, delta6, ifelse(t<t8, delta7, ifelse(t<t9, delta8,
             ifelse(t<t10, delta9, ifelse(t<t11, delta10, ifelse(t<t12, delta11, ifelse(t<t13,
             delta12, ifelse(t<t14, delta13, ifelse(t<t15, delta14, ifelse(t<t16, delta15,
             ifelse(t<t17, delta16, ifelse(t<t18, delta17, ifelse(t<t19, delta18, ifelse(t<t20,
             delta19, ifelse(t<t21, delta20, delta21))))))))))))))))))))
    
# toggle CT/CI on or off after December 4th, 2020
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

# SCENARIO 3: MEDIUM VIRAL IMPORTATION WITHOUT VISITOR RESTRICTION
seir_ski3 <- function(t, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    # age-structure the  entry and exit parameters by multiplying by the proportionate age distribution
    mu1 <- visit$mu_high[[t]]*(45228/205382)
    mu2 <- visit$mu_high[[t]]*(62369/205382)
    mu3 <- visit$mu_high[[t]]*(67646/205382)
    mu4 <- visit$mu_high[[t]]*(30139/205382) 
    eta1 <- visit$eta_high[[t]]*(45228/205382)
    eta2 <- visit$eta_high[[t]]*(62369/205382)
    eta3 <- visit$eta_high[[t]]*(67646/205382)
    eta4 <- visit$eta_high[[t]]*(30139/205382)
    
    # national prevalence of COVID-19 (using NYT past data and covid19sim projections)
    epsilon <- visit$epsilon_med[[t]]
    
    # population estimate
    n  <- 205382 # total population of Summit, Grand, Eagle, Pitkin, Routt, and Garfield Counties
    n1 <- 45228 # population age group 1
    n2 <- 62369 # 2
    n3 <- 67646 # 3
    n4 <- 30139 # 4
    
    # time-varying transmission control (TC) parameters, represented by delta
    delta <- ifelse(t<t2, delta1, ifelse(t<t3, delta2, ifelse(t<t4, delta3, ifelse(t<t5, delta4,
             ifelse(t<t6, delta5, ifelse(t<t7, delta6, ifelse(t<t8, delta7, ifelse(t<t9, delta8,
             ifelse(t<t10, delta9, ifelse(t<t11, delta10, ifelse(t<t12, delta11, ifelse(t<t13,
             delta12, ifelse(t<t14, delta13, ifelse(t<t15, delta14, ifelse(t<t16, delta15,
             ifelse(t<t17, delta16, ifelse(t<t18, delta17, ifelse(t<t19, delta18, ifelse(t<t20,
             delta19, ifelse(t<t21, delta20, delta21))))))))))))))))))))
    
# toggle CT/CI on or off after December 4th, 2020
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

# SCENARIO 4: MEDIUM VIRAL IMPORTATION WITH VISITOR RESTRICTION
seir_ski4 <- function(t, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    # age-structure the  entry and exit parameters by multiplying by the proportionate age distribution
    mu1 <- visit$mu_low[[t]]*(45228/205382)
    mu2 <- visit$mu_low[[t]]*(62369/205382)
    mu3 <- visit$mu_low[[t]]*(67646/205382)
    mu4 <- visit$mu_low[[t]]*(30139/205382) 
    eta1 <- visit$eta_low[[t]]*(45228/205382)
    eta2 <- visit$eta_low[[t]]*(62369/205382)
    eta3 <- visit$eta_low[[t]]*(67646/205382)
    eta4 <- visit$eta_low[[t]]*(30139/205382)
    
    # national prevalence of COVID-19 (using NYT past data and covid19sim projections)
    epsilon <- visit$epsilon_med[[t]]
    
    # population estimate
    n  <- 205382 # total population of Summit, Grand, Eagle, Pitkin, Routt, and Garfield Counties
    n1 <- 45228 # population age group 1
    n2 <- 62369 # 2
    n3 <- 67646 # 3
    n4 <- 30139 # 4
    
    # time-varying transmission control (TC) parameters, represented by delta
    delta <- ifelse(t<t2, delta1, ifelse(t<t3, delta2, ifelse(t<t4, delta3, ifelse(t<t5, delta4,
             ifelse(t<t6, delta5, ifelse(t<t7, delta6, ifelse(t<t8, delta7, ifelse(t<t9, delta8,
             ifelse(t<t10, delta9, ifelse(t<t11, delta10, ifelse(t<t12, delta11, ifelse(t<t13,
             delta12, ifelse(t<t14, delta13, ifelse(t<t15, delta14, ifelse(t<t16, delta15,
             ifelse(t<t17, delta16, ifelse(t<t18, delta17, ifelse(t<t19, delta18, ifelse(t<t20,
             delta19, ifelse(t<t21, delta20, delta21))))))))))))))))))))
    
# toggle CT/CI on or off after December 4th, 2020
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

# SCENARIO 1: HIGH VIRAL IMPORTATION WITHOUT VISITOR RESTRICTION
seir_ski1 <- function(t, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    # age-structure the  entry and exit parameters by multiplying by the proportionate age distribution
    mu1 <- visit$mu_high[[t]]*(45228/205382)
    mu2 <- visit$mu_high[[t]]*(62369/205382)
    mu3 <- visit$mu_high[[t]]*(67646/205382)
    mu4 <- visit$mu_high[[t]]*(30139/205382) 
    eta1 <- visit$eta_high[[t]]*(45228/205382)
    eta2 <- visit$eta_high[[t]]*(62369/205382)
    eta3 <- visit$eta_high[[t]]*(67646/205382)
    eta4 <- visit$eta_high[[t]]*(30139/205382)
    
    # national prevalence of COVID-19 (using NYT past data and covid19sim projections)
    epsilon <- visit$epsilon_high[[t]]
    
    # population estimate
    n  <- 205382 # total population of Summit, Grand, Eagle, Pitkin, Routt, and Garfield Counties
    n1 <- 45228 # population age group 1
    n2 <- 62369 # 2
    n3 <- 67646 # 3
    n4 <- 30139 # 4
    
    # time-varying transmission control (TC) parameters, represented by delta
    delta <- ifelse(t<t2, delta1, ifelse(t<t3, delta2, ifelse(t<t4, delta3, ifelse(t<t5, delta4,
             ifelse(t<t6, delta5, ifelse(t<t7, delta6, ifelse(t<t8, delta7, ifelse(t<t9, delta8,
             ifelse(t<t10, delta9, ifelse(t<t11, delta10, ifelse(t<t12, delta11, ifelse(t<t13,
             delta12, ifelse(t<t14, delta13, ifelse(t<t15, delta14, ifelse(t<t16, delta15,
             ifelse(t<t17, delta16, ifelse(t<t18, delta17, ifelse(t<t19, delta18, ifelse(t<t20,
             delta19, ifelse(t<t21, delta20, delta21))))))))))))))))))))
    
# toggle CT/CI on or off after December 4th, 2020
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

# SCENARIO 2: HIGH VIRAL IMPORTATION WITH VISITOR RESTRICTION
seir_ski2 <- function(t, state, parameters) {
  
  with(as.list(c(state, parameters)), {
    
    # age-structure the  entry and exit parameters by multiplying by the proportionate age distribution
    mu1 <- visit$mu_low[[t]]*(45228/205382)
    mu2 <- visit$mu_low[[t]]*(62369/205382)
    mu3 <- visit$mu_low[[t]]*(67646/205382)
    mu4 <- visit$mu_low[[t]]*(30139/205382) 
    eta1 <- visit$eta_low[[t]]*(45228/205382)
    eta2 <- visit$eta_low[[t]]*(62369/205382)
    eta3 <- visit$eta_low[[t]]*(67646/205382)
    eta4 <- visit$eta_low[[t]]*(30139/205382)
    
    # national prevalence of COVID-19 (using NYT past data and covid19sim projections)
    epsilon <- visit$epsilon_high[[t]]
    
    # population estimate
    n  <- 205382 # total population of Summit, Grand, Eagle, Pitkin, Routt, and Garfield Counties
    n1 <- 45228 # population age group 1
    n2 <- 62369 # 2
    n3 <- 67646 # 3
    n4 <- 30139 # 4
    
    # time-varying transmission control (TC) parameters, represented by delta
    delta <- ifelse(t<t2, delta1, ifelse(t<t3, delta2, ifelse(t<t4, delta3, ifelse(t<t5, delta4,
             ifelse(t<t6, delta5, ifelse(t<t7, delta6, ifelse(t<t8, delta7, ifelse(t<t9, delta8,
             ifelse(t<t10, delta9, ifelse(t<t11, delta10, ifelse(t<t12, delta11, ifelse(t<t13,
             delta12, ifelse(t<t14, delta13, ifelse(t<t15, delta14, ifelse(t<t16, delta15,
             ifelse(t<t17, delta16, ifelse(t<t18, delta17, ifelse(t<t19, delta18, ifelse(t<t20,
             delta19, ifelse(t<t21, delta20, delta21))))))))))))))))))))
    
# toggle CT/CI on or off after December 4th, 2020
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

# establish initial values
inits <- c(S1=45228-1, E1=0, I1=1, A1=0, II1=0, Ih1=0, R1=0, D1=0,
           S2=62369,   E2=0, I2=0, A2=0, II2=0, Ih2=0, R2=0, D2=0,
           S3=67646,   E3=0, I3=0, A3=0, II3=0, Ih3=0, R3=0, D3=0,
           S4=30139,   E4=0, I4=0, A4=0, II4=0, Ih4=0, R4=0, D4=0)

# time series from 1 to 500
dt <- seq(1, 450, 1)

# create empty lists to store time series data for all three scenarios
covid_ts1 <- list()
covid_ts2 <- list()
covid_ts3 <- list()
covid_ts4 <- list()
covid_ts5 <- list()
covid_ts6 <- list()

# base parameter list (includes everything except kappa and delta21)
params <- cbind(beta=0.4931, # infection rate, which is a product of transmission-relevant contact rate and probability of transmission given that contact
           gamma=1/9, # recovery rate, which is 1/average length of infection
           alpha=4.2, # latent period, which 1 day shorter than the incubation period
           lambda=1.395, # ratio of infectiousness (symptomatic vs. asymptomatic)
           sigma1=0.1100230677, sigma2=0.3570515767, sigma3=0.5612057201, sigma4=0.7748797315, # proportion of cases that are symptomatic for each age group 
           theta=0.49, # proportion of total infections detected
           tau=0.049, # contact tracing collapsed into a single parameter (see Appendix C)
           hosp1=0.01743, hosp2=0.02909127, hosp3=0.032122, hosp4=0.0501384, # proportion of sympomatic cases in each age group that end up hospitalized
           cc1=0.0052754, cc2a=0.007474, cc2b=0.00838, cc2c=0.00591, cc3a=0.01866, cc3b=0.01785, cc3c=0.012226,
           cc4a=0.02885, cc4b=0.02234, cc4c=0.0202, # proportion sympomatics in each age group that end up in the ICU
           hlos1a=4.05, hlos1b=5.0, hlos2a=5.49, hlos2b=3.6, hlos3a=7.36, hlos3b=5.3, hlos4a=10.02, hlos4b=7.6, # length of stay (non-ICU) for each age group
           clos1a=7.00, clos1b=4, clos2a=9.91, clos2b=4.4, clos3a=13.47, clos3b=8.5, clos4a=10.82, clos4b=8, # length of stay (ICU) for each age group
           dh1=0, dh2=0, dh3=0.01, dh3_2=0.00347, dh4=0.14106, dh4_2=0.06086, # death fraction (non-ICU) for each age group
           dc1=0.0408, dc2=0.0573, dc3=0.16498, dc3_2=0.1101, dc4=0.39066, dc4_2=0.30275, # death fraction (ICU) for each age group
           t2=50, t3=64, t4=78, t5=92, t6=106, t7=120, t8=134, t9=148, t10=162, t11=176, t12=190,
           t13=204, t14=218, t15=232, t16=246, t17=260, t18=274, t19=288, t20=302, t21=316, # biweekly time triggers
           delta1=0, delta2=0.5868, delta3=0.9900, delta4=0.7524, delta5=0.8496, delta6=0.6452,
           delta7=0.7832,  delta8=0.7717, delta9=0.8031, delta10=0.6642, delta11=0.7959, delta12=0.9696,
           delta13=0.9177, delta14=0.8150, delta15=0.7721, delta16=0.7465, delta17=0.4958, delta18=0.7434, delta19=0.5136,
           delta20=0.7146) # time-varying transmission control parameters (fitted)

# select t21 for each scenario from 100 samples from a random uniform distribution using r-unif
set.seed(1520)
list_a <- cbind(kappa=0, delta21=runif(100, min=0.59, max=0.99))
list_b <- cbind(kappa=1, delta21=runif(100, min=0.59, max=0.99))

# horizontally base parameter list with simulation values
params_a <- merge(params, list_a)
params_b <- merge(params, list_b)

# vertically combine 1a and 1b parameters and add incrementing scenario number
params_total <- rbind(params_a, params_b)
params_total$scenario <- 1:200

# loop through all 200 simulations of Scenario 1
for(i in 1:nrow(params_total)){
  out1 <- lsoda(y=inits, times=dt, func=seir_ski1, parms=params_total[i,])
  covid_ts1[[i]] <- as.matrix(out1)
}

# generate Scenario 1 output, creating I2wk (two week cumulative incidence per 100K)
# create Itotal and Etotal variables (for calculating Re)
all1 <-  as.data.frame(cbind(rep(1:nrow(params_total), each=450), do.call("rbind", covid_ts1)))
all1$date <- seq(as.Date("2020/1/24"), as.Date("2020/1/23") + 450, "days")
all1$scenario <- all1$V1
all1$V1 <- NULL
all_ski1 <- merge(params_total, all1, by="scenario")
all_ski1$I2wk <- zoo::rollsum(all_ski1$inc/205382*100000, 14, fill=NA, align="right")
all_ski1$Itotal <- rowSums(all_ski1[, c("I1", "I2", "I3", "I4", "A1", "A2", "A3", "A4")])
all_ski1$Etotal <- rowSums(all_ski1[, c("E1", "E2", "E3", "E4")])

# loop through all 200 simulations of Scenario 2
for(i in 1:nrow(params_total)){
  out2 <- lsoda(y=inits, times=dt, func=seir_ski2, parms=params_total[i,])
  covid_ts2[[i]] <- as.matrix(out2)
}

# generate Scenario 2 output, creating I2wk (two week cumulative incidence per 100K)
# create Itotal and Etotal variables (for calculating Re)
all2 <-  as.data.frame(cbind(rep(1:nrow(params_total), each=450), do.call("rbind", covid_ts2)))
all2$date <- seq(as.Date("2020/1/24"), as.Date("2020/1/23") + 450, "days")
all2$scenario <- all2$V1
all2$V1 <- NULL
all_ski2 <- merge(params_total, all2, by="scenario")
all_ski2$I2wk <- zoo::rollsum(all_ski2$inc/205382*100000, 14, fill=NA, align="right")
all_ski2$Itotal <- rowSums(all_ski2[, c("I1", "I2", "I3", "I4", "A1", "A2", "A3", "A4")])
all_ski2$Etotal <- rowSums(all_ski2[, c("E1", "E2", "E3", "E4")])

# loop through all 200 simulations of Scenario 3
for(i in 1:nrow(params_total)){
  out3 <- lsoda(y=inits, times=dt, func=seir_ski3, parms=params_total[i,])
  covid_ts3[[i]] <- as.matrix(out3)
}

# generate Scenario 3 output, creating I2wk (two week cumulative incidence per 100K)
# create Itotal and Etotal variables (for calculating Re)
all3 <-  as.data.frame(cbind(rep(1:nrow(params_total), each=450), do.call("rbind", covid_ts3)))
all3$date <- seq(as.Date("2020/1/24"), as.Date("2020/1/23") + 450, "days")
all3$scenario <- all3$V1
all3$V1 <- NULL
all_ski3 <- merge(params_total, all3, by="scenario")
all_ski3$I2wk <- zoo::rollsum(all_ski3$inc/205382*100000, 14, fill=NA, align="right")
all_ski3$Itotal <- rowSums(all_ski3[, c("I1", "I2", "I3", "I4", "A1", "A2", "A3", "A4")])
all_ski3$Etotal <- rowSums(all_ski3[, c("E1", "E2", "E3", "E4")])

# loop through all 200 simulations of Scenario 4
for(i in 1:nrow(params_total)){
  out4 <- lsoda(y=inits, times=dt, func=seir_ski4, parms=params_total[i,])
  covid_ts4[[i]] <- as.matrix(out4)
}

# generate Scenario 4 output, creating I2wk (two week cumulative incidence per 100K)
# create Itotal and Etotal variables (for calculating Re)
all4 <-  as.data.frame(cbind(rep(1:nrow(params_total), each=450), do.call("rbind", covid_ts4)))
all4$date <- seq(as.Date("2020/1/24"), as.Date("2020/1/23") + 450, "days")
all4$scenario <- all4$V1
all4$V1 <- NULL
all_ski4 <- merge(params_total, all4, by="scenario")
all_ski4$I2wk <- zoo::rollsum(all_ski4$inc/205382*100000, 14, fill=NA, align="right")
all_ski4$Itotal <- rowSums(all_ski4[, c("I1", "I2", "I3", "I4", "A1", "A2", "A3", "A4")])
all_ski4$Etotal <- rowSums(all_ski4[, c("E1", "E2", "E3", "E4")])

# loop through all 200 simulations of Scenario 5
for(i in 1:nrow(params_total)){
  out5 <- lsoda(y=inits, times=dt, func=seir_ski5, parms=params_total[i,])
  covid_ts5[[i]] <- as.matrix(out5)
}

# generate Scenario 5 output, creating I2wk (two week cumulative incidence per 100K)
# create Itotal and Etotal variables (for calculating Re)
all5 <-  as.data.frame(cbind(rep(1:nrow(params_total), each=450), do.call("rbind", covid_ts5)))
all5$date <- seq(as.Date("2020/1/24"), as.Date("2020/1/23") + 450, "days")
all5$scenario <- all5$V1
all5$V1 <- NULL
all_ski5 <- merge(params_total, all5, by="scenario")
all_ski5$I2wk <- zoo::rollsum(all_ski5$inc/205382*100000, 14, fill=NA, align="right")
all_ski5$Itotal <- rowSums(all_ski5[, c("I1", "I2", "I3", "I4", "A1", "A2", "A3", "A4")])
all_ski5$Etotal <- rowSums(all_ski5[, c("E1", "E2", "E3", "E4")])

# loop through all 200 simulations of Scenario 6
for(i in 1:nrow(params_total)){
  out6 <- lsoda(y=inits, times=dt, func=seir_ski6, parms=params_total[i,])
  covid_ts6[[i]] <- as.matrix(out6)
}

# generate Scenario 6 output, creating I2wk (two week cumulative incidence per 100K)
# create Itotal and Etotal variables (for calculating Re)
all6 <-  as.data.frame(cbind(rep(1:nrow(params_total), each=450), do.call("rbind", covid_ts6)))
all6$date <- seq(as.Date("2020/1/24"), as.Date("2020/1/23") + 450, "days")
all6$scenario <- all6$V1
all6$V1 <- NULL
all_ski6 <- merge(params_total, all6, by="scenario")
all_ski6$I2wk <- zoo::rollsum(all_ski6$inc/205382*100000, 14, fill=NA, align="right")
all_ski6$Itotal <- rowSums(all_ski6[, c("I1", "I2", "I3", "I4", "A1", "A2", "A3", "A4")])
all_ski6$Etotal <- rowSums(all_ski6[, c("E1", "E2", "E3", "E4")])

# export final model outputs for all three scenarios
write.csv(all_ski1, "./Thesis/Data/all_ski1.csv")
write.csv(all_ski2, "./Thesis/Data/all_ski2.csv")
write.csv(all_ski3, "./Thesis/Data/all_ski3.csv")
write.csv(all_ski4, "./Thesis/Data/all_ski4.csv")
write.csv(all_ski5, "./Thesis/Data/all_ski5.csv")
write.csv(all_ski6, "./Thesis/Data/all_ski6.csv")

