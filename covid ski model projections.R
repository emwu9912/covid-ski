# set file paths
path <- "C:/Users/Emma J. Wu/Documents/Writing Material/Manuscripts/COVID-19 Ski Paper/"

# load packages
library(pacman)
p_load(data.table, dplyr, ggplot2, ggpubr, scales, stringr)

#LRT <- 1318 #38% of infections detected between 11/21/2020 and 12/4/2020 (501+ as threshold)
#LRT <- 2887
LRT <- 1670

# if restarting R, read output back in to avoid having to run the model again
#all_ski1 <- read.csv(str_c(path, "Data/all_ski1.csv"))
#all_ski1$date <- as.Date(all_ski1$date, "%Y-%m-%d")

#all_ski2 <- read.csv(str_c(path, "Data/all_ski2.csv"))
#all_ski2$date <- as.Date(all_ski2$date, "%Y-%m-%d")

#all_ski3 <- read.csv(str_c(path, "Data/all_ski3.csv"))
#all_ski3$date <- as.Date(all_ski3$date, "%Y-%m-%d")

#all_ski4 <- read.csv(str_c(path, "Data/all_ski4.csv"))
#all_ski4$date <- as.Date(all_ski4$date, "%Y-%m-%d")

# graphics setup code
date.breaks <- "5 weeks"
date.limits <- c(as.Date("2020-10-01"), as.Date("2021-04-17"))

scalex <- scale_x_date(date_labels="%m/%d/%y", date_breaks=date.breaks, limits=date.limits)
scaley <-  scale_y_continuous(limits=c(0, 7500), breaks=c(0, LRT, seq(3000, 7500, 1500)), labels=scales::comma)
scalecolor <-   scale_color_gradient(low="goldenrod1", high="darkblue", breaks=c(0.60, 0.65, 0.70, 0.75, 0.8, 0.85, 0.9, 0.95),
                       labels=c("60% TC", "65% TC", "70% TC", "75% TC", "80% TC", "85% TC", "90% TC", "95% TC"))


psim <- theme(plot.margin=unit(c(1, 2, 1, 0.5), "cm"),
              plot.title=element_text(face="bold", size=26, hjust=0.5),
              legend.title=element_blank(), legend.key=element_rect(color=NA, fill=NA),
              legend.text=element_text(size=22, angle=30, margin=margin(20, 0, 0, 0, "pt")),
              legend.position="bottom", legend.box="vertical",
              legend.key.size=unit(1.5, 'cm'), legend.key.width=unit(6, "cm"),
              axis.line=element_line(), axis.title.x=element_blank(),
              axis.title.y=element_text(size=22, face ="bold", margin=margin(0, 20, 0, 0, 'pt')),
              axis.text.x=element_text(size=22, color ="black", angle=30, margin=margin(20, 0, 0, 0, "pt")),
              axis.text.y=element_text(size=22, color="black", margin=margin(0, 10, 0, 0, "pt"), hjust=0.5),
              axis.ticks=element_line(), axis.ticks.length=unit(0.25, "cm"))

# extract historical model output from Scenario 1
model_output1 <- subset(all_ski1, scenario==1 & date <= as.Date("2020-12-04"))
model_output1$tc21 <- 0.01
model_output1 <- model_output1[c("date", "Iwk", "kappa", "tc21")]

# extract projected model output from Scenario 1a
allscen_project1a <- subset(all_ski1, kappa==0 & date >= as.Date("2020-12-04"))
allscen_project1a <- allscen_project1a[c("date", "Iwk", "kappa", "tc21")]

# extract projected model output from Scenario 1b
allscen_project1b <- subset(all_ski1, kappa==1 & date >= as.Date("2020-12-04"))
allscen_project1b <- allscen_project1b[c("date", "Iwk", "kappa", "tc21")]

# extract historical model output from Scenario 2
model_output2 <- subset(all_ski2, scenario==1 & date <= as.Date("2020-12-04"))
model_output2$tc21 <- 0.01
model_output2 <- model_output2[c("date", "Iwk", "kappa", "tc21")]

# extract projected model output from Scenario 2a
allscen_project2a <- subset(all_ski2, kappa==0 & date >= as.Date("2020-12-04"))
allscen_project2a <- allscen_project2a[c("date", "Iwk", "kappa", "tc21")]

# extract projected model output from Scenario 2b
allscen_project2b <- subset(all_ski2, kappa==1 & date >= as.Date("2020-12-04"))
allscen_project2b <- allscen_project2b[c("date", "Iwk", "kappa", "tc21")]

# extract historical model output from Scenario 3
model_output3 <- subset(all_ski3, scenario==1 & date <= as.Date("2020-12-04"))
model_output3$tc21 <- 0.01
model_output3 <- model_output3[c("date", "Iwk", "kappa", "tc21")]

# extract projected model output from Scenario 3a
allscen_project3a <- subset(all_ski3, kappa==0 & date >= as.Date("2020-12-04"))
allscen_project3a <- allscen_project3a[c("date", "Iwk", "kappa", "tc21")]

# extract projected model output from Scenario 3b
allscen_project3b <- subset(all_ski3, kappa==1 & date >= as.Date("2020-12-04"))
allscen_project3b <- allscen_project3b[c("date", "Iwk", "kappa", "tc21")]

# extract historical model output from Scenario 4
model_output4 <- subset(all_ski4, scenario==1 & date <= as.Date("2020-12-04"))
model_output4$tc21 <- 0.01
model_output4 <- model_output4[c("date", "Iwk", "kappa", "tc21")]

# extract projected model output from Scenario 4a
allscen_project4a <- subset(all_ski4, kappa==0 & date >= as.Date("2020-12-04"))
allscen_project4a <- allscen_project4a[c("date", "Iwk", "kappa", "tc21")]

# extract projected model output from Scenario 4b
allscen_project4b <- subset(all_ski4, kappa==1 & date >= as.Date("2020-12-04"))
allscen_project4b <- allscen_project4b[c("date", "Iwk", "kappa", "tc21")]

# filter minimum TC out of all scenarios and calculate Re for each

all_ski1.dt <- as.data.table(arrange(all_ski1, tc21))

scen1a.dt <- as.data.table(arrange(allscen_project1a, tc21))
scen1a.nocap <- scen1a.dt %>% group_by(tc21) %>% filter(!any(Iwk > LRT)) %>% slice(1)
minTC_1a <- min(scen1a.nocap$tc21)
minTC_1a
label_percent()(minTC_1a)

#scen1a.Re <- all_ski1.dt %>% group_by(scenario) %>% filter(tc21==minTC_1a) %>%
 # mutate(Ilag=lag(Itotal, 3)) %>%
  #mutate(Re=(Etotal/4)/(Ilag/9)) %>% filter(time==328)
#signif(scen1a.Re$Re, 3)

scen1b.dt <- as.data.table(arrange(allscen_project1b, tc21))
scen1b.nocap <- scen1b.dt %>% group_by(tc21) %>% filter(!any(Iwk > LRT)) %>% slice(1)
minTC_1b <- min(scen1b.nocap$tc21)
minTC_1b
label_percent()(minTC_1b)

#scen1b.Re <- all_ski1.dt %>% group_by(scenario) %>% filter(tc21==minTC_1b) %>%
  #mutate(Ilag=lag(Itotal, 3)) %>%
  #mutate(Re=(Etotal/4)/(Ilag/9)) %>% filter(time==328)
#signif(scen1b.Re$Re, 3)

#all_ski2.dt <- as.data.table(arrange(all_ski2, tc21))

scen2a.dt <- as.data.table(arrange(allscen_project2a, tc21))
scen2a.nocap <- scen2a.dt %>% group_by(tc21) %>% filter(!any(Iwk > LRT)) %>% slice(1)
minTC_2a <- min(scen2a.nocap$tc21)
minTC_2a
label_percent()(minTC_2a)

#scen2a.Re <- all_ski2.dt %>% group_by(scenario) %>% filter(tc21==minTC_2a) %>%
 # mutate(Ilag=lag(Itotal, 3)) %>%
 # mutate(Re=(Etotal/4)/(Ilag/9)) %>% filter(time==328)
#signif(scen2a.Re$Re, 3)

scen2b.dt <- as.data.table(arrange(allscen_project2b, tc21))
scen2b.nocap <- scen2b.dt %>% group_by(tc21) %>% filter(!any(Iwk > LRT)) %>% slice(1)
minTC_2b <- min(scen2b.nocap$tc21)
minTC_2b
label_percent()(minTC_2b)

#scen2b.Re <- all_ski2.dt %>% group_by(scenario) %>% filter(tc21==minTC_2b) %>%
 # mutate(Ilag=lag(Itotal, 3)) %>%
  #mutate(Re=(Etotal/4)/(Ilag/9)) %>% filter(time==328)
#signif(scen2b.Re$Re, 3)

all_ski3.dt <- as.data.table(arrange(all_ski3, tc21))

scen3a.dt <- as.data.table(arrange(allscen_project3a, tc21))
scen3a.nocap <- scen3a.dt %>% group_by(tc21) %>% filter(!any(Iwk > LRT)) %>% slice(1)
minTC_3a <- min(scen3a.nocap$tc21)
minTC_3a
label_percent()(minTC_3a)

#scen3a.Re <- all_ski3.dt %>% group_by(scenario) %>% filter(tc21==minTC_3a) %>%
 # mutate(Ilag=lag(Itotal, 3)) %>%
 # mutate(Re=(Etotal/4)/(Ilag/9)) %>% filter(time==328)
#signif(scen3a.Re$Re, 3)

scen3b.dt <- as.data.table(arrange(allscen_project3b, tc21))
scen3b.nocap <- scen3b.dt %>% group_by(tc21) %>% filter(!any(Iwk > LRT)) %>% slice(1)
minTC_3b <- min(scen3b.nocap$tc21)
minTC_3b
label_percent()(minTC_3b)

#scen3b.Re <- all_ski3.dt %>% group_by(scenario) %>% filter(tc21==minTC_3b) %>%
  #mutate(Ilag=lag(Itotal, 3)) %>%
  #mutate(Re=(Etotal/4)/(Ilag/9)) %>% filter(time==328)
#signif(scen3b.Re$Re, 3)


all_ski4.dt <- as.data.table(arrange(all_ski4, tc21))

scen4a.dt <- as.data.table(arrange(allscen_project4a, tc21))
scen4a.nocap <- scen4a.dt %>% group_by(tc21) %>% filter(!any(Iwk > LRT)) %>% slice(1)
minTC_4a <- min(scen4a.nocap$tc21)
minTC_4a
label_percent()(minTC_4a)

#scen4a.Re <- all_ski4.dt %>% group_by(scenario) %>% filter(tc21==minTC_4a) %>%
  #mutate(Ilag=lag(Itotal, 3)) %>%
 # mutate(Re=(Etotal/4)/(Ilag/9)) %>% filter(time==328)
#signif(scen4a.Re$Re, 3)

scen4b.dt <- as.data.table(arrange(allscen_project4b, tc21))
scen4b.nocap <- scen4b.dt %>% group_by(tc21) %>% filter(!any(Iwk > LRT)) %>% slice(1)
minTC_4b <- min(scen4b.nocap$tc21)
minTC_4b
label_percent()(minTC_4b)

#scen4b.Re <- all_ski4.dt %>% group_by(scenario) %>% filter(tc21==minTC_4b) %>%
  #mutate(Ilag=lag(Itotal, 3)) %>%
 # mutate(Re=(Etotal/4)/(Ilag/9)) %>% filter(time==328)
#signif(scen4b.Re$Re, 3)

# plot two-week cumulative incidence

projection1a <- ggplot(data=allscen_project1a, aes(x=date, y=Iwk, group=tc21, color=tc21)) +
  geom_line(size=1.5) + labs(x="Date", y="7-Day Model Estimated\nCumulative New Infections Per 100K") +
  geom_line(data=model_output1, aes(x=date, y=Iwk), color="black", size=1.5) +
  geom_hline(yintercept=LRT, linetype="dashed", size=1, color="black") +
  annotate("text", x=as.Date("2020-11-07"), y=4000, size=14, label=paste("TCmin =", label_percent()(minTC_1a))) +
  annotate("text", x=as.Date("2021-04-10"), y=5000, size=16, label="1A") + scalex + scaley + scalecolor + psim

projection1b <- ggplot(data=allscen_project1b, aes(x=date, y=Iwk, group=tc21, color=tc21)) +
  geom_line(size=1.5) + labs(x="Date", y="7-Day Model Estimated\nCumulative New Infections Per 100K") +
  geom_line(data=model_output1, aes(x=date, y=Iwk), color="black", size=1.5) +
  geom_hline(yintercept=LRT, linetype="dashed", size=1, color="black") +
  annotate("text", x=as.Date("2020-11-07"), y=4000, size=14, label=paste("TCmin =", label_percent()(minTC_1b))) +
    annotate("text", x=as.Date("2021-04-10"), y=5000, size=16, label="1B") + scalex + scaley + scalecolor + psim

# combine and save Scenario 1 plots
sim1plots <- ggarrange(projection1a, projection1b, common.legend=TRUE, legend="bottom")
ggsave(str_c(path, "Figures/sim1plots.png"), height=10, width=24, plot=sim1plots)


# plot two-week cumulative incidence
projection2a <- ggplot(data=allscen_project2a, aes(x=date, y=Iwk, group=tc21, color=tc21)) +
  geom_line(size=1.5) + labs(x="Date", y="7-Day Model Estimated\nCumulative New Infections Per 100K") +
  geom_line(data=model_output2, aes(x=date, y=Iwk), color="black", size=1.5) +
  geom_hline(yintercept=LRT, linetype="dashed", size=1, color="black") +
  annotate("text", x=as.Date("2020-11-07"), y=4000, size=14, label=paste("TCmin =", label_percent()(minTC_2a))) +
  annotate("text", x=as.Date("2021-04-10"), y=5000, size=16, label="2A") + scalex + scaley + scalecolor + psim

projection2b <- ggplot(data=allscen_project2b, aes(x=date, y=Iwk, group=tc21, color=tc21)) +
  geom_line(size=1.5) + labs(x="Date", y="7-Day Model Estimated\nCumulative New Infections Per 100K") +
  geom_line(data=model_output2, aes(x=date, y=Iwk), color="black", size=1.5) +
  geom_hline(yintercept=LRT, linetype="dashed", size=1, color="black") +
  annotate("text", x=as.Date("2020-11-07"), y=4000, size=14, label=paste("TCmin =", label_percent()(minTC_2b))) +
  annotate("text", x=as.Date("2021-04-10"), y=5000, size=16, label="2B") + scalex + scaley + scalecolor + psim

# combine and save Scenario 2 plots
sim2plots <- ggarrange(projection2a, projection2b, common.legend=TRUE, legend="bottom")
ggsave(str_c(path, "Figures/sim2plots.png"), height=10, width=24, plot=sim2plots)



# plot two-week cumulative incidence
projection3a <- ggplot(data=allscen_project3a, aes(x=date, y=Iwk, group=tc21, color=tc21)) +
  geom_line(size=1.5) + labs(x="Date", y="7-Day Model Estimated\nCumulative New Infections Per 100K") +
  geom_line(data=model_output3, aes(x=date, y=Iwk), color="black", size=1.5) +
  geom_hline(yintercept=LRT, linetype="dashed", size=1, color="black") +
  annotate("text", x=as.Date("2020-11-07"), y=4000, size=14, label=paste("TCmin =", label_percent()(minTC_3a))) +
  annotate("text", x=as.Date("2021-04-10"), y=5000, size=16, label="3A") + scalex + scaley + scalecolor + psim

projection3b <- ggplot(data=allscen_project3b, aes(x=date, y=Iwk, group=tc21, color=tc21)) +
  geom_line(size=1.5) + labs(x="Date", y="7-Day Model Estimated\nCumulative New Infections Per 100K") +
  geom_line(data=model_output3, aes(x=date, y=Iwk), color="black", size=1.5) +
  geom_hline(yintercept=LRT, linetype="dashed", size=1, color="black") +
  annotate("text", x=as.Date("2020-11-07"), y=4000, size=14, label=paste("TCmin =", label_percent()(minTC_3b))) +
  annotate("text", x=as.Date("2021-04-10"), y=5000, size=16, label="3B") + scalex + scaley + scalecolor + psim

# combine and save Scenario 3 plots
sim3plots <- ggarrange(projection3a, projection3b, common.legend=TRUE, legend="bottom")
ggsave(str_c(path, "Figures/sim3plots.png"), height=10, width=24, plot=sim3plots)




# plot two-week cumulative incidence
projection4a <- ggplot(data=allscen_project4a, aes(x=date, y=Iwk, group=tc21, color=tc21)) +
  geom_line(size=1.5) + labs(x="Date", y="7-Day Model Estimated\nCumulative New Infections Per 100K") +
  geom_line(data=model_output4, aes(x=date, y=Iwk), color="black", size=1.5) +
  geom_hline(yintercept=LRT, linetype="dashed", size=1, color="black") +
  annotate("text", x=as.Date("2020-11-07"), y=4000, size=14, label=paste("TCmin =", label_percent()(minTC_4a))) +
  annotate("text", x=as.Date("2021-04-10"), y=5000, size=16, label="4A") + scalex + scaley + scalecolor + psim

projection4b <- ggplot(data=allscen_project4b, aes(x=date, y=Iwk, group=tc21, color=tc21)) +
  geom_line(size=1.5) + labs(x="Date", y="7-Day Model Estimated\nCumulative New Infections Per 100K") +
  geom_line(data=model_output4, aes(x=date, y=Iwk), color="black", size=1.5) +
  geom_hline(yintercept=LRT, linetype="dashed", size=1, color="black") +
  annotate("text", x=as.Date("2020-11-07"), y=4000, size=14, label=paste("TCmin =", label_percent()(minTC_4b))) +
  annotate("text", x=as.Date("2021-04-10"), y=5000, size=16, label="4B") + scalex + scaley + scalecolor + psim

# combine and save Scenario 4 plots
sim4plots <- ggarrange(projection4a, projection4b, common.legend=TRUE, legend="bottom")
ggsave(str_c(path, "Figures/sim4plots.png"), height=10, width=24, plot=sim4plots)

