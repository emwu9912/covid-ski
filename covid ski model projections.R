# set file paths
path <- "/Users/emilywu883/Documents/CU Anschutz/COVID-19/"

# load packages
library(pacman)
p_load(data.table, dplyr, ggplot2, ggpubr, scales)

dirt <- 2880

# if restarting R, read output back in to avoid having to run the model again
all_ski1 <- read.csv("./Thesis/Data/all_ski1.csv")
all_ski1$date <- as.Date(all_ski1$date, "%Y-%m-%d")

all_ski2 <- read.csv("./Thesis/Data/all_ski2.csv")
all_ski2$date <- as.Date(all_ski2$date, "%Y-%m-%d")

all_ski3 <- read.csv("./Thesis/Data/all_ski3.csv")
all_ski3$date <- as.Date(all_ski3$date, "%Y-%m-%d")

all_ski4 <- read.csv("./Thesis/Data/all_ski4.csv")
all_ski4$date <- as.Date(all_ski4$date, "%Y-%m-%d")

all_ski5 <- read.csv("./Thesis/Data/all_ski5.csv")
all_ski5$date <- as.Date(all_ski5$date, "%Y-%m-%d")

all_ski6 <- read.csv("./Thesis/Data/all_ski6.csv")
all_ski6$date <- as.Date(all_ski6$date, "%Y-%m-%d")

# graphics setup code
date.breaks <- "5 weeks"
date.limits <- c(as.Date("2020-10-01"), as.Date("2021-04-17"))

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
model_output1$delta21 <- 0.01
model_output1 <- model_output1[c("date", "I2wk", "kappa", "delta21")]

# extract projected model output from Scenario 1a
allscen_project1a <- subset(all_ski1, kappa==0 & date >= as.Date("2020-12-04"))
allscen_project1a <- allscen_project1a[c("date", "I2wk", "kappa", "delta21")]

# extract projected model output from Scenario 1b
allscen_project1b <- subset(all_ski1, kappa==1 & date >= as.Date("2020-12-04"))
allscen_project1b <- allscen_project1b[c("date", "I2wk", "kappa", "delta21")]

# extract historical model output from Scenario 2
model_output2 <- subset(all_ski2, scenario==1 & date <= as.Date("2020-12-04"))
model_output2$delta21 <- 0.01
model_output2 <- model_output2[c("date", "I2wk", "kappa", "delta21")]

# extract projected model output from Scenario 2a
allscen_project2a <- subset(all_ski2, kappa==0 & date >= as.Date("2020-12-04"))
allscen_project2a <- allscen_project2a[c("date", "I2wk", "kappa", "delta21")]

# extract projected model output from Scenario 2b
allscen_project2b <- subset(all_ski2, kappa==1 & date >= as.Date("2020-12-04"))
allscen_project2b <- allscen_project2b[c("date", "I2wk", "kappa", "delta21")]

# extract historical model output from Scenario 3
model_output3 <- subset(all_ski3, scenario==1 & date <= as.Date("2020-12-04"))
model_output3$delta21 <- 0.01
model_output3 <- model_output3[c("date", "I2wk", "kappa", "delta21")]

# extract projected model output from Scenario 3a
allscen_project3a <- subset(all_ski3, kappa==0 & date >= as.Date("2020-12-04"))
allscen_project3a <- allscen_project3a[c("date", "I2wk", "kappa", "delta21")]

# extract projected model output from Scenario 3b
allscen_project3b <- subset(all_ski3, kappa==1 & date >= as.Date("2020-12-04"))
allscen_project3b <- allscen_project3b[c("date", "I2wk", "kappa", "delta21")]

# extract historical model output from Scenario 4
model_output4 <- subset(all_ski4, scenario==1 & date <= as.Date("2020-12-04"))
model_output4$delta21 <- 0.01
model_output4 <- model_output4[c("date", "I2wk", "kappa", "delta21")]

# extract projected model output from Scenario 4a
allscen_project4a <- subset(all_ski4, kappa==0 & date >= as.Date("2020-12-04"))
allscen_project4a <- allscen_project4a[c("date", "I2wk", "kappa", "delta21")]

# extract projected model output from Scenario 4b
allscen_project4b <- subset(all_ski4, kappa==1 & date >= as.Date("2020-12-04"))
allscen_project4b <- allscen_project4b[c("date", "I2wk", "kappa", "delta21")]

# extract historical model output from Scenario 5
model_output5 <- subset(all_ski5, scenario==1 & date <= as.Date("2020-12-04"))
model_output5$delta21 <- 0.01
model_output5 <- model_output5[c("date", "I2wk", "kappa", "delta21")]

# extract projected model output from Scenario 5a
allscen_project5a <- subset(all_ski5, kappa==0 & date >= as.Date("2020-12-04"))
allscen_project5a <- allscen_project5a[c("date", "I2wk", "kappa", "delta21")]

# extract projected model output from Scenario 5b
allscen_project5b <- subset(all_ski5, kappa==1 & date >= as.Date("2020-12-04"))
allscen_project5b <- allscen_project5b[c("date", "I2wk", "kappa", "delta21")]

# extract historical model output from Scenario 6
model_output6 <- subset(all_ski6, scenario==1 & date <= as.Date("2020-12-04"))
model_output6$delta21 <- 0.01
model_output6 <- model_output6[c("date", "I2wk", "kappa", "delta21")]

# extract projected model output from Scenario 6a
allscen_project6a <- subset(all_ski6, kappa==0 & date >= as.Date("2020-12-04"))
allscen_project6a <- allscen_project6a[c("date", "I2wk", "kappa", "delta21")]

# extract projected model output from Scenario 6b
allscen_project6b <- subset(all_ski6, kappa==1 & date >= as.Date("2020-12-04"))
allscen_project6b <- allscen_project6b[c("date", "I2wk", "kappa", "delta21")]

# filter minimum TC out of all scenarios and calculate Re for each

all_ski1.dt <- as.data.table(arrange(all_ski1, delta21))

scen1a.dt <- as.data.table(arrange(allscen_project1a, delta21))
scen1a.nocap <- scen1a.dt %>% group_by(delta21) %>% filter(!any(I2wk > dirt)) %>% slice(1)
minTC_1a <- min(scen1a.nocap$delta21)
minTC_1a
label_percent()(minTC_1a)

scen1a.Re <- all_ski1.dt %>% group_by(scenario) %>% filter(delta21==minTC_1a) %>%
  mutate(Ilag=lag(Itotal, 3)) %>%
  mutate(Re=(Etotal/4)/(Ilag/9)) %>% filter(time==328)
signif(scen1a.Re$Re, 3)

scen1b.dt <- as.data.table(arrange(allscen_project1b, delta21))
scen1b.nocap <- scen1b.dt %>% group_by(delta21) %>% filter(!any(I2wk > dirt)) %>% slice(1)
minTC_1b <- min(scen1b.nocap$delta21)
minTC_1b
label_percent()(minTC_1b)

scen1b.Re <- all_ski1.dt %>% group_by(scenario) %>% filter(delta21==minTC_1b) %>%
  mutate(Ilag=lag(Itotal, 3)) %>%
  mutate(Re=(Etotal/4)/(Ilag/9)) %>% filter(time==328)
signif(scen1b.Re$Re, 3)

all_ski2.dt <- as.data.table(arrange(all_ski2, delta21))

scen2a.dt <- as.data.table(arrange(allscen_project2a, delta21))
scen2a.nocap <- scen2a.dt %>% group_by(delta21) %>% filter(!any(I2wk > dirt)) %>% slice(1)
minTC_2a <- min(scen2a.nocap$delta21)
minTC_2a
label_percent()(minTC_2a)

scen2a.Re <- all_ski2.dt %>% group_by(scenario) %>% filter(delta21==minTC_2a) %>%
  mutate(Ilag=lag(Itotal, 3)) %>%
  mutate(Re=(Etotal/4)/(Ilag/9)) %>% filter(time==328)
signif(scen2a.Re$Re, 3)

scen2b.dt <- as.data.table(arrange(allscen_project2b, delta21))
scen2b.nocap <- scen2b.dt %>% group_by(delta21) %>% filter(!any(I2wk > dirt)) %>% slice(1)
minTC_2b <- min(scen2b.nocap$delta21)
minTC_2b
label_percent()(minTC_2b)

scen2b.Re <- all_ski2.dt %>% group_by(scenario) %>% filter(delta21==minTC_2b) %>%
  mutate(Ilag=lag(Itotal, 3)) %>%
  mutate(Re=(Etotal/4)/(Ilag/9)) %>% filter(time==328)
signif(scen2b.Re$Re, 3)

all_ski3.dt <- as.data.table(arrange(all_ski3, delta21))

scen3a.dt <- as.data.table(arrange(allscen_project3a, delta21))
scen3a.nocap <- scen3a.dt %>% group_by(delta21) %>% filter(!any(I2wk > dirt)) %>% slice(1)
minTC_3a <- min(scen3a.nocap$delta21)
minTC_3a
label_percent()(minTC_3a)

scen3a.Re <- all_ski3.dt %>% group_by(scenario) %>% filter(delta21==minTC_3a) %>%
  mutate(Ilag=lag(Itotal, 3)) %>%
  mutate(Re=(Etotal/4)/(Ilag/9)) %>% filter(time==328)
signif(scen3a.Re$Re, 3)

scen3b.dt <- as.data.table(arrange(allscen_project3b, delta21))
scen3b.nocap <- scen3b.dt %>% group_by(delta21) %>% filter(!any(I2wk > dirt)) %>% slice(1)
minTC_3b <- min(scen3b.nocap$delta21)
minTC_3b
label_percent()(minTC_3b)

scen3b.Re <- all_ski3.dt %>% group_by(scenario) %>% filter(delta21==minTC_3b) %>%
  mutate(Ilag=lag(Itotal, 3)) %>%
  mutate(Re=(Etotal/4)/(Ilag/9)) %>% filter(time==328)
signif(scen3b.Re$Re, 3)


all_ski4.dt <- as.data.table(arrange(all_ski4, delta21))

scen4a.dt <- as.data.table(arrange(allscen_project4a, delta21))
scen4a.nocap <- scen4a.dt %>% group_by(delta21) %>% filter(!any(I2wk > dirt)) %>% slice(1)
minTC_4a <- min(scen4a.nocap$delta21)
minTC_4a
label_percent()(minTC_4a)

scen4a.Re <- all_ski4.dt %>% group_by(scenario) %>% filter(delta21==minTC_4a) %>%
  mutate(Ilag=lag(Itotal, 3)) %>%
  mutate(Re=(Etotal/4)/(Ilag/9)) %>% filter(time==328)
signif(scen4a.Re$Re, 3)

scen4b.dt <- as.data.table(arrange(allscen_project4b, delta21))
scen4b.nocap <- scen4b.dt %>% group_by(delta21) %>% filter(!any(I2wk > dirt)) %>% slice(1)
minTC_4b <- min(scen4b.nocap$delta21)
minTC_4b
label_percent()(minTC_4b)

scen4b.Re <- all_ski4.dt %>% group_by(scenario) %>% filter(delta21==minTC_4b) %>%
  mutate(Ilag=lag(Itotal, 3)) %>%
  mutate(Re=(Etotal/4)/(Ilag/9)) %>% filter(time==328)
signif(scen4b.Re$Re, 3)


all_ski5.dt <- as.data.table(arrange(all_ski5, delta21))

scen5a.dt <- as.data.table(arrange(allscen_project5a, delta21))
scen5a.nocap <- scen5a.dt %>% group_by(delta21) %>% filter(!any(I2wk > dirt)) %>% slice(1)
minTC_5a <- min(scen5a.nocap$delta21)
minTC_5a
label_percent()(minTC_5a)

scen5a.Re <- all_ski5.dt %>% group_by(scenario) %>% filter(delta21==minTC_5a) %>%
  mutate(Ilag=lag(Itotal, 3)) %>%
  mutate(Re=(Etotal/4)/(Ilag/9)) %>% filter(time==328)
signif(scen5a.Re$Re, 3)

scen5b.dt <- as.data.table(arrange(allscen_project5b, delta21))
scen5b.nocap <- scen5b.dt %>% group_by(delta21) %>% filter(!any(I2wk > dirt)) %>% slice(1)
minTC_5b <- min(scen5b.nocap$delta21)
minTC_5b
label_percent()(minTC_5b)

scen5b.Re <- all_ski5.dt %>% group_by(scenario) %>% filter(delta21==minTC_5b) %>%
  mutate(Ilag=lag(Itotal, 3)) %>%
  mutate(Re=(Etotal/4)/(Ilag/9)) %>% filter(time==328)
signif(scen5b.Re$Re, 3)


all_ski6.dt <- as.data.table(arrange(all_ski6, delta21))

scen6a.dt <- as.data.table(arrange(allscen_project6a, delta21))
scen6a.nocap <- scen6a.dt %>% group_by(delta21) %>% filter(!any(I2wk > dirt)) %>% slice(1)
minTC_6a <- min(scen6a.nocap$delta21)
minTC_6a
label_percent()(minTC_6a)

scen6a.Re <- all_ski6.dt %>% group_by(scenario) %>% filter(delta21==minTC_6a) %>%
  mutate(Ilag=lag(Itotal, 3)) %>%
  mutate(Re=(Etotal/4)/(Ilag/9)) %>% filter(time==328)
signif(scen6a.Re$Re, 3)

scen6b.dt <- as.data.table(arrange(allscen_project6b, delta21))
scen6b.nocap <- scen6b.dt %>% group_by(delta21) %>% filter(!any(I2wk > dirt)) %>% slice(1)
minTC_6b <- min(scen6b.nocap$delta21)
minTC_6b
label_percent()(minTC_6b)

scen6b.Re <- all_ski6.dt %>% group_by(scenario) %>% filter(delta21==minTC_6b) %>%
  mutate(Ilag=lag(Itotal, 3)) %>%
  mutate(Re=(Etotal/4)/(Ilag/9)) %>% filter(time==328)
signif(scen6b.Re$Re, 3)



# plot two-week cumulative incidence
projection1a <- ggplot(data=allscen_project1a, aes(x=date, y=I2wk, group=delta21, color=delta21)) +
  geom_line(size=1.5) + labs(x="Date", y="Two-Week Model Estimated\nCumulative New Infections Per 100K") +
  geom_line(data=model_output1, aes(x=date, y=I2wk), color="black", size=1.5) +
  geom_hline(yintercept=dirt, linetype="dashed", size=1, color="black") +
  annotate("text", x=as.Date("2020-11-07"), y=4000, size=14, label=paste("TCmin =", label_percent()(minTC_1a))) +
  annotate("text", x=as.Date("2021-04-10"), y=11000, size=16, label="1A") +
  scale_x_date(date_labels="%m/%d/%y", date_breaks=date.breaks, limits=date.limits) +
  scale_y_continuous(limits=c(0, 12000), breaks=c(0, dirt, seq(6000, 12000, 3000)), labels=scales::comma) + 
  scale_color_gradient(low="goldenrod1", high="darkblue", breaks=c(0.60, 0.65, 0.70, 0.75, 0.8, 0.85, 0.9, 0.95),
                       labels=c("60% TC", "65% TC", "70% TC", "75% TC", "80% TC", "85% TC", "90% TC", "95% TC")) + psim

projection1b <- ggplot(data=allscen_project1b, aes(x=date, y=I2wk, group=delta21, color=delta21)) +
  geom_line(size=1.5) + labs(x="Date", y="Two-Week Model Estimated\nCumulative New Infections Per 100K") +
  geom_line(data=model_output1, aes(x=date, y=I2wk), color="black", size=1.5) +
  geom_hline(yintercept=dirt, linetype="dashed", size=1, color="black") +
  annotate("text", x=as.Date("2020-11-07"), y=4000, size=14, label=paste("TCmin =", label_percent()(minTC_1b))) +
    annotate("text", x=as.Date("2021-04-10"), y=11000, size=16, label="1B") +

  scale_x_date(date_labels="%m/%d/%y", date_breaks=date.breaks, limits=date.limits) +
  scale_y_continuous(limits=c(0, 12000), breaks=c(0, dirt, seq(6000, 12000, 3000)), labels=scales::comma) + 
  scale_color_gradient(low="goldenrod1", high="darkblue", breaks=c(0.60, 0.65, 0.70, 0.75, 0.8, 0.85, 0.9, 0.95),
                       labels=c("60% TC", "65% TC", "70% TC", "75% TC", "80% TC", "85% TC", "90% TC", "95% TC")) + psim

# combine and save Scenario 1 plots
sim1plots <- ggarrange(projection1a, projection1b, common.legend=TRUE, legend="bottom")
ggsave(str_c(path, "/Thesis/Figures/sim1plots.png"), height=10, width=24, plot=sim1plots)


# plot two-week cumulative incidence
projection2a <- ggplot(data=allscen_project2a, aes(x=date, y=I2wk, group=delta21, color=delta21)) +
  geom_line(size=1.5) + labs(x="Date", y="Two-Week Model Estimated\nCumulative New Infections Per 100K") +
  geom_line(data=model_output2, aes(x=date, y=I2wk), color="black", size=1.5) +
  geom_hline(yintercept=dirt, linetype="dashed", size=1, color="black") +
  annotate("text", x=as.Date("2020-11-07"), y=4000, size=14, label=paste("TCmin =", label_percent()(minTC_2a))) +
  annotate("text", x=as.Date("2021-04-10"), y=11000, size=16, label="2A") +
  scale_x_date(date_labels="%m/%d/%y", date_breaks=date.breaks, limits=date.limits) +
  scale_y_continuous(limits=c(0, 12000), breaks=c(0, dirt, seq(6000, 12000, 3000)), labels=scales::comma) + 
  scale_color_gradient(low="goldenrod1", high="darkblue", breaks=c(0.6, 0.65, 0.70, 0.75, 0.8, 0.85, 0.9, 0.95),
                       labels=c("60% TC", "65% TC", "70% TC", "75% TC", "80% TC", "85% TC", "90% TC", "95% TC")) + psim

projection2b <- ggplot(data=allscen_project2b, aes(x=date, y=I2wk, group=delta21, color=delta21)) +
  geom_line(size=1.5) + labs(x="Date", y="Two-Week Model Estimated\nCumulative New Infections Per 100K") +
  geom_line(data=model_output2, aes(x=date, y=I2wk), color="black", size=1.5) +
  geom_hline(yintercept=dirt, linetype="dashed", size=1, color="black") +
  annotate("text", x=as.Date("2020-11-07"), y=4000, size=14, label=paste("TCmin =", label_percent()(minTC_2b))) +
  annotate("text", x=as.Date("2021-04-10"), y=11000, size=16, label="2B") +
  scale_x_date(date_labels="%m/%d/%y", date_breaks=date.breaks, limits=date.limits) +
  scale_y_continuous(limits=c(0, 12000), breaks=c(0, dirt, seq(6000, 12000, 3000)), labels=scales::comma) + 
  scale_color_gradient(low="goldenrod1", high="darkblue", breaks=c(0.6, 0.65, 0.70, 0.75, 0.8, 0.85, 0.9, 0.95),
                       labels=c("60% TC", "65% TC", "70% TC", "75% TC", "80% TC", "85% TC", "90% TC", "95% TC")) + psim

# combine and save Scenario 2 plots
sim2plots <- ggarrange(projection2a, projection2b, common.legend=TRUE, legend="bottom")
ggsave(str_c(path, "/Thesis/Figures/sim2plots.png"), height=10, width=24, plot=sim2plots)



# plot two-week cumulative incidence
projection3a <- ggplot(data=allscen_project3a, aes(x=date, y=I2wk, group=delta21, color=delta21)) +
  geom_line(size=1.5) + labs(x="Date", y="Two-Week Model Estimated\nCumulative New Infections Per 100K") +
  geom_line(data=model_output3, aes(x=date, y=I2wk), color="black", size=1.5) +
  geom_hline(yintercept=dirt, linetype="dashed", size=1, color="black") +
  annotate("text", x=as.Date("2020-11-07"), y=4000, size=14, label=paste("TCmin =", label_percent()(minTC_3a))) +
  annotate("text", x=as.Date("2021-04-10"), y=11000, size=16, label="3A") +
  scale_x_date(date_labels="%m/%d/%y", date_breaks=date.breaks, limits=date.limits) +
  scale_y_continuous(limits=c(0, 12000), breaks=c(0, dirt, seq(6000, 12000, 3000)), labels=scales::comma) + 
  scale_color_gradient(low="goldenrod1", high="darkblue", breaks=c(0.6, 0.65, 0.70, 0.75, 0.8, 0.85, 0.9, 0.95),
                       labels=c("60% TC", "65% TC", "70% TC", "75% TC", "80% TC", "85% TC", "90% TC", "95% TC")) + psim

projection3b <- ggplot(data=allscen_project3b, aes(x=date, y=I2wk, group=delta21, color=delta21)) +
  geom_line(size=1.5) + labs(x="Date", y="Two-Week Model Estimated\nCumulative New Infections Per 100K") +
  geom_line(data=model_output3, aes(x=date, y=I2wk), color="black", size=1.5) +
  geom_hline(yintercept=dirt, linetype="dashed", size=1, color="black") +
  annotate("text", x=as.Date("2020-11-07"), y=4000, size=14, label=paste("TCmin =", label_percent()(minTC_3b))) +
  annotate("text", x=as.Date("2021-04-10"), y=11000, size=16, label="3B") +
  scale_x_date(date_labels="%m/%d/%y", date_breaks=date.breaks, limits=date.limits) +
  scale_y_continuous(limits=c(0, 12000), breaks=c(0, dirt, seq(6000, 12000, 3000)), labels=scales::comma) + 
  scale_color_gradient(low="goldenrod1", high="darkblue", breaks=c(0.6, 0.65, 0.70, 0.75, 0.8, 0.85, 0.9, 0.95),
                       labels=c("60% TC", "65% TC", "70% TC", "75% TC", "80% TC", "85% TC", "90% TC", "95% TC")) + psim

# combine and save Scenario 3 plots
sim3plots <- ggarrange(projection3a, projection3b, common.legend=TRUE, legend="bottom")
ggsave(str_c(path, "Thesis/Figures/sim3plots.png"), height=10, width=24, plot=sim3plots)




# plot two-week cumulative incidence
projection4a <- ggplot(data=allscen_project4a, aes(x=date, y=I2wk, group=delta21, color=delta21)) +
  geom_line(size=1.5) + labs(x="Date", y="Two-Week Model Estimated\nCumulative New Infections Per 100K") +
  geom_line(data=model_output4, aes(x=date, y=I2wk), color="black", size=1.5) +
  geom_hline(yintercept=dirt, linetype="dashed", size=1, color="black") +
  annotate("text", x=as.Date("2020-11-07"), y=4000, size=14, label=paste("TCmin =", label_percent()(minTC_4a))) +
  annotate("text", x=as.Date("2021-04-10"), y=11000, size=16, label="4A") +
  scale_x_date(date_labels="%m/%d/%y", date_breaks=date.breaks, limits=date.limits) +
  scale_y_continuous(limits=c(0, 12000), breaks=c(0, dirt, seq(6000, 12000, 3000)), labels=scales::comma) + 
  scale_color_gradient(low="goldenrod1", high="darkblue", breaks=c(0.6, 0.65, 0.70, 0.75, 0.8, 0.85, 0.9, 0.95),
                       labels=c("60% TC", "65% TC", "70% TC", "75% TC", "80% TC", "85% TC", "90% TC", "95% TC")) + psim

projection4b <- ggplot(data=allscen_project4b, aes(x=date, y=I2wk, group=delta21, color=delta21)) +
  geom_line(size=1.5) + labs(x="Date", y="Two-Week Model Estimated\nCumulative New Infections Per 100K") +
  geom_line(data=model_output4, aes(x=date, y=I2wk), color="black", size=1.5) +
  geom_hline(yintercept=dirt, linetype="dashed", size=1, color="black") +
  annotate("text", x=as.Date("2020-11-07"), y=4000, size=14, label=paste("TCmin =", label_percent()(minTC_4b))) +
  annotate("text", x=as.Date("2021-04-10"), y=11000, size=16, label="4B") +
  scale_x_date(date_labels="%m/%d/%y", date_breaks=date.breaks, limits=date.limits) +
  scale_y_continuous(limits=c(0, 12000), breaks=c(0, dirt, seq(6000, 12000, 3000)), labels=scales::comma) + 
  scale_color_gradient(low="goldenrod1", high="darkblue", breaks=c(0.6, 0.65, 0.70, 0.75, 0.8, 0.85, 0.9, 0.95),
                       labels=c("60% TC", "65% TC", "70% TC", "75% TC", "80% TC", "85% TC", "90% TC", "95% TC")) + psim

# combine and save Scenario 4 plots
sim4plots <- ggarrange(projection4a, projection4b, common.legend=TRUE, legend="bottom")
ggsave(str_c(path, "Thesis/Figures/sim4plots.png"), height=10, width=24, plot=sim4plots)

# plot two-week cumulative incidence
projection5a <- ggplot(data=allscen_project5a, aes(x=date, y=I2wk, group=delta21, color=delta21)) +
  geom_line(size=1.5) + labs(x="Date", y="Two-Week Model Estimated\nCumulative New Infections Per 100K") +
  geom_line(data=model_output5, aes(x=date, y=I2wk), color="black", size=1.5) +
  geom_hline(yintercept=dirt, linetype="dashed", size=1, color="black") +
  annotate("text", x=as.Date("2020-11-07"), y=4000, size=14, label=paste("TCmin =", label_percent()(minTC_5a))) +
  annotate("text", x=as.Date("2021-04-10"), y=11000, size=16, label="5A") +
  scale_x_date(date_labels="%m/%d/%y", date_breaks=date.breaks, limits=date.limits) +
  scale_y_continuous(limits=c(0, 12000), breaks=c(0, dirt, seq(6000, 12000, 3000)), labels=scales::comma) + 
  scale_color_gradient(low="goldenrod1", high="darkblue", breaks=c(0.6, 0.65, 0.70, 0.75, 0.8, 0.85, 0.9, 0.95),
                       labels=c("60% TC", "65% TC", "70% TC", "75% TC", "80% TC", "85% TC", "90% TC", "95% TC")) + psim

projection5b <- ggplot(data=allscen_project5b, aes(x=date, y=I2wk, group=delta21, color=delta21)) +
  geom_line(size=1.5) + labs(x="Date", y="Two-Week Model Estimated\nCumulative New Infections Per 100K") +
  geom_line(data=model_output5, aes(x=date, y=I2wk), color="black", size=1.5) +
  geom_hline(yintercept=dirt, linetype="dashed", size=1, color="black") +
  annotate("text", x=as.Date("2020-11-07"), y=4000, size=14, label=paste("TCmin =", label_percent()(minTC_5b))) +
  annotate("text", x=as.Date("2021-04-10"), y=11000, size=16, label="5B") +
  scale_x_date(date_labels="%m/%d/%y", date_breaks=date.breaks, limits=date.limits) +
  scale_y_continuous(limits=c(0, 12000), breaks=c(0, dirt, seq(6000, 12000, 3000)), labels=scales::comma) + 
  scale_color_gradient(low="goldenrod1", high="darkblue", breaks=c(0.6, 0.65, 0.70, 0.75, 0.8, 0.85, 0.9, 0.95),
                       labels=c("60% TC", "65% TC", "70% TC", "75% TC", "80% TC", "85% TC", "90% TC", "95% TC")) + psim

# combine and save Scenario 5 plots
sim5plots <- ggarrange(projection5a, projection5b, common.legend=TRUE, legend="bottom")
ggsave(str_c(path, "Thesis/Figures/sim5plots.png"), height=10, width=24, plot=sim5plots)

# plot two-week cumulative incidence
projection6a <- ggplot(data=allscen_project5a, aes(x=date, y=I2wk, group=delta21, color=delta21)) +
  geom_line(size=1.5) + labs(x="Date", y="Two-Week Model Estimated\nCumulative New Infections Per 100K") +
  geom_line(data=model_output6, aes(x=date, y=I2wk), color="black", size=1.5) +
  geom_hline(yintercept=dirt, linetype="dashed", size=1, color="black") +
  annotate("text", x=as.Date("2020-11-07"), y=4000, size=14, label=paste("TCmin =", label_percent()(minTC_6a))) +
  annotate("text", x=as.Date("2021-04-10"), y=11000, size=16, label="6A") +
  scale_x_date(date_labels="%m/%d/%y", date_breaks=date.breaks, limits=date.limits) +
  scale_y_continuous(limits=c(0, 12000), breaks=c(0, dirt, seq(6000, 12000, 3000)), labels=scales::comma) + 
  scale_color_gradient(low="goldenrod1", high="darkblue", breaks=c(0.6, 0.65, 0.70, 0.75, 0.8, 0.85, 0.9, 0.95),
                       labels=c("60% TC", "65% TC", "70% TC", "75% TC", "80% TC", "85% TC", "90% TC", "95% TC")) + psim

projection6b <- ggplot(data=allscen_project6b, aes(x=date, y=I2wk, group=delta21, color=delta21)) +
  geom_line(size=1.5) + labs(x="Date", y="Two-Week Model Estimated\nCumulative New Infections Per 100K") +
  geom_line(data=model_output6, aes(x=date, y=I2wk), color="black", size=1.5) +
  geom_hline(yintercept=dirt, linetype="dashed", size=1, color="black") +
  annotate("text", x=as.Date("2020-11-07"), y=4000, size=14, label=paste("TCmin =", label_percent()(minTC_6b))) +
  annotate("text", x=as.Date("2021-04-10"), y=11000, size=16, label="6B") +
  scale_x_date(date_labels="%m/%d/%y", date_breaks=date.breaks, limits=date.limits) +
  scale_y_continuous(limits=c(0, 12000), breaks=c(0, dirt, seq(6000, 12000, 3000)), labels=scales::comma) + 
  scale_color_gradient(low="goldenrod1", high="darkblue", breaks=c(0.6, 0.65, 0.70, 0.75, 0.8, 0.85, 0.9, 0.95),
                       labels=c("60% TC", "65% TC", "70% TC", "75% TC", "80% TC", "85% TC", "90% TC", "95% TC")) + psim

# combine and save Scenario 6 plots
sim6plots <- ggarrange(projection6a, projection6b, common.legend=TRUE, legend="bottom")
ggsave(str_c(path, "Thesis/Figures/sim6plots.png"), height=10, width=24, plot=sim6plots)

