
#### LW Survival ####
library(survival)
library(readr)
library(ggplot2)
library(survminer)
library(coxme)

#upload data frame 
lw_survival<-read_csv("lw_survival.csv")

#Set treatments as a factor
lw_surv<-as.data.frame(lw_survival)
lw_surv$treatment<-as.factor(lw_surv$treatment)
lw_surv$treatment<-factor(lw_surv$treatment, c("no_resource","water","cmbs_eggs","wheast","sugar","wheast_cmbs","sugar_cmbs"))
summary(lw_surv$treatment)

#create a formula using Surv(time,event), time=time until event occur. event= 1(event happened), 0(event did not happen)
lw_surv_form<-Surv(time=lw_surv$survival, event=(lw_surv$event))

#univariate analysis
cox_prop<-coxph(lw_surv_form~treatment, data=lw_surv) 
cox_prop
summary(cox_prop)

#mixed effects analysis
cox_prop<-coxme(lw_surv_form~treatment+(1|date), data=lw_surv)
cox_prop
summary(cox_prop)

#check assumptions of proportional hazards
ph_test<-cox.zph(cox_prop) #assumption is met (p>0.05)
ph_test
plot(ph_test)
ggcoxzph(ph_test)

#deviance- results are symmetrical around 0
#define variable being plotted using "strata"
cox_prop1<-coxph(lw_surv_form~strata(treatment), data=lw_surv)
ggcoxdiagnostics(cox_prop1, type = "deviance", linear.predictions = FALSE)

#post-hoc analysis
library(emmeans)
emmeans(cox_prop, pairwise~treatment)

#Plot all data
library(survminer)

#define variable being plotted using "strata"
cox_prop1<-coxph(lw_surv_form~strata(treatment), data=lw_surv)
fit2<-survfit(cox_prop1)
fit2


lw_plot<-ggsurvplot(fit2, data=lw_surv, risk.table=FALSE)
lw_plot


summary(fit2)$table[,"median"]


#Graph1
#fit2[c(1,2)] defines which curves are plotted based on the order of results shown with summary(cox_prop)
lw_plot1<-ggsurvplot(fit2[c(1,2)], data=lw_surv, 
                     legend.labs= c("No resource","Water"),
                     risk.table=FALSE, 
                     palette = c("darkgoldenrod1","aquamarine"),
                     xlab="Time (days)",xlim=c(0,60),break.x.by=5)
lw_plot1

lw_plot2<-ggsurvplot(fit2[c(1,2,3)], data=lw_surv,
                     legend.labs= c("No resource","Water","CMBS"),
                     risk.table=FALSE,
                     palette = c("darkgoldenrod1","aquamarine","hotpink"),
                     xlab="Time (days)",xlim=c(0,60),break.x.by=5)
lw_plot2

lw_plot3<-ggsurvplot(fit2[c(1,2,3,4,5)], data=lw_surv,
                     legend.labs= c("No resource","Water","CMBS","Wheast","Sugar"),
                     risk.table=FALSE,
                     palette = c("darkgoldenrod1","aquamarine","hotpink","bisque2","cornflowerblue"),
                     xlab="Time (days)",xlim=c(0,60),break.x.by=5)
lw_plot3

#use "legend.title=" to change "strata" to "treatment"
lw_plot4<-ggsurvplot(fit2, data=lw_surv,
                     legend=c(0.8,0.7),
                     legend.labs= c("No resource","Water","CMBS","Wheast","Fructose","Wheast & CMBS","Fructose & CMBS"),
                     risk.table=FALSE, legend.title="Treatment",
                     palette = c("darkgoldenrod1","aquamarine","hotpink","bisque2","cornflowerblue","coral","darkorchid1"),
                     xlab="Time (days)",xlim=c(0,60),break.x.by=5)

lw_plot4




#### SDB survival ####
sdb_survival<-read_csv("sdb_survival.csv")

sdb_surv<-as.data.frame(sdb_survival)
sdb_surv$treatment<-as.factor(sdb_surv$treatment)
sdb_surv$treatment<-factor(sdb_surv$treatment, c("no_resource","water","cmbs_eggs","wheast","sugar","wheast_cmbs","sugar_cmbs"))

summary(sdb_surv$treatment)

sdb_surv_form<-Surv(time=sdb_surv$survival, event=(sdb_surv$event))


#univariate analysis
cox_prop2<-coxph(sdb_surv_form~treatment, data=sdb_surv)
cox_prop2

#mixed effects analysis
cox_prop1<-coxme(sdb_surv_form~treatment+(1|date), data=sdb_surv)
cox_prop1
summary(cox_prop1)


#check assumptions of proportional hazards
ph_test<-cox.zph(cox_prop1) #assumption is not met, p<0.05; suggested to add interaction of covariate*time
ph_test
plot(ph_test)
ggcoxzph(ph_test)

#deviance- are results symmetrical around 0?
ggcoxdiagnostics(cox_prop2, type = "deviance", linear.predictions = FALSE)

library(emmeans)
emmeans(cox_prop1, pairwise~treatment)

#define factor to graph
cox_prop3<-coxph(sdb_surv_form~strata(treatment), data=sdb_surv)
fit3<-survfit(cox_prop3)
fit3
summary(fit3)$table[,"median"]

ggsurvplot(fit3, data=sdb_surv)

sdb_plot1<-ggsurvplot(fit3[c(1,2)], data=sdb_surv, 
                      legend.labs= c("No resource","Water"),
                      risk.table=FALSE, 
                      palette = c("darkgoldenrod1","aquamarine"),
                      xlab="Time (days)",xlim=c(0,60),break.x.by=5)
sdb_plot1

sdb_plot2<-ggsurvplot(fit3[c(1,2,3)], data=sdb_survival,
                      legend.labs= c("No resource","Water","CMBS"),
                      risk.table=FALSE,
                      palette = c("darkgoldenrod1","aquamarine","hotpink"),
                      xlab="Time (days)",xlim=c(0,60),break.x.by=5)
sdb_plot2

sdb_plot3<-ggsurvplot(fit3[c(1,2,3,4,5)], data=sdb_survival,
                      legend.labs= c("No resource","Water","CMBS","Wheast","Sugar"),
                      risk.table=FALSE,
                      palette = c("darkgoldenrod1","aquamarine","hotpink","bisque2","cornflowerblue"),
                      xlab="Time (days)",xlim=c(0,60),break.x.by=5)
sdb_plot3

sdb_plot4<-ggsurvplot(fit3, data=sdb_survival,
                      legend=c(0.8,0.7),
                      legend.labs= c("No resource","Water","CMBS","Wheast","Fructose","Wheast & CMBS","Fructose & CMBS"),
                      risk.table=FALSE, legend.title="Treatments",
                      palette = c("darkgoldenrod1","aquamarine","hotpink","bisque2","cornflowerblue","coral","darkorchid1"),
                      xlab="Time (days)",xlim=c(0,60),break.x.by=5)
sdb_plot4


###LW Development###

#### Proportion of lacewings that molted ####

library(readr)
library(ggplot2)
library(lme4)
library(emmeans)
library(car)

lw_survival<-read_csv("lw_survival.csv")

#Set treatments as a factor
molt1_event<-as.data.frame(lw_survival)
molt1_event$event1<-as.factor(molt1_event$event1)
molt1_event$event2<-as.factor(molt1_event$event2)


#Fit binomial model for molt 1
bin_molt1<-glmer(event1~treatment + (1|date), binomial(link="logit"), data=molt1_event)
bin_molt1
summary(bin_molt1) 


Anova(bin_molt1)
emmeans(bin_molt1, pairwise~treatment)

#Fit binomial model for molt 2
library(dplyr)
lw_surv_molt<-as.data.frame(lw_survival)
lw_molt2 <- lw_surv_molt %>% filter(treatment %in% c("wheast", "wheast_cmbs","sugar_cmbs"))
lw_molt2$treatment <- as.factor(lw_molt2$treatment)
lw_molt2$treatment<-factor(lw_molt2$treatment, c("wheast","wheast_cmbs","sugar_cmbs"))

bin_molt2<-glmer(event2~treatment + (1|date), binomial(link="logit"), data=lw_molt2)
summary(bin_molt2)
Anova(bin_molt2)
emmeans(bin_molt2, pairwise~treatment)

#graph the data
percent_molt <- read_csv("percent_molt.csv")
percent_molt$treatment<-as.factor(percent_molt$treatment)
percent_molt$treatment <- factor(percent_molt$treatment, c('no_resource','water','cmbs','sugar','wheast','sugar_cmbs','wheast_cmbs'))

percentage_molted<-ggplot(percent_molt, mapping=aes(x=treatment, y=percent, fill=treatment))+
  geom_bar(stat = "identity", color="black")+
  scale_fill_manual(values = c("darkgoldenrod1","aquamarine","hotpink","cornflowerblue","bisque2","darkorchid1","coral"))+
  xlab("Treatment")+
  ylab("Percentage molted")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 40, vjust = 0.6, hjust=0.5), axis.title.x = element_blank(), axis.text.y=element_text(size=10), axis.title.y=element_text(size=15))+
  scale_x_discrete(labels=c('No resource', 'Water', 'CMBS', 'Fructose','Wheast','Fructose & CMBS', 'Wheast & CMBS'))+
  labs(fill='LW Development')+
  facet_wrap(~molt, labeller=labeller(molt=
                                        c("molt1"= "Molt 1",
                                          "molt2"="Molt 2")))+
  theme(legend.position="none")


percentage_molted



####Days to molt####

#### molting curves ####

#survival to molt- reanalyzing molt data
library(readr)
library(survival)
library(survminer)
library(ggplot2)
library(coxme)
lw_survival<-read_csv("lw_survival.csv")

#Set treatments as a factor
lw_surv_molt<-as.data.frame(lw_survival)


#molt1
lw_surv_molt$treatment<-factor(lw_surv_molt$treatment, c("no_resource","water","cmbs_eggs","wheast","sugar","wheast_cmbs","sugar_cmbs"))

#create a formula using Surv(time,event), time=time until event occur. event= 1(event happened), 0(event did not happen)
lw_molt1_surv<-Surv(time=lw_surv_molt$molt1, event=(lw_surv_molt$event1))

#mixed effects analysis
cox_prop3<-coxme(lw_molt1_surv~treatment+(1|date), data=lw_surv_molt)
cox_prop3
summary(cox_prop3)


#check assumptions of proportional hazards
ph_test<-cox.zph(cox_prop3)   
ph_test
plot(ph_test)

#deviance- results are symmetrical around 0
cox_prop4<-coxph(lw_molt1_surv~treatment, data=lw_surv_molt)
ggcoxdiagnostics(cox_prop4, type = "deviance", linear.predictions = FALSE)

#post-hoc analysis
library(emmeans)
emmeans(cox_prop3, pairwise~treatment)

#Plot all data
library(survminer)

#define variable being plotted using "strata"
cox_prop4<-coxph(lw_molt1_surv~strata(treatment), data=lw_surv_molt)
fit3<-survfit(cox_prop4)
fit3
summary(fit3)$table[,"median"]

lw_molt1_curv<-ggsurvplot(fit3, data=lw_surv_molt, legend="none",
                          risk.table=FALSE,
                          palette = c("darkgoldenrod1","aquamarine","hotpink","bisque2","cornflowerblue","coral","darkorchid1"),
                          xlab="Time (days)",xlim=c(0,50),break.x.by=5,
                          ylab= "Molting probability")
lw_molt1_curv

#assessing median days to molt
lw_survival %>% 
  group_by(treatment) %>% 
  summarize(median_raw_molt1 = median(raw_molt1, na.rm=TRUE))

#molt2
library(dplyr)
lw_molt2 <- lw_surv_molt %>% filter(treatment %in% c("wheast", "wheast_cmbs","sugar_cmbs"))
lw_molt2$treatment <- as.factor(lw_molt2$treatment)
lw_molt2$treatment<-factor(lw_molt2$treatment, c("wheast","sugar_cmbs","wheast_cmbs"))

lw_molt2_surv<-Surv(time=lw_molt2$molt2, event=(lw_molt2$event2))

#mixed effects analysis
cox_prop3<-coxme(lw_molt2_surv~treatment+(1|date), data=lw_molt2)
cox_prop3
summary(cox_prop3)

#check assumptions of proportional hazards
ph_test<-cox.zph(cox_prop3)   
ph_test
plot(ph_test)

#deviance- results are symmetrical around 0
cox_prop4<-coxph(lw_molt2_surv~treatment, data=lw_molt2)
ggcoxdiagnostics(cox_prop4, type = "deviance", linear.predictions = FALSE)

#post-hoc analysis
library(emmeans)
emmeans(cox_prop3, pairwise~treatment)

#Plot all data
library(survminer)

#define variable being plotted using "strata"
cox_prop4<-coxph(lw_molt2_surv~strata(treatment), data=lw_molt2)
fit2<-survfit(cox_prop4)
fit2

summary(fit2)$table[,"median"]

lw_molt2_curv<-ggsurvplot(fit2, data=lw_surv_molt, legend="none",
                          risk.table=FALSE, legend.title="Treatment",
                          palette = c("bisque2","darkorchid1","coral"),
                          xlab="Time (days)",xlim=c(0,50),break.x.by=5,
                          ylab= "Molting probability")
lw_molt2_curv

#assessing median days to molt
lw_survival %>% 
  group_by(treatment) %>% 
  summarize(median_raw_molt2 = median(raw_molt2, na.rm=TRUE))

#combine graphs
library(ggpubr)
require(grid)
days_to_molt_curves <- arrange_ggsurvplots(lw_molt1_curv, lw_molt2_curv,
                                           ncol = 2, nrow = 1)





#old code for molting analysis

#still do logistic for percentage for first and second molt
#first molt, analyze all treatments, using cph model for first molt and treatments for all molters in each treatment
#graphs= replace boxplots with survival curves, only includes treatments in which only one individuals

### MOLT 1 GLM
library(readr)
library(lme4)
glm_molt1<-glmer(raw_molt1~treatment+(1|date), poisson(link = 'log'), data=lw_survival)
summary(glm_molt1)

library(car)
Anova(glm_molt1)
emmeans(glm_molt1)

#SECOND MOLT
glm_molt2<-glm(molt2~treatment,poisson(link = 'log'), data=lw_survival)
Anova(glm_molt2)
summary(glm_molt2)


#GRAPH THE DATA
library(readr)
lw_molting <- read_csv("lw_molting.csv")
new.map_lwm<-na.omit(lw_molting)

new.map_lwm$treatment<-as.factor(new.map_lwm$treatment)
new.map_lwm$treatment <- factor(new.map_lwm$treatment, c('no_resource','water','wheast','sugar','cmbs_eggs','wheast_cmbs','sugar_cmbs'))

#Boxplots on same graph
library(ggplot2)
ggplot(new.map_lwm, mapping=aes(x=treatment, y=day, fill=molt))+
  geom_boxplot(aes(fill=molt))+
  theme_bw()+
  xlab("Treatment")+
  ylab("Days to molt")+
  scale_x_discrete(labels=c('No resource', 'Water', 'Wheast', 'Fructose','CMBS','Wheast & CMBS', 'Fructose & CMBS'))+
  labs(fill='LW Development')+
  scale_fill_manual(labels= c("Molt 1","Molt 2"),values=c('mistyrose2','khaki2'))

#Panelled Boxplots
days_to_molt<-ggplot(new.map_lwm, mapping=aes(x=treatment, y=day, fill=molt))+
  geom_boxplot(aes(fill=molt))+
  theme_bw()+
  xlab("Treatment")+
  ylab("Days to molt")+
  scale_x_discrete(labels=c('No resource', 'Water', 'Wheast', 'Fructose','CMBS','Wheast & CMBS', 'Fructose & CMBS'))+
  labs(fill='LW Development')+
  scale_fill_manual(labels= c("Molt 1","Molt 2"),values=c('mistyrose2','khaki2'))+
  facet_wrap(~molt, labeller=labeller(molt=
                                        c("molt1"= "Molt 1",
                                          "molt2"="Molt 2")))+
  theme(axis.text.x = element_text(angle = 40, vjust = 0.6, hjust=0.5))+
  theme(legend.position="none")

#Panel proportion molted and day taken to molt
library(ggpubr)
lw_development <- ggarrange(percentage_molted, days_to_molt,
                            labels = c("A", "B"),
                            ncol = 1, nrow = 2)
lw_development
#Set treatments as a factor

molt1_event <- read_csv("molt1_event.csv")
molt1_event$event<-as.factor(molt1_event$event1)
molt1_event$event<-as.factor(molt1_event$event2)

molt1_event$treatment<-factor(molt1_event$treatment, c("no_resource","water","cmbs_eggs","wheast","sugar","wheast_cmbs","sugar_cmbs"))

#create a formula using Surv(time,event), time=time until event occur. event= 1(event happened), 0(event did not happen)
lw_molt1_surv<-Surv(time=molt1_event$molt1, event=(molt1_event$event1))

#mixed effects analysis
cox_prop3<-coxme(lw_molt1_surv~treatment+(1|date), data=molt1_event)
cox_prop3
summary(cox_prop3)

#check assumptions of proportional hazards
ph_test<-cox.zph(cox_prop3) #assumption is not met, p<0.05; suggested to add interaction of covariate*time
ph_test
plot(ph_test)

#deviance- results are symmetrical around 0
cox_prop4<-coxph(lw_molt1_surv~treatment, data=molt1_event)
ggcoxdiagnostics(cox_prop4, type = "deviance", linear.predictions = FALSE)

#post-hoc analysis
library(emmeans)
emmeans(cox_prop3, pairwise~treatment)

#Plot all data
library(survminer)

#define variable being plotted using "strata"
cox_prop3<-coxph(lw_molt1_surv~strata(treatment), data=molt1_event)
fit2<-survfit(cox_prop3)
fit2
lw_plot<-ggsurvplot(fit2, data=molt1_event, risk.table=FALSE)
lw_plot

#molt 2 survival
molt1_event$event<-as.factor(molt1_event$event2)

#subset data where at least 1 individual molted
library(dplyr)

lw_molt2 <- molt1_event %>% filter(treatment %in% c("wheast", "wheast_cmbs","sugar_cmbs"))
lw_molt2$treatment <- as.factor(lw_molt2$treatment)


#create a formula using Surv(time,event), time=time until event occur. event= 1(event happened), 0(event did not happen)
lw_molt2_surv<-Surv(time=lw_molt2$molt2, event=(lw_molt2$event2))

#mixed effects analysis
cox_prop3<-coxme(lw_molt2_surv~treatment+(1|date), data=lw_molt2)
cox_prop3
summary(cox_prop3)

#check assumptions of proportional hazards
ph_test<-cox.zph(cox_prop3) #assumption is not met, p<0.05; suggested to add interaction of covariate*time
ph_test
plot(ph_test)

#deviance- results are symmetrical around 0
cox_prop4<-coxph(lw_molt2_surv~treatment, data=molt1_event)
ggcoxdiagnostics(cox_prop4, type = "deviance", linear.predictions = FALSE)

#post-hoc analysis
library(emmeans)
emmeans(cox_prop3, pairwise~treatment)

#Plot all data
library(survminer)

#define variable being plotted using "strata"
cox_prop3<-coxph(lw_molt2_surv~strata(treatment), data=molt1_event)
fit2<-survfit(cox_prop3)
fit2
lw_plot<-ggsurvplot(fit2, data=molt1_event, risk.table=FALSE)
lw_plot




####CHOICE ASSAYS####

#load package
choice<- read_csv("choice.csv")

# LACEWING CHOICE ASSAYS
#subset data
lw_total_choice<-choice[choice$predator=='lacewing',]

#remove non-choosers
lw_choice<-na.omit(lw_total_choice)

#WHEAST
lw_wheast<-lw_choice[lw_choice$treatment=='wheast_control',]
lw_wheast$lw_wheast_char<-ifelse(lw_wheast$choice == 0, "water", "wheast")
library(dplyr)
wheast_stats<- lw_wheast %>%
  group_by(lw_wheast_char) %>%
  summarize(count=n())
chisq.test(wheast_stats$count)

#CMBS
lw_cmbs<-lw_choice[lw_choice$treatment=='cmbs_control',]
lw_cmbs$lw_cmbs_char<-ifelse(lw_cmbs$choice == 0, "water", "wheast")
library(dplyr)
cmbs_stats<- lw_cmbs %>%
  group_by(lw_cmbs_char) %>%
  summarize(count=n())
chisq.test(cmbs_stats$count)

#APHID
lw_aphid<-lw_choice[lw_choice$treatment=='aphids_control',]
lw_aphid$lw_aphid_char<-ifelse(lw_aphid$choice == 0, "water", "wheast")
library(dplyr)
aphid_stats<- lw_aphid %>%
  group_by(lw_aphid_char) %>%
  summarize(count=n())
chisq.test(aphid_stats$count)

#Plot percentage of choices
lw_choice_percent <- read_csv("lw_choice_percent.csv")
library(ggplot2)

lw_plot <- ggplot(lw_choice_percent, aes(x = treatment)) +
  geom_col(data = subset(lw_choice_percent, choice == "treatment"), 
           aes(y = percent , fill = 'Treatment'), width=0.5,color='black') +
  geom_col(data = subset(lw_choice_percent, choice == "control"), 
           aes(y = -percent, fill = 'Control'),width=0.5,color='black') + coord_flip() +
  scale_fill_manual(values= c("lightgray","darkseagreen3"))+
  theme_bw()+
  ylab("Percent choice")+
  scale_y_continuous(limits= c(-100,100),breaks = seq(-100,100, by = 10),
                     labels = (c(seq(100, 0, by = -10), seq(10,100,by=10))))+
  annotate("text",x=3,y=-80, label="Water")+
  annotate("text",x=2,y=-80, label="No prey")+
  annotate("text",x=1,y=-80, label="No prey")+
  annotate("text",x=3,y=90, label="Wheast")+
  annotate("text",x=2,y=90, label="CMBS")+
  annotate("text",x=1,y=90, label="Aphids")+
  theme(axis.text.y = element_blank(), 
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")
lw_plot

#Bar graph of total reps for lacewings
library(readr)
library(reshape2)

x<-c("Wheast", "CMBS","Aphids") #define treatments
y1<-c(12,23,14) #total number of lacewings that did not choose
y2<-c(12,3,13) #total number of lacewings that chose treatment
y3<-c(8,4,7) #total number of lacewings that chose control

to_plot <- data.frame(x=x,y1=y1,y2=y2, y3=y3)
melted<-melt(to_plot, id='x')
melted$x<-factor(melted$x,c("Wheast","CMBS","Aphids"))

total_lw<-ggplot(melted,aes(x=x,y=value,fill=variable))+ 
  geom_bar(stat="identity", width=0.6, color="black")+
  scale_fill_manual(values=c("lightgoldenrod2","deepskyblue4","darkolivegreen4"),labels=c('Non-choosers', 'Chose treatment', 'Chose blank'))+
  ylab("Number of predators")+
  theme_bw()+
  theme(axis.title.x=element_blank(),legend.title=element_blank(),legend.position="top", legend.text=element_text(size=12), axis.title.y=element_text(size=15),axis.text=element_text(size=15))

total_lw

library(ggpubr)
lw_choice_combo <- ggarrange(total_lw, total_sdb,
                             labels = c("A", "B"),
                             ncol = 2, nrow = 1)
lw_choice_combo


##sdb choice

library(readr)
choice<-read_csv("choice.csv")

sdb_total_choices<-choice[choice$predator=='sdb',]

sdb_choice<- na.omit(sdb_total_choices)

#WHEAST
sdb_wheast<-sdb_choice[sdb_choice$treatment=='wheast_control',]
sdb_wheast$sdb_wheast_char<-ifelse(sdb_wheast$choice == 0, "water", "wheast")
library(dplyr)
sdb_wheast_stats<- sdb_wheast %>%
  group_by(sdb_wheast_char) %>%
  summarize(count=n())
chisq.test(sdb_wheast_stats$count)

#CMBS
sdb_cmbs<-sdb_choice[sdb_choice$treatment=='cmbs_control',]
sdb_cmbs$sdb_cmbs_char<-ifelse(sdb_cmbs$choice == 0, "water", "wheast")
library(dplyr)
sdb_cmbs_stats<- sdb_cmbs %>%
  group_by(sdb_cmbs_char) %>%
  summarize(count=n())
chisq.test(sdb_cmbs_stats$count)

#APHIDS
sdb_aphids<-sdb_choice[sdb_choice$treatment=='aphids_control',]
sdb_aphids$sdb_aphids_char<-ifelse(sdb_aphids$choice == 0, "water", "aphids")
library(dplyr)
sdb_aphids_stats<- sdb_aphids %>%
  group_by(sdb_aphids_char) %>%
  summarize(count=n())
chisq.test(sdb_aphids_stats$count)


#Plot percentage of choices made
sdb_choice_percent<-read_csv("sdb_choice_percent.csv")

sdb_plot <- ggplot(sdb_choice_percent, aes(x = treatment)) +
  geom_col(data = subset(sdb_choice_percent, choice == "treatment"), 
           aes(y = percent , fill = 'Treatment'), width=0.5,color='black') +
  geom_col(data = subset(sdb_choice_percent, choice == "control"), 
           aes(y = -percent, fill = 'Control'),width=0.5,color='black') + coord_flip() +
  scale_fill_manual(values= c("lightgray","darkseagreen3"))+
  theme_bw()+
  ylab("Percent choice")+
  scale_y_continuous(limits= c(-100,100),breaks = seq(-100,100, by = 10),
                     labels = (c(seq(100, 0, by = -10), seq(10,100,by=10))))+
  annotate("text",x=3,y=-80, label="Water")+
  annotate("text",x=2,y=-80, label="No prey")+
  annotate("text",x=1,y=-80, label="No prey")+
  annotate("text",x=3,y=90, label="Wheast")+
  annotate("text",x=2,y=90, label="CMBS")+
  annotate("text",x=1,y=90, label="Aphids")+
  theme(axis.text.y = element_blank(), 
        axis.ticks.y=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "none")
sdb_plot

#Plot all assays
x<-c("Wheast", "CMBS","Aphids") #define treatments
y1<-c(19,15,4) #total number of lacewings that did not choose
y2<-c(14,11,14) #total number of lacewings that chose treatment
y3<-c(6,9,6) #total number of lacewings that chose control

to_plot <- data.frame(x=x,y1=y1,y2=y2, y3=y3)
melted<-melt(to_plot, id='x')
melted$x<-factor(melted$x,c("Wheast","CMBS","Aphids"))

total_sdb<-ggplot(melted,aes(x=x,y=value,fill=variable))+ 
  geom_bar(stat="identity", width=0.6, color="black")+
  scale_fill_manual(values=c("lightgoldenrod2","deepskyblue4","darkolivegreen4"),labels=c('Non-choosers', 'Chose treatment', 'Chose blank'))+
  theme_bw()+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.title=element_blank(),legend.position="top", legend.text=element_text(size=12), axis.text=element_text(size=15))
total_sdb


####SDB Residency####

#randomization
control<-rep('control', 8)
wheast_paste<-rep('wheast_paste',8)
wheast_spray<-rep('wheast_spray',8)
fructose<-rep('fructose',8)
sprite<-rep('sprite',8)

ran<-sample(c(control, wheast_paste,wheast_spray, fructose, sprite),40)
ran

library(readr)
library(car)
sdb_res <- read_csv("sdb_res.csv")

sdb_res$ant_rating<-as.factor(sdb_res$ant_rating)
sdb_res$final_ant<-as.factor(sdb_res$final_ant)
sdb_res$treatment<-as.factor(sdb_res$treatment)

#Calculate mean number of SDBS on each treatment
library(plotrix)
library(dplyr)

sdb_res %>% 
  group_by(treatment) %>% 
  summarize(mean_sdb_sightings = mean(sdb_sightings, na.rm=TRUE), std.error(sdb_sightings))


#analyze number of SDBs sighted over the entire study

sightings<-glm(sdb_sightings ~ treatment + final_ant, family=poisson(link='log'), data=sdb_res)
summary(sightings)

Anova(sightings)
emmeans(sightings, pairwise~treatment)

#WE USED THIS ONE
sightings2_final <-glm(sdb_sightings ~ treatment + final_ant+ final_cmbs , family=poisson(link='log'), data=sdb_res)
summary(sightings2_final)
Anova(sightings2_final)
library(emmeans)
emmeans(sightings2_final, pairwise~treatment)



sightings2_initial <-glm(sdb_sightings ~ treatment + final_ant + total_cmbs, family=poisson(link='log'), data=sdb_res)
summary(sightings2_initial)
Anova(sightings2_initial)
emmeans(sightings2_initial, pairwise~treatment)

summary(lm(sdb_res$total_cmbs~sdb_res$final_cmbs))

library(ggplot2)

#sightings vs ant ratings
ggplot(sdb_res, mapping=aes(y= sdb_sightings, x=final_ant))+
  geom_boxplot()

#sightings vs cmbs density

sdb_res$percent_cmbs<-(sdb_res$final_cmbs * 10)

ggplot(sdb_res, mapping=aes(x=percent_cmbs, y=sdb_sightings))+
  geom_smooth()+
  theme_bw()+
  ylab("Number of R. lophanthae observed")+
  xlab("Plant area with CMBS (%)")

#cmbs density over the various treatments
ggplot(sdb_res, mapping=aes(x=treatment, y=final_cmbs))+
  geom_boxplot()

#ant vs cmbs density
ggplot(sdb_res, mapping=aes(x=final_ant, y=final_cmbs))+
  geom_boxplot()

#treatment vs ant
ggplot(sdb_res, mapping=aes(x=treatment, y=final_ant))+
  geom_count()

ggplot(sdb_res, mapping=aes(x=total_cmbs, y=final_cmbs))+
  geom_smooth()+
  geom_point()

#analyze the cumulative number of days beetles were present

sdb_res %>% 
  group_by(treatment) %>% 
  summarize(mean_pres_ab = mean(pres_ab, na.rm=TRUE), std.error(pres_ab))

days_sighted<-glm(pres_ab~treatment + final_ant + final_cmbs, family=poisson(link='log'), data=sdb_res)
summary(days_sighted)
Anova(days_sighted)


library(emmeans)
emmeans(days_sighted, pairwise~treatment)


ggplot(sdb_res, mapping=aes(x=treatment, y=sdb_sightings))+
  geom_boxplot(fill="peachpuff2")+
  ylab("SDB sightings")+
  xlab("Treatments")+
  scale_x_discrete(labels=c("Control","Fructose","Sprite","Wheast paste","Wheast spray"))+
  theme_bw()

ggplot(sdb_res, mapping=aes(x=ant_rating, y=sdb_sightings))+
  geom_boxplot()

ggplot(sdb_res, mapping=aes(x=total_cmbs, y=sdb_sightings))+
  geom_boxplot()

###PANEL RESIDENCY
#CONTROL
library(readr)
library(ggplot2)
control_res<-read_csv("control_res.csv")
control_res$plant<-as.factor(control_res$plant)

control<-ggplot(control_res, mapping=aes(x=day, y=num_sdb, colour=plant))+
  geom_line()+
  theme_bw()+
  xlab("Time (days)")+
  ylab("Number of SDB")+
  scale_colour_manual(values=c("lightgray","lightgray","lightgray","lightgray","lightgray","lightgray","lightgray","lightgray","black"))+
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(),legend.position="none")+
  annotate("text", x=28, y=3.75, label="A) Control", size=3)
control

###SPRITE

sprite_res<-read_csv("sprite_res.csv")
sprite_res$plant<-as.factor(sprite_res$plant)

sprite<-ggplot(sprite_res, mapping=aes(x=day, y=num_sdb, colour=plant))+
  geom_line()+
  theme_bw()+
  xlab("Time (days)")+
  ylab("Number of SDB")+
  scale_colour_manual(values=c("lightgray","lightgray","lightgray","lightgray","lightgray","lightgray","lightgray","lightgray","black"))+
  theme(axis.title.x=element_blank(), axis.title.y=element_blank(),legend.position="none")+
  annotate("text", x=28, y=3.75, label="B) Sprite", size=3)
sprite

#Fructose

fructose_res<-read_csv("fructose_res.csv")
fructose_res$plant<-as.factor(fructose_res$plant)

fructose<-ggplot(fructose_res, mapping=aes(x=day, y=num_sdb, colour=plant))+
  geom_line()+
  theme_bw()+
  xlab("Time (days)")+
  ylab("Number of SDB")+
  scale_colour_manual(values=c("lightgray","lightgray","lightgray","lightgray","lightgray","lightgray","lightgray","lightgray","black"))+
  theme(axis.title.x=element_blank(),axis.title.y=element_blank(), legend.position="none")+
  annotate("text", x=28, y=3.75, label="C) Fructose", size=3)+
  scale_y_continuous(limits=c(0,4))
fructose

#Wheast Paste

wheast_paste_res<-read_csv("wheast_paste_res.csv")
wheast_paste_res$plant<-as.factor(wheast_paste_res$plant)

wheast_paste<-ggplot(wheast_paste_res, mapping=aes(x=day, y=num_sdb, colour=plant))+
  geom_line()+
  theme_bw()+
  xlab("Time (days)")+
  ylab("Number of SDB")+
  scale_colour_manual(values=c("lightgray","lightgray","lightgray","lightgray","lightgray","lightgray","lightgray","lightgray","black"))+
  theme(axis.title.x = element_blank(), axis.title.y = element_blank(),legend.position="none")+
  annotate("text", x=25.5, y=3.75, label="D) Wheast paste", size=3)
wheast_paste

#Wheast spray

wheast_spray_res<-read_csv("wheast_spray_res.csv")
wheast_spray_res$plant<-as.factor(wheast_spray_res$plant)

wheast_spray<-ggplot(wheast_spray_res, mapping=aes(x=day, y=num_sdb, colour=plant))+
  geom_line()+
  theme_bw()+
  xlab("Time (days)")+
  ylab("Number of SDB")+
  scale_colour_manual(values=c("lightgray","lightgray","lightgray","lightgray","lightgray","lightgray","lightgray","lightgray","black"))+
  theme(axis.title.x = element_blank(),axis.title.y=element_blank(),legend.position="none")+
  annotate("text", x=25.5, y=3.75, label="E) Wheast spray",size=3)
wheast_spray


library(ggpubr)
require(grid)
residency_plot <- ggarrange(control, sprite, fructose, wheast_paste, wheast_spray,
                            ncol = 3, nrow = 2,
                            label.x="Time (days)")
residency_plot
final_residency<-annotate_figure(residency_plot,left = textGrob("Number of R. lophanthae", rot = 90), bottom = textGrob("Time (days)"))
final_residency

#Statistical analysis
library(readr)
library(lme4)
library(car)
stats_res <- read_csv("stats_res.csv")

glm_res<-glmer(num_sdb~treatment*day+(1+day|plant), family=poisson(link="log"), data=stats_res)
glm_res
summary(glm_res)
Anova(glm_res)
