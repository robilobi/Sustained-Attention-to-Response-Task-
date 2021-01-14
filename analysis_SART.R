####load libraries ######
library(ggplot2)
library(plyr)
library(readr)
library(ez)
library(reshape2)
library(gridExtra)

##################### ANALYSIS SART TASK ###################################################################
# input < Gorlla file, data in long format
# output > Results_SART.pdf (shows4 PLOTS:  RT in hit/FA/ % accuracy in no-go trials/ correlation bbetween RT 
# and FA, RT of trials before correct or incorrect no-GO trials)
# statitical tests: ANOVA, t-test, correlation
#################### Roberta Bianco -- January 2021 #########################################################


#### clean and load
ls()
rm(list=ls())
dir = "/Users/robertabianco/Documents/PROJECTs/ONLINE/CRM-SART/DATA/data_exp_35248-v14_N49_NOBONUSvsinLab/"
setwd(dir)
codes <- c("gfad")  ## SART task "gfad"
list_of_files <- list.files(recursive = TRUE, pattern = "\\.csv", full.names = TRUE)
list_of_files
tasks <- c()

for (i in 1:length(codes)){
  task <- grep(codes[i], list_of_files, value=TRUE, fixed=TRUE)
  tasks <- append(tasks, task, after=length(tasks))
}
alldat = ldply(tasks, read_csv)
dat = alldat
colnames(dat)[colnames(dat)=="Participant Public ID"] <- "subj"

#### select columns 
summary(dat)
table(dat$subj); dat$subj=as.factor(dat$subj)
length(unique(dat$subj))  ###note that one subject is NA (?)
colnames(dat)
dat=dat[, which(colnames(dat) %in% c("subj", "Trial Number","Stimulus", "Reaction Time", "Response","Answer" ,"Correct" , "Incorrect", "randomise_trials"))]
head(dat)
colnames(dat)[colnames(dat)=="Trial Number"] <- "TrN"
colnames(dat)[colnames(dat)=="Reaction Time"] <- "RT"
dat=droplevels(dat[dat$randomise_trials==4,])
dat = dat[!is.na(dat$subj),] ###remove raws with NaN in all columns 

dat$Response=as.factor(dat$Response)
dat$Answer=as.factor(dat$Answer)
dat$subj=as.factor(dat$subj)
accuracy<- with(dat, aggregate(cbind(Correct, Incorrect) ~ subj+Answer, FUN="sum"))

###compute % accuracy
accuracy$totNtrial= ifelse(accuracy$Answer=='Go', 200, 25)
accuracy$percInc= (accuracy$Incorrect*100)/accuracy$totNtrial
accuracy$percCor= (accuracy$Correct*100)/accuracy$totNtrial
rt<- with(dat, aggregate(cbind(RT) ~ subj+Answer+Correct, FUN="mean"))
rt$cond=paste0(rt$Answer, rt$Correct)
rt$cond=as.factor(rt$cond)
dfrt=rt[rt$cond=='Go1' | rt$cond=='No Go0',]
meanrt=with(dfrt, aggregate(RT~subj, FUN='mean'))
perf=dcast(dfrt, subj ~ cond, mean, value.var = "RT")
meanFA=with(accuracy[accuracy$Answer=='No Go',], aggregate(percInc~subj, FUN='mean'))
perf=merge(perf, meanFA, by=c('subj'))
perf=merge(perf, meanrt, by=c('subj'))
colnames(perf)[2]='SART RT_success'
colnames(perf)[3]='SART RT_failure'
colnames(perf)[4]='SART_percent'
colnames(perf)[5]='SART RT_mean'

#### write outputs csv
# write.table(accuracy, file=paste0(dir, "SART_accuracy.csv"), row.names=F, col.names=T, sep=",") ###write csv 
# write.table(rt, file=paste0(dir, "SART_rt.csv"), row.names=F, col.names=T, sep=",") ###write csv 
# write.table(perf, file=paste0(dir, "SART_online.csv"), row.names=F, col.names=T, sep=",") ###write csv 

#### correlate % incorrect in Go and no-go trials
perf= with(accuracy, aggregate(cbind(percInc) ~ subj+Answer, FUN="mean"))
perf=dcast(perf, subj ~ Answer, mean, value.var = "percInc")
cor.test(perf$Go, perf$`No Go`)

#####PLOT 1: RTS in Go and Nogo trials
s=15 ## font size for the plots
rt$cond=paste0(rt$Answer, rt$Correct)
rt$cond=as.factor(rt$cond)
dfrt=rt[rt$cond=='Go1' | rt$cond=='No Go0',]
p=ggplot(dfrt, aes(cond, RT, group=cond))+theme_bw()+
  stat_summary(fun=mean, geom="point")+
  stat_summary(fun=mean, geom="line")+
  geom_point(aes(group=subj, fill=Answer, color=Answer), shape=21, size=1)+
  geom_line(aes(group=subj), alpha=0.3, color='grey')+
  stat_summary(fun.data = 'mean_se', geom = "errorbar", width=0, alpha=0.6)+
  stat_summary(fun= 'mean',  geom ="point", size=2, shape=21)+
  xlab("") + ylab("Reaction times (ms)")+ggtitle('RT in hit and FA')+
  #coord_cartesian(ylim = c(1.3, 1.65))+
  theme( axis.text=element_text(size=s),
         axis.title=element_text(size=s),legend.title = element_text(size=s),
         legend.text = element_text(size = s))+theme(legend.position="none")

######PLOT2 : % accuracy in noGO trials
df=accuracy[accuracy$Answer=='No Go',]
p1=ggplot(df, aes(Answer, percInc))+theme_bw()+
  stat_summary(fun=mean, geom="point")+
  stat_summary(fun=mean, geom="line")+
  geom_jitter(aes(group=subj, fill=Answer, color=Answer), width=0.1,shape=21, size=1)+
  geom_line(aes(group=subj), alpha=0.3, color='grey')+
  stat_summary(fun.data = 'mean_se', geom = "errorbar", width=0, alpha=0.6)+
  stat_summary(fun= 'mean',  geom ="point", size=2, shape=21)+
  xlab("") + ylab("% No Go fail")+ggtitle('% accuracy in no-go trials')+
  coord_cartesian(ylim = c(0, 100))+
  theme( axis.text=element_text(size=s),
         axis.title=element_text(size=s),legend.title = element_text(size=s),
         legend.text = element_text(size = s))+theme(legend.position="none")

###### PLOT3 : Correlation between RT and FA
ag= with(dfrt, aggregate(cbind(RT) ~ subj, FUN="mean"))
df=merge(df, ag, by=c('subj'))
with(df, cor.test(percInc, RT)) # test stats
p2=ggplot(df, aes(x=percInc, y=RT)) + geom_point()+geom_smooth(method='lm')+theme_bw()+xlab('% No Go fail')+ggtitle('Correlation between RT and FA')

####### tests if RT preceding successful no-go trials were significantly longer than those preceding fail- ure to inhibit a response. 
dat$cond= paste0(dat$Answer, dat$Correct)
dat$RTpre = c(0,dat$RT[-length(dat$RT)]) 
df3=droplevels(dat[dat$Answer=='No Go',])
ag<- with(df3, aggregate(cbind(RTpre) ~ subj+cond, FUN="mean"))
ag$cond=as.factor(ag$cond)

library('afex')   #https://ademos.people.uic.edu/Chapter20.html#3_what_is_the_afex_package
library('emmeans')
fit_all <- aov_ez("subj","RTpre",ag,within=c( "cond")); fit_all
summary(fit_all)
knitr::kable(nice(fit_all))
m2 <- emmeans(fit_all, "cond")
pairs(m2)

ag2=dcast(ag, subj ~ cond, mean, value.var = "RTpre")
with(ag2, t.test(`No Go0`, `No Go1`, paired=T))

######PLOT4 : RT preceding correct vs incorrect no-go trials
p3=ggplot(ag, aes(cond, RTpre, group=cond))+theme_bw()+
  stat_summary(fun=mean, geom="point")+
  stat_summary(fun=mean, geom="line")+
  geom_point(aes(group=subj), shape=21, size=1)+
  geom_line(aes(group=subj), alpha=0.3, color='grey')+
  stat_summary(fun.data = 'mean_se', geom = "errorbar", width=0, alpha=0.6)+
  stat_summary(fun= 'mean',  geom ="point", size=2, shape=21)+
  xlab("") + ylab("Reaction times (ms)")+ggtitle('RT preceding correct vs incorrect no-go trials')+
  #coord_cartesian(ylim = c(1.3, 1.65))+
  theme( axis.text=element_text(size=s),
         axis.title=element_text(size=s),legend.title = element_text(size=s),
         legend.text = element_text(size = s))+theme(legend.position="none")


####### SAVE all plots together
pdf(file="Results_SART.pdf", 10,10)
grid.arrange(p,p1,p2,p3,ncol=2)
dev.off()

