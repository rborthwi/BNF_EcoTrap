#Testing inter-observer differences:
library(agricolae)

cw_data<-readRDS("data/cw_data.rds")

dt<-cw_data[which(cw_data$Collection_Team!="Doug Raybuck"),]
obs.m<-aov(mass~Collection_Team,dt)
summary(obs.m)
HSD.test(obs.m,"Collection_Team",alpha=0.05,group=TRUE,console=TRUE)

obs.m1<-aov(wg~Collection_Team,dt)
summary(obs.m1)
HSD.test(obs.m1,"Collection_Team",alpha=0.05,group=TRUE,console=TRUE)
#No differences
