#Modeling Morphometry along a latitudinal cline
library(extrafont)

cw_data<-readRDS("data/cw_data.rds")
#One aesthetic twist first:
cw_data$colour<-"black"
cw_data$colour[cw_data$Lat<=35.3]="red"

#First model, threw them in raw. 
m1<-lm(mass~Lat,data=cw_data)
summary(m1)
ggplot(data=cw_data,aes(x=Lat,y=mass,colour=colour))+geom_point(stat="identity")+scale_color_manual(values=c("black","red"),labels=c("Not Alabama","Alabama"))+xlab("Degrees Latitude")+ylab("Mass (g)")+stat_smooth(aes(group=1),method="lm")+theme_bw()+theme(text=element_text(family="Times New Roman",size=18))
#adheres to Bergmann's Rule, just like Jones Found.

#Same but for wingchord
m2<-lm(wg~Lat,cw_data)
summary(m2)
ggplot(data=cw_data,aes(x=Lat,y=wg,colour=colour))+geom_point(stat="identity")+scale_color_manual(values=c("black","red"),labels=c("Not Alabama","Alabama"))+xlab("Degrees Latitude")+ylab("Wing Chord (mm)")+stat_smooth(aes(group=1),method="lm")+theme_bw()+theme(text=element_text(family="Times New Roman",size=18))

#now we try a condition index
cw_data$ci<-cw_data$mass/blood$wg
m3<-lm(ci~Lat,cw_data)
summary(m3)
ggplot(data=cw_data,aes(x=Lat,y=ci,colour=colour))+geom_point(stat="identity")+scale_color_manual(values=c("black","red"),labels=c("Not Alabama","Alabama"))+xlab("Degrees Latitude")+ylab("Relative Condition")+stat_smooth(aes(group=1),method="lm")+theme_bw()+theme(text=element_text(family="Times New Roman",size=18))

cor(cw_data$mass,cw_data$wg,use="complete.obs")
#Limited correlation. Speaks to Doug's thought about migration distance with longer wings.


#---------------------------------------------------------------------------
#Models done for morphometry basics - now explore the migration trap: What happens w/o AL?
#---------------------------------------------------------------------------

noAL<-cw_data[which(cw_data$State!="AL"),]
m4<-lm(noAL$mass~noAL$Lat)
summary(m4)
ggplot(data=noAL,aes(x=Lat,y=mass,col=State))+geom_point(stat="identity")+xlab("Degrees Latitude")+ylab("Mass (g)")+stat_smooth(aes(group=1),method="lm")+theme_bw()+theme(text=element_text(family="Times New Roman",size=18))

m5<-lm(noAL$wg~noAL$Lat)
summary(m5)
ggplot(data=noAL,aes(x=Lat,y=wg,colour=State))+geom_point(stat="identity")+xlab("Degrees Latitude")+ylab("Wing Chord (mm)")+stat_smooth(aes(group=1),method="lm")+theme_bw()+theme(text=element_text(family="Times New Roman",size=12))

m6<-lm(noAL$ci~noAL$Lat)
summary(m6)
ggplot(data=noAL,aes(x=Lat,y=ci,colour=State))+geom_point(stat="identity")+xlab("Degrees Latitude")+ylab("Mass (g)")+stat_smooth(aes(group=1),method="lm")+theme_bw()+theme(text=element_text(family="Times New Roman",size=12))