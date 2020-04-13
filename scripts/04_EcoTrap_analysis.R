##Using the data from Exploratory Code we're looking at support for ecological traps.
fivenum(cw_data$Lat)
so<-cw_data[which(cw_data$Lat<=36.0700),]
table(so$Sex)
so$Sex<-ifelse(so$Sex=="F","F","M")
sr<-so[which(so$Age2=="ASY"),]

##mass significant, extract it:
reg.m4<-aov(mass~Region,data=sr)
summary(reg.m4)

HSD.test(reg.m4,"Region",alpha=0.05,group=TRUE,console=TRUE)

reg.m5<-aov(mass~Region,data=sr)
loc.m1<-aov(mass~Location,data=sr)
st.m1<-aov(mass~State,data=sr)

HSD.test(reg.m5,"Region",alpha=0.05,group=TRUE,console=TRUE)
HSD.test(loc.m1,"Location",alpha=0.05,group=TRUE,console=TRUE)
HSD.test(st.m1,"State",alpha=0.05,group=TRUE,console=TRUE)

ob.m1<-lm(wg~Lat,data=sr)
summary(ob.m1)

ob.m2<-lm(mass~Lat,data=sr)
summary(ob.m2)

pc<-princomp(~mass+wg,data=sr,cor=F)
plot(pc)


reg.m10<-aov(mass~Region,data=sr)
agricolae::HSD.test(reg.m10,"Region",alpha=0.05,group=TRUE,console=TRUE)

tapply(sr$mass,sr$Lat2,mean,na.rm=T)
tapply(sr$wg,sr$Lat2,mean,na.rm=T)

reg.m11<-aov(wg~Region,data=sr)
summary(reg.m11)
agricolae::HSD.test(reg.m11,"Region",alpha=0.05,group=TRUE,console=TRUE)

