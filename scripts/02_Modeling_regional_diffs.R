#Modeling differences in size by regions, beyond Bergman.

#--------------------------------------------------
#Comparison in mass and wing chord across ALL regions:
#--------------------------------------------------
cw_data<-readRDS("data/cw_data.rds")

cw_data$Region<-factor(cw_data$Region, levels = levels(addNA(cw_data$Region)), labels = c(levels(cw_data$Region), "Ozark National Forest"), exclude = NULL)
reg.m1<-aov(cbind(mass,wg)~Region,data=cw_data,na.action="na.omit")
summary(reg.m1)
##mass significant, extract it:
reg.m2<-aov(mass~Region,data=cw_data,na.action="na.omit")
summary(reg.m2)

HSD.test(reg.m2,"Region",alpha=0.05,group=TRUE,console=TRUE)

#--------------------------------------------------
#Comparison in mass and wing chord across SOUTHERN regions:
#--------------------------------------------------
#Bankhead National Forest (AL) has significantly smaller birds (by mass) than High Point State Park (NJ), Wyalusing State Park (WI), and Barry State Game Park(MI), providing some support for Bergman's rule. Interestingly, Tennessee birds in the Royal Blue Wildlife Management areas (though largely variable) were similar to those northernmost populations. This was true for MO Ozarks as well. Kentucky birds were in the middle of the pack. Tested to see if Bankhead was different from everything combined, and then AL as a whole:

#This was an unnecesarily elaborate test to see if BNF average mass differed significantly from the global average as an expected value.

bh.mean<-mean(cw_data$mass[c(74:80,88,89)],na.rm = T)
rest.mean<-mean(cw_data$mass[-c(74:80,88,89)],na.rm = T)
length(cw_data$mass[-c(74:80,88,89)])
se1<-sd(cw_data$mass[c(74:80,88,89)],na.rm = T)/sqrt(9)
se2<-sd(cw_data$mass[-c(74:80,88,89)],na.rm = T)/sqrt(95)
df<-(se1^2+se2^2)^2/(se1^4/8+se2^4/94)
qt(0.05,9)
sed<-sqrt(var(cw_data$mass[c(74:80,88,89)],na.rm = T)/9+var(cw_data$mass[-c(74:80,88,89)],na.rm = T)/95)
sed
(bh.mean-rest.mean)/sed
##Significantly different from all other birds when pooled.

#Now let's check all AL birds together:


AL.mean<-mean(cw_data$mass[c(70:89)],na.rm = T)
rest.mean.2<-mean(cw_data$mass[-c(70:89)],na.rm = T)
length(cw_data$mass[-c(70:89)])
se1<-sd(cw_data$mass[c(70:89)],na.rm = T)/sqrt(20)
se2<-sd(cw_data$mass[-c(70:89)],na.rm = T)/sqrt(84)
df<-(se1^2+se2^2)^2/(se1^4/19+se2^4/83)
qt(0.05,24)
sed<-sqrt(var(cw_data$mass[c(70:89)],na.rm = T)/20+var(cw_data$mass[-c(70:89)],na.rm = T)/84)
sed
(AL.mean-rest.mean)/sed
##Woah, AL birds are significantly smaller than combined global average.

#Let's group all ASY/ATY:
cw_data$Age2<-ifelse(cw_data$Age=="SY","SY",ifelse(is.na(cw_data$Age),NA,"ASY"))

cw_asy<-cw_data[which(cw_data$Age2=="ASY"),]

reg.m3<-aov(mass~Region,data=cw_asy)
summary(reg.m3)

HSD.test(reg.m3,"Region",alpha=0.05,group=TRUE,console=TRUE)