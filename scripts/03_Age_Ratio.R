#AGE RATIO Tests
cw_data<-readRDS("data/cw_data.rds")

#Let's group all ASY/ATY:
cw_data$Age2<-ifelse(cw_data$Age=="SY","SY",ifelse(is.na(cw_data$Age),NA,"ASY"))

cw_asy<-cw_data[which(cw_data$Age2=="ASY"),]

##Next:Age Structure in AL:
chisq.test(as.matrix(table(cw_data$Age2)))#Global - significantly different 106:24
chisq.test(as.matrix(table(cw_data$Age2[c(70:89)])))#AL - NOT at all different 10:10

tbl2<-table(cw_data$Region,cw_data$Age2)
tbl2
chisq.test(tbl2)

tbl3<-table(cw_data$State,cw_data$Age2)
tbl3
chisq.test(tbl3)

t.test(cw_data$wg~cw_data$Age2)
t.test(cw_data)
a<-table(cw_data$Age2)
sd(a)