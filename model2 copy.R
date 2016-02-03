pwt.2005_2011<-subset(pwt.full[pwt.full$year %in% c(2005:2011),])
model2.2005_2011.pwt<-subset(pwt.full[pwt.full$year %in% c(2005:2011),])
model2.2005_2011.cpi<-aggregate(x=pwt.2005_2011$cpi,by = list(pwt.2005_2011$countrycode),FUN = mean)
pwt2004<- subset(pwt81[pwt81$countrycode %in% ccode.full & pwt81$year==2004,])
pwt2005<- subset(pwt81[pwt81$countrycode %in% ccode.full & pwt81$year==2005,])

model2.2005x.unlog<-pwt2005[,5:20] / pwt2004[,5:20]-1
colnames(model2.2005_2011.cpi)<-c("countrycode","cpi")
cbind(model2.2005_2011.cpi,model2.2005x.unlog)

model2.2005_2011y<-c()
for (i in 1:37) {
  model2.2005_2011y<-rbind(model2.2005_2011y,
                           pwt.2005_2011[7+(i-1)*7,5:20] / pwt.2005_2011[1+(i-1)*7,5:20]-1)}

model2.2005_2011y<-model2.2005_2011y[-c(3:8,11:16)]
colnames(model2.2005_2011y)<-c(paste(names(model2.2005_2011y),".y",sep=""))
model2.p3.data<-cbind(model2.2005_2011.cpi,model2.2005_2011y,model2.2005x.unlog)