pwt.2000_2005<-subset(pwt.full[pwt.full$year %in% c(2000:2005),])
model2.2000_2005.pwt<-subset(pwt.full[pwt.full$year %in% c(2000:2005),])
model2.2000_2005.cpi<-aggregate(x=pwt.2000_2005$cpi,by = list(pwt.2000_2005$countrycode),FUN = mean)
pwt1999<- subset(pwt81[pwt81$countrycode %in% ccode.full & pwt81$year==1999,])
pwt2000<- subset(pwt81[pwt81$countrycode %in% ccode.full & pwt81$year==2000,])

model2.2000x.unlog<-pwt2000[,5:20] / pwt1999[,5:20]-1
colnames(model2.2000_2005.cpi)<-c("countrycode","cpi")
cbind(model2.2000_2005.cpi,model2.2000x.unlog)

model2.2000_2005y<-c()
for (i in 1:37) {
  model2.2000_2005y<-rbind(model2.2000_2005y,
                           pwt.full[11+(i-1)*17,5:20] / pwt.full[6+(i-1)*17,5:20]-1)}

model2.2000_2005y<-model2.2000_2005y[-c(3:8,11:16)]
colnames(model2.2000_2005y)<-c(paste(names(model2.2000_2005y),".y",sep=""))
model2.p2.data<-cbind(model2.2000_2005.cpi,model2.2000_2005y,model2.2000x.unlog)


