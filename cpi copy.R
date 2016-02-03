pwt<- subset(pwt81[pwt81$year %in% c(1995:2011),])

#add cpi values to dataset
cpi2011$Code<-as.character(cpi2011$Code)

for (i in 1:nrow(cpi2011)){
   code<- cpi2011[i,3]
   pwt[which(pwt$countrycode==code & pwt$year==2011),48]<-cpi2011[i,4]
}
#for cpi1995-cpi2011
colnames(pwt.oamt)[48]<-"cpi"
pwt.oamt<-pwt[!(pwt$cpi %in% NA),]

null=lm(cpi~1,data=pwt.oamt)
full=lm(cpi~rgdpe+rgdpo+pop+hc+ccon+cda+cgdpe+cgdpo+ck+ctfp+cwtfp+rgdpna+rconna+rdana+rkna+rtfpna+rwtfpna+labsh+xr+i_cig+i_xm+i_xr+i_outlier+statcap, data=pwt.oamt)
step(full, scope=list(lower=null, upper=full), direction="backward")
#backward models selection

#model result
Call:
  lm(formula = cpi ~ rgdpe + rgdpo + pop + hc + cwtfp + rgdpna + 
       rconna + rdana + rtfpna + labsh + xr + i_xr + i_outlier + 
       statcap, data = pwt.oamt)

#summary 
Call:
  lm(formula = cpi ~ rgdpe + rgdpo + pop + hc + cwtfp + rgdpna + 
       rconna + rdana + rtfpna + labsh + xr + i_xr + i_outlier + 
       statcap, data = pwt.oamt)

Residuals:
  Min      1Q  Median      3Q     Max 
-2.3699 -0.6112 -0.0996  0.4511  3.4246 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)      -2.002e+00  9.064e-01  -2.209 0.027581 *  
  rgdpe             7.897e-06  1.764e-06   4.477 9.04e-06 ***
  rgdpo            -7.958e-06  1.940e-06  -4.102 4.66e-05 ***
  pop               1.703e-03  5.125e-04   3.323 0.000944 ***
  hc                3.556e-01  9.646e-02   3.687 0.000247 ***
  cwtfp             2.997e+00  2.413e-01  12.423  < 2e-16 ***
  rgdpna           -3.237e-06  1.324e-06  -2.444 0.014800 *  
  rconna           -3.700e-06  7.864e-07  -4.705 3.15e-06 ***
  rdana             5.410e-06  1.393e-06   3.883 0.000115 ***
  rtfpna           -2.147e+00  4.236e-01  -5.070 5.31e-07 ***
  labsh             1.374e+00  3.353e-01   4.099 4.71e-05 ***
  xr               -7.201e-05  2.328e-05  -3.094 0.002068 ** 
  i_xrMarket        1.505e+00  5.442e-01   2.765 0.005860 ** 
  i_outlierRegular  2.272e+00  4.396e-01   5.168 3.22e-07 ***
  statcap           1.236e-02  3.459e-03   3.575 0.000379 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.9156 on 603 degrees of freedom
(1332 observations deleted due to missingness)
Multiple R-squared:  0.4101,  Adjusted R-squared:  0.3964 
F-statistic: 29.94 on 14 and 603 DF,  p-value: < 2.2e-16

pwt.oamt$countrycode<-droplevels(pwt.oamt$countrycode)
ccode<-names(table(pwt.oamt$countrycode))
var=c("cpi","rgdpe","rgdpo","pop","hc","cwtfp","rgdpna","rconna"
      ,"rdana","rtfpna","labsh","xr","i_xr","i_outlier","statcap")

library(gridGraphics)
library(grid)

for (i in 1:164){
  plot.new()
  par(mfrow = c(4,4))
  nam <- paste("", ccode[i], sep = "")
  assign(nam, pwt.oamt[which(pwt.oamt$countrycode==ccode[i]),])
  for (j in 1:15){ 
    for (k in 5:48){
      if (colnames(get(nam))[k] == var[j] & !is.na(get(nam)[1,k])){
        plot(get(nam)$year,get(nam)[,k],ylab=var[j],xlab=paste(ccode[i],var[j],sep="-"))}
    }
  }
  grid.echo()
  p<-grid.grab()
  pdf(file = paste("",ccode[i],".pdf",sep=""))
  grid.draw(p)
  dev.off()
}

#p = ggplot(data=get(nam), aes_string(x=ccode[i], y=var[j])) +
#  geom_point(size=3, aes(x=get(nam)$year,y=get(nam)[,k]))
#print(p)

> cor(data.frame(pwt.oamt$cpi,pwt.oamt$rgdpe,pwt.oamt$rgdpo,pwt.oamt$pop,pwt.oamt$rgdpna,pwt.oamt$rdana,pwt.oamt$xr))
                pwt.oamt.cpi pwt.oamt.rgdpe pwt.oamt.rgdpo pwt.oamt.pop pwt.oamt.rgdpna pwt.oamt.rdana pwt.oamt.xr
pwt.oamt.cpi      1.00000000     0.17132480     0.16984812  -0.09736236      0.17175994     0.16724525 -0.21743720
pwt.oamt.rgdpe    0.17132480     1.00000000     0.99979876   0.59429688      0.99772103     0.99798444 -0.04359997
pwt.oamt.rgdpo    0.16984812     0.99979876     1.00000000   0.59819876      0.99803063     0.99796650 -0.04305897
pwt.oamt.pop     -0.09736236     0.59429688     0.59819876   1.00000000      0.60021778     0.58283821  0.01700276
pwt.oamt.rgdpna   0.17175994     0.99772103     0.99803063   0.60021778      1.00000000     0.99844084 -0.04337075
pwt.oamt.rdana    0.16724525     0.99798444     0.99796650   0.58283821      0.99844084     1.00000000 -0.04216311
pwt.oamt.xr      -0.21743720    -0.04359997    -0.04305897   0.01700276     -0.04337075    -0.04216311  1.00000000


> cor(data.frame(pwt.oamt$cpi,pwt.oamt$rgdpe,pwt.oamt$pop,pwt.oamt$xr))
                pwt.oamt.cpi pwt.oamt.rgdpe pwt.oamt.pop pwt.oamt.xr
pwt.oamt.cpi     1.00000000     0.17132480  -0.09736236 -0.21743720
pwt.oamt.rgdpe   0.17132480     1.00000000   0.59429688 -0.04359997
pwt.oamt.pop    -0.09736236     0.59429688   1.00000000  0.01700276
pwt.oamt.xr     -0.21743720    -0.04359997   0.01700276  1.00000000

summary(lm(formula = cpi ~ rgdpe  + pop + hc + cwtfp + 
     rconna  + rtfpna + labsh + xr + 
     statcap, data = pwt.oamt))
Call:
  lm(formula = cpi ~ rgdpe + pop + hc + cwtfp + rconna + rtfpna + 
       labsh + xr + statcap, data = pwt.oamt)

Residuals:
  Min      1Q  Median      3Q     Max 
-2.2298 -0.6497 -0.0749  0.4664  3.3763 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.235e+00  5.280e-01   4.234 2.65e-05 ***
  rgdpe        1.613e-06  4.155e-07   3.881 0.000115 ***
  pop          7.330e-04  4.694e-04   1.561 0.118932    
hc           3.354e-01  1.020e-01   3.288 0.001066 ** 
  cwtfp        2.282e+00  2.312e-01   9.871  < 2e-16 ***
  rconna      -2.625e-06  6.645e-07  -3.951 8.71e-05 ***
  rtfpna      -2.549e+00  4.365e-01  -5.841 8.47e-09 ***
  labsh        1.090e+00  3.448e-01   3.161 0.001651 ** 
  xr          -9.236e-05  2.439e-05  -3.786 0.000168 ***
  statcap      1.843e-02  3.476e-03   5.304 1.59e-07 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.9707 on 608 degrees of freedom
(1332 observations deleted due to missingness)
Multiple R-squared:  0.3314,  Adjusted R-squared:  0.3215 
F-statistic: 33.49 on 9 and 608 DF,  p-value: < 2.2e-16
Call:
  lm(formula = cpi ~ rgdpe + hc + cwtfp + rconna + rtfpna + labsh + 
       xr + statcap, data = pwt.oamt)

Residuals:
  Min      1Q  Median      3Q     Max 
-2.1771 -0.6609 -0.0722  0.4730  3.3828 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  2.366e+00  5.219e-01   4.534 6.97e-06 ***
  rgdpe        1.512e-06  4.110e-07   3.679 0.000254 ***
  hc           3.028e-01  9.996e-02   3.029 0.002555 ** 
  cwtfp        2.240e+00  2.299e-01   9.744  < 2e-16 ***
  rconna      -2.296e-06  6.309e-07  -3.639 0.000297 ***
  rtfpna      -2.606e+00  4.355e-01  -5.984 3.72e-09 ***
  labsh        1.090e+00  3.452e-01   3.157 0.001673 ** 
  xr          -9.300e-05  2.442e-05  -3.809 0.000154 ***
  statcap      1.874e-02  3.474e-03   5.393 9.92e-08 ***
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.9719 on 609 degrees of freedom
(1332 observations deleted due to missingness)
Multiple R-squared:  0.3287,  Adjusted R-squared:  0.3199 
F-statistic: 37.28 on 8 and 609 DF,  p-value: < 2.2e-16

## Dataset Divided Table
pwt<- subset(pwt81[pwt81$year %in% c(1995:2011),])
for (i in 1:nrow(cpi2005)){
  code<- cpi2005[i,3]
  pwt[which(pwt$countrycode==code & pwt$year==2005),48]<-cpi2005[i,4]
}
#for cpi1995 to cpi2011
colnames(pwt)[48]<-"cpi"
pwt.full<-subset(pwt[pwt$countrycode %in% cpi1995$Code,])
ccode.full<-as.character(pwt.full$countrycode)
ccode.full<-unique(ccode.full)
ccode.full<-ccode.full[-c(3,25,26,32,34)]
pwt.full<-subset(pwt.full[pwt.full$countrycode %in% ccode.full,])
# Model 1

model1.cpi<-aggregate(x=pwt.full$cpi,by = list(pwt.full$countrycode),FUN = mean)
pwt1994<- subset(pwt81[pwt81$countrycode %in% ccode.full & pwt81$year==1994,])
pwt1995<- subset(pwt81[pwt81$countrycode %in% ccode.full & pwt81$year==1995,])

model1.1995x.unlog<-pwt1995[,5:20] / pwt1994[,5:20]-1
colnames(model1.cpi)<-c("countrycode","cpi")
cbind(model1.cpi,model1.1995x.unlog)

model1.1995_2011y<-c()
for (i in 1:37) {
model1.1995_2011y<-rbind(model1.1995_2011y,
                         pwt.full[17+(i-1)*17,5:20] / pwt.full[1+(i-1)*17,5:20]-1)}

model1.1995_2011y<-model1.1995_2011y[-c(3:8,11:16)]
colnames(model1.1995_2011y)<-c(paste(names(model1.1995_2011y),".y",sep=""))
model1.data<-cbind(model1.cpi,model1.1995_2011y,model1.1995x.unlog)

# Model 2
pwt.1995_2000<-subset(pwt.full[pwt.full$year %in% c(1995:2000),])
model2.1995_2000.pwt<-subset(pwt.full[pwt.full$year %in% c(1995:2000),])
model2.1995_2000.cpi<-aggregate(x=pwt.1995_2000$cpi,by = list(pwt.1995_2000$countrycode),FUN = mean)
pwt1994<- subset(pwt81[pwt81$countrycode %in% ccode.full & pwt81$year==1994,])
pwt1995<- subset(pwt81[pwt81$countrycode %in% ccode.full & pwt81$year==1995,])

model2.1995x.unlog<-pwt1995[,5:20] / pwt1994[,5:20]-1
colnames(model2.1995_2000.cpi)<-c("countrycode","cpi")
cbind(model2.1995_2000.cpi,model2.1995x.unlog)

model2.1995_2000y<-c()
for (i in 1:37) {
  model2.1995_2000y<-rbind(model2.1995_2000y,
                           pwt.full[6+(i-1)*17,5:20] / pwt.full[1+(i-1)*17,5:20]-1)}

model2.1995_2000y<-model2.1995_2000y[-c(3:8,11:16)]
colnames(model2.1995_2000y)<-c(paste(names(model2.1995_2000y),".y",sep=""))
model2.p1.data<-cbind(model2.1995_2000.cpi,model2.1995_2000y,model2.1995x.unlog)


model2.2000_2005.pwt<-subset(pwt.full[pwt.full$year %in% c(2000:2005),])
model2.2005_2011.pwt<-subset(pwt.full[pwt.full$year %in% c(2005:2011),])

# List all the cpi 1995-2011 datasets and remove them
cpi.list<-list(cpi1995,cpi1996,cpi1997,cpi1998,cpi1999,cpi2000,cpi2001,cpi2002,cpi2003,cpi2004,cpi2005,cpi2006,cpi2007,cpi2008,cpi2009,cpi2010,cpi2011)
rm(cpi1995,cpi1996,cpi1997,cpi1998,cpi1999,cpi2000,cpi2001,cpi2002,cpi2003,cpi2004,cpi2005,cpi2006,cpi2007,cpi2008,cpi2009,cpi2010,cpi2011)