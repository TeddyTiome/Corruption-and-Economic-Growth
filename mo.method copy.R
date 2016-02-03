# Mo Method for 1995 - 2011

# select first part of variales from PWT dataset. (economic)
pwt2011<-subset(pwt.full[pwt.full$year %in% c(2011),])
pwt.1995_2011<-subset(pwt.full[pwt.full$year %in% c(1995:2011),])

mo.cpi <- model1.cpi$cpi
mo.y95 <- pwt1995$rgdpna
mo.egr <- (( pwt2011$rgdpna / pwt1995$rgdpna ) ^ (1/16) -1) *100
mo.pgr <- (( pwt2011$pop / pwt1995$pop ) ^ (1/16) - 1) *100
mo.hc <- aggregate(x=pwt.1995_2011$hc,by = list(pwt.1995_2011$countrycode),FUN = mean)

mo.data<-cbind(mo.hc,mo.cpi,mo.y95,mo.egr,mo.pgr)
colnames(mo.data)[1:2]<-c("ccode","mo.hc")

mo.hc<-unlist(mo.hc[2])


# select second part of variables from WGI dataset. (political)
mo.voiacc<-subset(wgi.voiacc[wgi.voiacc$WBCode %in% ccode.full,])
mo.voiacc<-mo.voiacc[order(mo.voiacc[2]),]

mo.stavol<-subset(wgi.stavol[wgi.stavol$WBCode %in% ccode.full,])
mo.stavol<-mo.stavol[order(mo.stavol[2]),]

mo.rullaw<-subset(wgi.rullaw[wgi.rullaw$WBCode %in% ccode.full,])
mo.rullaw<-mo.rullaw[order(mo.rullaw[2]),]

mo.regqua<-subset(wgi.regqua[wgi.regqua$WBCode %in% ccode.full,])
mo.regqua<-mo.regqua[order(mo.regqua[2]),]

mo.goveff<-subset(wgi.goveff[wgi.goveff$WBCode %in% ccode.full,])
mo.goveff<-mo.goveff[order(mo.goveff[2]),]

mo.ctrcop<-subset(wgi.ctrcop[wgi.ctrcop$WBCode %in% ccode.full,])
mo.ctrcop<-mo.ctrcop[order(mo.ctrcop[2]),]

# for the second part selection, use means of estimates of all variables
mo.wgi<-cbind(
  as.numeric(unlist(mo.voiacc[81])),as.numeric(unlist(mo.stavol[81])),
  as.numeric(unlist(mo.rullaw[81])),as.numeric(unlist(mo.regqua[81])),
  as.numeric(unlist(mo.goveff[81])),as.numeric(unlist(mo.ctrcop[81])))
colnames(mo.wgi)<-c("voiacc","stavol","rullaw","regqua","goveff","ctrcop")
mo.wgi<-as.data.frame(mo.wgi)

#- correlation about the WGI variables
cor(mo.wgi)
        voiacc    stavol    rullaw    regqua    goveff    ctrcop
voiacc 1.0000000 0.7659665 0.8351882 0.7756751 0.7764094 0.7995595
stavol 0.7659665 1.0000000 0.9006219 0.8727059 0.8976192 0.8906035
rullaw 0.8351882 0.9006219 1.0000000 0.9533912 0.9766719 0.9679775
regqua 0.7756751 0.8727059 0.9533912 1.0000000 0.9563887 0.9535767
goveff 0.7764094 0.8976192 0.9766719 0.9563887 1.0000000 0.9759305
ctrcop 0.7995595 0.8906035 0.9679775 0.9535767 0.9759305 1.0000000
#-

# use the mean of six variables to proxy the political stabibility variable in model.
mo.pol<-rowMeans(mo.wgi)
mo.data<-cbind(mo.data,mo.pol)

#- correlation matrix for the simple model
cor(mo.data[2:7])
          mo.hc     mo.cpi      mo.y95      mo.egr     mo.pgr      mo.pol
mo.hc   1.0000000  0.7412405  0.21492757 -0.43196991 -0.4171438  0.79890782
mo.cpi  0.7412405  1.0000000  0.01147780 -0.35740890 -0.3128780  0.96974868
mo.y95  0.2149276  0.0114778  1.00000000 -0.01106279 -0.1541843  0.01928429
mo.egr -0.4319699 -0.3574089 -0.01106279  1.00000000  0.4940964 -0.43754351
mo.pgr -0.4171438 -0.3128780 -0.15418434  0.49409641  1.0000000 -0.43488726
mo.pol  0.7989078  0.9697487  0.01928429 -0.43754351 -0.4348873  1.00000000
#-

#- Mo model: corrupt, intial GDP and population growth
summary(lm(mo.egr~mo.cpi+mo.y95+mo.pgr))

Call:
  lm(formula = mo.egr ~ mo.cpi + mo.y95 + mo.pgr)

Residuals:
  Min        1Q    Median        3Q       Max 
-0.023791 -0.006599 -0.003562  0.003983  0.064051 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)   
(Intercept) -2.025e-01  4.501e-01  -0.450  0.65575   
mo.cpi      -1.565e-03  1.085e-03  -1.442  0.15861   
mo.y95       6.314e-10  1.605e-09   0.393  0.69652   
mo.pgr       1.232e+00  4.434e-01   2.777  0.00897 **
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.01523 on 33 degrees of freedom
Multiple R-squared:  0.293,  Adjusted R-squared:  0.2288 
F-statistic:  4.56 on 3 and 33 DF,  p-value: 0.008845
#-
# since the pgr variable distract the significance level
# no population growth data included in following models

#- Mo B1 model: only corrupt and intial GDP 
summary(lm(mo.egr~mo.cpi+mo.y95))
Call:
  lm(formula = mo.egr ~ mo.cpi + mo.y95)

Residuals:
  Min        1Q    Median        3Q       Max 
-0.027201 -0.008506 -0.003484  0.004393  0.059171 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.047e+00  7.611e-03 137.620   <2e-16 ***
  mo.cpi      -2.514e-03  1.127e-03  -2.231   0.0324 *  
  mo.y95      -7.536e-11  1.734e-09  -0.043   0.9656    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.01667 on 34 degrees of freedom
Multiple R-squared:  0.1278,  Adjusted R-squared:  0.07648 
F-statistic: 2.491 on 2 and 34 DF,  p-value: 0.09785
#-

#- Mo B2 model: add only human capital
summary(lm(mo.egr~mo.cpi+mo.y95+mo.hc))

Call:
  lm(formula = mo.egr ~ mo.cpi + mo.y95 + mo.hc)

Residuals:
  Min        1Q    Median        3Q       Max 
-0.025437 -0.010564 -0.002457  0.007411  0.056693 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.080e+00  2.123e-02  50.888   <2e-16 ***
  mo.cpi      -4.037e-04  1.680e-03  -0.240    0.812    
mo.y95       8.319e-10  1.777e-09   0.468    0.643    
mo.hc       -1.676e-02  1.009e-02  -1.660    0.106    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.01625 on 33 degrees of freedom
Multiple R-squared:  0.195,  Adjusted R-squared:  0.1218 
F-statistic: 2.665 on 3 and 33 DF,  p-value: 0.06394
#-

#- Mo B3 model: add only political stability
summary(lm(mo.egr~mo.cpi+mo.y95+mo.pol))

Call:
  lm(formula = mo.egr ~ mo.cpi + mo.y95 + mo.pol)

Residuals:
  Min        1Q    Median        3Q       Max 
-0.032867 -0.006500 -0.002213  0.002094  0.048270 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.008e+00  1.730e-02  58.268   <2e-16 ***
  mo.cpi       7.903e-03  4.298e-03   1.839   0.0750 .  
mo.y95       5.945e-11  1.615e-09   0.037   0.9709    
mo.pol      -3.057e-02  1.223e-02  -2.499   0.0176 *  
  ---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.01551 on 33 degrees of freedom
Multiple R-squared:  0.2666,  Adjusted R-squared:  0.1999 
F-statistic: 3.998 on 3 and 33 DF,  p-value: 0.0156
#-

#- Mo B4 model: add human capital and political stability
summary(lm(mo.egr~mo.cpi+mo.y95+mo.pol+mo.hc))

Call:
  lm(formula = mo.egr ~ mo.cpi + mo.y95 + mo.pol + mo.hc)

Residuals:
  Min        1Q    Median        3Q       Max 
-0.031657 -0.007787 -0.001833  0.003826  0.048767 

Coefficients:
  Estimate Std. Error t value Pr(>|t|)    
(Intercept)  1.025e+00  3.602e-02  28.464   <2e-16 ***
  mo.cpi       7.333e-03  4.466e-03   1.642   0.1104    
mo.y95       3.785e-10  1.732e-09   0.219   0.8284    
mo.pol      -2.660e-02  1.431e-02  -1.859   0.0722 .  
mo.hc       -6.216e-03  1.127e-02  -0.552   0.5850    
---
  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

Residual standard error: 0.01568 on 32 degrees of freedom
Multiple R-squared:  0.2735,  Adjusted R-squared:  0.1827 
F-statistic: 3.012 on 4 and 32 DF,  p-value: 0.03238
