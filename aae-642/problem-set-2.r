library("foreign")

#install.packages("AER")
library(AER)

#install.packages("ivpack")
library(ivpack)

#install.packages("rms")
library(rms)

library(stargazer)


nic.df <- read.dta("/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 2/pset2.dta")

# QUESTION 1.7

naive.est.ivreg <- ivreg(lwage3 ~ S  | P ,
  data=nic.df)

summary(naive.est.ivreg, diagnostics=TRUE)

cluster.robust.se(naive.est.ivreg, nic.df[, "area"]) 


# QUESTION 1.8


wald.est.ivreg <- ivreg(lwage3 ~ S + C + P | 
      C + P + P:C,
  data=nic.df)

summary(wald.est.ivreg, diagnostics=TRUE)

cluster.robust.se(wald.est.ivreg, nic.df[-wald.est.ivreg$na.action, "area"])

stargazer(naive.est.ivreg, wald.est.ivreg, 
  se=list(cluster.robust.se(naive.est.ivreg, nic.df[, "area"])[, "Std. Error"],
    cluster.robust.se(wald.est.ivreg, nic.df[-wald.est.ivreg$na.action, "area"])[, "Std. Error"]
  ),
  align=TRUE, no.space=TRUE,  single.row=TRUE, 
  notes="White-corrected standard errors clustered at the region level are in parentheses")


# QUESTION 1.9

# Dif-in-dif

dif.in.dif.edu.lm <- ols(S ~ P*C , data=nic.df, x=TRUE, y=TRUE) 

robcov(dif.in.dif.edu.lm , nic.df$area, method='huber')


dif.in.dif.illit.lm <- lrm(I ~ P*C , data=nic.df, x=TRUE, y=TRUE)

robcov(dif.in.dif.illit.lm , nic.df$area, method='huber')


summary(lm(S ~ P*C:I(S<=6) , data=nic.df))

summary(dif.in.dif.primary.check.lm <- ols(S ~ P*C , data=nic.df, subset=S<=6, x=TRUE, y=TRUE))
summary(dif.in.dif.secondary.check.lm <- ols(S ~ P*C , data=nic.df, subset=S>6, x=TRUE, y=TRUE))


robcov(dif.in.dif.primary.check.lm , nic.df$area[nic.df$S<=6], method='huber') 

sqrt(diag(robcov(dif.in.dif.primary.check.lm , nic.df$area[nic.df$S<=6], method='huber')$var ))

stargazer(dif.in.dif.edu.lm, dif.in.dif.illit.lm, dif.in.dif.primary.check.lm, dif.in.dif.secondary.check.lm,
  se=list(
    sqrt(diag(robcov(dif.in.dif.edu.lm, nic.df$area, method='huber')$var )),
    sqrt(diag(robcov(dif.in.dif.illit.lm, nic.df$area, method='huber')$var )),
    sqrt(diag(robcov(dif.in.dif.primary.check.lm , nic.df$area[nic.df$S<=6], method='huber')$var )),
    sqrt(diag(robcov(dif.in.dif.secondary.check.lm , nic.df$area[nic.df$S>6], method='huber')$var ))
  ),
  align=TRUE, no.space=FALSE,  single.row=FALSE, 
  notes="White-corrected standard errors clustered at the region level are in parentheses")


# With help from http://stats.stackexchange.com/questions/21815/is-there-a-r-command-for-testing-the-difference-in-coefficients-of-two-linear-re
d <- coef(dif.in.dif.primary.check.lm)[4] - coef(dif.in.dif.secondary.check.lm)[4] 
var1<-sqrt(diag(robcov(dif.in.dif.primary.check.lm , nic.df$area[nic.df$S<=6], method='huber')$var ))[4]
var2<-sqrt(diag(robcov(dif.in.dif.secondary.check.lm , nic.df$area[nic.df$S>6], method='huber')$var ))[4]
1-pnorm(d / sqrt(var1+var2))




