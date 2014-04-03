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

# TODO: no region dummies

summary( lm(S~ P + factor(area), data=nic.df ))


nic.dropped.region.df <- nic.df 

#nic.dropped.region.df$area[nic.dropped.region.df$area==2] <- 1 

#naive.est.ivreg <- ivreg(lwage3 ~ S + factor(area)  | 
#     P + factor(area),
#  data=nic.dropped.region.df)

naive.est.ivreg <- ivreg(lwage3 ~ S  | 
     P ,
  data=nic.df)

summary(naive.est.ivreg, diagnostics=TRUE)

cluster.robust.se(naive.est.ivreg, nic.df[, "area"]) # -naive.est.ivreg$na.action



# QUESTION 1.8



#wald.est.ivreg <- ivreg(lwage3 ~ factor(area) + C + S | 
#     factor(area) + C + P:C,
#  data=nic.df)

wald.est.ivreg <- ivreg(lwage3 ~ S + C + P | 
      C + P + P:C,
  data=nic.df)

summary(wald.est.ivreg, diagnostics=TRUE)

cluster.robust.se(wald.est.ivreg, nic.df[-wald.est.ivreg$na.action, "area"])
#[, "Std. Error"]

stargazer(naive.est.ivreg, wald.est.ivreg, 
  se=list(cluster.robust.se(naive.est.ivreg, nic.df[, "area"])[, "Std. Error"],
    cluster.robust.se(wald.est.ivreg, nic.df[-wald.est.ivreg$na.action, "area"])[, "Std. Error"]
  ),
  align=TRUE, no.space=TRUE,  single.row=TRUE, omit="factor")

# stargazer( wald.est.ivreg,  se=list(cluster.robust.se(wald.est.ivreg, nic.df[-wald.est.ivreg$na.action, "area"])[, "Std. Error"]),  align=TRUE, no.space=TRUE,  single.row=TRUE, omit="factor")

# QUESTION 1.9

# Dif-in-dif

#summary( dif.in.dif.edu.lm <- lm(S ~ P*C, data=nic.df) )

dif.in.dif.edu.lm <- ols(S ~ P*C , data=nic.df, x=TRUE, y=TRUE) # + factor(area) ?

robcov(dif.in.dif.edu.lm , nic.df$area, method='huber')


dif.in.dif.illit.lm <- ols(I ~ P*C , data=nic.df, x=TRUE, y=TRUE) # + factor(area) ?

robcov(dif.in.dif.illit.lm , nic.df$area, method='huber')



summary(lm(S ~ P*C:I(S<=6) , data=nic.df))

summary(dif.in.dif.primary.check.lm <- ols(S ~ P*C , data=nic.df, subset=S<=6, x=TRUE, y=TRUE))
summary(dif.in.dif.secondary.check.lm <- ols(S ~ P*C , data=nic.df, subset=S>6, x=TRUE, y=TRUE))
# OK, I think the above reg does it

# dif.in.dif.primary.check.lm <- ols(S ~ P*C:I(S<=6) , data=nic.df, x=TRUE, y=TRUE) # + factor(area) ?

# robcov(dif.in.dif.primary.check.lm , nic.df$area, method='huber')[, "Std. Error"]


robcov(dif.in.dif.primary.check.lm , nic.df$area[nic.df$S<=6], method='huber') 

sqrt(diag(robcov(dif.in.dif.primary.check.lm , nic.df$area[nic.df$S<=6], method='huber')$var ))




stargazer(dif.in.dif.edu.lm, dif.in.dif.illit.lm, dif.in.dif.primary.check.lm, dif.in.dif.secondary.check.lm,
  se=list(
    sqrt(diag(robcov(dif.in.dif.edu.lm, nic.df$area, method='huber')$var )),
    sqrt(diag(robcov(dif.in.dif.illit.lm, nic.df$area, method='huber')$var )),
    sqrt(diag(robcov(dif.in.dif.primary.check.lm , nic.df$area[nic.df$S<=6], method='huber')$var )),
    sqrt(diag(robcov(dif.in.dif.secondary.check.lm , nic.df$area[nic.df$S>6], method='huber')$var ))
  ),
  align=TRUE, no.space=FALSE,  single.row=FALSE, omit="factor")

















# TODO: DELETE:

wald.est.primary.check.ivreg <- ivreg(lwage3 ~ factor(area) + C + S | 
     factor(area) + C + P:C,
  data=nic.df, subset= S<=6)

summary(wald.est.primary.check.ivreg, diagnostics=TRUE)

cluster.robust.se(wald.est.primary.check.ivreg, nic.df[-wald.est.primary.check.ivreg$na.action, "area"])


wald.est.secondary.check.ivreg <- ivreg(lwage3 ~ factor(area) + C + S | 
     factor(area) + C + P:C,
  data=nic.df, subset= S>6)

summary(wald.est.secondary.check.ivreg, diagnostics=TRUE)

cluster.robust.se(wald.est.secondary.check.ivreg, nic.df[-wald.est.secondary.check.ivreg$na.action, "area"])





# Test test ( TODO: Ok, will not use this. Just split the sample):

nic.dropped.region.2.df <- nic.df 

nic.dropped.region.2.df$area[nic.dropped.region.2.df$area==6] <- 1 


wald.est.secondary.check.ivreg <- ivreg(lwage3 ~ (factor(area) + C + S)*I(S<=6) | 
     (factor(area) + C + P:C)*I(S<=6),
  data=nic.dropped.region.2.df)

summary(wald.est.secondary.check.ivreg, diagnostics=TRUE)

cluster.robust.se(wald.est.secondary.check.ivreg, nic.df[-wald.est.secondary.check.ivreg$na.action, "area"])





wald.est.secondary.check.ivreg <- ivreg(lwage3 ~ factor(area) + C + S*I(S<=6) | 
     factor(area) + C + P:C*I(S<=6),
  data=nic.dropped.region.2.df)

summary(wald.est.secondary.check.ivreg, diagnostics=TRUE)


