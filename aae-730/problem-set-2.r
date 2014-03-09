

work.dir <- "/Users/travismcarthur/Desktop/Dev - AAE 730/Problem sets/PS 2/"

library(foreign)

# TODO: May be good to put the keep.stats for all regression output from stargazer


main.df <- read.dta(paste0(work.dir, "pset2.dta"))

table(paste0(main.df$vill, main.df$hhid), main.df$year)
# So it is an unbalances panel

main.df$unique.hhid <- paste0(main.df$vill, main.df$hhid)

#str(main.df)

# rsrf above34

# Do the rainfall variable generations by region

main.df <- main.df[order(main.df$vill),]

main.df$rsrf.dev <- unlist(
  by(main.df, INDICES=list(main.df$vill), FUN=function(x) {
    print(x$rsrf[!duplicated(x$year)])
    x$rsrf - mean(x$rsrf[!duplicated(x$year)])
  })
)

# main.df$rsrf.sq.dev <- unlist(
#   by(main.df, INDICES=list(main.df$vill), FUN=function(x) {
#     (x$rsrf - mean(x$rsrf[!duplicated(x$year)]))^2
#   })
# )

main.df$above34.dev <- unlist(
  by(main.df, INDICES=list(main.df$vill), FUN=function(x) {
    x$above34 - mean(x$above34[!duplicated(x$year)])
  })
)

# main.df$above34.sq.dev <- unlist(
#   by(main.df, INDICES=list(main.df$vill), FUN=function(x) {
#     (x$above34 - mean(x$above34[!duplicated(x$year)]))^2
#   })
# )

main.df$rsrf.sq.dev <- main.df$rsrf.dev^2
main.df$above34.sq.dev <- main.df$above34.dev^2

summary(main.df$rsrf.dev)
summary(main.df$rsrf.sq.dev)
summary(main.df$above34.dev)
summary(main.df$above34.sq.dev)


table(paste0(main.df$vill, main.df$hhid), main.df$year)


main.df$hhnone_females15plus <- with(main.df, {
  hhcomp_females15to55 + hhcomp_females56plus - 
  (hhhigher_females15plus + hhmiddle_females15plus + hhprimary_females15plus)
} )
  
main.df$hhnone_males15plus <- with(main.df, {
  hhcomp_males15to55 + hhcomp_males56plus - 
  (hhhigher_males15plus + hhmiddle_males15plus + hhprimary_males15plus)
} )

# main.df <- main.df[!main.df$netinc_real %in% max(main.df$netinc_real, na.rm=TRUE), ]


tab3.col1.lm <- lm( netinc_real ~ rsrf.dev + rsrf.sq.dev + above34.dev + above34.sq.dev +
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill, data=main.df)

# TODO: do we do panel reg here even though Paxyon did not, since we have a panel?
# All I am missing would be teh individual fixed effects
# Well, probably not, since we are using some vars that are 

# TODO: need to use White-corrected SE when output to stargazer

summary(tab3.col1.lm)

library(lmtest)
library(sandwich)
library(stargazer)

tab3.col1.no.weather.lm <- lm( netinc_real ~ 
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill, data=main.df)

#summary(tab3.col1.no.weather.lm)

waldtest(tab3.col1.lm, tab3.col1.no.weather.lm, vcov=vcovHC(tab3.col1.lm, type = "HC0") )
# waldtest(tab3.col1.lm, tab3.col1.no.weather.lm)
lrtest(tab3.col1.lm, tab3.col1.no.weather.lm )

library(reshape)

compute.land.df <- main.df[, c("unique.hhid", "year", "landsize_cat")]
compute.land.df$value <- 1

compute.land.df <- cast(compute.land.df, unique.hhid + year ~ landsize_cat , fill=0)

compute.vill.df <- main.df[, c("unique.hhid", "year", "vill")]
compute.vill.df$value <- 1

compute.vill.df <- cast(compute.vill.df, unique.hhid + year ~ vill , fill=0)

stargazer(main.df[, c("netinc_real", "rsrf.dev", "rsrf.sq.dev", "above34.dev", "above34.sq.dev", 
    "hhcomp_females0to14", "hhcomp_males0to14", 
    "hhhigher_females15plus", "hhhigher_males15plus", "hhmiddle_females15plus", 
    "hhmiddle_males15plus", "hhprimary_females15plus", "hhprimary_males15plus",
    "hhnone_females15plus", "hhnone_males15plus", "landsize_cat") ], compute.land.df,
  median = TRUE)

stargazer(compute.vill.df, median = TRUE)

summary(main.df$landsize_cat)

#coef.names.crosstable <- 

#names(tab3.col1.lm$coefficients) <-paste0(names(tab3.col1.lm$coefficients), "test") 

stargazer(tab3.col1.lm, se=list(sqrt(diag(vcovHC(tab3.col1.lm, type = "HC0"))) ) , 
  align=TRUE, no.space=TRUE,  single.row=TRUE,
  notes="White-corrected standard errors in parentheses")
# font.size="small",

# omit - a vector of regular expressions that speciﬁes which of the explanatory variables
#should be omitted from presentation in the table. Alternatively, this argument
#can be a numeric vector whose elements indicate which variables (from top to
#bottom, or left to right) should be omitted. This argument might be used, for
#instance, to exclude ﬁxed effects dummies from being presented. The default
#value of NULL means that no variables will be excluded.





# B.

perm.inc.pred.df <- main.df 

perm.inc.pred.df[, c("rsrf.dev",  "above34.dev", "rsrf.sq.dev", "above34.sq.dev")] <- 0

# predict(tab3.col1.lm, perm.inc.pred.df) == predict(tab3.col1.lm, main.df)
head(predict(tab3.col1.lm, main.df))
head(predict(tab3.col1.lm, perm.inc.pred.df))

main.df$incperm <- predict(tab3.col1.lm, perm.inc.pred.df)

perm.inc.pred.check.df <- main.df


perm.inc.pred.check.df[, ! colnames(perm.inc.pred.check.df) %in% c("rsrf.dev",  "above34.dev", "rsrf.sq.dev", "above34.sq.dev", "year", "vill", "landsize_cat")] <- 0

perm.inc.pred.check.df$year <- unique(perm.inc.pred.check.df$year)[1]
perm.inc.pred.check.df$vill <- unique(perm.inc.pred.check.df$vill)[1]
perm.inc.pred.check.df$landsize_cat <- levels(perm.inc.pred.check.df$landsize_cat)[1]

summary( main.df$incperm + predict(tab3.col1.lm, perm.inc.pred.check.df) - coef(tab3.col1.lm)[1] - predict(tab3.col1.lm, main.df)
)

temp.inc.pred.df <- main.df 

temp.inc.pred.df[, c("hhcomp_females0to14", "hhcomp_males0to14", "hhhigher_females15plus", "hhhigher_males15plus", "hhmiddle_females15plus", "hhmiddle_males15plus", "hhprimary_females15plus", "hhprimary_males15plus", "hhnone_females15plus", "hhnone_males15plus")] <- 0

temp.inc.pred.df$landsize_cat <- levels(temp.inc.pred.df$landsize_cat)[1]
temp.inc.pred.df$vill <- unique(temp.inc.pred.df$vill)[1]
temp.inc.pred.df$year <- unique(temp.inc.pred.df$year)[1]

head(predict(tab3.col1.lm, main.df))
head(predict(tab3.col1.lm, temp.inc.pred.df))

main.df$inctrans <- predict(tab3.col1.lm, temp.inc.pred.df) - coef(tab3.col1.lm)[1]

main.df$incunexp <- main.df$netinc_real - main.df$incperm - main.df$inctrans

library(stargazer)

stargazer(main.df[-tab3.col1.lm$na.action, c("incperm", "inctrans", "incunexp")], median = TRUE)


# TODO: Do I now erase all observations that did not come into the prediction?


# C.

STD.DEV.rain <-  unclass(unlist(
  by(main.df, INDICES=list(main.df$vill), FUN=function(x) {
    x <- x[!duplicated(x$year), ]
    sd(x$rsrf)
  })
))

main.df <- merge(main.df,
  data.frame(STD.DEV.rain=STD.DEV.rain, vill=names(STD.DEV.rain), stringsAsFactors=FALSE))

STD.DEV.above34 <-  unclass(unlist(
  by(main.df, INDICES=list(main.df$vill), FUN=function(x) {
    x <- x[!duplicated(x$year), ]
    sd(x$above34)
  })
))



main.df <- merge(main.df,
  data.frame(STD.DEV.above34=STD.DEV.above34, vill=names(STD.DEV.above34), stringsAsFactors=FALSE))




tab4.save1.lm <- lm( I( -netincassetsale_real) ~ incperm + inctrans + incunexp + 
    hhcomp_kids0to14 + hhcomp_adults15to55 + hhcomp_elders56plus + 
    STD.DEV.rain + STD.DEV.above34 +
    factor(year), data=main.df[-tab3.col1.lm$na.action, ])
  
summary(tab4.save1.lm)

#with(main.df[-tab3.col1.lm$na.action, ],  {summary(
#  sqrt(rsrf.sq.dev)/length(unique(year))
#  )})

main.df$sd.rain.for.check <- with(main.df,  {summary(
  sqrt(rsrf.sq.dev/length(unique(year)))
  )})

main.df[!duplicated(main.df[, c("sd.rain.for.check", "vill")]), c("sd.rain.for.check", "rsrf", "vill")]



# TODO: is the square root of the weather vars the correct thing to do?

tab4.save2.lm <- lm( I( netinc_real - cons_real  ) ~ incperm + inctrans + incunexp + 
    hhcomp_kids0to14 + hhcomp_adults15to55 + hhcomp_elders56plus + 
    STD.DEV.rain + STD.DEV.above34 +
    factor(year), data=main.df[-tab3.col1.lm$na.action, ])
  
#summary(tab4.save2.lm)

# TODO: not sure that putting in netincassetsale_real is correct. Ok, I have taken it out now after convo with Rashesh.

library(car)

# TODO: White-correct standard errors?

linearHypothesis(tab4.save1.lm, hypothesis.matrix="inctrans = 1", test="Chisq", white.adjust="hc0")
linearHypothesis(tab4.save1.lm, hypothesis.matrix="inctrans = incperm", test="Chisq", white.adjust="hc0")

linearHypothesis(tab4.save2.lm, hypothesis.matrix="inctrans = 1", test="Chisq", white.adjust="hc0")
linearHypothesis(tab4.save2.lm, hypothesis.matrix="inctrans = incperm", test="Chisq", white.adjust="hc0")

# I will have to put these manually into the LaTex table

stargazer(tab4.save1.lm, tab4.save2.lm, 
  se=list(
    sqrt(diag(vcovHC(tab4.save1.lm, type = "HC0"))),
    sqrt(diag(vcovHC(tab4.save2.lm, type = "HC0"))) 
    ),
  notes="White-corrected standard errors in parentheses"
  )

# D.



tab3.col2.lm <- lm( I( -netincassetsale_real) ~ rsrf.dev + rsrf.sq.dev + above34.dev + above34.sq.dev +
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill, data=main.df)

tab3.col3.lm <- lm( I( netinc_real - cons_real  ) ~ 
    rsrf.dev + rsrf.sq.dev + above34.dev + above34.sq.dev +
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill, data=main.df)


tab3.col2.only.weather.lm <- lm( I( -netincassetsale_real) ~ rsrf.dev + rsrf.sq.dev + above34.dev + above34.sq.dev +
   factor(year) + vill, data=main.df[-tab3.col2.lm$na.action, ])

tab3.col3.only.weather.lm <- lm( I( netinc_real - cons_real  ) ~ 
    rsrf.dev + rsrf.sq.dev + above34.dev + above34.sq.dev +
    factor(year) + vill, data=main.df[-tab3.col3.lm$na.action, ])

#linearHypothesis(tab3.col2.lm,  white.adjust="hc0")

lrtest(tab3.col2.lm, tab3.col2.only.weather.lm )
lrtest(tab3.col3.lm, tab3.col3.only.weather.lm )


stargazer(tab3.col1.lm, tab3.col2.lm, tab3.col3.lm,   se=list(
    sqrt(diag(vcovHC(tab3.col1.lm, type = "HC0"))),
    sqrt(diag(vcovHC(tab3.col2.lm, type = "HC0"))),
    sqrt(diag(vcovHC(tab3.col3.lm, type = "HC0")))
   ),
  omit=c("year", "vill"),
  align=TRUE, no.space=TRUE,  single.row=TRUE, float.env = "sidewaystable", float=TRUE,
  notes = c("Year and village dummies omitted from table", "White-corrected standard errors in parentheses")
)



tab3.col1.col2.joint.lm <- lm( cbind(netinc_real, I( -netincassetsale_real)) ~ rsrf.dev + rsrf.sq.dev + above34.dev + above34.sq.dev +
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill, data=main.df)


tab3.col1.col2.joint.sur <- systemfit( list(netinc_real ~ rsrf.dev + rsrf.sq.dev + above34.dev + above34.sq.dev +
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill,
  save1 ~ rsrf.dev + rsrf.sq.dev + above34.dev + above34.sq.dev +
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill), data=main.df, method="SUR")

# Ok, with the OLS method , the var-cov matrix is block diag, but with SUR, we get a full var-cov, but the estimates are different.

linearHypothesis(tab3.col1.col2.joint.sur, 
  c("eq1_rsrf.dev = eq2_rsrf.dev",
    "eq1_rsrf.sq.dev = eq2_rsrf.sq.dev",
    "eq1_above34.dev = eq2_above34.dev",
    "eq1_above34.sq.dev = eq2_above34.sq.dev"))

linear.hypothesis(tab3.col1.col2.joint.sur, c("eq1_rsrf.dev = eq2_rsrf.dev",
    "eq1_rsrf.sq.dev = eq2_rsrf.sq.dev",
    "eq1_above34.dev = eq2_above34.dev",
    "eq1_above34.sq.dev = eq2_above34.sq.dev"), test = "Chisq")

#linearHypothesis(tab3.col1.col2.joint.lm, "rsrf.dev")
# "0.5*SPPversicolor = 0.5*SPPvirginica"
# linearHypothesis(tab3.col1.col2.joint.lm, c("rsrf.dev", "rsrf.sq.dev", "above34.dev", "above34.sq.dev"))

#waldtest(tab3.col1.lm, tab3.col2.lm)
#waldtest(tab3.col1.lm, tab3.col3.lm)



#linearHypothesis(tab3.col1.col2.joint.lm, "rsrf.dev")
# "0.5*SPPversicolor = 0.5*SPPvirginica"
# linearHypothesis(tab3.col1.col2.joint.lm, c("rsrf.dev", "rsrf.sq.dev", "above34.dev", "above34.sq.dev"))


# See http://datavis.ca/papers/Fox+Friendly+Weisberg.pdf

#linearHypothesis(tab3.col1.col2.joint.lm, c("hhhigher_females15plus"), verbose=TRUE)
#linearHypothesis(tab3.col1.col2.joint.lm, c("rsrf.dev = 4", "rsrf.sq.dev", "above34.dev", "above34.sq.dev"), verbose=TRUE)

#coef(tab3.col1.col2.joint.lm)

library(aod)

cross.eq.test <- function(y1, y2, formula, hypothesis, data) {
  
  fitted.lm <- lm(as.formula(paste0("cbind(", y1, ", ", y2, ") ~ ", formula)), data=data)
  
  hypothesis.mat <- car:::makeHypothesis(rownames(vcov(fitted.lm)), 
    hypothesis=hypothesis, rhs=NULL)
  hypothesis.mat <- hypothesis.mat[, -ncol(hypothesis.mat)]
  
  wald.test(Sigma=vcov(fitted.lm),  # vcovHC(fitted.lm, type = "HC0")
  b=rbind(coef(fitted.lm)[, 1, drop=FALSE], coef(fitted.lm)[, 2, drop=FALSE]), 
  L=hypothesis.mat )
}

main.df$save1 <- -main.df$netincassetsale_real
#neg.netincassetsale_real


# sandwich:::vcovHC.mlm
# This exists, so it is doing mlm-specific White correction


main.df$save2 <- main.df$netinc_real - main.df$cons_real
#- main.df$netincassetsale_real 


cross.eq.test(y1="netinc_real", 
  y2="save1",
  formula="rsrf.dev + rsrf.sq.dev + above34.dev + above34.sq.dev +
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill", 
  hypothesis=c("netinc_real:rsrf.dev = save1:rsrf.dev",
    "netinc_real:rsrf.sq.dev = save1:rsrf.sq.dev",
    "netinc_real:above34.dev = save1:above34.dev",
    "netinc_real:above34.sq.dev = save1:above34.sq.dev"),
  data=main.df)

cross.eq.test(y1="netinc_real", 
  y2="save2",
  formula="rsrf.dev + rsrf.sq.dev + above34.dev + above34.sq.dev +
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill", 
  hypothesis=c("netinc_real:rsrf.dev = save2:rsrf.dev",
    "netinc_real:rsrf.sq.dev = save2:rsrf.sq.dev",
    "netinc_real:above34.dev = save2:above34.dev",
    "netinc_real:above34.sq.dev = save2:above34.sq.dev"),
  data=main.df)




test.systemfit <- systemfit(formula=list(first= I( -netincassetsale_real) ~ rsrf.dev + rsrf.sq.dev + above34.dev + above34.sq.dev +
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill,
  second= I( netinc_real - cons_real  ) ~ 
    rsrf.dev + rsrf.sq.dev + above34.dev + above34.sq.dev +
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill
   ), method="OLS", data=main.df)

summary(test.systemfit)

# E.

tab4.revised.save1.lm <- lm( save1 ~ inctrans + 
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill, data=main.df[-tab3.col1.lm$na.action, ])


# TODO: keep or drop village dummies?

tab4.revised.save2.lm <- lm( save2 ~ inctrans + 
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill, data=main.df[-tab3.col1.lm$na.action, ])


#summary(tab4.save1.and.save2.lm)


linearHypothesis(tab4.revised.save1.lm, hypothesis.matrix="inctrans = 1", test="Chisq", white="hc0")
linearHypothesis(tab4.revised.save2.lm, hypothesis.matrix="inctrans = 1", test="Chisq", white="hc0")


stargazer(tab4.revised.save1.lm, tab4.revised.save2.lm, se=list(
  sqrt(diag(vcovHC(tab4.revised.save1.lm, type = "HC0"))), 
  sqrt(diag(vcovHC(tab4.revised.save2.lm, type = "HC0")))
  ),
  align=TRUE, no.space=TRUE,  single.row=TRUE,
  notes="White-corrected standard errors in parentheses"
)
#  omit=c("year", "vill"),
#  align=TRUE, no.space=TRUE,  single.row=TRUE, float.env = "sidewaystable", float=TRUE,
#  notes = "Year and village dummies omitted from table"



# G.

library(systemfit)

save1.2sls <- systemfit( save1 ~ netinc_real  +
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill,
  method="2SLS", 
  inst= ~ rsrf.dev + rsrf.sq.dev + above34.dev + above34.sq.dev,
  data=main.df)
# + factor(year) + vill

# TODO: should we use the STD.DEV or the ones we use above?


# BELOW: put all regressors as instruments? This works a heck of a lot better...
# ALso taking out netinc_real since that is what we are instrumenting for

save1.2sls <- systemfit( save1 ~ netinc_real  +
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill,
  method="2SLS", 
  inst= ~ rsrf.dev + rsrf.sq.dev + above34.dev + above34.sq.dev +
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill,
  data=main.df)

save2.2sls <- systemfit( save2 ~ netinc_real  +
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill,
  method="2SLS", 
  inst= ~ rsrf.dev + rsrf.sq.dev + above34.dev + above34.sq.dev +
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill,
  data=main.df)

summary(save1.2sls)


summary(main.df$rsrf.dev)

aggregate(main.df$rsrf.dev, by=list(main.df$vill), FUN=mean, na.rm=TRUE)




# save1.2sls <- systemfit( save1 ~ netinc_real  +
#     hhcomp_females0to14 + hhcomp_males0to14 +
#     hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
#     hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
#     hhnone_females15plus + hhnone_males15plus +
#     landsize_cat + factor(year) + vill,
#   method="2SLS", 
#   inst= ~ rsrf.dev + rsrf.sq.dev + above34.dev + above34.sq.dev +
#     hhcomp_females0to14 + hhcomp_males0to14 +
#     hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
#     hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
#     hhnone_females15plus + hhnone_males15plus +
#     landsize_cat + factor(year) + vill,
#   data=main.df)
# + factor(year) + vill

# TODO: should we use the STD.DEV or the ones we use above?

summary(save1.2sls)


# "Weak instrumental variables can lead to large standard errors of the IV/2SLS estimators. "
# - http://www3.grips.ac.jp/~yamanota/Lecture%20Note%208%20to%2010%202SLS%20&%20others.pdf

# May have huge standard errors due to weak instruments

# really not sure what the marginal proponsity to save is, since this is no longer transitory income



# save2.2sls <- systemfit( save2 ~ netinc_real  +
#     hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
#     hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
#     hhnone_females15plus + hhnone_males15plus +
#     landsize_cat + factor(year) + vill,
#   method="2SLS", 
#   inst= ~ rsrf.dev + rsrf.sq.dev + above34.dev + above34.sq.dev,
#   data=main.df)
# + factor(year) + vill

# TODO: should we use the STD.DEV or the ones we use above?

summary(save2.2sls)


# save1.2sls

stargazer(save1.2sls,  save2.2sls,
  align=TRUE, no.space=TRUE,  single.row=TRUE
)

# Cannot do White-corrected standard errors since singular vcov matrix:
#se=list(
#  sqrt(diag(vcovHC(save1.2sls, type = "HC0"))), 
#  sqrt(diag(vcovHC(save2.2sls, type = "HC0")))
#  )




library(texreg)

texreg(list(save1.2sls, save2.2sls))


#vcovHC(save1.2sls, type="HC0")

linearHypothesis(save1.2sls, hypothesis.matrix="eq1_netinc_real = 1", test="Chisq", white.adjust="hc0")
linearHypothesis(save2.2sls, hypothesis.matrix="eq1_netinc_real = 1", test="Chisq", white.adjust="hc0")


linearHypothesis(save1.2sls, hypothesis.matrix="eq1_netinc_real = 1", test="Chisq")
linearHypothesis(save2.2sls, hypothesis.matrix="eq1_netinc_real = 1", test="Chisq")


# TODO: Weird; the white correction should be failing. It seems that it is not actually White-correcting since without it there is no change. ALso, I'm not sure what "transitory income" is.


#install.packages("texreg")


# Ok, I don't know how we are going to outreg this

save1.2sls$coefCov[] <- 1
save2.2sls$coefCov[] <- 1

texreg(list(save1.2sls, save2.2sls), 
  override.pval = list(rep(1, length(coef(save2.2sls))), rep(1, length(coef(save2.2sls)))),
  override.se = list(rep(1, length(coef(save2.2sls))), rep(1, length(coef(save2.2sls)))),
  stars = numeric(0),
  ci.force=FALSE
)


#install.packages("AER")
library(AER)

save1.ivreg <- ivreg(save1 ~ netinc_real  +
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill |
    hhcomp_females0to14 + hhcomp_males0to14 +
    rsrf.dev + rsrf.sq.dev + above34.dev + above34.sq.dev + 
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill,
  data=main.df)

summary(save1.ivreg, diagnostics=TRUE)

summary(save1.ivreg)

save2.ivreg <- ivreg(save2 ~ netinc_real  +
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill |
    hhcomp_females0to14 + hhcomp_males0to14 +
    rsrf.dev + rsrf.sq.dev + above34.dev + above34.sq.dev + 
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill,
  data=main.df)



stargazer(save1.ivreg, save2.ivreg,   align=TRUE, no.space=TRUE,  single.row=TRUE)
#   align=TRUE, no.space=TRUE,  single.row=TRUE

# install.packages("sem")
#  library(sem)
#  
#  save.2sls <- tsls(save1 ~ netinc_real  +
#      hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
#      hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
#      hhnone_females15plus + hhnone_males15plus +
#      landsize_cat + factor(year) + vill, 
#    instruments = ~ rsrf.sq.dev, 
#    data = main.df
#  )
#  
# summary(save.2sls)

# install.packages("gmm")
library(gmm)


summary( test.gmm<- gmm(g= save1 ~ netinc_real  +
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill, 
  x = ~ 1 + rsrf.dev + rsrf.sq.dev + above34.dev + above34.sq.dev +
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill,
  type= "iterative", data=main.df, traceIter=TRUE  # , wmatrix="ident"  # ,  "iterative" itermax=0
))


test.gmm$algoinfo

# TODO: ok maybe we should not have het robust SE for the naive 2nd stage least squares. That correction may add invalidity to something that is already invalid.

# 3SLS



specTest(save1.2sls)

save1.2sls <- systemfit( save1 ~ netinc_real  +
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill,
  method="W2SLS", 
  inst= ~ rsrf.dev + rsrf.sq.dev + above34.dev + above34.sq.dev +
    hhcomp_females0to14 + hhcomp_males0to14 +
    hhhigher_females15plus + hhhigher_males15plus + hhmiddle_females15plus + 
    hhmiddle_males15plus + hhprimary_females15plus + hhprimary_males15plus +
    hhnone_females15plus + hhnone_males15plus +
    landsize_cat + factor(year) + vill,
  data=main.df, control=systemfit.control( maxiter=3 ) )

summary(save1.2sls)

# ok, whatevs, I belive gmm with weight matrix is more efficient













# H.



mean.income <- aggregate(main.df$netinc_real, by=list(vill=main.df$vill, year=main.df$year), FUN=mean, na.rm=TRUE)

colnames(mean.income)[3] <- "mean.netinc_real"

mean.consumption <- aggregate(main.df$cons_real, by=list(vill=main.df$vill, year=main.df$year), FUN=mean, na.rm=TRUE)

colnames(mean.consumption)[3] <- "mean.cons_real"

library(rgl)

main.df <- merge(main.df, mean.income)
main.df <- merge(main.df, mean.consumption)

main.df$idiosyc.income <- main.df$netinc_real - main.df$mean.netinc_real
main.df$idiosyc.consumption <- main.df$cons_real - main.df$mean.cons_real

plot.variables <- expand.grid(village=unique(main.df$vill), measure=c("idiosyc.income", "idiosyc.consumption"), stringsAsFactors=FALSE)

i <- 1

for ( i in 1:nrow(plot.variables)) {
  
  max.z.value<- max(as.matrix(main.df[main.df$vill==plot.variables$village[i], c("idiosyc.income", "idiosyc.consumption")]), na.rm=TRUE)
  min.z.value<- min(as.matrix(main.df[main.df$vill==plot.variables$village[i], c("idiosyc.income", "idiosyc.consumption")]), na.rm=TRUE)

idiosyc.inc.mat<- reshape(main.df[main.df$vill==plot.variables$village[i], c("unique.hhid", "year", plot.variables$measure[i])], timevar="year", idvar="unique.hhid",  direction="wide")

idiosyc.inc.mat <- idiosyc.inc.mat[!apply(idiosyc.inc.mat, 1, FUN=function(x) any(is.na(x))), ]

idiosyc.inc.mat <- idiosyc.inc.mat[order(rowSums(idiosyc.inc.mat[, -1]), decreasing=FALSE), ]

idiosyc.inc.mat$hh.index <- nrow(idiosyc.inc.mat):1

idiosyc.inc.df<- reshape(idiosyc.inc.mat, timevar="year", idvar="unique.hhid",  direction="long")

idiosyc.inc.df <- idiosyc.inc.df[order(idiosyc.inc.df$year, idiosyc.inc.df$hh.index),]

par3d(cex=.6)

persp3d(y = unique(idiosyc.inc.df$year),
  x = unique(idiosyc.inc.df$hh.index),
  z= idiosyc.inc.df[, 4],
  col="white", smooth=FALSE,
  ylab="", xlab="", zlab="", cex=.3, box=FALSE,
  zlim=c(min.z.value, max.z.value),
  sub=paste0("Village ", plot.variables$village[i], ", ",
    ifelse(plot.variables$measure[i]=="idiosyc.consumption", "Consumption", "Income"))
)

rgl.viewpoint(userMatrix=
  as.matrix(c(0.630089521408081, -0.148683235049248, 0.762154996395111, 
0, 0.776505708694458, 0.12709641456604, -0.617158949375153, 0, 
-0.0051060002297163, 0.980683267116547, 0.195535406470299, 0, 
0, 0, 0, 1), ncol=4, nrow=4)
)

rgl.postscript( filename=paste(work.dir, "Townsend",
  plot.variables$village[i], plot.variables$measure[i],
".pdf"), fmt="pdf", drawText=TRUE )

}





# 
# 
# townsend.plot.df <- main.df
# 
# townsend.plot.df <- townsend.plot.df[townsend.plot.df$vill=="A", c("unique.hhid", "year", "idiosyc.income")]
# 
# townsend.plot.df <- townsend.plot.df[!apply(townsend.plot.df, 1, FUN=function(x) any(is.na(x))), ]
# 
# townsend.plot.df <- townsend.plot.df[order(townsend.plot.df$idiosyc.income, townsend.plot.df$year, decreasing=FALSE), ]
# 
# townsend.plot.df$hh.index <- 1:nrow(townsend.plot.df)
# 
# persp3d(y = townsend.plot.df$year,
#   x = townsend.plot.df$hh.index,
#   z=townsend.plot.df$idiosyc.income
# )
# 
# persp3d(z=t(as.matrix(idiosyc.inc.mat[, -1])), 
# #  y= as.character(2001:2004),
#   color="white", 
#   smooth=FALSE, xlab="Year", ylab="HH", zlab="", cex=.3, box=FALSE) 
# 
# # saved.viewpoint <- par3d(no.readonly=TRUE)
# # saved.viewpoint$userMatrix
# # par3d(saved.viewpoint)
# 
# 
# 
# idiosyc.con.mat<- reshape(main.df[main.df$vill=="A", c("unique.hhid", "year", "idiosyc.consumption")], timevar="year", idvar="unique.hhid",  direction="wide")
# 
# idiosyc.con.mat <- idiosyc.con.mat[!apply(idiosyc.con.mat, 1, FUN=function(x) any(is.na(x))), ]
# 
# idiosyc.con.mat <- idiosyc.con.mat[order(rowSums(idiosyc.con.mat[, -1]), decreasing=TRUE), ]
# 
# persp3d(z=as.matrix(idiosyc.con.mat[, -1]), color="red", smooth=FALSE) 
# 
# # TODO: need to a do a few things with keeping the ylim the same across cons and income, and having the same order() across cons and income


library(Hmisc)

by(main.df, INDICES=list(main.df$vill), FUN=function(x) {
  rcorr(x$idiosyc.income, x$idiosyc.consumption, type="pearson")
}) 



# Now onto the regressions

# Within estimator means person FE, I think
# install.packages("plm")
library(plm)

cons.vill.mean.df<- aggregate(main.df$cons_real, by=list(vill=main.df$vill, year=main.df$year), FUN=mean, na.rm=TRUE)
names(cons.vill.mean.df)[3]<- "vill.year.mean.cons"

main.df <- merge(main.df, cons.vill.mean.df)

main.df$cons_real.minus.mean <- main.df$cons_real - main.df$vill.year.mean.cons

inc.vill.mean.df<- aggregate(main.df$netinc_real, by=list(vill=main.df$vill, year=main.df$year), FUN=mean, na.rm=TRUE)
names(inc.vill.mean.df)[3]<- "vill.year.mean.inc"

main.df <- merge(main.df, inc.vill.mean.df)

main.df$netinc_real.minus.mean <- main.df$netinc_real - main.df$vill.year.mean.inc


library(Hmisc)

by(main.df, INDICES=list(main.df$vill), FUN=function(x) {
  rcorr(x$netinc_real.minus.mean, x$cons_real.minus.mean, type="pearson")
}) 

# Ok, let's ignore the correlation analysis



#townsend.within.plm <- plm(cons_real.minus.mean ~ netinc_real, model="within",
#  data=main.df, index=c("vill", "year"))

#townsend.within.plm <- lm(cons_real.minus.mean ~ netinc_real + vill + factor(year),
#  data=main.df)
# TODO: What control do we have? year or village? 

townsend.within.plm <- lm(cons_real.minus.mean ~ netinc_real.minus.mean,
  data=main.df)

summary(townsend.within.plm)


#summary(lm(cons_real.minus.mean ~ netinc_real + unique.hhid, data=main.df) )


# Doing eqn 19, without measurement error correction

#townsend.fd.plm <- plm(cons_real.minus.mean~ netinc_real, model="fd",
#  data=main.df, index=c("unique.hhid", "year"))

#townsend.fd.plm <- lm(diff(cons_real.minus.mean) ~ diff(netinc_real),
#  data=main.df)

#townsend.fd.plm <- lm(diff(cons_real.minus.mean) ~ diff(netinc_real.minus.mean),
#  data=main.df)

townsend.fd.plm <- plm(cons_real.minus.mean ~ netinc_real.minus.mean,
  model="fd",
  data=main.df, index=c("unique.hhid", "year"))

summary(townsend.fd.plm)


# stargazer(townsend.within.plm, townsend.fd.plm, 
#   coef=list(coef(townsend.within.plm),
#     coef(townsend.fd.plm)
#     ),
#   se=list(
#     sqrt(diag(vcovHC(townsend.within.plm, type = "HC0"))),
#     NULL
#     ) , 
#   align=TRUE, no.space=TRUE,  single.row=TRUE,
#   notes="Standard errors in parentheses"
# )

# Thiis fails to give me what I want. Use the below.


stargazer(townsend.within.plm, townsend.fd.plm,
  notes="Standard errors in parentheses")


texreg(list(townsend.within.plm, townsend.fd.plm), digits=5, notes="Standard errors in parentheses")

#test <- plm::vcovHC(townsend.fd.plm,  method = "white1",  type = "HC0")
#c("arellano", "white1", "white2")

# TODO: OK, have not figure out how to do het-robust SE
# Looks like they can't be het-robust







#persp3d(x=main.df[main.df$vill=="A", "year"], 
#  y=main.df[main.df$vill=="A", "unique.hhid"],
#  z=main.df[main.df$vill=="A", "idiosyc.income"],
#  color="red", smooth=FALSE) 







