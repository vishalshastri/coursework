


# QUESTION 1.a

#install.packages("plm")
library("stargazer")
library("plm")
library("plyr")
library("sandwich")
library("lmtest")

data(Produc)
Produc$hwy <- Produc$hwy / 1000
Produc$water <- Produc$water / 1000
Produc$util <- Produc$util / 1000


summary( trad.fe.plm <- plm(emp ~ hwy + water + util, data=Produc, effect = "individual", model = "within"))

coeftest(trad.fe.plm, vcov=vcovBK(trad.fe.plm, type="HC1"))[,"Std. Error"]

stargazer(trad.fe.plm, trad.fe.plm,
se=list(sqrt(diag(vcov(trad.fe.plm))),
  coeftest(trad.fe.plm, vcov=vcovBK(trad.fe.plm, type="HC1"))[,"Std. Error"]),  
  out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 2/table1.tex", 
  no.space=TRUE, single.row=TRUE, keep.stat=c("n", "adj.rsq"),
  notes=c("Unadjusted standard errors in column (1)", "State-clustered standard errors in column (2)"))

# QUESTION 1.b


Produc.means.df <- ddply(Produc[, c("state", "emp", "hwy", "water", "util")], "state",
  function(x) {data.frame(t(colMeans(x[, -1])))} )

colnames(Produc.means.df)[!colnames(Produc.means.df) %in% "state"] <- 
  paste0(colnames(Produc.means.df)[!colnames(Produc.means.df) %in% "state"], ".mean")

Produc <-merge(Produc, Produc.means.df)

summary( demeaned.fe.plm <- plm( 
  I(emp - emp.mean) ~ - 1 + I(hwy - hwy.mean) + I(water - water.mean) + I(util - util.mean), data=Produc, effect = "individual", model = "pooling"))
# -1 for no intercept

coeftest(demeaned.fe.plm, vcov=vcovBK(demeaned.fe.plm, type="HC1"))

stargazer(demeaned.fe.plm, demeaned.fe.plm,
se=list(sqrt(diag(vcov(demeaned.fe.plm))),
  coeftest(demeaned.fe.plm, vcov=vcovBK(demeaned.fe.plm, type="HC1"))[,"Std. Error"]),  
  out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 2/table2.tex", 
  no.space=TRUE, single.row=TRUE, keep.stat=c("n", "adj.rsq"),
  notes=c("Unadjusted standard errors in column (1)", "State-clustered standard errors in column (2)"))

# QUESTION 1.c

summary( state.dummies.fe.plm <- plm( 
  emp  ~ - 1 + hwy + water + util + state, data=Produc, effect = "individual", model = "pooling"))

coeftest(state.dummies.fe.plm, vcov=vcovBK(state.dummies.fe.plm, type="HC1"))

stargazer(state.dummies.fe.plm, state.dummies.fe.plm,
se=list(sqrt(diag(vcov(state.dummies.fe.plm))),
  coeftest(state.dummies.fe.plm, vcov=vcovBK(state.dummies.fe.plm, type="HC1"))[,"Std. Error"]),  
  out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 2/table3.tex", 
  no.space=TRUE, single.row=TRUE, keep.stat=c("n", "adj.rsq"),
  notes=c("Unadjusted standard errors in column (1)", "State-clustered standard errors in column (2)"), omit="state")

# two periods:

summary( two.period.fe.plm <- plm(emp ~ hwy + water + util, data=Produc[Produc$year %in% 1980:1981, ], effect = "individual", model = "within"))

coeftest(two.period.fe.plm, vcov=vcovBK(two.period.fe.plm, type="HC1"))[,"Std. Error"]

stargazer(two.period.fe.plm, two.period.fe.plm,
se=list(sqrt(diag(vcov(two.period.fe.plm))),
  coeftest(two.period.fe.plm, vcov=vcovBK(two.period.fe.plm, type="HC1"))[,"Std. Error"]),  
  out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 2/table4.tex", 
  no.space=TRUE, single.row=TRUE, keep.stat=c("n", "adj.rsq"),
  notes=c("Unadjusted standard errors in column (1)", "State-clustered standard errors in column (2)"), omit="state")



# TODO: what happens if only use two periods?


# QUESTION 2.a.i

library("foreign")

jtrain.df <- read.dta("http://www.stata.com/data/jwooldridge/eacsap/jtrain1.dta")

jtrain.df <- jtrain.df[jtrain.df$year %in% 1987:1988 , ]
firms.with.missings<- jtrain.df$fcode[is.na(jtrain.df$hrsemp) ]
jtrain.df <- jtrain.df[ !jtrain.df$fcode %in% firms.with.missings, ]

table(jtrain.df$year)
table(jtrain.df$fcode)

treatment.indiv <- jtrain.df$fcode[jtrain.df$grant==1 ]

jtrain.df$treatment <- ifelse(jtrain.df$fcode %in% treatment.indiv, 1, 0)

dif.in.dif.means <- aggregate(hrsemp ~ treatment + year, data=jtrain.df, FUN=mean)

dif.in.dif.tab<- reshape(dif.in.dif.means , direction="wide", idvar="treatment", timevar="year")

dif.in.dif.tab$dif <- dif.in.dif.tab$hrsemp.1987 - dif.in.dif.tab$hrsemp.1988

dif.in.dif.tab <- rbind(dif.in.dif.tab, dif.in.dif.tab[1, ] - dif.in.dif.tab[2, ])

dif.in.dif.tab$treatment <- c(0,1, "dif")

stargazer( dif.in.dif.tab, summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 2/table5.tex", 
  rownames=FALSE, digits=5 )




# QUESTION 2.a.ii

summary(first.dif.dif.lm <- lm(hrsemp ~ grant + as.factor(year) + treatment, data=jtrain.df) )

stargazer( first.dif.dif.lm , out.header = FALSE, out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 2/table6.tex", keep.stat=c("n", "adj.rsq"), no.space=TRUE, single.row=TRUE, digits=5 )


# QUESTION 2.a.iii

summary(second.dif.dif.lm <- lm(hrsemp ~ grant + as.factor(year) + as.factor(fcode), data=jtrain.df) )


stargazer( second.dif.dif.lm , out.header = FALSE, out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 2/table7.tex", keep.stat=c("n", "adj.rsq"), no.space=TRUE, single.row=TRUE, omit="fcode", digits=5 )


# QUESTION 2.b.i

summary(firm.time.trend.lm <- lm(hrsemp ~ grant + year:as.factor(fcode), data=jtrain.df) )


stargazer( firm.time.trend.lm  , out.header = FALSE, out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 2/table8.tex", keep.stat=c("n", "adj.rsq"), no.space=TRUE, single.row=TRUE, omit="fcode", digits=5 )


# QUESTION 2.b.ii

summary(y.time.trend.lm <- lm(hrsemp ~ year, data=jtrain.df) )

summary(x.time.trend.lm <- lm(grant ~ year, data=jtrain.df) )

summary(y.x.time.trend.lm<- lm(resid(y.time.trend.lm) ~ resid(x.time.trend.lm)))

stargazer( y.x.time.trend.lm  , out.header = FALSE, out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 2/table9.tex", keep.stat=c("n", "adj.rsq"), no.space=TRUE, single.row=TRUE, digits=5 )


# QUESTION 3.a


download.file("http://www.ssc.wisc.edu/~ctaber/DD/regm.raw.gz", 
  destfile="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 2/regm.raw.gz")

unzip("/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 2/regm.raw.gz",
  exdir = "/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 2/regm.raw")

regm.df <- read.table("/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 2/regm.raw", col.names=c("coll", "merit", "male", "black", "asian", "year", "state", "chst"))

summary(standard.coll.lm <- lm(coll ~ merit + male + black + asian + factor(year) + factor(state), data=regm.df ))

standard.coll.het.se<- coeftest(standard.coll.lm, vcov=vcovHC(standard.coll.lm, type="HC1"))
# Het-corrected

#source("http://people.su.se/~ma/clmclx.R")

standard.coll.state.year.se <- mclx(fm=standard.coll.lm, dfcw=1, cluster1=regm.df$state, cluster2=regm.df$year)

standard.coll.state.se <-clx(fm=standard.coll.lm, dfcw=1, cluster=regm.df$state)

stargazer( standard.coll.lm, standard.coll.lm, standard.coll.lm,
  se=list(standard.coll.het.se[, "Std. Error"], 
    standard.coll.state.year.se[, "Std. Error"], 
    standard.coll.state.se[, "Std. Error"]),
  out.header = FALSE, out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 2/table10.tex", keep.stat=c("n", "adj.rsq"), no.space=TRUE, single.row=TRUE, omit=c("state", "year"),
  notes=c("White-corrected standard errors in column (1)", "State- and year-clustered standard errors in column (2)", "State-clustered standard errors in column (3)") )

# QUESTION 3.b

state.weight.df <- data.frame(prop.table(table(regm.df$state)))
colnames(state.weight.df) <- c("state", "inverse.state.weight")

regm.df <- merge(regm.df, state.weight.df)

summary(standard.coll.weighted.lm <- lm(coll ~ merit + male + black + asian + factor(year) + factor(state), data=regm.df, weight= 1/inverse.state.weight))


standard.coll.weighted.het.se<- coeftest(standard.coll.weighted.lm, vcov=vcovHC(standard.coll.weighted.lm, type="HC1"))

standard.coll.weighted.state.year.se <- mclx(fm=standard.coll.weighted.lm , dfcw=1, cluster1=regm.df$state, cluster2=regm.df$year)

standard.coll.weighted.state.se <-clx(fm=standard.coll.weighted.lm , dfcw=1, cluster=regm.df$state)

stargazer( standard.coll.weighted.lm, standard.coll.weighted.lm, standard.coll.weighted.lm,
  se=list(standard.coll.weighted.het.se[, "Std. Error"], 
    standard.coll.weighted.state.year.se[, "Std. Error"], 
    standard.coll.weighted.state.se[, "Std. Error"]),
  out.header = FALSE, out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 2/table11.tex", keep.stat=c("n", "adj.rsq"), no.space=TRUE, single.row=TRUE, omit=c("state", "year"), notes=c("White-corrected standard errors in column (1)", "State- and year-clustered standard errors in column (2)", "State-clustered standard errors in column (3)") )

# QUESTION 3.c


regm.means.df <- ddply(regm.df, c("state", "year"), function(x) {data.frame(t(colMeans(x[, !names(x) %in% c("state", "year")])))} )

summary(mean.coll.lm <- lm(coll ~ merit + male + black + asian + factor(year) + factor(state), data=regm.means.df) )

mean.coll.het.se<- coeftest(mean.coll.lm, vcov=vcovHC(mean.coll.lm, type="HC1"))

mean.coll.state.se <-clx(fm=mean.coll.lm , dfcw=1, cluster=regm.means.df$state)

stargazer( mean.coll.lm, mean.coll.lm,
  se=list(mean.coll.het.se[, "Std. Error"], 
    mean.coll.state.se[, "Std. Error"]), 
  out.header = FALSE, out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 2/table12.tex", keep.stat=c("n", "adj.rsq"), no.space=TRUE, single.row=TRUE, omit=c("state", "year"), notes=c("White-corrected standard errors in column (1)", "State-clustered standard errors in column (2)") )

# QUESTION 3.d

regm.means.df$inverse.state.weight  <- NULL

regm.df.only.weights <- regm.df[, c("state", "year", "inverse.state.weight")]
regm.df.only.weights <- regm.df.only.weights[!duplicated(regm.df.only.weights), ]

regm.means.df <- merge(regm.means.df, regm.df.only.weights)

summary(mean.coll.weighted.lm <- lm(coll ~ merit + male + black + asian + factor(year) + factor(state), data=regm.means.df, weight=inverse.state.weight ))


mean.coll.weighted.het.se<- coeftest(mean.coll.weighted.lm, vcov=vcovHC(mean.coll.weighted.lm, type="HC1"))

mean.coll.weighted.state.se <-clx(fm=mean.coll.weighted.lm , dfcw=1, cluster=regm.means.df$state)

stargazer( mean.coll.weighted.lm, mean.coll.weighted.lm,
  se=list(mean.coll.weighted.het.se[, "Std. Error"], 
    mean.coll.weighted.state.se[, "Std. Error"]), 
  out.header = FALSE, out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 2/table13.tex", keep.stat=c("n", "adj.rsq"), no.space=TRUE, single.row=TRUE, omit=c("state", "year"), notes=c("White-corrected standard errors in column (1)", "State-clustered standard errors in column (2)") )



regm.means.df <- ddply(regm.df, c("state", "year"), function(x) {data.frame(t(colMeans(x[, !names(x) %in% c("state", "year")])))} )

regm.means.df

colnames(regm.means.df )[!colnames(regm.means.df ) %in% c("state", "year")] <- 
  paste0(colnames(regm.means.df )[!colnames(regm.means.df) %in% c("state", "year")], ".mean")

regm.df <- merge(regm.df, regm.means.df)

summary( demeaned.coll.plm <- lm( 
  I(coll - coll.mean) ~ - 1 + I(merit - merit.mean) + I(male - male.mean) + I(black - black.mean) + I(asian - asian.mean), data=regm.df))







