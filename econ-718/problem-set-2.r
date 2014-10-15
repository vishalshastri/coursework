


# QUESTION 1.a

#install.packages("plm")
library("plm")
library("plyr")
library("sandwich")
library("lmtest")

data(Produc)



summary( trad.fe.plm <- plm(unemp ~ hwy + water + util, data=Produc, effect = "individual", model = "within"))

coeftest(trad.fe.plm, vcov=vcovBK)

# QUESTION 1.b


Produc.means.df <- ddply(Produc[, c("state", "unemp", "hwy", "water", "util")], "state",
  function(x) {data.frame(t(colMeans(x[, -1])))} )

colnames(Produc.means.df)[!colnames(Produc.means.df) %in% "state"] <- 
  paste0(colnames(Produc.means.df)[!colnames(Produc.means.df) %in% "state"], ".mean")

Produc <-merge(Produc, Produc.means.df)

summary( demeaned.fe.plm <- plm( 
  I(unemp - unemp.mean) ~ - 1 + I(hwy - hwy.mean) + I(water - water.mean) + I(util - util.mean), data=Produc, effect = "individual", model = "pooling"))
# -1 for no intercept

coeftest(demeaned.fe.plm, vcov=vcovBK)

# QUESTION 1.c

summary( state.dummies.fe.plm <- plm( 
  unemp  ~ - 1 + hwy + water + util + state, data=Produc, effect = "individual", model = "pooling"))

coeftest(state.dummies.fe.plm, vcov=vcovBK)




# QUESTION 2.a.i

library("foreign")

jtrain.df <- read.dta("http://www.stata.com/data/jwooldridge/eacsap/jtrain1.dta")

jtrain.df <- jtrain.df[jtrain.df$year %in% 1987:1988 & !is.na(jtrain.df$hrsemp), ]

treatment.indiv <- jtrain.df$fcode[jtrain.df$grant==1 ]

jtrain.df$treatment <- ifelse(jtrain.df$fcode %in% treatment.indiv, 1, 0)

dif.in.dif.means <- aggregate(hrsemp ~ treatment + year, data=jtrain.df, FUN=mean)

dif.in.dif.tab<- reshape(dif.in.dif.means , direction="wide", idvar="treatment", timevar="year")

dif.in.dif.tab$dif <- dif.in.dif.tab$hrsemp.1987 - dif.in.dif.tab$hrsemp.1988

dif.in.dif.tab <- rbind(dif.in.dif.tab, dif.in.dif.tab[1, ] - dif.in.dif.tab[2, ])

dif.in.dif.tab$treatment <- c(0,1, "dif")


# QUESTION 2.a.ii

summary(first.dif.dif.lm <- lm(hrsemp ~ grant + as.factor(year) + treatment, data=jtrain.df) )


# QUESTION 2.a.iii

summary(second.dif.dif.lm <- lm(hrsemp ~ grant + as.factor(year) + as.factor(fcode), data=jtrain.df) )

# Hmm, I do not get exactly the same answer

# P.S. we have some mising vals for hrsemp

# QUESTION 2.b.i

summary(firm.time.trend.lm <- lm(hrsemp ~ grant + year:as.factor(fcode), data=jtrain.df) )

# QUESTION 2.b.ii

summary(y.time.trend.lm <- lm(hrsemp ~ year, data=jtrain.df) )

summary(x.time.trend.lm <- lm(grant ~ year, data=jtrain.df) )

summary(y.x.time.trend.lm<- lm(resid(y.time.trend.lm) ~ resid(x.time.trend.lm)))

# QUESTION 3.a


download.file("http://www.ssc.wisc.edu/~ctaber/DD/regm.raw.gz", 
  destfile="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 2/regm.raw.gz")

unzip("/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 2/regm.raw.gz",
  exdir = "/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 2/")

regm.df <- read.table("/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 2/regm.raw", col.names=c("coll", "merit", "male", "black", "asian", "year", "state", "chst"))

# What is chst?

# regm.df <- plm.data(regm.df, indexes=c("state", "year"))

summary(standard.coll.lm <- lm(coll ~ merit + male + black + asian + factor(year) + factor(state), data=regm.df ))


coeftest(standard.coll.lm, vcov=vcovHC)
# Het-corrected

source("http://people.su.se/~ma/clmclx.R")

mclx(fm=standard.coll.lm, dfcw=1, cluster1=regm.df$state, cluster2=regm.df$year)

clx(fm=standard.coll.lm, dfcw=1, cluster=regm.df$state)


# QUESTION 3.b

state.weight.df <- data.frame(prop.table(table(regm.df$state)))
colnames(state.weight.df) <- c("state", "inverse.state.weight")

regm.df <- merge(regm.df, state.weight.df)

summary(standard.coll.weighted.lm <- lm(coll ~ merit + male + black + asian + factor(year) + factor(state), data=regm.df, weight= 1/inverse.state.weight))

# TODO: do various standard errors

# QUESTION 3.c


regm.means.df <- ddply(regm.df, c("state", "year"), function(x) {data.frame(t(colMeans(x)))} )

colnames(regm.means.df )[!colnames(regm.means.df ) %in% c("state", "year")] <- 
  paste0(colnames(regm.means.df )[!colnames(regm.means.df) %in% c("state", "year")], ".mean")

regm.df <- merge(regm.df, regm.means.df)

summary( demeaned.coll.plm <- lm( 
  I(coll - coll.mean) ~ - 1 + I(merit - merit.mean) + I(male - male.mean) + I(black - black.mean) + I(asian - asian.mean), data=regm.df))







