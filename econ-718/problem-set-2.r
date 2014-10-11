



# QUESTION 2.a.i

library("foreign")

jtrain.df <- read.dta("http://www.stata.com/data/jwooldridge/eacsap/jtrain1.dta")

jtrain.df <- jtrain.df[jtrain.df$year %in% 1987:1988 & !is.na(jtrain.df$hrsemp), ]

treatment.indiv <- jtrain.df$fcode[jtrain.df$grant==1 ]

jtrain.df$treatment <- ifelse(jtrain.df$fcode %in%treatment.indiv, 1, 0)

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















