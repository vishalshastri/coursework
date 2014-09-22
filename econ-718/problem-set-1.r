


work.dir <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/"

load(paste0(work.dir, "crop wide df4.Rdata"))



library("foreign")

hogar01.df<-read.spss(paste0(work.dir, "bd18 (2001).zip Folder/hogar.sav"), to.data.frame = TRUE)

crop.wide.df <- merge(crop.wide.df , hogar01.df[, c("FOLIO", "FACTOR")])

crop.wide.df <- crop.wide.df[!is.na(crop.wide.df$mean.ann.rain.5yr) & !is.na(crop.wide.df$T.CEC.CLAY), ]


#crop.wide.df$fert.exp <- crop.wide.df$fert.exp/crop.wide.df$total.area


crop.wide.df$drive.time.amanzanada[is.na(crop.wide.df$drive.time.amanzanada)] <-
  mean(crop.wide.df$drive.time.amanzanada, na.rm=TRUE)
  
crop.wide.df$drive.time.urban[is.na(crop.wide.df$drive.time.urban)] <-
  mean(crop.wide.df$drive.time.urban, na.rm=TRUE)


# vars to rescale:
#drive.time.amanzanada
#mean.ann.rain.5yr
#h1.elevation
#REMPAIS
#REMEXT

# cor(crop.wide.df$drive.time.amanzanada, crop.wide.df$drive.time.urban)


crop.wide.df$drive.time.amanzanada <- crop.wide.df$drive.time.amanzanada/60/60
# original is in seconds, so convert to hrs
crop.wide.df$drive.time.amanzanada[crop.wide.df$drive.time.amanzanada>20] <- median(crop.wide.df$drive.time.amanzanada)*5

crop.wide.df$drive.time.urban <- crop.wide.df$drive.time.urban/60/60
# original is in seconds, so convert to hrs
crop.wide.df$drive.time.urban[crop.wide.df$drive.time.urban>20] <- median(crop.wide.df$drive.time.urban)*5


crop.wide.df$elevation <- crop.wide.df$elevation/1000

#summary(crop.wide.df$mean.ann.rain.5yr) This is probably ok. it's in cm, I think
# nevermind, I will convert to meters
crop.wide.df$mean.ann.rain.5yr <- crop.wide.df$mean.ann.rain.5yr/100
crop.wide.df$REMPAIS <- crop.wide.df$REMPAIS/100 
crop.wide.df$REMEXT <- crop.wide.df$REMEXT/100



prod01.df<-read.spss(paste0(work.dir, "bd18 (2001).zip Folder/agricola.sav"), to.data.frame = TRUE)




colnames(prod01.df)<-c("FOLIO", "CODA", "crop", "crop.code", "area", "area.unit", "harvest", "harvest.unit", "sales.quant", "sales.value", "consumption", "seeds", "animal.or.subproducts", "bartered", "lost")

prod01.df<-within(prod01.df, {
  # TODO: fix the is.na after we have imputed the values
  area.r<-0
	area.r[area.unit==2 & !is.na(area.unit)] <- area[area.unit==2 & !is.na(area.unit)]
	area.r[area.unit==1 & !is.na(area.unit)] <- area[area.unit==1 & !is.na(area.unit)] / 10000
} )
	
quant.to.conv<-c("harvest", "sales.quant", "consumption", "seeds", "animal.or.subproducts", "bartered", "lost")
for (i in quant.to.conv) {
	prod01.df[, paste0(i, ".r")]<-0
	prod01.df[prod01.df$harvest.unit=="Arroba" & !is.na(prod01.df$harvest.unit), paste0(i, ".r")] <- 
		prod01.df[prod01.df$harvest.unit=="Arroba" & !is.na(prod01.df$harvest.unit), i]*11.5
	prod01.df[prod01.df$harvest.unit=="Quintal" & !is.na(prod01.df$harvest.unit), paste0(i, ".r")] <- 
		prod01.df[prod01.df$harvest.unit=="Quintal" & !is.na(prod01.df$harvest.unit), i]*46
	prod01.df[prod01.df$harvest.unit=="Libra" & !is.na(prod01.df$harvest.unit), paste0(i, ".r")] <- 
		prod01.df[prod01.df$harvest.unit=="Libra" & !is.na(prod01.df$harvest.unit), i]*0.453592
	prod01.df[prod01.df$harvest.unit=="Unidad" & !is.na(prod01.df$harvest.unit), paste0(i, ".r")] <- 
		prod01.df[prod01.df$harvest.unit=="Unidad" & !is.na(prod01.df$harvest.unit), i]
		prod01.df[prod01.df$harvest.unit=="Kilogramo" & !is.na(prod01.df$harvest.unit), paste0(i, ".r")] <- 
		prod01.df[prod01.df$harvest.unit=="Kilogramo" & !is.na(prod01.df$harvest.unit), i]
}

prod01.df$crop.hh.id<-1:nrow(prod01.df)

top.crops<-names(rev(sort(table(prod01.df$crop))))[1:12]

recs.to.fix.hectare<-by(prod01.df[prod01.df$crop %in% top.crops, ], 
  INDICES=factor(prod01.df$crop[prod01.df$crop %in% top.crops]),
	FUN = function(x) {
		crop.quantiles<-quantile(x$area.r, probs=c(.05, .95), na.rm=TRUE)
		yield<-x$harvest.r/x$area.r
		yield.quantiles<-quantile(yield, probs=c(.05, .95), na.rm=TRUE)
		keep.index<-x$area.r>crop.quantiles[1]*10000 & !is.na(x$area.r) & 
			(yield*10000>yield.quantiles[1] & yield*10000<yield.quantiles[2])
		if (all(!keep.index)) return(NULL)
		
		x$crop.hh.id[keep.index]

	}
)

#prod01.df$area.unit[unlist(recs.to.fix.hectare)]

prod01.df$area.unit.cleaned<-prod01.df$crop.hh.id %in% unlist(recs.to.fix.hectare)

prod01.df$area.r[prod01.df$crop.hh.id %in% unlist(recs.to.fix.hectare)]<-
	prod01.df$area.r[prod01.df$crop.hh.id %in% unlist(recs.to.fix.hectare)]/10000


area.agg <- aggregate(prod01.df[, "area.r", drop=FALSE], by=list(FOLIO=prod01.df$FOLIO), FUN=sum, na.rm=TRUE)

crop.wide.df <- merge(crop.wide.df, area.agg)

crop.wide.df$area.r[crop.wide.df$area.r==0] <- median(crop.wide.df$area.r)/2
# only 2 observations have this

 crop.wide.df$fert.exp <- crop.wide.df$fert.exp/crop.wide.df$area.r

# ok, nevermind; I will not change this to expenditure per hectare

crop.wide.df$fert.exp[crop.wide.df$fert.exp>5000] <- 
  median(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0])*5


#summary(crop.wide.df$hhh.edu.measure)

















crop.wide.df$received.credit <- abs(as.numeric(crop.wide.df$received.credit)-2 )


# OK, NOW START HERE FOR 718 PS:


# QUESTION 1.a

# install.packages("stargazer")
library("stargazer")



summary( fert.big.lm <- lm(log(fert.exp +1) ~ indig.prop + drive.time.urban + T.CASO4 + received.credit, data=crop.wide.df))


stargazer(fert.big.lm, out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/table1.tex", no.space=TRUE, single.row=TRUE)
  
# align=FALSE,  rownames=TRUE,  column.separate=c(4,4), column.labels =c("test1", "test2"), float.env = "sidewaystable", font.size="small", title="Summary statistics of treatment and control groups"
    


# QUESTION 1.b

summary(partitioned.1.lm <- lm( log(fert.exp +1) ~ T.CASO4 + received.credit, data=crop.wide.df))

#summary(partitioned.2.lm <- lm( indig.prop + drive.time.urban  ~ T.CASO4 + received.credit, data=crop.wide.df))

X.1 <- as.matrix(crop.wide.df[, c("indig.prop", "drive.time.urban")])

X.2 <- as.matrix(crop.wide.df[, c("T.CASO4",  "received.credit")])

X.2 <- cbind(1, X.2)


M.2 <- diag(nrow(X.2)) - X.2 %*% solve(t(X.2) %*% X.2) %*% t(X.2)


summary(partitioned.3.lm <- lm( resid(partitioned.1.lm) ~ I(M.2 %*% X.1) - 1 ))

stargazer(partitioned.3.lm, out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/table2.tex", no.space=TRUE, single.row=TRUE)



summary(partitioned.4.lm <- lm( log(fert.exp +1) ~ I(M.2 %*% X.1) - 1 , data=crop.wide.df))

stargazer(partitioned.4.lm, out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/table3.tex", no.space=TRUE, single.row=TRUE)



coef(partitioned.3.lm) - coef(partitioned.4.lm)
coef(partitioned.3.lm) 
coef(partitioned.4.lm)


with(crop.wide.df, cor(resid(partitioned.1.lm) , log(fert.exp +1) ))


cor()

# Question 2.a


# install.packages("AER")
library("AER")


#summary(lm( REMPAIS ~ hhh.age + num.pers.agropecuaria , data=crop.wide.df))


#summary(lm( fert.exp ~ REMPAIS + num.pers.agropecuaria , data=crop.wide.df))

#summary(lm( log(fert.exp+1) ~ log(REMPAIS+1)  , data=crop.wide.df))

#ivreg(y ~ x1 + x2 | z1 + z2 + z3

#summary(ivreg(fert.exp ~ REMPAIS | REMPAIS + hhh.sex, data=crop.wide.df))


# Ok, let's go with this:

# summary(lm( log(fert.exp+1) ~ log(REMPAIS+1)  , data=crop.wide.df))


summary(fert.ivreg <- ivreg( log(fert.exp+1) ~ log(REMPAIS+1) + total.area | hhh.sex + total.area, data=crop.wide.df))


stargazer(fert.ivreg, out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/table4.tex", no.space=TRUE, single.row=TRUE)

# with(crop.wide.df, cor(cbind(log(fert.exp + 1) , log(REMPAIS + 1),  hhh.sex))

# QUESTION 2.b

summary( first.stage.lm <- lm(log(REMPAIS+1) ~ hhh.sex + total.area, data=crop.wide.df))
  
summary( second.stage.lm <- lm( log(fert.exp+1) ~ predict(first.stage.lm) + total.area, data=crop.wide.df))

stargazer(first.stage.lm, out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/table5.tex", no.space=TRUE, single.row=TRUE)

stargazer(second.stage.lm, out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/table6.tex", no.space=TRUE, single.row=TRUE)


# QUESTION 2.3
  
# log(REMPAIS+1) +
summary( red.form.1.lm <- lm(log(fert.exp+1) ~  hhh.sex + total.area, data=crop.wide.df))

summary( red.form.2.lm <- lm( log(REMPAIS+1) ~ hhh.sex + total.area, data=crop.wide.df))

coef(red.form.1.lm)["hhh.sexmujer"] / coef(red.form.2.lm)["hhh.sexmujer"]

coef(fert.ivreg)

# TODO: just write it in the text without a table


# Question 2.4
  
cov.Y.lm<- lm( log(fert.exp+1) ~  total.area, data=crop.wide.df)
  
cov.T.lm<- lm( log(REMPAIS+1) ~  total.area , data=crop.wide.df)

cov.Z.lm<- lm( as.numeric(hhh.sex) ~  total.area, data=crop.wide.df)

cov(resid(cov.Z.lm), resid(cov.Y.lm)) /
  cov(resid(cov.Z.lm), resid(cov.T.lm))

# TODO: just write it in the text without a table


# -1.956320937 

# QUESTION 3.a
  
num.obs <- 10000
  
set.seed(100)
  
T.i <- rgamma(num.obs, shape=3)

beta.0 <- -32
beta.1 <- 8.7
  
u.i <- rnorm(num.obs, sd=20)
  
Y.i <- beta.0 + beta.1 * T.i + u.i 
  
tau.1.i <- T.i + rnorm(num.obs, sd=5)
  
tau.2.i <- T.i + rnorm(num.obs, sd=10)

summary(Q.a.lm <-  lm(Y.i ~ T.i))

stargazer(Q.a.lm, out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/table7.tex", no.space=TRUE, single.row=TRUE)


# QUESTION 3.b

summary(Q.b.lm <-  lm(Y.i ~ tau.1.i))
  
beta.1 * var(T.i) / (var(T.i) + 5^2)

stargazer(Q.b.lm, out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/table8.tex", no.space=TRUE, single.row=TRUE)


# QUESTION 3.c 

summary( meas.err.ivreg  <- ivreg(Y.i ~ tau.1.i |  tau.2.i))

stargazer(meas.err.ivreg, out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/table9.tex", no.space=TRUE, single.row=TRUE)



