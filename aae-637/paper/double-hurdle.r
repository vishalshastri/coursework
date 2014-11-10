
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







# lognormal
# truncnorm
# loglogit
# cauchy


posi.fert <- crop.wide.df$fert.exp[crop.wide.df$fert.exp>0]

posi.fert.density <- density(posi.fert, n=2^13)




pdf(paste0(work.dir, "tex building/hurdledist.pdf"),  width=5, height=5)

par(mfcol=c(2,2))

par(cex=.65)


# include this plot
plot(posi.fert.density, xlab=NA, 
  main="Density of positive fertilizer \nexpediture per hectare, levels",
  xlim=c(0,1000), xaxs="i", yaxs="i", 
  ylim=c(0, 1.1*max(posi.fert.density$y)))

log.posi.fert.density <- density(log(posi.fert))

# include this plot
plot(log.posi.fert.density,  xlab=NA, 
  main="Density of positive fertilizer \nexpediture per hectare, logs",
  xaxs="i", yaxs="i", 
  ylim=c(0, 1.1*max(log.posi.fert.density$y)))
  
# TODO: Say that this could be picking up on price effects, too



# install.packages("truncnorm")
library("truncnorm")
#install.packages("msm")
library("msm")
#install.packages("actuar")
library("actuar")
# install.packages("tmvtnorm")
library("tmvtnorm")
# install.packages("mhurdle")
library("mhurdle")



lnorm.fit <- fitdistr(x=posi.fert, densfun = dlnorm,
start=list(meanlog=log(mean(posi.fert)), 
  sdlog=log(sd(posi.fert))), 
   method="BFGS", control=list(trace=36, maxit=10000) )
   
   
# If a standard normal distribution is left-truncated at k then the
# mean becomes m = f/Q, where f is the density at k and Q is the
# area to the right of k, and the variance becomes s^2 = 1 - m(m-k).

# For an aribitrary normal distribution, with mean M, variance S^2,
# and truncation point K, get m and s as above using k = (K-M)/S.
# To get back to the original units, use m -> m*S + M and s -> s*S.

# used this for help: http://mathforum.org/kb/message.jspa?messageID=3704949
# truncnorm.start <- m = max(posi.fert.density$y)/prop.table(table(crop.wide.df$fert.exp==0))[1]

truncnorm.fit <- mle.tmvnorm(as.matrix(posi.fert, ncol=1), lower=0, upper=Inf, start=list(mu=-5000,
sigma=var(posi.fert)*3 ))

# sd(posi.fert) * 5)

llog.fit <- fitdistr(x=posi.fert, densfun = dllogis,
start=list(shape=2, rate = 1),  method="BFGS", control=list(trace=36, maxit=10000) )

lcauchy.fit <- fitdistr(x=log(posi.fert), densfun = dcauchy,
start=list(location=median(log(posi.fert)), 
  scale = 5),  method="BFGS", control=list(trace=36, maxit=10000) )

set.seed(100)

# posi.fert <- jitter(posi.fert)

#while(any(posi.fert<=0)) { posi.fert <- jitter(posi.fert) }

ks.test(posi.fert, plnorm, 
  lnorm.fit$estimate[1], 
  lnorm.fit$estimate[2] )


ks.test(posi.fert, ptmvnorm.marginal, 
  mean = truncnorm.fit@coef[1], sigma = matrix(truncnorm.fit@coef[2]),
  lower=0, upper=Inf
  )

ks.test(posi.fert, pllogis, 
  llog.fit$estimate[1], 
  llog.fit$estimate[2] )

ks.test(log(posi.fert), pcauchy, 
  lcauchy.fit$estimate[1], 
  lcauchy.fit$estimate[2] )
  

# TODO: need summary stats



# include this plot
plot(ecdf(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0]), xlim=c(0,1000),
  main="Empirical CDF and estimated\ntruncated normal CDF",
  xlab="Fert expenditure per hectare")

lines(seq(from=0, to=1000, by=1), 
 ptmvnorm.marginal(seq(from=0, to=1000, by=1), mean = truncnorm.fit@coef[1], sigma = matrix(truncnorm.fit@coef[2]), lower=0, upper=Inf)
, col="red")


# include this plot
plot(ecdf(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0]), xlim=c(0,1000),
  main="Empirical CDF and estimated\nlog-normal CDF",
  xlab="Fert expenditure per hectare")

lines(seq(from=0, to=max(crop.wide.df$fert.exp), by=.1), 
 plnorm(seq(from=0, to=max(crop.wide.df$fert.exp), by=.1), 
 lnorm.fit$estimate[1], 
  lnorm.fit$estimate[2] ), col="red")

dev.off()

par(mfcol=c(1,1))





# plot( seq(from=0, to=1000, by=1),  dtmvnorm.marginal(seq(from=0, to=1000, by=1), mean = truncnorm.fit@coef[1], sigma = matrix(truncnorm.fit@coef[2]), lower=0, upper=Inf), type="l")

# plot( seq(from=0, to=1000, by=1),  dnorm(seq(from=0, to=1000, by=1), mean = truncnorm.fit@coef[1], sd = sqrt(truncnorm.fit@coef[2])), type="l")

# plot( seq(from=-2000, to=1000, by=1),  dnorm(seq(from=-2000, to=1000, by=1), mean = truncnorm.fit@coef[1], sd = sqrt(truncnorm.fit@coef[2])), type="l")


#qnorm(0.71224 , mean = truncnorm.fit@coef[1], sd = sqrt(truncnorm.fit@coef[2]))
# frequency of zero fert.exp is 0.71224 
# qnorm(0.89 , mean = truncnorm.fit@coef[1], sd = sqrt(truncnorm.fit@coef[2]))
# 0.89 gets us to about zero




 
#crop.wide.df$credit.source[
#  crop.wide.df$credit.source=="Prestamo.de.algna.Cooperativa.de.ahorro"] <- 
#  "Prestamo.de.algn.Fondo.financiero.Privado"

# simplifying the credit categories does not give us a higher success rate in the bootstraps, so dont do it.

#FALSE  TRUE 
#   13     7 









# Start double hurdle here:










mode.factor <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# thanks to http://stackoverflow.com/questions/2547402/standard-library-function-in-r-for-finding-the-mode


factors.to.convert <- c( "hhh.edu.measure", "hhh.sex", "credit.source", "hhh.literacy", "department")


for ( i in factors.to.convert) {
  crop.wide.df[, i]<- factor(make.names(crop.wide.df[, i]) )
  crop.wide.df[, i] <- relevel(crop.wide.df[, i], as.character(mode.factor(crop.wide.df[, i])))
}


crop.wide.df$hhh.edu.measure.r <- ""
crop.wide.df$hhh.edu.measure.r[crop.wide.df$hhh.edu.measure %in% "ninguno"] <- "None"
crop.wide.df$hhh.edu.measure.r[crop.wide.df$hhh.edu.measure %in% 
  c("educacin.bsica.de.adultos..eba.", "curso.de.alfabetizacin", 
  "primaria..1.a.8.aos.", "bsico..1.a.5.aos.")] <- "Primary"
crop.wide.df$hhh.edu.measure.r[crop.wide.df$hhh.edu.measure %in% 
  c("normal", "secundaria..1.a.4.aos.", "secundaria..1.a.4.aos.", 
  "medio..1.a.4.aos.", "intermedio..1.a.3.aos.", 
  "centro.de.educacin.media.de.adultos..cema.")] <- "Secondary"
crop.wide.df$hhh.edu.measure.r[crop.wide.df$hhh.edu.measure %in% 
  c("tcnico.de.universidad", "tcnico.de.instituto", "universidad..licenciatura.")] <- "Tertiary"

crop.wide.df$hhh.edu.measure.r <- factor(crop.wide.df$hhh.edu.measure.r)

crop.wide.df[, "hhh.edu.measure.r"] <- 
  relevel(crop.wide.df[, "hhh.edu.measure.r"], as.character(mode.factor(crop.wide.df[, "hhh.edu.measure.r"])))




# This is end of initial processing







table(round(crop.wide.df$indig.prop), crop.wide.df$hhh.edu.measure.r)


levels(crop.wide.df$hhh.edu.measure)

sort(table(crop.wide.df$hhh.edu.measure))

sort(table(crop.wide.df$hhh.edu.measure.r))







m110d <- mhurdle(as.formula(paste("fert.exp ~ ", "department + indig.prop + indig.practices + hhh.literacy + hhh.age  + hhh.edu.measure.r*indig.prop + hhh.sex + REMPAIS + REMEXT + credit.source + drive.time.amanzanada + drive.time.urban + mean.ann.rain.5yr + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY  ", " | ", "department + indig.prop + indig.practices + hhh.age  + REMPAIS + REMEXT +  mean.ann.rain.5yr + hhh.edu.measure.r*indig.prop + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY  ", " | 0" ) ), 
  data = crop.wide.df, weights=crop.wide.df$FACTOR, corr = "d" , dist = "ln", method =  "BFGS" ,  print.level=2, iterlim=500)

summary(m110d)



m110d <- mhurdle(as.formula(paste("fert.exp ~ ", "department + indig.prop + indig.practices + hhh.literacy + hhh.age  + hhh.edu.measure.r + hhh.sex + REMPAIS + REMEXT + credit.source + drive.time.amanzanada + drive.time.urban + mean.ann.rain.5yr + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY  ", " | ", "department + indig.prop + indig.practices + hhh.age  + REMPAIS + REMEXT +  mean.ann.rain.5yr + hhh.edu.measure.r + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY  ", " | 0" ) ), 
  data = crop.wide.df, weights=crop.wide.df$FACTOR, corr = "d" , dist = "ln", method =  "BFGS" ,  print.level=2)

summary(m110d)




m110d <- mhurdle(as.formula(paste("fert.exp ~ ", " indig.prop + indig.practices + hhh.literacy + hhh.age  + hhh.edu.measure + hhh.sex + REMPAIS + REMEXT + credit.source + drive.time.amanzanada + drive.time.urban + mean.ann.rain.5yr  ", " | ", "indig.prop + indig.practices   + REMPAIS + REMEXT + mean.ann.rain.5yr  + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY + area.r  ", " | 0" ) ), 
  data = crop.wide.df, weights=crop.wide.df$FACTOR, corr = "d" , dist = "ln", method =  "BFGS" ,  print.level=2)
  # , finalHessian=FALSE
# I'm not sure if I should put in land too
  
table(crop.wide.df$hhh.edu.measure, crop.wide.df$fert.exp>0)

  
m110d <- mhurdle(as.formula(paste("fert.exp ~ ", " indig.prop + indig.practices + hhh.literacy + hhh.age  + hhh.edu.measure + hhh.sex + REMPAIS + REMEXT + credit.source + drive.time.amanzanada + drive.time.urban + mean.ann.rain.5yr  ", " | ", "indig.prop + indig.practices   + REMPAIS + REMEXT + mean.ann.rain.5yr + hhh.edu.measure + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY  ", " | 0" ) ), 
  data = crop.wide.df, weights=crop.wide.df$FACTOR, corr = "d" , dist = "ln", method =  "BFGS" ,  print.level=2, subset=hhh.edu.measure!="tcnico.de.universidad")

m110d.test <- mhurdle(as.formula(paste("fert.exp ~ ", "indig.prop*hhh.edu.measure + indig.practices    + hhh.sex + REMPAIS + REMEXT + credit.source + drive.time.amanzanada + mean.ann.rain.5yr  ", " | ", "indig.prop + indig.practices   + REMPAIS + REMEXT + mean.ann.rain.5yr  + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY  ", " | 0" )), 
  data = crop.wide.df, weights=crop.wide.df$FACTOR, corr = "d" , dist = "ln", method =  "BFGS" ,  print.level=2,
  subset= ! hhh.edu.measure %in% c("centro.de.educacin.media.de.adultos..cema.", "tcnico.de.universidad"))

summary(m110d.test)

m110d.test <- mhurdle(as.formula(paste("fert.exp ~ ", "indig.prop*hhh.edu.measure + indig.practices  ", " | ", "indig.prop + indig.practices   ", " | 0" )), 
  data = crop.wide.df, weights=crop.wide.df$FACTOR, corr = "d" , dist = "ln", method =  "BFGS" ,  print.level=2,
  subset= ! hhh.edu.measure %in% c("centro.de.educacin.media.de.adultos..cema.", "tcnico.de.universidad"))

summary(m110d.test)

# TODO: think about doing an interaction test for the final paper

#indig.prop:hhh.edu.measurecentro.de.educacin.media.de.adultos..cema.
#indig.prop:hhh.edu.measuretcnico.de.universidad  
#tcnico.de.universidad
# These are unidentified

#subset= ! hhh.edu.measure %in% c("centro.de.educacin.media.de.adultos..cema.", "tcnico.de.universidad")

glm.test <- glm(I(fert.exp>0) ~ indig.prop*hhh.edu.measure,   data=crop.wide.df, family=binomial(link = "logit"), x=TRUE,
  subset= ! hhh.edu.measure %in% c("centro.de.educacin.media.de.adultos..cema.", "tcnico.de.universidad"))
summary(glm.test)

library("erer")

maBina(glm.test, x.mean = TRUE, rev.dum = TRUE, digits = 3,
subset.name = NULL, subset.value)


glm.test <- glm(I(fert.exp>0) ~ indig.prop + hhh.edu.measure,   data=crop.wide.df, family=binomial(link = "logit"), x=TRUE)
summary(glm.test)

maBina(w=glm.test, x.mean = TRUE, rev.dum = TRUE, digits = 3,
subset.name = NULL, subset.value)





as.data.frame(lapply(model.frame(as.formula(paste("fert.exp ~ ", "indig.prop + indig.practices   + hhh.edu.measure + hhh.sex + REMPAIS + REMEXT + credit.source + drive.time.amanzanada + mean.ann.rain.5yr  + indig.prop + indig.practices   + REMPAIS + REMEXT + mean.ann.rain.5yr  + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY" ) ), crop.wide.df),  FUN=mean)) 

summary(crop.wide.df$fert.exp)



  
  as.formula(paste("fert.exp ~ ", "indig.prop + indig.practices   + hhh.edu.measure + hhh.sex + REMPAIS + REMEXT + credit.source + drive.time.amanzanada + mean.ann.rain.5yr + elevation ", " | ", "indig.prop + indig.practices   + REMPAIS + REMEXT + mean.ann.rain.5yr + elevation + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE  ", " | 0" ) )
  


main.specification.w.elev <- as.formula(paste("fert.exp ~ ", "department + indig.prop + indig.practices + hhh.literacy + hhh.age  + hhh.edu.measure.r + hhh.sex + REMPAIS + REMEXT + credit.source + drive.time.amanzanada + drive.time.urban + mean.ann.rain.5yr + elevation + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY  ", " | ", "department + indig.prop + indig.practices + hhh.age  + REMPAIS + REMEXT +  mean.ann.rain.5yr + elevation + hhh.edu.measure.r + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY  ", " | 0" ) )
  
  
m110d.w.elev <- mhurdle(main.specification.w.elev, 
  data = crop.wide.df, weights=crop.wide.df$FACTOR, corr = "d" , dist = "ln", method =  "BFGS" ,  print.level=2)

summary(m110d.w.elev)

save.image(file = "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/hurdle workspace save.RData")
  
  
  
  
  
  
#main.specification <- as.formula(paste("fert.exp ~ ", "department + indig.prop + indig.practices + hhh.literacy + hhh.age  + hhh.edu.measure.r + hhh.sex + REMPAIS + REMEXT + credit.source + drive.time.amanzanada + drive.time.urban + mean.ann.rain.5yr + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY  ", " | ", "department + indig.prop + indig.practices + hhh.age  + REMPAIS + REMEXT +  mean.ann.rain.5yr + hhh.edu.measure.r + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY  ", " | 0" ) )
# Above is without elevation

#main.specification <- as.formula(paste("fert.exp ~ ", "department + indig.prop + indig.practices + hhh.literacy + hhh.age  + hhh.edu.measure.r + hhh.sex + REMPAIS + REMEXT + credit.source + drive.time.amanzanada + drive.time.urban + mean.ann.rain.5yr + elevation + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY  ", " | ", "department + indig.prop + indig.practices + hhh.age  + REMPAIS + REMEXT +  mean.ann.rain.5yr + elevation + hhh.edu.measure.r + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY  ", " | 0" ) )

# main.specification is the specification that we use for the final results. 
# Above is what we used for the 637 paper that was actually turned in 






library("mhurdle")

crop.wide.df$credit.source.r <- 
  factor(ifelse(crop.wide.df$credit.source=="No.Credit", "No.Credit", "Received.Credit"))
  

outcome.vars <- c(# "fert.exp",  
"seed.exp"  ,
"hired.labor.exp" ,      
"manure.exp"       ,                      
"transport.exp"     ,                     
"pesticide.exp"      ,                    
"extension.exp"       ,                   
"machine.exp"          ,                  
"draft.anmial.exp"      ,                 
"other.exp"   )

dhurdle.mfx.bootstrapped.input.ls <- list()

for ( target.input in outcome.vars) {
  crop.wide.df[, target.input] <- crop.wide.df[, target.input]/crop.wide.df$area.r
}
# Set vars to per-hectare. Don't forget that fert.exp is already set as that.


for ( target.input in outcome.vars) {
  

main.specification <- as.formula(paste(target.input, " ~ ", "department + indig.prop*hhh.edu.measure.r + indig.practices + hhh.literacy + hhh.age  + hhh.sex + REMPAIS + REMEXT + credit.source.r + drive.time.amanzanada + drive.time.urban + mean.ann.rain.5yr + elevation + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY  ", " | ", "department + indig.prop + indig.practices + hhh.age  + REMPAIS + REMEXT +  mean.ann.rain.5yr + elevation + hhh.edu.measure.r + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY  ", " | 0" ) )
# Note that we are not including interaction term indig.prop*hhh.edu.measure.r in
# the second hurdle since I wonder if we will have enough observations for the bootstrap

  
m110d <- mhurdle(main.specification, 
  data = crop.wide.df, weights=crop.wide.df$FACTOR, corr = "d" , dist = "ln", method =  "BFGS" ,  print.level=2)



d.hurdle.coef <- as.data.frame(summary(m110d)$CoefTable)

d.hurdle.coef$stars <- ""
d.hurdle.coef$stars[d.hurdle.coef[, "Pr(>|t|)"]<.05] <- "*" 
d.hurdle.coef$stars[d.hurdle.coef[, "Pr(>|t|)"]<.01] <- "**"
d.hurdle.coef$stars[d.hurdle.coef[, "Pr(>|t|)"]<.001] <- "***"

#0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# ok, let's try to fix this for a side-by-side table


d.hurdle.coef <- d.hurdle.coef[, c("Estimate", "Std. Error", "stars")] 

d.hurdle.coef$param.names <- rownames(d.hurdle.coef)

d.hurdle.coef.2nd <- d.hurdle.coef[ substr(d.hurdle.coef$param.names, 1, 3)=="h2.", ]
d.hurdle.coef.2nd$param.names <- gsub("h2[.]", "", d.hurdle.coef.2nd$param.names)

d.hurdle.coef <- d.hurdle.coef[ substr(d.hurdle.coef$param.names, 1, 3)!="h2.", ]
d.hurdle.coef$param.names <- gsub("h1[.]", "", d.hurdle.coef$param.names)

colnames(d.hurdle.coef)[1:3] <- paste0("h1.", colnames(d.hurdle.coef)[1:3])
colnames(d.hurdle.coef.2nd)[1:3] <- paste0("h2.", colnames(d.hurdle.coef.2nd)[1:3])

d.hurdle.coef$orig.order <- 1:nrow(d.hurdle.coef)
d.hurdle.coef<- merge(d.hurdle.coef, d.hurdle.coef.2nd, all=TRUE)
d.hurdle.coef <- d.hurdle.coef[order(d.hurdle.coef$orig.order), ]
d.hurdle.coef$orig.order <- NULL

colnames(d.hurdle.coef)[grepl("stars", colnames(d.hurdle.coef))] <- ""
colnames(d.hurdle.coef)[colnames(d.hurdle.coef)==""][2] <- " "


#stargazer( d.hurdle.coef, out=paste0(work.dir, "tex building/hurdleparam.tex"),
#  summary=FALSE, out.header = FALSE, table.layout="a", rownames=FALSE, font.size="small",
#  column.sep.width="0pt", align=TRUE,
#  title=paste0("Double hurdle model: parameter estimates. (n = ", nrow(crop.wide.df), ")"),
#  notes=c("Signif codes: *** = 0.1\\%; ** = 1\\%; * = 5\\%; . = 10\\%.",
#   "Reference levels: Department - La Paz; Education - Primary; Credit - no loans.") )

# doesnt work for simple tables,just models:
# column.separate=c(1, 3,3), column.labels =c("test0", "test1", "test2"),


for ( i in factors.to.convert ) {
  print(t(t(sort(table(crop.wide.df[, i])))))
}



mfx.bootstrap <- function(crop.wide.df) {

cat(target.input)

m110d <- mhurdle(main.specification, 
  data = crop.wide.df, weights=crop.wide.df$FACTOR, corr = "d" , dist = "ln", method =  "BFGS",  print.level=2, iterlim=500)

test.frame <-as.data.frame(lapply(model.frame(m110d$formula, crop.wide.df),  FUN=mean)) 

#factors.to.convert <- c( "hhh.edu.measure.r", "hhh.sex", "credit.source", "hhh.literacy", "department")
# ABove are the vars in the turned-in 637 paper

factors.to.convert <- c( "hhh.edu.measure.r", "hhh.sex", "credit.source.r", "hhh.literacy", "department")


for ( i in factors.to.convert ) {
  test.frame[, i]<- mode.factor(crop.wide.df[, i])
  crop.wide.df[, i] <- factor(crop.wide.df[, i])
  # this gets rid of any unused levels
}
# TODO: tell the mhurdle authors about this make.names issue for factors
# TODO: also tell them about problem with the zero-level factors not showing up in 2nd hurdle
# TODO: But is it related to probit perfect prediction?
# TODO: Also tell about weights not working
mean.pred <- predict(m110d, newdata = test.frame)
epsilon.dist <- 1e-6

estimate.v<- c()
length(estimate.v) <- length(coef(m110d))

estimate.v.names <-  c( names(test.frame),
  paste0(factors.to.convert[1], levels(crop.wide.df[, factors.to.convert[1]])[-1]),
  paste0(factors.to.convert[2], levels(crop.wide.df[, factors.to.convert[2]])[-1]),
  paste0(factors.to.convert[3], levels(crop.wide.df[, factors.to.convert[3]])[-1]),
  paste0(factors.to.convert[4], levels(crop.wide.df[, factors.to.convert[4]])[-1]),
  paste0(factors.to.convert[5], levels(crop.wide.df[, factors.to.convert[5]])[-1])
)

estimate.v.names <- setdiff(estimate.v.names, c("fert.exp", factors.to.convert))

estimate.v <- rep(0, length(estimate.v.names))

names(estimate.v) <- estimate.v.names

estimate.v.2 <- estimate.v

for ( i in setdiff(names(test.frame), "fert.exp")) {
  test.frame.epsilon <- test.frame
  if (is.factor(test.frame.epsilon[, i])) {
#    crop.wide.df[, i] <- factor(crop.wide.df[, i])
    for ( j in levels(crop.wide.df[, i])[-1]) {
      test.frame.epsilon[, i] <- factor(j, levels=levels(crop.wide.df[, i]))
      epsilon.pred <- predict(m110d, newdata = test.frame.epsilon)
      pred.deriv <- (epsilon.pred - mean.pred) 
      estimate.v[paste0(i, j)] <- pred.deriv[1,1]
      estimate.v.2[paste0(i, j)] <- pred.deriv[1,2]
    }
    next
  }
  
  test.frame.epsilon[, i] <- test.frame.epsilon[, i] + epsilon.dist 
  epsilon.pred <- predict(m110d, newdata = test.frame.epsilon)
  pred.deriv <- (epsilon.pred - mean.pred) / epsilon.dist
  estimate.v[i] <- pred.deriv[1,1]
  estimate.v.2[i] <- pred.deriv[1,2]
}
      
names(estimate.v) <- paste0("h1.", names(estimate.v) )
names(estimate.v.2) <- paste0("h2.", names(estimate.v.2) )

estimate.complete <- c(estimate.v, estimate.v.2)

estimate.complete <- estimate.complete[!sapply(as.list(estimate.complete), FUN=function(x) isTRUE(all.equal(x, 0)))]

as.data.frame(t(estimate.complete) )

}

# predict(m110d)   #, newdata = test.frame.epsilon)


dhurdle.mfx <- mfx.bootstrap(crop.wide.df)

dhurdle.mfx <- t(dhurdle.mfx)


set.seed(100)

replications <- 1500
# replications <- 20

y.df <- crop.wide.df

system.time(

bootstraps <- apply(matrix(
    sample(1:nrow(y.df), size=replications*nrow(y.df), replace=TRUE),nrow=replications),
    1, FUN=function(x) { tryCatch(mfx.bootstrap(y.df[x, ]), error = function(e) "error thrown") }
)

)

table(sapply(bootstraps, FUN=function(x) length(x)==1) )

#Most recent:

#FALSE  TRUE 
# 2857  1143 

# FALSE  TRUE 
#  2114   886 
#Now with elevation:
# FALSE  TRUE 
# 2829  1171 



# test.mfx <- mfx.bootstrap(crop.wide.df[sample(1:nrow(crop.wide.df), size=nrow(crop.wide.df), replace=TRUE),])



bootstraps.cleaned <- bootstraps[!sapply(bootstraps, FUN=function(x) length(x)==1)]

library("plyr")

bootstraps.df <- do.call(rbind.fill, bootstraps.cleaned)

bootstraps.ls <- lapply( bootstraps.df, FUN=function(x) quantile(x, probs=c(.025, .975), na.rm=TRUE))
bootstraps.ls <- bootstraps.ls[ names(bootstraps.ls) %in% rownames(dhurdle.mfx) ]
bootstraps.ls <- bootstraps.ls[match(names(bootstraps.ls), rownames(dhurdle.mfx)) ]
bootstraps.mat <- matrix(unlist(bootstraps.ls), ncol=2, byrow = TRUE)
dhurdle.mfx.bootstrapped <- cbind(dhurdle.mfx, bootstraps.mat)
dhurdle.mfx.bootstrapped <- as.data.frame(dhurdle.mfx.bootstrapped)
dhurdle.mfx.bootstrapped$stars <- ""
dhurdle.mfx.bootstrapped$stars[
  (dhurdle.mfx.bootstrapped[, 2]>0 & dhurdle.mfx.bootstrapped[, 3]>0) |
  (dhurdle.mfx.bootstrapped[, 2]<0 & dhurdle.mfx.bootstrapped[, 3]<0)
  ] <- "*"
  
bootstraps.ls <- lapply( bootstraps.df, FUN=function(x) quantile(x, probs=c(.005, .995), na.rm=TRUE))
bootstraps.ls <- bootstraps.ls[ names(bootstraps.ls) %in% rownames(dhurdle.mfx) ]
bootstraps.ls <- bootstraps.ls[match(names(bootstraps.ls), rownames(dhurdle.mfx)) ]
bootstraps.mat <- matrix(unlist(bootstraps.ls), ncol=2, byrow = TRUE)
dhurdle.mfx.bootstrapped.stars <- cbind(dhurdle.mfx, bootstraps.mat)
dhurdle.mfx.bootstrapped.stars <- as.data.frame(dhurdle.mfx.bootstrapped.stars)
dhurdle.mfx.bootstrapped$stars[
  (dhurdle.mfx.bootstrapped.stars[, 2]>0 & dhurdle.mfx.bootstrapped.stars[, 3]>0) |
  (dhurdle.mfx.bootstrapped.stars[, 2]<0 & dhurdle.mfx.bootstrapped.stars[, 3]<0)
  ] <- "**"
  
  
bootstraps.ls <- lapply( bootstraps.df, FUN=function(x) quantile(x, probs=c(.0005, .9995), na.rm=TRUE))
bootstraps.ls <- bootstraps.ls[ names(bootstraps.ls) %in% rownames(dhurdle.mfx) ]
bootstraps.ls <- bootstraps.ls[match(names(bootstraps.ls), rownames(dhurdle.mfx)) ]
bootstraps.mat <- matrix(unlist(bootstraps.ls), ncol=2, byrow = TRUE)
dhurdle.mfx.bootstrapped.stars <- cbind(dhurdle.mfx, bootstraps.mat)
dhurdle.mfx.bootstrapped.stars <- as.data.frame(dhurdle.mfx.bootstrapped.stars)
dhurdle.mfx.bootstrapped$stars[
  (dhurdle.mfx.bootstrapped.stars[, 2]>0 & dhurdle.mfx.bootstrapped.stars[, 3]>0) |
  (dhurdle.mfx.bootstrapped.stars[, 2]<0 & dhurdle.mfx.bootstrapped.stars[, 3]<0)
  ] <- "***"

bootstraps.ls <- lapply( bootstraps.df, FUN=function(x) quantile(x, probs=c(.05, .95), na.rm=TRUE))
bootstraps.ls <- bootstraps.ls[ names(bootstraps.ls) %in% rownames(dhurdle.mfx) ]
bootstraps.ls <- bootstraps.ls[match(names(bootstraps.ls), rownames(dhurdle.mfx)) ]
bootstraps.mat <- matrix(unlist(bootstraps.ls), ncol=2, byrow = TRUE)
dhurdle.mfx.bootstrapped.stars <- cbind(dhurdle.mfx, bootstraps.mat)
dhurdle.mfx.bootstrapped.stars <- as.data.frame(dhurdle.mfx.bootstrapped.stars)
dhurdle.mfx.bootstrapped$stars[
  ((dhurdle.mfx.bootstrapped.stars[, 2]>0 & dhurdle.mfx.bootstrapped.stars[, 3]>0) |
  (dhurdle.mfx.bootstrapped.stars[, 2]<0 & dhurdle.mfx.bootstrapped.stars[, 3]<0)) &
  dhurdle.mfx.bootstrapped$stars==""
  ] <- "."

colnames(dhurdle.mfx.bootstrapped) <- c("Marg eff", "95% C.I.", "", "stars")

# dhurdle.mfx.bootstrapped.saved <- dhurdle.mfx.bootstrapped
# dhurdle.mfx.bootstrapped <- dhurdle.mfx.bootstrapped.saved

#d.hurdle.coef <- d.hurdle.coef[, c("Estimate", "Std. Error", "stars")] 

dhurdle.mfx.bootstrapped$param.names <- rownames(dhurdle.mfx.bootstrapped)

dhurdle.mfx.bootstrapped.2nd <- dhurdle.mfx.bootstrapped[ substr(dhurdle.mfx.bootstrapped$param.names, 1, 3)=="h2.", ]
dhurdle.mfx.bootstrapped.2nd$param.names <- gsub("h2[.]", "", dhurdle.mfx.bootstrapped.2nd$param.names)

dhurdle.mfx.bootstrapped <- dhurdle.mfx.bootstrapped[ substr(dhurdle.mfx.bootstrapped$param.names, 1, 3)!="h2.", ]
dhurdle.mfx.bootstrapped$param.names <- gsub("h1[.]", "", dhurdle.mfx.bootstrapped$param.names)

colnames(dhurdle.mfx.bootstrapped)[1:4] <- paste0("h1.", colnames(dhurdle.mfx.bootstrapped)[1:4])
colnames(dhurdle.mfx.bootstrapped.2nd)[1:4] <- paste0("h2.", colnames(dhurdle.mfx.bootstrapped.2nd)[1:4])

dhurdle.mfx.bootstrapped$orig.order <- 1:nrow(dhurdle.mfx.bootstrapped)
dhurdle.mfx.bootstrapped<- merge(dhurdle.mfx.bootstrapped, dhurdle.mfx.bootstrapped.2nd, all=TRUE)
dhurdle.mfx.bootstrapped <- dhurdle.mfx.bootstrapped[order(dhurdle.mfx.bootstrapped$orig.order), ]
dhurdle.mfx.bootstrapped$orig.order <- NULL

colnames(dhurdle.mfx.bootstrapped)[grepl("(stars)|(h[12][.]$)", colnames(dhurdle.mfx.bootstrapped))] <- ""

colnames(dhurdle.mfx.bootstrapped)[colnames(dhurdle.mfx.bootstrapped)==""][2] <- " "
colnames(dhurdle.mfx.bootstrapped)[colnames(dhurdle.mfx.bootstrapped)==""][2] <- "  "
colnames(dhurdle.mfx.bootstrapped)[colnames(dhurdle.mfx.bootstrapped)==""][2] <- "   "

dhurdle.mfx.bootstrapped[, 2] <-  (-1) * dhurdle.mfx.bootstrapped[, 2]
dhurdle.mfx.bootstrapped[, 3] <-  (-1) * dhurdle.mfx.bootstrapped[, 3]
dhurdle.mfx.bootstrapped[, 4] <-  (-1) * dhurdle.mfx.bootstrapped[, 4]

reverse.ci.order.v <- dhurdle.mfx.bootstrapped[, 3]
dhurdle.mfx.bootstrapped[, 3] <- dhurdle.mfx.bootstrapped[, 4]
dhurdle.mfx.bootstrapped[, 4] <- reverse.ci.order.v


dhurdle.mfx.bootstrapped.input.ls[[target.input]] <- dhurdle.mfx.bootstrapped


}

# TODO: something not messed up with the alignment of the 2nd hurdle and the results are wrong
# Also look at drive time alignments

stargazer( dhurdle.mfx.bootstrapped, out=paste0(work.dir, "tex building/hurdlemfx.tex"),
  summary=FALSE, out.header = FALSE, align=TRUE,
  rownames=FALSE, font.size="small",
  column.sep.width="0pt",
  title=paste0("Double hurdle model: marginal effects. (n = ", nrow(crop.wide.df), ")"),
  float.env = "sidewaystable",
  notes="Signif codes: *** = 0.1\\%; ** = 1\\%; * = 5\\%; . = 10\\%. Reference levels: Department - La Paz; Education - Primary; Credit - no loans.")


#nrow(crop.wide.df)

# sort(table(crop.wide.df$department))





m110d$formula








m110d.formula.simplified <- as.formula("fert.exp ~ department + indig.prop + indig.practices + hhh.literacy + hhh.age  + hhh.edu.measure.r + hhh.sex + REMPAIS + REMEXT + credit.source.r + drive.time.amanzanada + drive.time.urban + mean.ann.rain.5yr + elevation + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY" )
#as.formula(paste("fert.exp ~ ", "indig.prop + indig.practices   + hhh.edu.measure + hhh.sex + REMPAIS + REMEXT + credit.source + drive.time.amanzanada + mean.ann.rain.5yr  ", " + ", "indig.prop + indig.practices   + REMPAIS + REMEXT + mean.ann.rain.5yr  + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY  "))


data.for.summary<- as.data.frame(model.matrix(m110d.formula.simplified,  model.frame(m110d.formula.simplified, crop.wide.df) ))

data.for.summary$fert.exp <- crop.wide.df$fert.exp

#mex.df$regionNW <- ifelse(mex.df$region==1, 1, 0)
#mex.df$regionCN <- ifelse(mex.df$region==2, 1, 0)
#mex.df$regionCS <- ifelse(mex.df$region==3, 1, 0)
#mex.df$regionS  <- ifelse(mex.df$region==4, 1, 0)

#mex.df$region.factor <- factor(mex.df$region , levels=1:4, labels=c("NW", "CN", "CS", "S"))

#data.for.summary

#mex.df[, c("area", "pctpd0306", "dcarr", "avgelev", "avgslope", "ejido", "regionNW", "regionCN", "regionCS", "regionS", "psemidecid", "pselva")]

unprocessed.summary.table <- by(data.for.summary, INDICES=factor(rep(1, nrow(data.for.summary))), 
  FUN=function(x) {
    cbind(
      sapply(x, FUN=mean),
      sapply(x, FUN=median),
      sapply(x, FUN=sd),
      sapply(x, FUN=min),
      sapply(x, FUN=max)
      )
  } 
)

unprocessed.summary.table <- unprocessed.summary.table[[1]]


colnames(unprocessed.summary.table) <- c("Mean", "Median", "Std dev", "Min", "Max")

unprocessed.summary.table<- unprocessed.summary.table[-1, ]
# eliminating the "intercept" row

save(crop.wide.df, dhurdle.mfx.bootstrapped, unprocessed.summary.table, file="/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/double hurdle.Rdata")



stargazer(unprocessed.summary.table , summary=FALSE, out.header = FALSE, out=paste0(work.dir, "tex building/hurdlesummary.tex"),  
  rownames=TRUE,
  title="Summary statistics of MECOVI data", align=TRUE,
  notes="Drive time in hours. Rainfall in meters.  Remittances in hundreds of Bolivianos.")
  
 #   column.separate=c(4,4), column.labels =c("test1", "test2"), float.env = "sidewaystable", font.size="small", title="Summary statistics of treatment and control groups"
 #   )


#stargazer(processed.summary.table , summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/table1.tex", 
#  rownames=TRUE,  column.separate=c(4,4), column.labels =c("test1", "test2"), float.env = "sidewaystable", font.size="small", title="Summary statistics of treatment and control groups"
    )





# NOTE: here is where I replace the variable names
source("/Users/travismcarthur/git/misc/authored-functions/authored-R-fns.R")

replacement.var.names <- read.csv("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/tex building/MECOVI var name replacements.csv", stringsAsFactors=FALSE)

replace.vars(replacement.matrix=replacement.var.names, 
  directory="/Users/travismcarthur/Desktop/Metrics (637)/Final paper/tex building/", 
  file.pattern=".tex", table.only=TRUE)


























































  
elevation
mean.ann.rain.5yr

# install.packages("mhurdle")
library("mhurdle")



# Took this out: paste0(names(livestock.wide.df)[-1],





  
fmla <- as.formula(paste("fert.exp ~ ", expl.vars, " | ", expl.vars, " | 0" ) )



expl.vars <- paste( paste0(names(crop.wide.df)[grepl("area.r", names(crop.wide.df))], collapse= " + "), 	
	"+ indig.prop + seed.exp + credit.source + department + hhh.edu.measure + hhh.sex + REMPAIS + REMEXT + num.pers.agropecuaria + AWC.CLASS + T.GRAVEL + T.SILT + T.CLAY + T.BULK.DENSITY + T.OC + T.PH.H2O + T.CEC.CLAY + T.CEC.SOIL + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + elevation + drive.time.amanzanada + drive.time.urban + mean.ann.rain.5yr") 
	
expl.vars <- paste( paste0(names(crop.wide.df)[grepl("area.r", names(crop.wide.df))], collapse= " + ")) 


expl.vars <- paste( "indig.prop + seed.exp + credit.source + department + hhh.edu.measure + hhh.sex + REMPAIS + REMEXT + num.pers.agropecuaria + AWC.CLASS + T.GRAVEL + T.SILT + T.CLAY + T.BULK.DENSITY + T.OC + T.PH.H2O + T.CEC.CLAY + T.CEC.SOIL + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + elevation + drive.time.amanzanada + drive.time.urban + mean.ann.rain.5yr") 


expl.vars.1 <- paste( "indig.prop + seed.exp + credit.source + hhh.edu.measure + hhh.sex + REMPAIS + REMEXT + num.pers.agropecuaria  ")

# + department 

expl.vars.2 <- paste( "AWC.CLASS + T.GRAVEL + T.SILT + T.CLAY + elevation + drive.time.amanzanada + drive.time.urban + mean.ann.rain.5yr") 

# + T.BULK.DENSITY + T.OC + T.PH.H2O + T.CEC.CLAY + T.CEC.SOIL + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE +
 
# + drive.time.urban


# + AWC.CLASS + T.GRAVEL + T.SILT + T.CLAY + T.OC + T.CEC.CLAY + T.CEC.SOIL + drive.time.urban

m110d <- mhurdle(as.formula(paste("fert.exp ~ ", "indig.prop + indig.practices   + hhh.edu.measure + hhh.sex + REMPAIS + REMEXT + credit.source + drive.time.amanzanada + mean.ann.rain.5yr + elevation ", " | ", "indig.prop + indig.practices   + REMPAIS + REMEXT + mean.ann.rain.5yr + elevation + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE  ", " | 0" ) ), 
  data = crop.wide.df, weights=crop.wide.df$FACTOR,  corr = "d", dist = "ln", method =  "BFGS",  print.level=2)
  # "ln"   T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE
  # + AWC.CLASS + T.GRAVEL + T.SILT + T.CLAY + T.OC + T.CEC.CLAY + T.CEC.SOIL
  # weights=FACTOR,
  # 'bfgs'
  # Ok, the weights argument doesnt actually do anything.

summary(m110d)


m110d <- mhurdle(as.formula(paste("fert.exp ~ ", "indig.prop + hhh.edu.measure + hhh.sex + credit.source + drive.time.amanzanada + mean.ann.rain.5yr + elevation ", " | ", "indig.prop  +  mean.ann.rain.5yr + elevation   ", " | 0" ) ), 
  data = crop.wide.df, corr = "d", dist = "ln", method = 'bfgs' )
  # "ln" "tn"
  
library(effects)

plot(allEffects(m110d), ask = FALSE, rescale.axis = FALSE)

∂F/∂x = φ(x) Φ(y; ρx, sqrt(1-ρ^2))
∂F/∂y = φ(y) Φ(x; ρy, sqrt(1-ρ^2))

hurdle.mfx.cont <- function(model, var.name) {
  rho <- model$coefficients["corr12"]
  targ.var <- 


}

predict(m110d, newdata=as.data.frame(lapply(crop.wide.df, FUN=mean)) )


# install.packages("mfx")
library(mfx)

targ.formula <- m110d$formula

<- 




m110d <- mhurdle(as.formula(paste("fert.exp ~ ", "indig.prop + indig.practices   + hhh.edu.measure + hhh.sex + REMPAIS + REMEXT + credit.source + drive.time.amanzanada + mean.ann.rain.5yr + elevation ", " | ", "indig.prop + indig.practices   + REMPAIS + REMEXT + mean.ann.rain.5yr + elevation + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE  ", " | 0" ) ), 
  data = crop.wide.df, weights=crop.wide.df$FACTOR,  corr = "d", dist = "ln", method =  "BFGS",  print.level=2)


model.char <- as.character(m110d$formula[[3]])[[2]]


for ( i in factors.to.convert ) {

  model.char <- gsub(i, paste0(paste0(i, gsub("( )|([()])", ".", levels(crop.wide.df[, i]))), collapse=" + "), model.char)

}

as.formula( paste0("fert.exp ~ ", model.char, " | 0"))



test.frame <- model.matrix(m110d$formula,  model.frame(m110d$formula, crop.wide.df) )

test.frame <-as.data.frame(t(apply(test.frame, 2,  FUN=mean)) )




test.frame <-as.data.frame(t(apply(crop.wide.df, 2,  FUN=mean)) )


 i <- "credit.source"

# crop.wide.df[, i] <- relevel(crop.wide.df[, i], levels(crop.wide.df[, i])[2])

factors.to.convert <- c( "hhh.edu.measure", "hhh.sex", "credit.source")

for ( i in factors.to.convert) {
  crop.wide.df[, i]<- factor(make.names(crop.wide.df[, i]) )
}

m110d <- mhurdle(as.formula(paste("fert.exp ~ ", "credit.source   ", " | ", " elevation + AWC.CLASS   ", " | 0" ) ), 
  data = crop.wide.df, corr = "d", dist = "ln", method =  "BFGS",  print.level=2)

#  weights=crop.wide.df$FACTOR, 









m110d <- mhurdle(as.formula(paste("fert.exp ~ ", "indig.prop + indig.practices   + hhh.edu.measure + hhh.sex + REMPAIS + REMEXT + credit.source + drive.time.amanzanada + mean.ann.rain.5yr + elevation ", " | ", "indig.prop + indig.practices   + REMPAIS + REMEXT + mean.ann.rain.5yr + elevation + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE  ", " | 0" ) ), 
  data = crop.wide.df, weights=crop.wide.df$FACTOR, corr = "i" , dist = "ln", method =  "BFGS",  print.level=2)
# corr = "d"


test.frame <-as.data.frame(lapply(model.frame(m110d$formula, crop.wide.df),  FUN=mean)) 


#test.frame <-as.data.frame(lapply(model.frame(m110d$formula, crop.wide.df),  FUN=median))
# Ok, try median 

#test.frame$fert.exp <- mean(crop.wide.df$fert.exp)

factors.to.convert <- c( "hhh.edu.measure", "hhh.sex", "credit.source")

for ( i in factors.to.convert ) {
  test.frame[, i]<- mode.factor(crop.wide.df[, i])
}


# TODO: tell the mhurdle authors about this make.names issue for factors

mean.pred <- predict(m110d, newdata=  test.frame)

epsilon.dist <- 1e-6

# test.frame[, !sapply(test.frame, FUN=is.factor)] <- test.frame[, !sapply(test.frame, FUN=is.factor)] + epsilon.dist 

# test.frame[, "elevation"] <- test.frame[, "elevation"] + epsilon.dist 
 test.frame[, "indig.prop"] <- test.frame[, "indig.prop"] + epsilon.dist 
#test.frame[, "T.ESP"] <- test.frame[, "T.ESP"] + epsilon.dist 

epsilon.pred <- predict(m110d, newdata=  test.frame)

pred.deriv <- (epsilon.pred - mean.pred) / epsilon.dist
pred.deriv


lapply(model.frame(m110d$formula, crop.wide.df), sd)

predict(m110d, newdata=  rbind( test.frame, model.frame(m110d$formula, crop.wide.df)))   #, type="terms") 


predict(m110d, newdata=   model.frame(m110d$formula, crop.wide.df))

predict(m110d, newdata=   crop.wide.df)


sort(prop.table(table(crop.wide.df$hhh.edu.measure)))








mode.factor <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
# thanks to http://stackoverflow.com/questions/2547402/standard-library-function-in-r-for-finding-the-mode


factors.to.convert <- c( "hhh.edu.measure", "hhh.sex", "credit.source")

for ( i in factors.to.convert) {
  crop.wide.df[, i] <- relevel(crop.wide.df[, i], mode.factor(crop.wide.df[, i]))
}




mfx.bootstrap(crop.wide.df) 

m110d <- mhurdle(as.formula(paste("fert.exp ~ ", "indig.prop + indig.practices   + hhh.edu.measure + hhh.sex + REMPAIS + REMEXT + credit.source + drive.time.amanzanada + mean.ann.rain.5yr + elevation ", " | ", "indig.prop + indig.practices   + REMPAIS + REMEXT + mean.ann.rain.5yr + elevation + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE  ", " | 0" ) ), 
  data = crop.wide.df, weights=crop.wide.df$FACTOR, corr = "i" , dist = "ln", method =  "BFGS",  print.level=2)

summary(m110d)

as.data.frame(summary(m110d))

stargazer(summary(m110d), summary=FALSE, out.header = FALSE

d.hurdle.coef <- as.data.frame(summary(m110d)$CoefTable)

d.hurdle.coef$stars <- ""
d.hurdle.coef$stars[d.hurdle.coef[, "Pr(>|t|)"]<.05] <- "*" 
d.hurdle.coef$stars[d.hurdle.coef[, "Pr(>|t|)"]<.01] <- "**"
d.hurdle.coef$stars[d.hurdle.coef[, "Pr(>|t|)"]<.001] <- "***"

#0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

stargazer( d.hurdle.coef, summary=FALSE, out.header = FALSE, table.layout="a")

coef(m110d)

class(m110d) <- class(m110d)[2]
class(m110d) <- "lm"

<- coef(m110d), 

 summary(m110d)$CoefTable


, out="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/table13b.tex", 
  rownames=TRUE,
  title="Test of unconfoundedness: Tobit marginal effects",
  notes=c(paste0("Number of observations: ", unconfound.tobit$nObs[1]),
    paste0("Mean of dependent variable: ", round(mean(mex.df[mex.df$mah.matched.control ==1, "pctpd0306"]), 2))
    )
    )



# ok, we are taking out elevation + mean.ann.rain.5yr


mfx.bootstrap <- function(crop.wide.df) {

m110d <- mhurdle(as.formula(paste("fert.exp ~ ", "indig.prop + indig.practices   + hhh.edu.measure + hhh.sex + REMPAIS + REMEXT + credit.source + drive.time.amanzanada + mean.ann.rain.5yr + elevation ", " | ", "indig.prop + indig.practices   + REMPAIS + REMEXT + mean.ann.rain.5yr + elevation + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE  ", " | 0" ) ), 
  data = crop.wide.df, weights=crop.wide.df$FACTOR, corr = "i" , dist = "ln", method =  "BFGS",  print.level=2)

test.frame <-as.data.frame(lapply(model.frame(m110d$formula, crop.wide.df),  FUN=mean)) 

factors.to.convert <- c( "hhh.edu.measure", "hhh.sex", "credit.source")


for ( i in factors.to.convert ) {
  test.frame[, i]<- mode.factor(crop.wide.df[, i])
  crop.wide.df[, i] <- factor(crop.wide.df[, i])
  # this gets rid of any unused levels
}
# TODO: tell the mhurdle authors about this make.names issue for factors
mean.pred <- predict(m110d, newdata = test.frame)
epsilon.dist <- 1e-6

estimate.v<- c()
length(estimate.v) <- length(coef(m110d))

estimate.v.names <-  c( names(test.frame),
  paste0(factors.to.convert[1], levels(crop.wide.df[, factors.to.convert[1]])[-1]),
  paste0(factors.to.convert[2], levels(crop.wide.df[, factors.to.convert[2]])[-1]),
  paste0(factors.to.convert[3], levels(crop.wide.df[, factors.to.convert[3]])[-1])
)

estimate.v.names <- setdiff(estimate.v.names, c("fert.exp", factors.to.convert))

estimate.v <- rep(0, length(estimate.v.names))

names(estimate.v) <- estimate.v.names

estimate.v.2 <- estimate.v

for ( i in setdiff(names(test.frame), "fert.exp")) {
  test.frame.epsilon <- test.frame
  if (is.factor(test.frame.epsilon[, i])) {
#    crop.wide.df[, i] <- factor(crop.wide.df[, i])
    for ( j in levels(crop.wide.df[, i])[-1]) {
      test.frame.epsilon[, i] <- factor(j, levels=levels(crop.wide.df[, i]))
      epsilon.pred <- predict(m110d, newdata = test.frame.epsilon)
      pred.deriv <- (epsilon.pred - mean.pred) 
      estimate.v[paste0(i, j)] <- pred.deriv[1,1]
      estimate.v.2[paste0(i, j)] <- pred.deriv[1,2]
    }
    next
  }
  
  test.frame.epsilon[, i] <- test.frame.epsilon[, i] + epsilon.dist 
  epsilon.pred <- predict(m110d, newdata = test.frame.epsilon)
  pred.deriv <- (epsilon.pred - mean.pred) / epsilon.dist
  estimate.v[i] <- pred.deriv[1,1]
  estimate.v.2[i] <- pred.deriv[1,2]
}
      
names(estimate.v) <- paste0("h1.", names(estimate.v) )
names(estimate.v.2) <- paste0("h2.", names(estimate.v.2) )

estimate.complete <- c(estimate.v, estimate.v.2)

estimate.complete <- estimate.complete[!sapply(as.list(estimate.complete), FUN=function(x) isTRUE(all.equal(x, 0)))]

as.data.frame(t(estimate.complete) )

}



dhurdle.mfx <- mfx.bootstrap(crop.wide.df)

dhurdle.mfx <- t(dhurdle.mfx)


# + drive.time.amanzanada 

# hhh.sex +  + REMPAIS + REMEXT + mean.ann.rain.5yr + elevation + AWC.CLASS + mean.ann.rain.5yr + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE  + elevation 

mm12 <- model.matrix(as.formula(paste("fert.exp ~ ", "indig.prop + indig.practices   + hhh.edu.measure  + credit.source "  ) ), data=crop.wide.df)
# + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE

mm12 <- model.matrix(as.formula(paste("fert.exp ~ ", " T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + elevation "  ) ), data=crop.wide.df)

det(cov(mm12[,-1]))
# Multicollinearity: http://stackoverflow.com/questions/3042117/screening-multicollinearity-in-a-regression-model

kappa(mm12)

# The problem is the elevation variable


replications <- 50

y <- crop.wide.df

bootstraps <- apply(matrix(
    sample(1:nrow(y), size=replications*nrow(y), replace=TRUE),nrow=replications),
    1, FUN=function(x) { tryCatch(mfx.bootstrap(y[x, ]), error = function(e) "error thrown") }
  )

table(sapply(bootstraps, FUN=function(x) length(x)==1) )


test.mfx <- mfx.bootstrap(crop.wide.df[sample(1:nrow(crop.wide.df), size=nrow(crop.wide.df), replace=TRUE),])




bootstraps <- bootstraps[!sapply(bootstraps, FUN=function(x) length(x)==1)]

library("plyr")

bootstraps.df <- do.call(rbind.fill, bootstraps)

lapply( bootstraps.df, FUN=function(x) quantile(x, probs=c(.05, .95), na.rm=TRUE))


  
  epsilon.pred <- predict(m110d, newdata=  test.frame)
  
  
 test.frame[, "indig.prop"] <- test.frame[, "indig.prop"] + epsilon.dist 
#test.frame[, "T.ESP"] <- test.frame[, "T.ESP"] + epsilon.dist 

epsilon.pred <- predict(m110d, newdata=  test.frame)

pred.deriv <- (epsilon.pred - mean.pred) / epsilon.dist
pred.deriv






































 sc2.f <- function(x){
    n <- length(x)
    sum((1:n) * (exp(x) - x)) / n
    }

  x0 <- rnorm(5)
  x0 <-factor("yes", "no")
  x0 <-data.frame(2, 1)
  hess <- hessian(func=sc2.f, x=x0)



x.s, beta.s, x.s.factors

input.args <- c( unlist(test.frame), coef(m110d))

test.hessian<- hessian(func=test.function, x=input.args, 
  x.s=unlist(test.frame), beta.s=coef(m110d), 
  x.s.factors=sapply(test.frame, FUN=is.factor), model=m110d)

image(test.hessian)
image(test.hessian^.1)
table(c(test.hessian) !=0)

test.function <- function(arg, x.s, beta.s, x.s.factors, model) {
  
  which.args<- which(arg!=c(x.s, beta.s))
  cat( which.args, names(arg), "\n")
  
  if (length(which.args)==0) return( predict(model, newdata = data.frame(t(x.s)))[1, 1] )
  
  arg <- arg[which.args]
  
  cat( which.args, names(arg), "\n")
  
  which.args.x.s <- setdiff(intersect(which.args, 1:length(x.s)), which(x.s.factors))
  
  x.s[which.args.x.s] <- arg[which.args.x.s]
  
  which.args.beta.s <- intersect(which.args, (length(x.s)+1):(length(x.s)+length(beta.s)))
  
  beta.s[which.args.beta.s] <- arg[which.args.beta.s]
  
  model$coef <- beta.s
  ret <- predict(model, newdata = data.frame(t(x.s)))[1, 1]
  ret
}

complement

setdiff(1:6, c(2,4))


 if (names(arg) %in% names(x.s) ) {
    if (! x.s.factors[names(arg)]) {
    x.s[names(arg)] <- arg
    }
  }
  if (names(arg) %in% names(beta.s) ) {
    beta.s[names(arg)] <- arg
  }






data.frame(t(unlist(test.frame)))



# test.frame[, !sapply(test.frame, FUN=is.factor)] <- test.frame[, !sapply(test.frame, FUN=is.factor)] + epsilon.dist 

# test.frame[, "elevation"] <- test.frame[, "elevation"] + epsilon.dist 
 test.frame[, "indig.prop"] <- test.frame[, "indig.prop"] + epsilon.dist 
#test.frame[, "T.ESP"] <- test.frame[, "T.ESP"] + epsilon.dist 

epsilon.pred <- predict(m110d, newdata=  test.frame)
    
    
    
    
    
    
  model

predict(m110d, newdata=   crop.wide.df)






 library("numDeriv")


















Testing theirs

data("Comics", package = "mhurdle")

Comics$incu <- with(Comics, income / cu)
Comics$incum <- with(Comics, incu / mean(incu))



m110d <- mhurdle(comics ~ gender + educ + age + area | log(incum) +
 I(log(incum)^2) + I(log(incum)^3) + size | 0,
 data = Comics, corr = "d", dist = "ln", method = 'bfgs')
#  dist = "n"

predict(m110d,
 newdata = data.frame(
 area= c("rural", "small", "medium"),
 comics = c(0, 1, 2),
 gender = c("female", "female", "male"),
 age = c(20, 18, 32),
 educ = c(10, 20, 5),
 incum = c(4, 8, 2),
 size = c(2, 1, 3)))




















m110.posi.resids <- (crop.wide.df$fert.exp - m110d$fitted.value[, "E(y|y>0)"])[crop.wide.df$fert.exp>0 & crop.wide.df$fert.exp<5000]

m110.posi.resids <- (crop.wide.df$fert.exp - m110d$fitted.value[, "E(y|y>0)"])[crop.wide.df$fert.exp<5000]

m110.posi.resids <- m110.posi.resids[is.finite(m110.posi.resids)]

ks.test(jitter(m110.posi.resids), "pnorm", mean(m110.posi.resids), sd(m110.posi.resids))


ks.test(jitter(m110.posi.resids), plnorm, log(mean(m110.posi.resids)), log(sd(m110.posi.resids)))


m110.posi.resids.binary <- (crop.wide.df$fert.exp - m110d$fitted.value[, "P(y=0)"])[crop.wide.df$fert.exp<5000]


m110d.alt <- mhurdle(as.formula(paste("fert.exp ~ ", "indig.prop + hhh.edu.measure + hhh.sex + credit.source + drive.time.amanzanada + mean.ann.rain.5yr  ", " | ", "indig.prop  +  mean.ann.rain.5yr + elevation   ", " | 0" ) ), 
  data = crop.wide.df, corr = "d", dist = "tn", method = 'bfgs' )
  
summary(m110d.alt )

vuongtest(m110d, m110d.alt)


vuongtest( mhurdle(as.formula(paste("fert.exp ~ ", "indig.prop + hhh.edu.measure + hhh.sex + credit.source + drive.time.amanzanada + mean.ann.rain.5yr + elevation ", " | ", "indig.prop  +  mean.ann.rain.5yr + elevation   ", " | 0" ) ), 
  data = crop.wide.df, corr = "i", dist = "ln", method = 'bfgs' ),
  mhurdle(as.formula(paste("fert.exp ~ ", "indig.prop + hhh.edu.measure + hhh.sex + credit.source + drive.time.amanzanada + mean.ann.rain.5yr + elevation ", " | ", "indig.prop  +  mean.ann.rain.5yr + elevation   ", " | 0" ) ), 
  data = crop.wide.df, corr = "d", dist = "ln", method = 'bfgs' ), type="nested"
)
  # "ln" "tn"
  
  











plot(density(log(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0])))

ks.test(jitter(log(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0])), "pnorm", 
  mean(log(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0])), 
  sd(log(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0])) )
  
  
  ks.test(jitter(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0]), plnorm, 
  mean(log(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0])), 
  sd(log(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0])) )


  ks.test(jitter(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0]), plnorm, 
  mean(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0]), 
  sd(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0]) )

ks.test( ( jitter(log(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0])) -
 mean(log(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0])) ) /
 sd(log(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0])) 
, "pnorm"
)


ks.test( ( jitter((crop.wide.df$fert.exp[crop.wide.df$fert.exp>0])) -
 mean((crop.wide.df$fert.exp[crop.wide.df$fert.exp>0])) ) /
 sd((crop.wide.df$fert.exp[crop.wide.df$fert.exp>0])) 
, "pnorm"
)

ks.test( (fitdistr(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0], "normal")

# install.packages("truncnorm")
library("truncnorm")

dtruncnorm.fit <- fitdistr(x=crop.wide.df$fert.exp[crop.wide.df$fert.exp>0], densfun = dtruncnorm,
start=list(mean=-200,
sd=sd(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0]) * 5), a=0, method="Nelder-Mead", control=list(trace=36, maxit=1000) )


ks.test(jitter(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0]), ptruncnorm, 
  a=0,
  mean=dtruncnorm.fit$estimate[1], 
  sd=dtruncnorm.fit$estimate[2] )


llog.fit <- fitdistr(x=crop.wide.df$fert.exp[crop.wide.df$fert.exp>0], densfun = dllogis,
start=list(shape=2, rate = 1),  method="BFGS", control=list(trace=36, maxit=10000) )


ks.test(jitter(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0]), pllogis, 
  llog.fit$estimate[1], 
  llog.fit$estimate[2] )





plot(dcauchy(seq(from=0, to=max(crop.wide.df$fert.exp), by=.1), location=cauchy.fit$estimate[1],   scale=cauchy.fit$estimate[2] ))


cauchy.fit <- fitdistr(x=log(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0]), densfun = dcauchy,
start=list(location=median(log(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0])), 
  scale = 5),  method="BFGS", control=list(trace=36, maxit=10000) )


ks.test(jitter(log(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0])), pcauchy, 
  location=cauchy.fit$estimate[1], 
  scale=cauchy.fit$estimate[2] )


plot(ecdf(log(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0])))

lines(seq(from=0, to=max(log(crop.wide.df$fert.exp)), by=.1), 
 pcauchy(seq(from=0, to=max(log(crop.wide.df$fert.exp)), by=.1), location=cauchy.fit$estimate[1],   scale=cauchy.fit$estimate[2] ), col="red")
 
 
 
lnorm.fit <- fitdistr(x=crop.wide.df$fert.exp[crop.wide.df$fert.exp>0], densfun = dlnorm,
start=list(meanlog=log(mean(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0])), 
  sdlog=log(sd(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0]))), 
   method="BFGS", control=list(trace=36, maxit=10000) )

ks.test(jitter(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0]), plnorm, 
  lnorm.fit$estimate[1], 
  lnorm.fit$estimate[2] )

plot(ecdf(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0]), xlim=c(0,1000))

lines(seq(from=0, to=max(crop.wide.df$fert.exp), by=.1), 
 plnorm(seq(from=0, to=max(crop.wide.df$fert.exp), by=.1), 
 lnorm.fit$estimate[1], 
  lnorm.fit$estimate[2] ), col="red")



plot(ecdf(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0]), xlim=c(0,1000))

lines(exp(seq(from=0, to=max(log(crop.wide.df$fert.exp)), by=.1)), 
 pcauchy(seq(from=0, to=max(log(crop.wide.df$fert.exp)), by=.1), location=cauchy.fit$estimate[1],   scale=cauchy.fit$estimate[2]) , col="red")






plot(ecdf(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0]), xlim=c(0,1000))

lines(seq(from=0, to=1000, by=1), 
 cumsum(dtruncnorm(seq(from=0, to=1000, by=1), a=0, mean=dtnorm.fit$estimate[1],   sd=dtnorm.fit$estimate[2]) ), col="red")
 
plot(ecdf(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0]), xlim=c(0,1000))

lines(seq(from=0, to=1000, by=1), 
 exp(ptnorm(seq(from=0, to=1000, by=1), lower=0, mean=dtnorm.fit$estimate[1],   sd=dtnorm.fit$estimate[2], log.p = TRUE)) , col="red")



lines(seq(from=0, to=1000, by=1), 
 exp(ptruncnorm(seq(from=0, to=1000, by=1), a=0, mean=dtnorm.fit$estimate[1],   sd=dtnorm.fit$estimate[2], log.p = TRUE)) , col="red")
 
 
 pnorm(seq(from=0, to=1000, by=1), mean=dtnorm.fit$estimate[1],   sd=dtnorm.fit$estimate[2])
 
 
lines(seq(from=0, to=1000, by=.1), 
 cumsum(dtnorm(seq(from=0, to=1000, by=.1), lower=0, mean=dtnorm.fit$estimate[1],   sd=dtnorm.fit$estimate[2]) ), col="red")
 
plot(seq(from=0, to=1000, by=1), 
 cumsum(dtnorm(seq(from=0, to=1000, by=1), lower=0, mean=dtnorm.fit$estimate[1],   sd=dtnorm.fit$estimate[2]) ), col="red")

#install.packages("msm")
library("msm")

rtnorm(1,mean=120,low=0,upp=1)




dtnorm.fit <- fitdistr(x=crop.wide.df$fert.exp[crop.wide.df$fert.exp>0], densfun = dtnorm.2,
start=list(mean=-200,
sd=sd(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0]) * 5), lower.a=0, method="Nelder-Mead", control=list(trace=36, maxit=1000) )



# install.packages("tmvtnorm")
library("tmvtnorm")

mle.fit1 <- mle.tmvnorm(as.matrix(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0], ncol=1), lower=0, upper=Inf, start=list(mu=-200,
sigma=sd(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0]) * 5))


dtmvt(x, mean = mle.fit1@coef[1], sigma = mle.fit1@coef[2])

plot(ecdf(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0]), xlim=c(0,1000))

lines(seq(from=0, to=1000, by=1), 
 dtmvt(seq(from=0, to=1000, by=1), mean = mle.fit1@coef[1], sigma = matrix(mle.fit1@coef[2]))
 plnorm(, 
 lnorm.fit$estimate[1], 
  lnorm.fit$estimate[2] ), col="red")



ptmvnorm.marginal(seq(from=0, to=1000, by=1), mean = mle.fit1@coef[1], sigma = matrix(mle.fit1@coef[2]),
lower=0)


ks.test(jitter(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0]), ptmvnorm.marginal, 
  mean = mle.fit1@coef[1], sigma = matrix(mle.fit1@coef[2]),
lower=0)



plot(ecdf(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0]), xlim=c(0,1000))

lines(seq(from=0, to=1000, by=1), 
 ptmvnorm.marginal(seq(from=0, to=1000, by=1), mean = mle.fit1@coef[1], sigma = matrix(mle.fit1@coef[2]), lower=0, upper=Inf)
, col="red")





dtnorm.2<- function (x, mean = 0, sd = 1, lower.a = -Inf, upper = Inf, log = FALSE) 
{
lower <- lower.a
    ret <- numeric(length(x))
    ret[x < lower | x > upper] <- if (log) 
        -Inf
    else 0
    ret[upper < lower] <- NaN
    ind <- x >= lower & x <= upper
    if (any(ind)) {
        denom <- pnorm(upper, mean, sd) - pnorm(lower, mean, 
            sd)
        xtmp <- dnorm(x, mean, sd, log)
        if (log) 
            xtmp <- xtmp - log(denom)
        else xtmp <- xtmp/denom
        ret[x >= lower & x <= upper] <- xtmp[ind]
    }
    ret
}
















mean(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0])

<- density(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0])[[2]][2]


 ,


If a standard normal distribution is left-truncated at k then the
mean becomes m = f/Q, where f is the density at k and Q is the
area to the right of k, and the variance becomes s^2 = 1 - m(m-k).

For an aribitrary normal distribution, with mean M, variance S^2,
and truncation point K, get m and s as above using k = (K-M)/S.
To get back to the original units, use m -> m*S + M and s -> s*S.






(



library(MASS)
?fitdistr
You can define your customized truncated density as a function in the parameter densfun of fitdistr.



ks.test(  jitter(log(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0])) 
, "pnorm"
)


ks.test(  jitter(log(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0])) 
, "plnorm"
)


m110d$rho
  
  m110d <- mhurdle(as.formula(paste("fert.exp ~ ", expl.vars.1, " | ", expl.vars.2, " | 0" ) ), 
  data = crop.wide.df, corr = "d", dist = "ln", method = 'bfgs')

data("Comics", package = "mhurdle")


m110d <- mhurdle(comics ~ gender + educ + age | log(incum) +
  I(log(incum)^2) + I(log(incum)^3) + size | 0,
  data = Comics, corr = "d", dist = "n", method = 'bfgs')

m110d <- mhurdle(comics ~ gender + educ + age | log(income) +
  I(log(income)^2) + I(log(income)^3) + size | 0,
  data = Comics, corr = "d", dist = "n", method = 'bfgs')

  
# comics ~ gender + educ + age | log(incum) +
#+ I(log(incum)^2) + I(log(incum)^3) + size | 0
table(sapply(crop.wide.df, FUN=function(x) all(!is.finite(x) & !is.na(x)))

summary(lm(as.formula(paste("fert.exp ~ ", expl.vars)), data=crop.wide.df))

summary(crop.wide.df)






"hhh.literacy"                            "hhh.age"


m110d <- mhurdle(as.formula(paste("fert.exp ~ ", " indig.prop + indig.practices + hhh.literacy + hhh.age  + hhh.edu.measure + hhh.sex + REMPAIS + REMEXT + credit.source + drive.time.amanzanada + drive.time.urban + mean.ann.rain.5yr  ", " | ", "indig.prop + indig.practices   + REMPAIS + REMEXT + mean.ann.rain.5yr  + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY  ", " | 0" ) ), 
  data = crop.wide.df, weights=crop.wide.df$FACTOR, corr = "d" , dist = "ln", method =  "BFGS" ,  print.level=2)

summary(m110d)
  


m110d <- mhurdle(as.formula(paste("fert.exp ~ ", "indig.prop + indig.practices   + hhh.edu.measure + hhh.sex + REMPAIS + REMEXT + credit.source + drive.time.amanzanada + mean.ann.rain.5yr  ", " | ", "indig.prop + indig.practices   + REMPAIS + REMEXT + mean.ann.rain.5yr  + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY  ", " | 0" ) ), 
  data = crop.wide.df, weights=crop.wide.df$FACTOR, corr = "d" , dist = "ln", method =  "BFGS" ,  print.level=2)




  tcnico.de.universidad                         2
  educacin.bsica.de.adultos..eba.               3
  curso.de.alfabetizacin                        4
  centro.de.educacin.media.de.adultos..cema.    6
  tcnico.de.instituto                           8
  universidad..licenciatura.                   12
  normal                                       30
  secundaria..1.a.4.aos.                       34
  medio..1.a.4.aos.                           118
  intermedio..1.a.3.aos.                      202
  ninguno                                     341
  primaria..1.a.8.aos.                        368
  bsico..1.a.5.aos.                           620


Proposed education groupings:

ninguno

tcnico.de.universidad
tcnico.de.instituto
universidad..licenciatura.


educacin.bsica.de.adultos..eba.
curso.de.alfabetizacin
primaria..1.a.8.aos.
bsico..1.a.5.aos.


normal
secundaria..1.a.4.aos.
medio..1.a.4.aos.
intermedio..1.a.3.aos.
centro.de.educacin.media.de.adultos..cema.

crop.wide.df$hhh.edu.measure.r <- ""
crop.wide.df$hhh.edu.measure.r[crop.wide.df$hhh.edu.measure %in% "ninguno"] <- "None"
crop.wide.df$hhh.edu.measure.r[crop.wide.df$hhh.edu.measure %in% 
  c("educacin.bsica.de.adultos..eba.", "curso.de.alfabetizacin", 
  "primaria..1.a.8.aos.", "bsico..1.a.5.aos.")] <- "Primary"
crop.wide.df$hhh.edu.measure.r[crop.wide.df$hhh.edu.measure %in% 
  c("normal", "secundaria..1.a.4.aos.", "secundaria..1.a.4.aos.", 
  "medio..1.a.4.aos.", "intermedio..1.a.3.aos.", 
  "centro.de.educacin.media.de.adultos..cema.")] <- "Secondary"
crop.wide.df$hhh.edu.measure.r[crop.wide.df$hhh.edu.measure %in% 
  c("tcnico.de.universidad", "tcnico.de.instituto", "universidad..licenciatura.")] <- "Tertiary"

crop.wide.df$hhh.edu.measure.r <- factor(crop.wide.df$hhh.edu.measure.r)



  Prestamo.de.algna.Cooperativa.de.ahorro      9
  Prestamo.de.algn.Fondo.financiero.Privado   21
  Prestamo.de.familiares.o.amigos             24
  Prestamo.de.algn.banco.comercial            51
  No.Credit                                 1643


# Let's group Prestamo.de.algna.Cooperativa.de.ahorro with Prestamo.de.algn.Fondo.financiero.Privado


tryCatch(.Fortran("PBIVNORM", prob=.5, lower=0, uppera=2, upperb=.Machine$double.xmax, infin=TRUE, correl=.1, 
       lt=.2, PACKAGE = "pbivnorm"), error = function(e) "error test")
       
summary(glm("I(manure.exp>0) ~ department + indig.prop*hhh.edu.measure.r + indig.practices + hhh.literacy + hhh.age  + hhh.sex + REMPAIS + REMEXT + credit.source.r + drive.time.amanzanada + drive.time.urban + mean.ann.rain.5yr + elevation + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY", data=crop.wide.df, family=binomial(link = "logit")))


table(crop.wide.df$manure.exp>0, crop.wide.df$department)

table(crop.wide.df$manure.exp>0, crop.wide.df$AWC.CLASS)


for ( i in outcome.vars[-c(3,6,7,8,9)]) {


print(paste0(i, "\n"))

main.specification <- as.formula(paste(i, " ~ ", "department + indig.prop*hhh.edu.measure.r + indig.practices + hhh.literacy + hhh.age  + hhh.sex + REMPAIS + REMEXT + credit.source.r + drive.time.amanzanada + drive.time.urban + mean.ann.rain.5yr + elevation + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY  ", " | ", "department + indig.prop + indig.practices + hhh.age  + REMPAIS + REMEXT +  mean.ann.rain.5yr + elevation + hhh.edu.measure.r + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY  ", " | 0" ) )
# Note that we are not including interaction term indig.prop*hhh.edu.measure.r in
# the second hurdle since I wonder if we will have enough observations for the bootstrap

  
m110d <- mhurdle(main.specification, 
  data = crop.wide.df, weights=crop.wide.df$FACTOR, corr = "d" , dist = "ln", method =  "BFGS" ,  print.level=2)






}



  
drop.unused.levels = FALSE



# [, !apply(X2, 2, FUN=function(x) all(x[y>0]==0))]

my.fit.simple.mhurdle <- function (X1, X2, y, dist = NULL) 
{
    probit <- glm(y != 0 ~ X1 - 1, family = binomial(link = "probit"))
    lin <- switch(dist, ln = lm(log(y) ~ X2 - 1,  subset = y != 0), n = lm(y ~ X2 - 1, subset = y != 0), tn = truncreg(y ~ 
        X2 - 1, subset = y != 0))
        
    lin$coefficients[is.na(lin$coefficients)] <- 0
    df <- df.residual(lin)
    np <- sum(y != 0)
    K1 <- ncol(X1)
    beta1 <- coef(probit)
    bX1 <- as.numeric(crossprod(beta1, t(X1)))
    K2 <- ncol(X2)
    if (dist == "tn") {
        sigma <- coef(lin)[ncol(X2) + 1]
        beta2 <- coef(lin)[-(ncol(X2) + 1)]
    }
    else beta2 <- coef(lin)
    bX2 <- as.numeric(crossprod(beta2, t(X2)))
    L.null <- (y == 0) * log(1 - pnorm(bX1))
    if (dist == "ln") {
        logy <- rep(0, length(y))
        logy[y != 0] <- log(y[y != 0])
        resid <- (logy - bX2)
    }
    else resid <- y - bX2
    scr <- sum(resid[y != 0]^2)
    if (dist != "tn") 
        sigma <- sqrt(scr/np)
    mills1 <- mhurdle:::mills(bX1)
    mills2 <- mhurdle:::mills(bX2/sigma)/pnorm(bX2/sigma)
    mills1m <- mhurdle:::mills(-bX1)
    L.pos <- switch(dist, ln = (y != 0) * (-logy + pnorm(bX1, 
        log.p = TRUE) + dnorm(resid/sigma, log = TRUE) - log(sigma)), 
        n = (y != 0) * (pnorm(bX1, log.p = TRUE) + dnorm(resid/sigma, 
            log = TRUE) - log(sigma)), tn = (y != 0) * (pnorm(bX1, 
            log.p = TRUE) + dnorm(resid/sigma, log = TRUE) - 
            log(sigma) - pnorm(bX2/sigma, log.p = TRUE)))
    gbX1 <- switch(dist, ln = (y == 0) * (-mills1m) + (y != 0) * 
        mills1, n = (y == 0) * (-mills1m) + (y != 0) * mills1, 
        tn = (y == 0) * (-mills1m) + (y != 0) * mills1)
    gbX2 <- switch(dist, ln = (y != 0) * (resid/sigma^2), n = (y != 
        0) * (resid/sigma^2), tn = (y != 0) * (resid/sigma^2 - 
        1/sigma * mills2))
    gsigma <- switch(dist, ln = (y != 0) * (resid^2/sigma^3 - 
        1/sigma), n = (y != 0) * (resid^2/sigma^3 - 1/sigma), 
        tn = (y != 0) * (resid^2/sigma^3 - 1/sigma + bX2/sigma^2 * 
            mills2))
    gradi <- cbind(gbX1 * X1, gbX2 * X2, as.numeric(gsigma))
    dss <- -3 * scr/sigma^4 + sum(y != 0)/sigma^2
    if (dist == "tn") {
        vcov <- mhurdle:::bdiag(vcov(probit), vcov(lin))
        coef <- c(coef(probit), coef(lin))
    }
    else {
        vcov <- mhurdle:::bdiag(vcov(probit), vcov(lin)/np * df, -1/dss)
        coef <- c(coef(probit), coef(lin), sigma)
    }
    fit <- cbind(zero = bX1, pos = bX2)
    other.coef <- c("sd")
    coef.names <- list(h1 = colnames(X1), h2 = colnames(X2), 
        sd = other.coef)
    fitted <- attr(mhurdle:::mhurdle.lnl(coef, X1 = X1, X2 = X2, X3 = NULL, 
        X4 = NULL, y = y, gradient = FALSE, fitted = TRUE, dist = dist, 
        corr = NULL), "fitted")
    logLik <- structure(sum(L.null + L.pos), df = length(coef), 
        nobs = length(y), class = "logLik")
    result <- list(coefficients = coef, vcov = vcov, fitted.values = fitted, 
        logLik = logLik, gradient = gradi, model = NULL, formula = NULL, 
        coef.names = coef.names, call = NULL)
    class(result) <- c("mhurdle", "maxLik")
    result
}




assignInNamespace( "fit.simple.mhurdle", my.fit.simple.mhurdle, "mhurdle")



# X2 <- model.matrix(formula, data = mf, rhs = 2)


my.mhurdle <- function (formula, data, subset, weights, na.action, start = NULL, 
    dist = c("ln", "tn", "n", "bc", "ihs"), corr = NULL, ...) 
{
    dots <- list(...)
    oldoptions <- options(warn = -1)
    on.exit(options(oldoptions))
    cl <- match.call()
    posT <- as.list(cl) == "T"
    posF <- as.list(cl) == "F"
    cl[posT] <- TRUE
    cl[posF] <- FALSE
    cl.save <- cl
    dist <- match.arg(dist)
    if (!inherits(formula, "Formula")) 
        formula <- Formula(formula)
    if (length(formula)[2] > 4) 
        stop("at most 4 rhs should be provided in the formula")
    mf <- match.call(expand.dots = FALSE)
    m <- match(c("formula", "data", "subset", "na.action", "weights"), 
        names(mf), 0L)
    mf <- mf[c(1L, m)]
    mf$drop.unused.levels <- TRUE
    mf[[1L]] <- as.name("model.frame")
    mf$formula <- formula
    mf <- eval(mf, parent.frame())
    mf.s <- droplevels(mf[mf[,1]>0,])
    X1 <- model.matrix(formula, data = mf, rhs = 1)
 #   X2 <- model.matrix(formula, data = mf.s, rhs = 2, drop.unused.levels=TRUE)
 X2 <- model.matrix(formula, data = mf, rhs = 2)
    X3 <- model.matrix(formula, data = mf, rhs = 3)
    y <- model.response(mf)
    n <- length(y)
    if (length(X1) == 0) 
        X1 <- NULL
    if (length(X3) == 0) 
        X3 <- NULL
    if (length(X2) == 0) 
        stop("the second hurdle (consumption equation) is mandatory")
    h1 <- !is.null(X1)
    h3 <- !is.null(X3)
    if (length(formula)[2] == 4) {
        X4 <- model.matrix(formula, data = mf, rhs = 4)
        if (length(X4) == 0) 
            X4 <- NULL
    }
    else X4 <- NULL
    if (!is.null(corr)) {
        if ((h1 + h3) == 2) {
            if (nchar(corr) != 3) 
                stop("corr should contain three characters")
            if (!(corr %in% c("dii", "iid"))) 
                stop("corr should currently be either dii or iid")
            if (corr == "dii") 
                corr <- "h1"
            if (corr == "iid") 
                corr <- "h3"
        }
        else {
            if (nchar(corr) != 1) 
                stop("corr should contain just one character")
            if (!(corr %in% c("i", "d"))) 
                stop("corr should be one of d or i")
            if (corr == "i") 
                corr <- NULL
            else {
                if (h1) 
                  corr <- "h1"
                if (h3) 
                  corr <- "h3"
            }
        }
    }
    Pnull <- mean(y == 0)
    if (dist != "ln") {
        Ec <- mean(y[y > 0])
        Vc <- var(y[y > 0])
    }
    else {
        Ec <- mean(log(y[y > 0]))
        Vc <- var(log(y[y > 0]))
    }
    start.naive <- c(rep(0.1, 1 + h1 + h3), 1)
    moments <- c(Pnull, Ec, Vc)
    dist.naive <- dist
    if (dist %in% c("bc", "ihs")) 
        dist.naive <- "n"
    naive <- maxLik(mhurdle:::lnl.naive, start = start.naive, dist = dist.naive, 
        moments = moments, h1 = h1, h3 = h3)
    coef.naive <- naive$est
    logLik.naive <- structure(naive$max * n, nobs = length(y), 
        df = length(coef.naive), class = "logLik")
    naive <- list(coefficients = coef.naive, logLik = logLik.naive, 
        code = naive$code)
    if (!h1 && !h3 && dist != "n") {
        if (dist == "ln") 
            result <- lm(log(y) ~ X2 - 1)
        if (dist == "tn") 
            result <- truncreg(y ~ X2 - 1)
        return(result)
    }
    if (h1 && !h3 && !(dist %in% c("n", "bc", "ihs")) && is.null(corr)) {
        result <- mhurdle:::fit.simple.mhurdle(X1, X2, y, dist = dist)
        result$naive <- naive
        result$call <- cl.save
        result$model <- mf
        result$formula <- formula
        return(result)
    }
    dist.start <- dist
    if (dist %in% c("bc", "ish")) 
        dist.start <- "n"
    if (is.null(start)) 
        start <- mhurdle:::start.mhurdle(X1, X2, X3, y, dist.start)
    if (!is.null(X4)) {
        sd.int.pos <- ifelse(h1, ncol(X1), 0) + ncol(X2) + ifelse(h3, 
            ncol(X3), 0) + 1
        sd.last.pos <- sd.int.pos - 1 + ncol(X4)
        start[sd.int.pos] <- log(start[sd.int.pos])
        start <- c(start[1:sd.int.pos], rep(0, ncol(X4) - 1))
    }
    if (!is.null(corr)) 
        start <- c(start, 0.1)
    if (dist == "bc") 
        start <- c(start, 1)
    if (dist == "ihs") 
        start <- c(start, 0.2)
    result <- mhurdle:::mhurdle.fit(start, X1, X2, X3, X4, y, gradient = TRUE, 
        fit = FALSE, dist = dist, corr = corr, ...)
    result$naive <- naive
    result$call <- cl.save
    result$formula <- formula
#    names(result$coefficients) <- colnames(result$vcov) <- rownames(result$vcov) <- mhurdle:::nm.mhurdle(result)
names(result$coefficients)  <- mhurdle:::nm.mhurdle(result)
    result$model <- mf
    result
}


my.mhurdle.fit <- function (start, X1, X2, X3, X4, y, gradient = FALSE, fit = FALSE, 
    dist = c("ln", "n", "tn", "bc", "ihs"), corr = NULL, ...) 
{
    start.time <- proc.time()
    f <- function(param) mhurdle:::mhurdle.lnl(param, X1 = X1, X2 = X2, 
        X3 = X3, X4 = X4, y = y, gradient = TRUE, fitted = FALSE, 
        dist = dist, corr = corr)
    check.gradient <- FALSE
    if (check.gradient) {
        ngrad <- c()
        oparam <- start
        fo <- f(start)
        agrad <- apply(attr(fo, "gradient"), 2, sum)
        eps <- 1e-05
        for (i in 1:length(start)) {
            oparam[i] <- oparam[i] + eps
            ngrad <- c(ngrad, sum((as.numeric(f(oparam)) - fo)/eps))
            oparam <- start
        }
        print(cbind(start, agrad, ngrad))
    }
    maxl <- maxLik(f, start = start, ...)
    coefficients <- maxl$estimate
    fitted <- attr(mhurdle:::mhurdle.lnl(coefficients, X1 = X1, X2 = X2, 
        X3 = X3, X4 = X4, y = y, gradient = FALSE, fitted = TRUE, 
        dist = dist, corr = corr), "fitted")
    logLik <- f(coefficients)
    gradi <- attr(logLik, "gradi")
    logLik <- structure(as.numeric(logLik), df = length(coefficients), 
        nobs = length(y), class = "logLik")
    hessian <- maxl$hessian
    convergence.OK <- maxl$code <= 2
    elaps.time <- proc.time() - start.time
    nb.iter <- maxl$iterations
#    eps <- with(maxl, gradient %*% solve(-hessian) %*% gradient)
#    est.stat <- list(elaps.time = elaps.time, nb.iter = nb.iter, 
#        eps = eps, method = maxl$type, message = maxl$message)
#    class(est.stat) <- "est.stat"
    if (!is.null(X4)) 
        sd.names <- colnames(X4)
    else sd.names <- "sd"
    if (!is.null(corr)) 
        rho.names <- ifelse(corr == "h1", "corr12", "corr13")
    else rho.names <- NULL
    if (dist %in% c("bc", "ihs")) 
        tr.names <- "tr"
    else tr.names <- NULL
    coef.names <- list(h1 = colnames(X1), h2 = colnames(X2), 
        h3 = colnames(X3), sd = sd.names, corr = rho.names, tr = tr.names)
    result <- list(coefficients = coefficients, vcov = NULL, 
        fitted.values = fitted, logLik = logLik, gradient = gradi, 
        formula = NULL, model = NULL, coef.names = coef.names, 
        call = NULL, est.stat = NULL, naive = NULL)
    if (ncol(X2) > 1) 
        class(result) <- c("mhurdle")
    result
}




assignInNamespace( "mhurdle.fit", my.mhurdle.fit, "mhurdle")

#environment(my.mhurdle) <- as.environment("package:mhurdle")

unlockBinding("mhurdle", as.environment("package:mhurdle"))
assignInNamespace( "mhurdle", my.mhurdle, "mhurdle")
assign("mhurdle", my.mhurdle, as.environment("package:mhurdle"))
lockBinding("mhurdle", as.environment("package:mhurdle"))

#  unlockBinding(name, env);
 # assignInNamespace(name, value, ns=pkgName, envir=env);
 # assign(name, value, envir=env);
 # lockBinding(name, env);



main.specification <- as.formula(paste("manure.exp", " ~ ", "department + indig.prop*hhh.edu.measure.r + indig.practices + hhh.literacy + hhh.age  + hhh.sex + REMPAIS + REMEXT + credit.source.r + drive.time.amanzanada + drive.time.urban + mean.ann.rain.5yr + elevation + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY  ", " | ", "department + indig.prop + indig.practices + hhh.age  + REMPAIS + REMEXT +  mean.ann.rain.5yr + elevation + hhh.edu.measure.r + AWC.CLASS + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + T.GRAVEL + T.SILT + T.CLAY  ", " | 0" ) )
# Note that we are not including interaction term indig.prop*hhh.edu.measure.r in
# the second hurdle since I wonder if we will have enough observations for the bootstrap

  
m110d <- mhurdle(main.specification, 
  data = crop.wide.df, weights=crop.wide.df$FACTOR, corr = "d" , dist = "ln", method =  "BFGS" ,  print.level=2)
  
  
m110d <- my.mhurdle(main.specification, 
  data = crop.wide.df, weights=crop.wide.df$FACTOR, corr = "d" , dist = "ln", method =  "BFGS" ,  print.level=2)












mhurdle:::mhurdle.lnl

my.mhurdle.lnl <-function (param, X1, X2, X3, X4, y, gradient = FALSE, fitted = FALSE, 
    dist = NULL, corr = NULL) 
{
    h1 <- !is.null(X1)
    K1 <- ifelse(is.null(X1), 0, ncol(X1))
    h3 <- !is.null(X3)
    K3 <- ifelse(is.null(X3), 0, ncol(X3))
    K2 <- ncol(X2)
    beta2 <- param[(K1 + 1):(K1 + K2)]
    beta2[is.na(beta2)] <- 0
    # MY FIX
    bX2 <- as.numeric(crossprod(t(X2), beta2))
    if (h1) {
        beta1 <- param[1:K1]
        bX1 <- as.numeric(crossprod(t(X1), beta1))
        Phi1 <- pnorm(bX1)
        phi1 <- dnorm(bX1)
    }
    else {
        bX1 <- beta1 <- NULL
        Phi1 <- 1
        phi1 <- 0
    }
    if (h3) {
        beta3 <- param[(K1 + K2 + 1):(K1 + K2 + K3)]
        bX3 <- as.numeric(crossprod(t(X3), beta3))
        Phi3 <- pnorm(bX3)
        phi3 <- dnorm(bX3)
    }
    else {
        bX3 <- beta3 <- NULL
        Phi3 <- 1
        phi3 <- 0
    }
    if (is.null(X4)) 
        K4 <- 1
    else K4 <- ncol(X4)
    beta4 <- param[(K1 + K2 + K3 + 1):(K1 + K2 + K3 + K4)]
    if (is.null(X4)) 
        sigma <- beta4
    else sigma <- as.numeric(exp(crossprod(t(X4), beta4)))
    rho1 <- rho3 <- 0
    if (!is.null(corr)) {
        if (corr == "h1") {
            rho1 <- param[K1 + K2 + K3 + K4 + 1]
            if (rho1 < -1) 
                rho1 <- -0.99
            if (rho1 > 1) 
                rho1 <- 0.99
        }
        if (corr == "h3") {
            rho3 <- param[K1 + K2 + K3 + K4 + 1]
            if (rho3 < -1) 
                rho3 <- -0.99
            if (rho3 > 1) 
                rho3 <- 0.99
        }
    }
    if (dist == "bc") {
        lambda <- param[K1 + K2 + K3 + K4 + (!is.null(corr)) + 
            1]
        Phi2 <- pnorm((bX2 + 1/lambda)/sigma)
        phi2 <- dnorm((bX2 + 1/lambda)/sigma)
        Phi12 <- mhurdle:::mypbivnorm(bX1, (bX2 + 1/lambda)/sigma, rho1)
        Phi23 <- mhurdle:::mypbivnorm((bX2 + 1/lambda)/sigma, bX3, rho3)
    }
    else {
        Phi2 <- pnorm(bX2/sigma)
        phi2 <- dnorm(bX2/sigma)
        Phi12 <- mhurdle:::mypbivnorm(bX1, bX2/sigma, rho1)
        Phi23 <- mhurdle:::mypbivnorm(bX2/sigma, bX3, rho3)
    }
    if (dist == "ihs") 
        lambda <- param[K1 + K2 + K3 + K4 + (!is.null(corr)) + 
            1]
    Ty <- switch(dist, ln = log2(y) + log(Phi3), bc = ((y * Phi3)^lambda - 
        1)/lambda, ihs = log(lambda * y * Phi3 + sqrt(1 + (lambda * 
        y * Phi3)^2))/lambda, y * Phi3)
    lnJ <- switch(dist, ln = -log2(y), bc = (lambda - 1) * log2(y) + 
        lambda * log(Phi3), ihs = -0.5 * log(1 + (lambda * Phi3 * 
        y)^2) + log(Phi3), log(Phi3))
    lnJlb <- switch(dist, bc = log2(y) + log(Phi3), ihs = -lambda * 
        y^2 * Phi3^2/(1 + (lambda * y * Phi3)^2))
    resid <- Ty - bX2
    z <- function(x, resid, rho) {
        if (is.null(x)) 
            result <- list(f = 100, g = 0, h = 0)
        else {
            f <- (x + rho/sigma * resid)/sqrt(1 - rho^2)
            g <- x * rho * (1 - rho^2)^-1.5 + ((1 - rho^2)^-0.5 + 
                rho^2 * (1 - rho^2)^-1.5) * resid/sigma
            h <- x * ((1 - rho^2)^-1.5 + 3 * rho^2 * (1 - rho^2)^-2.5) + 
                resid/sigma * (3 * rho * (1 - rho^2)^-1.5 + 3 * 
                  rho^3 * (1 - rho^2)^-2.5)
            result <- list(f = f, g = g, h = h)
        }
        result
    }
    z1 <- z(bX1, resid, rho1)
    z3 <- z(bX3, resid, rho3)
    lnL.null <- switch(dist, tn = log(1 - Phi12$f * Phi23$f/Phi2^2), 
        ln = log(1 - Phi1 * Phi3), log(1 - Phi12$f * Phi23$f/Phi2))
    lnL.pos <- -log(sigma) + dnorm(resid/sigma, log = TRUE) + 
        pnorm(z1$f, log.p = TRUE) + pnorm(z3$f, log.p = TRUE) + 
        lnJ - (dist == "tn") * log(Phi2)
    lnL <- lnL.null * (y == 0) + lnL.pos * (y != 0)
    if (gradient) {
        gradi <- c()
        if (h1) {
            lnL.beta1 <- (y == 0) * (switch(dist, tn = -(Phi12$a * 
                Phi23$f)/(Phi2^2 - Phi12$f * Phi23$f), ln = -phi1 * 
                Phi3/(1 - Phi1 * Phi3), -(Phi12$a * Phi23$f)/(Phi2 - 
                Phi12$f * Phi23$f))) + (y != 0) * (mhurdle:::mills(z1$f)/sqrt(1 - 
                rho1^2))
            gradi <- cbind(gradi, lnL.beta1 * X1)
        }
        lnL.beta2 <- (y == 0) * (switch(dist, tn = (2 * Phi2 * 
            phi2 - Phi12$b * Phi23$f - Phi12$f * Phi23$a)/(Phi2^2 - 
            Phi12$f * Phi23$f)/sigma - 2 * mhurdle:::mills(bX2/sigma)/sigma, 
            ln = 0, bc = (phi2 - Phi12$b * Phi23$f - Phi12$f * 
                Phi23$a)/(Phi2 - Phi12$f * Phi23$f)/sigma - mhurdle:::mills((bX2 + 
                1/lambda)/sigma)/sigma, (phi2 - Phi12$b * Phi23$f - 
                Phi12$f * Phi23$a)/(Phi2 - Phi12$f * Phi23$f)/sigma - 
                mhurdle:::mills(bX2/sigma)/sigma)) + (y != 0) * (resid/sigma^2 - 
            mhurdle:::mills(z1$f) * rho1/sigma/sqrt(1 - rho1^2) - mhurdle:::mills(z3$f) * 
            rho3/sigma/sqrt(1 - rho3^2) - (dist == "tn") * mhurdle:::mills(bX2/sigma)/sigma)
        gradi <- cbind(gradi, lnL.beta2 * X2)
        if (h3) {
            Ty3 <- switch(dist, ln = mhurdle:::mills(bX3), bc = (y * Phi3)^lambda * 
                mhurdle:::mills(bX3), ihs = y * phi3/sqrt(1 + (lambda * 
                y * Phi3)^2), y * phi3)
            lnJ3 <- switch(dist, ln = 0, bc = lambda * mhurdle:::mills(bX3), 
                ihs = -phi3 * Phi3 * lambda^2 * y^2/(1 + (lambda * 
                  y * Phi3)^2) + mhurdle:::mills(bX3), mhurdle:::mills(bX3))
            lnL.beta3 <- (y == 0) * (switch(dist, tn = -(Phi12$f * 
                Phi23$b)/(Phi2^2 - Phi12$f * Phi23$f), ln = -Phi1 * 
                phi3/(1 - Phi1 * Phi3), -(Phi12$f * Phi23$b)/(Phi2 - 
                Phi12$f * Phi23$f))) + (y != 0) * (-resid/sigma^2 * 
                Ty3 + mhurdle:::mills(z1$f) * rho1/sqrt(1 - rho1^2)/sigma * 
                Ty3 + mhurdle:::mills(z3$f) * (1 + rho3/sigma * Ty3)/sqrt(1 - 
                rho3^2) + lnJ3)
            gradi <- cbind(gradi, lnL.beta3 * X3)
        }
        lnL.sigma <- (y == 0) * (switch(dist, tn = -(2 * Phi2 * 
            phi2 - Phi12$b * Phi23$f - Phi12$f * Phi23$a)/(Phi2^2 - 
            Phi12$f * Phi23$f) * bX2/sigma^2 + 2 * mhurdle:::mills(bX2/sigma) * 
            bX2/sigma^2, ln = 0, bc = -(phi2 - Phi12$b * Phi23$f - 
            Phi12$f * Phi23$a)/(Phi2 - Phi12$f * Phi23$f) * (bX2 + 
            1/lambda)/sigma^2 + mhurdle:::mills((bX2 + 1/lambda)/sigma) * 
            (bX2 + 1/lambda)/sigma^2, -(phi2 - Phi12$b * Phi23$f - 
            Phi12$f * Phi23$a)/(Phi2 - Phi12$f * Phi23$f) * bX2/sigma^2 + 
            mhurdle:::mills(bX2/sigma) * bX2/sigma^2)) + (y != 0) * (-1/sigma + 
            resid^2/sigma^3 - mhurdle:::mills(z1$f) * rho1/sqrt(1 - rho1^2) * 
            resid/sigma^2 - mhurdle:::mills(z3$f) * rho3/sqrt(1 - rho3^2) * 
            resid/sigma^2 + (dist == "tn") * mhurdle:::mills(bX2/sigma) * 
            bX2/sigma^2)
        if (!is.null(X4)) 
            lnL.sigma <- lnL.sigma * sigma * X4
        gradi <- cbind(gradi, lnL.sigma)
        if (h1 && !is.null(corr) && corr == "h1") {
            lnL.rho1 <- (y == 0) * (switch(dist, tn = -Phi12$rho * 
                Phi23$f/(Phi2^2 - Phi12$f * Phi23$f), ln = 0, 
                -Phi12$rho * Phi23$f/(Phi2 - Phi12$f * Phi23$f))) + 
                (y != 0) * (mhurdle:::mills(z1$f) * z1$g)
            gradi <- cbind(gradi, lnL.rho1)
        }
        if (h3 && !is.null(corr) && corr == "h3") {
            lnL.rho3 <- (y == 0) * (switch(dist, tn = -Phi12$f * 
                Phi23$rho/(Phi2^2 - Phi12$f * Phi23$f), ln = 0, 
                -Phi12$f * Phi23$rho/(Phi2 - Phi12$f * Phi23$f))) + 
                (y != 0) * (mhurdle:::mills(z3$f) * z3$g)
            gradi <- cbind(gradi, lnL.rho3)
        }
        if (dist == "bc") {
            Tylb <- (log(Phi3 * y) * (Phi3 * y)^lambda * lambda - 
                ((Phi3 * y)^lambda - 1))/lambda^2
            lnL.lambda <- vector(mode = "numeric", length = length(y))
            lnL.lambda[y == 0] <- (((phi2 - Phi12$b * Phi23$f - 
                Phi12$f * Phi23$a)/(Phi2 - Phi12$f * Phi23$f)/sigma - 
                mhurdle:::mills((bX2 + 1/lambda)/sigma)/sigma) * (-1/lambda^2))[y == 
                0]
            lnL.lambda[y != 0] <- ((-resid/sigma^2 + mhurdle:::mills(z1$f) * 
                rho1/sigma/sqrt(1 - rho1^2) + mhurdle:::mills(z3$f) * rho3/sigma/sqrt(1 - 
                rho3^2)) * Tylb + lnJlb)[y != 0]
            gradi <- cbind(gradi, lnL.lambda)
        }
        if (dist == "ihs") {
            Tylb <- (y * Phi3)/lambda/sqrt(1 + (lambda * y * 
                Phi3)^2) - Ty/lambda
            lnL.lambda <- vector(mode = "numeric", length = length(y))
            lnL.lambda[y != 0] <- ((-resid/sigma^2 + mhurdle:::mills(z1$f) * 
                rho1/sigma/sqrt(1 - rho1^2) + mhurdle:::mills(z3$f) * rho3/sigma/sqrt(1 - 
                rho3^2)) * Tylb + lnJlb)[y != 0]
            gradi <- cbind(gradi, lnL.lambda)
        }
        attr(lnL, "gradient") <- gradi
    }
    if (fitted) {
        P0 <- exp(lnL.null)
        if (dist != "ln") {
            if (h3) 
                Psi23 <- rho3 * phi3 * pnorm((bX2/sigma - rho3 * 
                  bX3)/sqrt(1 - rho3^2)) + phi2 * pnorm((bX3 - 
                  rho3 * bX2/sigma)/sqrt(1 - rho3^2))
            else Psi23 <- phi2
            if (h1) 
                Psi21 <- rho1 * phi1 * pnorm((bX2/sigma - rho1 * 
                  bX1)/sqrt(1 - rho1^2)) + phi2 * pnorm((bX1 - 
                  rho1 * bX2/sigma)/sqrt(1 - rho1^2))
            else Psi21 <- phi2
            Econd <- bX2/Phi3 + sigma * (Psi23 * Psi21 * Phi2)/(Phi12$f * 
                Phi23$f * Phi3 * phi2)
        }
        else {
            if (h3) 
                Psi23 <- pnorm(bX3 + sigma * rho3)
            else Psi23 <- 1
            if (h1) 
                Psi21 <- pnorm(bX1 + sigma * rho1)
            else Psi21 <- 1
            Econd <- exp(bX2 + sigma^2/2) * Psi21 * Psi23/(Phi1 * 
                Phi3^2)
        }
        attr(lnL, "fitted") <- cbind(`P(y=0)` = P0, `E(y|y>0)` = Econd)
    }
    lnL
}

assignInNamespace( "mhurdle.lnl", my.mhurdle.lnl, "mhurdle")


