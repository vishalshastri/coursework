

#  RUN THIS BEFORE 718 code runs:



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















crop.wide.df <- crop.wide.df[crop.wide.df$total.area>0, ]

crop.wide.df$received.credit <- abs(as.numeric(crop.wide.df$received.credit)-2 )

# STOP RUN

########################




coef(partitioned.3.lm) - coef(partitioned.4.lm)
coef(partitioned.3.lm) 
coef(partitioned.4.lm)


with(crop.wide.df, cor(resid(partitioned.1.lm) , log(fert.exp +1) ))







#summary(lm( REMPAIS ~ hhh.age + num.pers.agropecuaria , data=crop.wide.df))


#summary(lm( fert.exp ~ REMPAIS + num.pers.agropecuaria , data=crop.wide.df))

#summary(lm( log(fert.exp+1) ~ log(REMPAIS+1)  , data=crop.wide.df))

#ivreg(y ~ x1 + x2 | z1 + z2 + z3

#summary(ivreg(fert.exp ~ REMPAIS | REMPAIS + hhh.sex, data=crop.wide.df))


# Ok, let's go with this:

# summary(lm( log(fert.exp+1) ~ log(REMPAIS+1)  , data=crop.wide.df))



#Run after you run main script:
  
source("/Users/travismcarthur/git/misc/authored-functions/authored-R-fns.R")

var.names <- read.csv("/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 1/var namesPS1.csv", stringsAsFactors=FALSE)

replace.vars(replacement.matrix=var.names, 
  directory="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 1/", 
  file.pattern="table.*tex", table.only=TRUE)




