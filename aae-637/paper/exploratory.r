install.packages("foreign")
library("foreign")
install.packages("VGAM")
library("VGAM")
install.packages("AER")
library("AER")
install.packages("MASS")
library("MASS")
install.packages("censReg")
library("censReg")
install.packages("knitr")
library("knitr")



mcv02full.df<-read.spss(paste0(work.dir, "bd19 (2002).zip Folder/mcv02final1.sav"))

costos02.df<-read.spss(paste0(work.dir, "bd19 (2002).zip Folder/mcv02 - Costos produccion agropecuaria.sav"))

prod02.df<-read.spss(paste0(work.dir, "bd19 (2002).zip Folder/hogar.sav"))
)	, reencode=)
prod02.df<-read.spss(paste0(work.dir, "bd19 (2002).zip Folder/Produccion agricola-utf8.sav"))


prod01.df<-read.spss(paste0(work.dir, "bd18 (2001).zip Folder/agricola.sav"), to.data.frame = TRUE)

hogar01.df<-read.spss(paste0(work.dir, "bd18 (2001).zip Folder/hogar.sav"), to.data.frame = TRUE)

attr(costos01.df, "variable.labels")

attr(hogar01.df, "variable.labels")

attr(prod01.df, "label.table")

# p. 18 of ftp://ftp.fao.org/docrep/fao/010/ah868e/ah868e00.pdf
# "One Bolivian arroba is equivalent to 11.5 kg"
# "One Bolivian quintal is equivalent to 46 kg"

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
}

area.agg<-aggregate(prod01.df$area.r, by=list(FOLIO=prod01.df$FOLIO), FUN=sum, na.rm=TRUE)
names(area.agg)[2]<-"total.area"
	
crop.wide.df<-reshape(prod01.df[ prod01.df$crop %in% names(rev(sort(table(prod01.df$crop))))[1:12], 
	!names(prod01.df) %in% c("CODA", "area", "crop.code", "harvest", "sales.quant", "consumption", "seeds", "animal.or.subproducts", "bartered", "lost")], timevar="crop", idvar="FOLIO", direction="wide")


hogar01.df.temp<-hogar01.df[, c("FOLIO", "S815D2", "S1036", "ID05", "ID06")]
names(hogar01.df.temp)<-c("FOLIO", "fert.exp", "received.credit", "city", "canton")
hogar01.df.temp$city <- as.factor(hogar01.df.temp$city)
hogar01.df.temp$canton <- as.factor(hogar01.df.temp$canton)

crop.wide.df<-merge(crop.wide.df, hogar01.df.temp)
crop.wide.df<-merge(crop.wide.df, area.agg)
	
crop.wide.df<-data.frame(lapply(crop.wide.df, FUN=function(x) {
	x[is.na(x)]<-0 
	x
	}))
	
glm(, S815D2 ~ ., family= )
	
fmla <- as.formula(paste("fert.exp ~ ", 
	paste0(names(crop.wide.df)[grepl("area.r", names(crop.wide.df))], collapse= " + ")))

censReg

tob <- censReg(fmla, data = crop.wide.df)
margEff( tob )
summary( margEff( tob ) )

tob <- censReg(fert.exp ~ total.area + received.credit + city, data = crop.wide.df)
margEff( tob )
summary( margEff( tob ) )


tob.null <- censReg(fert.exp ~ total.area + received.credit , data = crop.wide.df)


lrtest(tob, tob.null )


"fert.exp", "received.credit", "city", "canton"



	
pnorm(apply(as.matrix(crop.wide.df[, grepl("(area.r)", names(crop.wide.df) )]), 2, mean)  %*% 
		tob$coef[-1]/tob$scale) %*% tob$coef[-1]

	
summary(m <- vglm( fert.exp ~  area.r.MAIZ........... + area.r.PAPA........... + area.r.HABA........... + area.r.CEBADA......... + area.r.TRIGO.......... + area.r.CEBOLLA........ + area.r.ARVEJA......... + area.r.QUINUA......... + area.r.OCA............ + area.r.YUCA........... + area.r.PLATANO........ + area.r.ARROZ.........., family=tobit, 
	data = crop.wide.df[, grepl("(fert.exp)|(area.r)", names(crop.wide.df) )]))

summary(m <- tobit( formula=fmla, 
	data = crop.wide.df[, grepl("(fert.exp)|(area.r)", names(crop.wide.df) )]))
	
class(m)<-"censReg" 
m$coefficient["logSigma"] <- m$icoef["Log(scale)"] 
colnames(m$var)[length(colnames(m$var))]<-"logSigma"
rownames(m$var)[length(colnames(m$var))]<-"logSigma"

summary( margEff( m, logSigma = TRUE ) )
	
test.ag<-aggregate(prod01.df$sales.quant.r, by=list(prod01.df$crop), FUN=sum, na.rm=TRUE)
test.ag[order(test.ag$x),]
	
	
harvest.r.HABA
	
	
	
	
	
12
	
	

	


	
	
	



prod01.df$area.r<-0





cor(as.matrix(as.data.frame(costos02.df[ c("S916D", "S916C")])), use="complete.obs")

cor(as.matrix(as.data.frame(costos02.df[ c("S916D", "S916C")])))

round(cor(as.matrix(as.data.frame(costos02.df[ 8:(length(costos02.df)-1)])), use="pairwise.complete.obs"), 2)*10

heatmap(cor(as.matrix(as.data.frame(costos02.df[ 8:(length(costos02.df)-1)])), use="pairwise.complete.obs"))

round(cor(as.matrix(as.data.frame(lapply(costos02.df[ 8:(length(costos02.df)-1)],  FUN=function(x) x>0))), use="pairwise.complete.obs"), 2)*10

heatmap(cor(as.matrix(as.data.frame(lapply(costos02.df[ 8:(length(costos02.df)-1)],  FUN=function(x) x>0))), use="pairwise.complete.obs"))

Fert: S916D

cor(costos02.df$S916D>0, costos02.df$S916C>0, use="complete.obs")





precip.df<-read.fortran(paste0(work.dir, "Global2011P/precip.2000"), 
  format=c("F8.3", "F8.3", "12F8.1"))



TODO: Delete:
		assign(paste0(i, ".r"), 0) 
	assign(paste0(i, ".r")[harvest.unit=="Arroba"],  get(i)[harvest.unit=="Arroba"]*11.5)
	assign(paste0(i, ".r")[harvest.unit=="Quintal"],  get(i)[harvest.unit=="Quintal"]*46)
	assign(paste0(i, ".r")[harvest.unit=="Libra"],  get(i)[harvest.unit=="Libra"]*0.453592)
	assign(paste0(i, ".r")[harvest.unit=="Unidad"],  get(i)[harvest.unit=="Unidad"])

