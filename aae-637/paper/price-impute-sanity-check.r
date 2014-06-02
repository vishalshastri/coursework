
# NOTE: Need to go back to around line 430 of setbuilding and reset crop.wide.df before running this

library("foreign")

work.dir <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/"



options(encoding = "CP850")
prod01.df<-read.spss(paste0(work.dir, "bd18 (2001).zip Folder/agricola.sav"), to.data.frame = TRUE, reencode="CP850")
prod01.df[, 3]<-gsub("(^[[:space:]]+)|([[:space:]]+$)", "", prod01.df[, 3])
#write.csv(rev(sort(table(prod01.df[, 3]))), file=paste0(work.dir, "unclean cropnames.csv"), fileEncoding="CP850")


hogar01.df<-read.spss(paste0(work.dir, "bd18 (2001).zip Folder/hogar.sav"), to.data.frame = TRUE, reencode="CP850")






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




cropname.corrections<-as.matrix( read.csv(paste0(work.dir, "unclean cropnames01 corrections.csv"), header=FALSE, stringsAsFactors=FALSE, encoding = "UTF-8") )

# MAKE SURE TO ENCODE THE csv in EXCEL AS csv (MS-DOS)

prod01.df$crop.name.cleaned<-FALSE

prod01.df$crop.r<-prod01.df$crop

for (i in 1:nrow(cropname.corrections)) {
  if ( (sum(cropname.corrections[i, ]=="")+1)==ncol(cropname.corrections)) { next }
  clean.index<-prod01.df$crop.r %in% cropname.corrections[i, -1]
  prod01.df$crop.r[clean.index] <- cropname.corrections[i, 1]
  prod01.df$crop.name.cleaned[clean.index] <- TRUE
}
  
prod01.df$crop.r <- as.factor(prod01.df$crop.r)





prod.geog.df<-hogar01.df[, c("FOLIO", "ID01", "ID02", "ID03", "ID04", "ID05")]
names(prod.geog.df)<-c("FOLIO", "department", "province", "seccion", "canton", "village")
prod.geog.df$village<-do.call(paste0, prod.geog.df[, c("department", "province", "seccion", "canton", "village")])
prod.geog.df$canton<-do.call(paste0, prod.geog.df[, c("department", "province", "seccion", "canton")])
prod.geog.df$seccion<-do.call(paste0, prod.geog.df[, c("department", "province", "seccion")])
prod.geog.df$province<-do.call(paste0, prod.geog.df[, c("department",  "province")])
prod.geog.df$nation<-1


prod01.df<-merge(prod01.df, prod.geog.df, all.x=TRUE)



# TODO: Make sure that prices are matched by units or weight
# TODO: Make sure the three-HH rule is not overcome by imputation as we're going along


prod01.df$total.value<-0
prod01.df$impute.level<-""
prod01.df$price<-0

impute.levels<-c("household", "village", "canton", "seccion", "province", "department", "nation")

for (i in 1:nrow(prod01.df)) {

for (impute.level in impute.levels) {
  
  if (impute.level=="household" & !is.na(prod01.df$sales.value[i]) & prod01.df$sales.quant.r[i]!=0) {
    prod01.df$total.value[i] <- 
      (prod01.df$harvest.r[i]-prod01.df$lost.r[i]) * prod01.df$sales.value[i]/prod01.df$sales.quant.r[i]
    prod01.df$price[i] <- prod01.df$sales.value[i]/prod01.df$sales.quant.r[i]
    prod01.df$impute.level[i]<-impute.level
    prod01.df$impute.sample.size[i]<-0
    break
  }
  if (impute.level=="household") {next}
  
  #target.crop<-prod01.df$crop[i]
  match.index<-prod01.df[, impute.level] == prod01.df[i, impute.level] & 
    prod01.df$crop == prod01.df$crop[i] & !is.na(prod01.df$sales.value) & prod01.df$sales.quant.r!=0
  
  impute.sample.size <- sum(match.index)
  
  if (impute.sample.size>=3) {
    prod01.df$total.value[i] <- 
      (prod01.df$harvest.r[i]-prod01.df$lost.r[i]) * 
        median(prod01.df$sales.value[match.index]/prod01.df$sales.quant.r[match.index])
    prod01.df$price[i] <- median(prod01.df$sales.value[match.index]/prod01.df$sales.quant.r[match.index])
    prod01.df$impute.level[i]<-impute.level
    prod01.df$impute.sample.size[i] <- impute.sample.size
    break
  }
  
}
  
}









prod01.df$crop.hh.id<-1:nrow(prod01.df)


hogar01.df.temp<-hogar01.df[, c("FOLIO", "S815D2", "S815B2", "S1036", "S1037", "PERSAGRO", "ID05", "ID06", "ID01",  "S815A2", "S815C2", "S815E2", "S815F2", "S815G2", "S815H2", "S815I2", "S815N2", "S1032A", "S1033", "S1034A")]

#"comunidad.id"

# "X", "Y", "CAT_LOC", "num.of.localidades", "comunidad.id"

#hogar01.df.temp<-hogar01.df[, c("FOLIO", "S815D2", "S815B2", "S1036", "S1037", "PERSAGRO", "ID05", "ID06", "ID01", "S815A2", "S815C2", "S815E2", "S815F2", "S815G2", "S815H2", "S815I2", "S815N2", "S1032A", "S1033", "S1034A")]

colnames(hogar01.df.temp)<-c("FOLIO", "fert.exp", "seed.exp", "received.credit", "credit.source", "num.pers.agropecuaria", "city", "canton", "department",  "hired.labor.exp", "manure.exp", "transport.exp", "pesticide.exp", "extension.exp", "machine.exp", "draft.anmial.exp", "other.exp", "hh.member.incident",  "hh.member.incident.income", "hogar.incident")

#  "X", "Y", "CAT_LOC", "num.of.localidades", "comunidad.id",


#area.agg<-aggregate(prod01.df$area.r, by=list(FOLIO=prod01.df$FOLIO), FUN=sum, na.rm=TRUE)
#names(area.agg)[2]<-"total.area"

#area.agg<-aggregate(prod01.df$area.r, by=list(FOLIO=prod01.df$FOLIO), FUN=sum, na.rm=TRUE)
#names(area.agg)[2]<-"total.area"

#crop.wide.df<-merge(crop.wide.df, hogar01.df.temp)
#crop.wide.df<-merge(crop.wide.df, area.agg)

# intersect(names(crop.wide.df), names(hogar01.df.temp))

crop.wide.df<- merge(crop.wide.df, hogar01.df.temp, by="FOLIO")

# Hired labor is the last input



###################


miembros01.df<-read.spss(paste0(work.dir, "bd18 (2001).zip Folder/mcv01.sav"), to.data.frame = TRUE)





self.emp.prim <- miembros01.df$S617 %in% levels(miembros01.df$S617)[c(3:5,7,8)]
self.emp.sec <- miembros01.df$S617 %in% levels(miembros01.df$S635)[c(3:5,7,8)]



miembros01.df<-within(miembros01.df, {
  crop.labor <- rowSums( matrix(c(S624A * (S624HRS + S624MIN/60) * self.emp.prim * (Z613ACTI %in% c("111", "113", "0111", "0112",  "0113")),
                     S638A * (S638HRS + S638MIN/60) * self.emp.sec * (Z634ACTI %in% c("111", "113", "0111",  "0112",  "0113"))), ncol=2), na.rm=TRUE)
  crop.and.livestick.labor<- rowSums( matrix(c(S624A * (S624HRS + S624MIN/60) * self.emp.prim * (Z613ACTI %in% c("130", "0130")),
                        S638A * (S638HRS + S638MIN/60) * self.emp.sec * (Z634ACTI %in% c("130", "0130"))), ncol=2), na.rm=TRUE)
  }
)


miembros01.df<-within(miembros01.df, {
  crop.labor <- rowSums( matrix(c(S624A * (S624HRS + S624MIN/60) * !self.emp.prim * (Z613ACTI %in% c("111", "113", "0111", "0112",  "0113")),
                     S638A * (S638HRS + S638MIN/60) * self.emp.sec * (Z634ACTI %in% c("111", "113", "0111",  "0112",  "0113"))), ncol=2), na.rm=TRUE)
  crop.and.livestick.labor<- rowSums( matrix(c(S624A * (S624HRS + S624MIN/60) * self.emp.prim * (Z613ACTI %in% c("130", "0130")),
                        S638A * (S638HRS + S638MIN/60) * !self.emp.sec * (Z634ACTI %in% c("130", "0130"))), ncol=2), na.rm=TRUE)
  }
)






self.emp.prim <- miembros01.df$S617 %in% levels(miembros01.df$S617)[c(3:5,7,8)]
self.emp.sec <- miembros01.df$S617 %in% levels(miembros01.df$S635)[c(3:5,7,8)]


miembros01.df<-within(miembros01.df, {
  actual.ag.wage<- apply(matrix(c( YAPTF / (S624A * (S624HRS + S624MIN/60) * (!self.emp.prim) * (Z613ACTI %in% c("111", "113", "0111", "0112",  "0113", "130", "0130")) * 4.345),
  YASTF / (S638A * (S638HRS + S638MIN/60) * (!self.emp.sec) * (Z634ACTI %in% c("111", "113", "0111", "0112",  "0113", "130", "0130")) * 4.345)), ncol=2 ) ,  MARGIN=1, FUN=mean, na.rm=TRUE)
  }
)

# 4.345 weeks in a month according to http://www.convertunits.com/from/weeks/to/months
# And the consolidated earnings are per month

miembros01.df$actual.ag.wage[ !(is.finite(miembros01.df$actual.ag.wage) & miembros01.df$actual.ag.wage>0) ] <- NA

miembros01.df<-merge(miembros01.df, hogar01.df[, c("FOLIO", "ID01", "ID02", "ID03", "ID04", "ID05")])

prod.geog.df<-miembros01.df[, c("FOLIO", "ID01", "ID02", "ID03", "ID04", "ID05")]
names(prod.geog.df)<-c("FOLIO", "department.1", "province.1", "seccion.1", "canton.1", "village.1")
prod.geog.df$village.1<-do.call(paste0, prod.geog.df[, c("department.1", "province.1", "seccion.1", "canton.1", "village.1")])
prod.geog.df$canton.1<-do.call(paste0, prod.geog.df[, c("department.1", "province.1", "seccion.1", "canton.1")])
prod.geog.df$seccion.1<-do.call(paste0, prod.geog.df[, c("department.1", "province.1", "seccion.1")])
prod.geog.df$province.1<-do.call(paste0, prod.geog.df[, c("department.1",  "province.1")])
prod.geog.df$nation.1<-1
prod.geog.df<-unique(prod.geog.df)

intersect(names(prod.geog.df), names(miembros01.df))

miembros01.df<-merge(miembros01.df, prod.geog.df, all.x=TRUE)


crop.wide.df<-merge(crop.wide.df, prod.geog.df, all.x=TRUE)


# TODO: Make sure that prices are matched by units or weight

crop.wide.df$imputed.ag.wage<-0
crop.wide.df$ag.wages.impute.level<-""
crop.wide.df$ag.wages.impute.sample.size<-0

impute.levels<-c("FOLIO", "village.1", "canton.1", "seccion.1", "province.1", "department.1", "nation.1")

for (i in 1:nrow(crop.wide.df)) {

for (impute.level in impute.levels) {
  
  impute.aux<-miembros01.df$actual.ag.wage[miembros01.df[, impute.level] == crop.wide.df[i, impute.level] ]
  match.index<-!is.na(impute.aux)
  
  impute.sample.size <- sum(match.index)
  
  if (impute.sample.size>=3 | (impute.sample.size>=1 & impute.level=="FOLIO")) {
    crop.wide.df$imputed.ag.wage[i] <- rlnorm(1)
    # above is key part of what I changed. Used to be:
    # crop.wide.df$imputed.ag.wage[i] <- median(impute.aux[match.index])
    crop.wide.df$ag.wages.impute.level[i]<-impute.level
    crop.wide.df$ag.wages.impute.sample.size[i] <- impute.sample.size
    break
  }
  
}
  
}








input.prices.df<-read.delim(paste0(work.dir, "2008 input prices.txt"), stringsAsFactors=FALSE, na.strings="-", dec=",")
# lapply(test, FUN=summary)

input.prices.df$crop<-gsub(" +$", "", input.prices.df$crop)

#   PAPA    MAIZ    HABA   TRIGO  CEBADA   ARROZ    YUCA     OCA PLATANO CEBOLLA 
#   1533    1174     500     480     448     410     300     290     266     219 
# QUINUA  ARVEJA 
#    185     174 

#"PAPA", "MAIZ", "HABA", "TRIGO", "CEBADA", "ARROZ", "YUCA", "OCA", "PLATANO", "CEBOLLA", "QUINUA", "ARVEJA"

input.prices.df$crop[input.prices.df$crop=="Papa"] <- "PAPA"
input.prices.df$crop[input.prices.df$crop=="Mai_z"] <- "MAIZ"
input.prices.df$crop[input.prices.df$crop=="Haba"] <- "HABA"
input.prices.df$crop[input.prices.df$crop=="Trigo"] <- "TRIGO"
input.prices.df$crop[input.prices.df$crop=="Cebada grano"] <- "CEBADA"
input.prices.df$crop[input.prices.df$crop=="Arroz"] <- "ARROZ"
input.prices.df$crop[input.prices.df$crop=="Yuca"] <- "YUCA"
input.prices.df$crop[input.prices.df$crop=="Oca"] <- "OCA"
input.prices.df$crop[input.prices.df$crop=="Pla_tano"] <- "PLATANO"
input.prices.df$crop[input.prices.df$crop=="Cebolla"] <- "CEBOLLA"
input.prices.df$crop[input.prices.df$crop=="Quinua"] <- "QUINUA"
input.prices.df$crop[input.prices.df$crop=="Arveja"] <- "ARVEJA"
input.prices.df$crop[input.prices.df$crop=="Avena grano"] <- "AVENA"
input.prices.df$crop[input.prices.df$crop=="Mani_"] <- "MANI"
input.prices.df$crop[input.prices.df$crop=="Naranja"] <- "NARANJA"


for ( i in 3:8) {
  input.prices.df[!is.na(input.prices.df[, i]), i] <- rlnorm(sum(!is.na(input.prices.df[, i])))
}
    


# "Avena berza"       "Avena grano"

prod01.df$department.caps<-toupper(prod01.df$department)

prod01.df<-merge(prod01.df, input.prices.df, by.x=c("department.caps", "crop.r"), by.y=c("department", "crop"), all.x=TRUE)

# see below - we do not have Mani and Naranja in the operation below
misc.input.prices.ls<-by(input.prices.df, INDICES=list(input.prices.df$department), FUN=function(x) {
  x<-x[ x$crop %in% c("PAPA", "MAIZ", "HABA", "TRIGO", "CEBADA", "ARROZ", 
    "YUCA", "OCA", "PLATANO", "CEBOLLA", "QUINUA", "ARVEJA", "AVENA", "MANI", "NARANJA"), !colnames(x) %in% c("department", "crop")]
  
    as.data.frame(lapply(x, FUN=median, na.rm=TRUE))
  }
)





misc.input.prices.df<-do.call(rbind, misc.input.prices.ls)
misc.input.prices.df$department<-names(misc.input.prices.ls)

# TODO: I seem to have taken the median for wages, but mean for other inputs

for ( i in 1:(ncol(misc.input.prices.df)-1)) {
  misc.input.prices.df[is.na(misc.input.prices.df[, i]), i] <- mean(misc.input.prices.df[, i], na.rm=TRUE)
}

#i<-unique(prod01.df$department)[1]
#j<-"seed.price"

for (i in unique(prod01.df$department.caps)) {

  for ( j in c("seed.price", "abono.price", "fert.price.quintal", 
  "fert.price.liter", "plaguicida.price.quintal", "plaguicida.price.liter") ) {
  
    prod01.df[prod01.df$department.caps==i, colnames(prod01.df)==j][
      is.na(prod01.df[prod01.df$department.caps==i, colnames(prod01.df)==j]) ] <-
    misc.input.prices.df[misc.input.prices.df$department==i, colnames(misc.input.prices.df)==j]

  }
  
}



hh.input.prices.ls<-by(prod01.df, INDICES=list(prod01.df$FOLIO), FUN=function(x) {

  as.data.frame(lapply(x[, colnames(x) %in% c("seed.price", "abono.price", "fert.price.quintal", 
    "fert.price.liter", "plaguicida.price.quintal", "plaguicida.price.liter")]  , 
     FUN=function(y) {sum(y*x$area.r)/sum(x$area.r)} ))
  }
)

hh.input.prices.df<-do.call(rbind, hh.input.prices.ls)
hh.input.prices.df$FOLIO<-rownames(hh.input.prices.df)

# Deflate prices to 2000 
# TODO: do we want 2000 or 2001 prices?

hh.input.prices.df$seed.price <- hh.input.prices.df$seed.price * (192.73/265.32)
hh.input.prices.df$abono.price <- hh.input.prices.df$abono.price * (192.73/265.32)
hh.input.prices.df$plaguicida.price.quintal <- hh.input.prices.df$plaguicida.price.quintal * (192.73/265.32)
hh.input.prices.df$plaguicida.price.liter <- hh.input.prices.df$plaguicida.price.liter * (192.73/265.32)
hh.input.prices.df$fert.price.quintal <- hh.input.prices.df$fert.price.quintal * (0.335527868 * 6.193302093) / (0.416741871 * 7.794167074)
hh.input.prices.df$fert.price.liter <- hh.input.prices.df$fert.price.liter * (0.335527868 * 6.193302093) / (0.416741871 * 7.794167074)



land.agg<-aggregate(prod01.df$area.r, by=list(FOLIO=prod01.df$FOLIO), FUN=sum, na.rm=TRUE); colnames(land.agg)[2] <- "land.area"

#colnames(crop.wide.df)[colnames(crop.wide.df)=="PERSAGRO"] <- "num.pers.agropecuaria"
#colnames(crop.wide.df)[384] <- "num.pers.agropecuaria"

firm.df <- crop.wide.df[, c("FOLIO", "fert.exp", "seed.exp", "manure.exp", "pesticide.exp", 
  "hired.labor.exp", "imputed.ag.wage",  "num.pers.agropecuaria" )]
# "crop.labor", "crop.and.livestick.labor",
#c("FOLIO", "fert.exp", "seed.exp", "manure.exp", "pesticide.exp", 
#  "hired.labor.exp", "imputed.ag.wage", "crop.labor", "crop.and.livestick.labor", #"num.pers.agropecuaria" ) %in% names(crop.wide.df)

firm.df <- crop.wide.df
# TODO: we would have to fix this (i.e. change to getting all the columns) on the non-sanity-check 


  
firm.df<-data.frame(lapply(firm.df, FUN=function(x) {
	x[!is.finite(x)]<-0 
	x
	}))


firm.df <- merge(firm.df, land.agg)
firm.df <- merge(firm.df, hh.input.prices.df)

firm.df<-data.frame(lapply(firm.df, FUN=function(x) {
	x[!is.finite(x)]<-0 
	x
	}))

#firm.df$labor.hours <- firm.df$hired.labor.exp / firm.df$imputed.ag.wage +
#  (firm.df$crop.labor + firm.df$crop.and.livestick.labor) * (4.345 * 6)
# 6 month growing season,and 4.3 weeks in a month

# Below is for the new way (hired labor only)
firm.df$labor.hours <- firm.df$hired.labor.exp / firm.df$imputed.ag.wage 

# PERSAGRO
  
firm.df$fert.quintals <- firm.df$fert.exp / firm.df$fert.price.quintal
firm.df$plaguicida.liters <- firm.df$pesticide.exp / firm.df$plaguicida.price.liter
firm.df$seed.quintals <- firm.df$seed.exp / firm.df$seed.price
firm.df$abono.quintals <- firm.df$manure.exp / firm.df$abono.price
