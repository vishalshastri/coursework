
# DATA PREPARATION


prod01.df<-read.spss(paste0(work.dir, "bd18 (2001).zip Folder/agricola.sav"), to.data.frame = TRUE)
hogar01.df<-read.spss(paste0(work.dir, "bd18 (2001).zip Folder/hogar.sav"), to.data.frame = TRUE)
miembros01.df<-read.spss(paste0(work.dir, "bd18 (2001).zip Folder/mcv01.sav"), to.data.frame = TRUE)
pecuaria01.df<-read.spss(paste0(work.dir, "bd18 (2001).zip Folder/pecuaria.sav"), to.data.frame = TRUE)
#pob01.df<-read.spss(paste0(work.dir, "bd18 (2001).zip Folder/poblacion.sav"), to.data.frame = TRUE)
# hogar01.df.labels<-read.spss(paste0(work.dir, "bd18 (2001).zip Folder/agricola.sav"), to.data.frame = F)

#attr(hogar01.df, "variable.labels")

#attr(hogar01.df, "variable.labels")
#attr(miembros01.df, "variable.labels")[grepl("ocup", attr(miembros01.df, "variable.labels"), ignore.case=TRUE)]

# "bd" = bienes durados = consumer durables?

#miembros01.df[10:50, c("FOLIO", "REMPAIS", "REMEXT", "S703B1", "S703B2", "S703C1", "S703C2")]


table(miembros01.df$S632)



# START GEOG WORK

hogar01.df$Dept.no<-""
hogar01.df$Dept.no[hogar01.df$ID01=="Santa cruz"]<-"07"
hogar01.df$Dept.no[hogar01.df$ID01=="Beni"]<-"08"
hogar01.df$Dept.no[hogar01.df$ID01=="Chuquisaca"]<-"01"
hogar01.df$Dept.no[hogar01.df$ID01=="La Paz"]<-"02"
hogar01.df$Dept.no[hogar01.df$ID01=="Oruro"]<-"04"
hogar01.df$Dept.no[hogar01.df$ID01=="Tarija"]<-"06"
hogar01.df$Dept.no[hogar01.df$ID01=="Potosi"]<-"05"
hogar01.df$Dept.no[hogar01.df$ID01=="Cochabamba"]<-"03"
hogar01.df$Dept.no[hogar01.df$ID01=="Pando"]<-"09"

hogar01.df.ids<-with(hogar01.df,
										 paste(
										 	Dept.no, # 2
										 	sprintf("%02.f", ID02), # 4
										 	sprintf("%02.f", ID03), # 6
										 	sprintf("%02.f", ID04), # 8
										 	sprintf("%03.f", ID05), # 11
#										 	sprintf("%03.f", ID06),
										 	sep="")
)


#table(hogar01.df.ids %in% substr(pob.shp$COD_BD_CEN, 1, 14))


pob.shp<-importShapefile(paste0(work.dir, "centros_poblados.zip Folder/centros_poblados.shp"))



combine.localidades<-function(x) {
  ret<-x[1, ]
  ret$num.of.localidades<-nrow(x)
  ret$X <- sum(x$X * x$VIVIENDA)/sum(x$VIVIENDA)
  ret$Y <- sum(x$Y * x$VIVIENDA)/sum(x$VIVIENDA)
  ret$LOCALIDAD<-"combined localidades"
  ret$N_ZONLOC<-"combined localidades"
  ret
}

# combine.localidades(pob.shp.rev[substr(pob.shp$COD_BD_CEN, 1, 11)=="01070301002", ])
# pob.shp[substr(pob.shp$COD_BD_CEN, 1, 11)=="01070301002", c("X", "Y")]
# plot(pob.shp[substr(pob.shp$COD_BD_CEN, 1, 11)=="01070301002", c("X", "Y")])
# points(pob.shp.rev[substr(pob.shp.rev$COD_BD_CEN, 1, 11)=="01070301002", c("X", "Y")], col="red")
# points(as.data.frame(lapply(pob.shp[substr(pob.shp$COD_BD_CEN, 1, 11)=="01070301002", c("X", "Y")], 2, FUN=mean)), col="blue")

dup.indices<- duplicated(substr(pob.shp$COD_BD_CEN, 1, 11)) |
  duplicated(substr(pob.shp$COD_BD_CEN, 1, 11), fromLast=TRUE)

combined.localidades.ls<-by(pob.shp[dup.indices, ], INDICES=substr(pob.shp$COD_BD_CEN[dup.indices], 1, 11), FUN=combine.localidades)

combined.localidades.df<-do.call(rbind, combined.localidades.ls)

pob.shp.rev <- rbind(combined.localidades.df,
  cbind(pob.shp[!dup.indices, ], data.frame(num.of.localidades=rep(1, nrow(pob.shp[!dup.indices, ]))))
)

pob.shp.rev$comunidad.id<-substr(pob.shp.rev$COD_BD_CEN, 1, 11)
hogar01.df$comunidad.id <- hogar01.df.ids

hogar01.df<-merge(hogar01.df, pob.shp.rev, by="comunidad.id", all.x=TRUE)

#hist(combined.localidades.df$num.of.localidades)




#attr(prod01.df, "label.table")

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



crop.wide.df<-reshape(prod01.df[ prod01.df$crop %in% names(rev(sort(table(prod01.df$crop))))[1:12], 
	!names(prod01.df) %in% c("CODA", "area", "crop.code", "harvest", "sales.quant", "consumption", "seeds", "animal.or.subproducts", "bartered", "lost")], timevar="crop", idvar="FOLIO", direction="wide")

livestock.wide.df<-reshape(pecuaria01.df[, c("FOLIO", "CODB", "S806")], 
	timevar="CODB", idvar="FOLIO", direction="wide")

names(livestock.wide.df) <- c("FOLIO", "Bovinos", "Ovinos", "Porcinos", "Caprinos", "Camelidos", "Aves", "Conejos.cuis")

crop.wide.df<-merge(crop.wide.df, livestock.wide.df, all.x=TRUE)


hogar01.df.temp<-hogar01.df[, c("FOLIO", "S815D2", "S815B2", "S1036", "S1037", "PERSAGRO", "ID05", "ID06", "ID01", "X", "Y", "CAT_LOC", "num.of.localidades", "comunidad.id", "S815A2", "S815C2", "S815E2", "S815F2", "S815G2", "S815H2", "S815I2", "S815N2", "S1032A", "S1033", "S1034A")]

#hogar01.df.temp<-hogar01.df[, c("FOLIO", "S815D2", "S815B2", "S1036", "S1037", "PERSAGRO", "ID05", "ID06", "ID01", "S815A2", "S815C2", "S815E2", "S815F2", "S815G2", "S815H2", "S815I2", "S815N2", "S1032A", "S1033", "S1034A")]



names(hogar01.df.temp)<-c("FOLIO", "fert.exp", "seed.exp", "received.credit", "credit.source", "num.pers.agropecuaria", "city", "canton", "department", "X", "Y", "CAT_LOC", "num.of.localidades", "comunidad.id", "hired.labor.exp", "manure.exp", "transport.exp", "pesticide.exp", "extension.exp", "machine.exp", "draft.anmial.exp", "other.exp", "hh.member.incident",  "hh.member.incident.income", "hogar.incident")
# names(hogar01.df.temp)<-c("FOLIO", "fert.exp", "seed.exp", "received.credit", "credit.source", "num.pers.agropecuaria", "city", "canton", "department", "hired.labor.exp", "manure.exp", "transport.exp", "pesticide.exp", "extension.exp", "machine.exp", "draft.anmial.exp", "other.exp", "hh.member.incident",  "hh.member.incident.income", "hogar.incident")

hogar01.df.temp$city <- as.factor(hogar01.df.temp$city)
hogar01.df.temp$canton <- as.factor(hogar01.df.temp$canton)
hogar01.df.temp$hh.member.incident <- relevel(hogar01.df.temp$hh.member.incident, ref="Ninguna")
hogar01.df.temp$hogar.incident <- relevel(hogar01.df.temp$hogar.incident, ref="Ninguna")
hogar01.df.temp$hh.member.incident.income <- as.character(hogar01.df.temp$hh.member.incident.income)


hogar01.df.temp$hh.member.incident.income[
  hogar01.df.temp$hh.member.incident.income=="No" | is.na(hogar01.df.temp$hh.member.incident.income)
] <-0

hogar01.df.temp$hh.member.incident.income[
  hogar01.df.temp$hh.member.incident.income=="Si" 
] <-1

hogar01.df.temp$hh.member.incident.income<-as.numeric(hogar01.df.temp$hh.member.incident.income)



area.agg<-aggregate(prod01.df$area.r, by=list(FOLIO=prod01.df$FOLIO), FUN=sum, na.rm=TRUE)
names(area.agg)[2]<-"total.area"

area.agg<-aggregate(prod01.df$area.r, by=list(FOLIO=prod01.df$FOLIO), FUN=sum, na.rm=TRUE)
names(area.agg)[2]<-"total.area"

crop.wide.df<-merge(crop.wide.df, hogar01.df.temp)
crop.wide.df<-merge(crop.wide.df, area.agg)


miembros01.df<-within(miembros01.df, {
  crop.labor <- rowSums( matrix(c(S624A * S624HRS * Z613ACTI %in% c("111", "113", "0111",  "0112",  "0113"), 
                         S638A * S638HRS * Z634ACTI %in% c("111", "113", "0111",  "0112",  "0113")), ncol=2), na.rm=TRUE)
  crop.and.livestick.labor<- rowSums( matrix(c(S624A * S624HRS * Z613ACTI %in% c("130", "0130"), 
                                      S638A * S638HRS * Z634ACTI %in% c("130", "0130")), ncol=2), na.rm=TRUE)
  }
)


# START FIDDLING


#empresa labor:


self.emp.prim <- miembros01.df$S617 %in% levels(miembros01.df$S617)[c(3:5,7,8)]
self.emp.sec <- miembros01.df$S617 %in% levels(miembros01.df$S635)[c(3:5,7,8)]



miembros01.df<-within(miembros01.df, {
  crop.labor <- rowSums( matrix(c(S624A * (S624HRS + S624MIN/60) * self.emp.prim * (Z613ACTI %in% c("111", "113", "0111", "0112",  "0113")),
                     S638A * (S638HRS + S638MIN/60) * self.emp.sec * (Z634ACTI %in% c("111", "113", "0111",  "0112",  "0113"))), ncol=2), na.rm=TRUE)
  crop.and.livestick.labor<- rowSums( matrix(c(S624A * (S624HRS + S624MIN/60) * self.emp.prim * (Z613ACTI %in% c("130", "0130")),
                        S638A * (S638HRS + S638MIN/60) * self.emp.sec * (Z634ACTI %in% c("130", "0130"))), ncol=2), na.rm=TRUE)
  }
)

# STOP FIDDLING


# most likely http://unstats.un.org/unsd/cr/registry/regcs.asp?Cl=2&Lg=1&Co=01 due to http://books.google.com/books?id=8JmsM599rEQC&pg=PA240&lpg=PA240&dq=bolivia+industrial+classification+system&source=bl&ots=zCQedY0QKq&sig=zqzEg7yFFoCBoAWIRzCWYv_VoEA&hl=en&sa=X&ei=ahTGUaUGhuvSAdHNgJgC&ved=0CCsQ6AEwAA#v=onepage&q=bolivia%20industrial%20classification%20system&f=false

labor.df <- miembros01.df[, c("FOLIO", "crop.labor", "crop.and.livestick.labor")]

labor.df <- aggregate(miembros01.df[, c("crop.labor", "crop.and.livestick.labor")], by=list(FOLIO=miembros01.df[, "FOLIO"]), FUN=sum, na.rm=TRUE)

# TODO: did not consider fact that days per week or hours per day may be missing
# TODO: also did not consider the domestic work data like "actividad e) cria de animales y/o cultivos"

crop.wide.df<-merge(crop.wide.df, labor.df)


# table(miembros01.df$S115, miembros01.df$S111A, useNA="always")




indig.1.ls<-by(miembros01.df$S111A, INDICES=miembros01.df$FOLIO, FUN=function(x) {
	indig.tab <- table(x)
	sum(indig.tab[names(indig.tab)!="ninguno"])/sum(indig.tab)
	}
)




indig.2.ls<-by(miembros01.df$S115, INDICES=miembros01.df$FOLIO, FUN=function(x) {
	indig.tab <- table(x)
	if (sum(indig.tab)==0 | sum(indig.tab)==indig.tab[names(indig.tab)=="nunca o casi nunca"]) { 
	  ret <- 0
	} else {
	  ret <- indig.tab[names(indig.tab)=="si, algunas veces"] + 
	    2 * indig.tab[names(indig.tab)=="si habitualmente"]
	  ret <- ret/sum(indig.tab)
	}
	ret
	}
)

indig.3.ls<-by(miembros01.df$S107A, INDICES=miembros01.df$FOLIO, FUN=function(x) {
	indig.tab <- table(x)
	indig.tab[names(indig.tab) == "no puede hablar" ] <- 0
	sum(indig.tab[! names(indig.tab) %in% c("extranjero", "castellano")])/sum(indig.tab)
	}
)


indig.df <- data.frame(FOLIO=attr(indig.1.ls, "dimnames")[[1]], indig.prop=unclass(unlist(indig.1.ls)), indig.practices=unclass(unlist(indig.2.ls)), indig.lang=unclass(unlist(indig.3.ls)))


hh.head.char.df<-miembros01.df[miembros01.df$S105=="jefe o jefa del hogar", c("FOLIO", "S402A", "S102", "S401", "S103")]
names(hh.head.char.df) <- c("FOLIO", "hhh.edu.measure", "hhh.sex", "hhh.literacy", "hhh.age")
#apply(miembros01.df[, c("S401", "S402A", "S402B", "S403A", "S403B")], 2, FUN=table)


remesas.df<-aggregate(miembros01.df[, c("REMPAIS", "REMEXT")], by=list(FOLIO=miembros01.df$FOLIO), FUN=sum, na.rm=TRUE )

crop.wide.df<-merge(crop.wide.df, indig.df)
crop.wide.df<-merge(crop.wide.df, hh.head.char.df, all.x=TRUE)
crop.wide.df<-merge(crop.wide.df, remesas.df, all.x=TRUE)

levels(crop.wide.df$credit.source)<-c("No Credit", levels(crop.wide.df$credit.source))
crop.wide.df$credit.source[is.na(crop.wide.df$credit.source)]<-factor("No Credit")
# This above is a hack and I need to learn how to work with factors better
	
crop.wide.df<-data.frame(lapply(crop.wide.df, FUN=function(x) {
	x[is.na(x)]<-0 
	x
	}))

crop.wide.df<-data.frame(lapply(crop.wide.df, FUN=function(x) {
	levels(x)<-gsub("[^0-9A-Za-z[:punct:][:blank:]]", "", levels(x))
	x
}))



pob.shp$comunidad.id<-substr(pob.shp$COD_BD_CEN, 1, 11)

village.geog.df<-pob.shp[pob.shp$comunidad.id %in% hogar01.df.ids, c("EID", "comunidad.id", "X", "Y", "VIVIENDA")]



#soil.shp<-importShapefile(paste0(work.dir, "DSMW/DSMW.shp"))

#soil.bil<- readGDAL(paste0(work.dir, "HWSD_RASTER/hwsd.bil"))
#save(file=paste0( work.dir, "HWSD_RASTER/hwsd.Rdata"),soil.bil)
load(file=paste0( work.dir, "HWSD_RASTER/hwsd.Rdata"))

soil.mdb<-mdb.get(paste0("'", work.dir, "HWSD.mdb'"))



villages.spatialpixels<-SpatialPixels(
  SpatialPoints(as.data.frame(village.geog.df[, c("X", "Y")])),
  grid=soil.bil@grid)

# max(crop.wide.df$fert.exp)!=crop.wide.df$fert.exp

village.geog.df$MU.GLOBAL<-soil.bil$band1[villages.spatialpixels@grid.index]

village.geog.df<-merge(village.geog.df, soil.mdb$HWSD_DATA[soil.mdb$HWSD_DATA$SEQ==1, ], all.x=TRUE)

rm(soil.bil)
rm(soil.mdb)
gc()


riego.asc <- readGDAL(paste0(work.dir, "gmia_v4_0_1_pct.asc"))

villages.spatialpixels<-SpatialPixels(
  SpatialPoints(as.data.frame(village.geog.df[, c("X", "Y")])),
  grid=riego.asc@grid)

village.geog.df$riego.pct<-riego.asc$band1[villages.spatialpixels@grid.index]
village.geog.df$riego.pct[is.na(village.geog.df$riego.pct)] <- 0

rm(riego.asc)
gc()

# TODO: irrigation data is in percent of cell that is equipped for irrigation, but we may want to have another measure, like percent of cell divided by number of people or hectares under production

elev.dem1<-readGDAL(paste0(work.dir, "w060s10/W060S10.DEM"))
elev.dem2<-readGDAL(paste0(work.dir, "w100s10/W100S10.DEM"))

#hist(elev.dem2$band1)
#image(elev.dem2["band1"])
max(village.geog.df$Y[village.geog.df$Y!=0])
# Since this is south of -10 latitude, we don;t have to download additional elevation panels
# TODO: Ok, now the max is actually -9.704484 and we are missing two villages, so we need to fix this

keep.coords.1<-point.in.polygon(village.geog.df$X , village.geog.df$Y, bboxx(bbox(elev.dem1))[, 1], bboxx(bbox(elev.dem1))[, 2])==1

villages.elev.1.spatialpixels<-SpatialPixels(
  SpatialPoints(as.data.frame(village.geog.df[keep.coords.1, c("X", "Y")])),
  grid=elev.dem1@grid)

elevation.df<-data.frame(EID=village.geog.df[keep.coords.1, "EID"],
  elevation=elev.dem1$band1[villages.elev.1.spatialpixels@grid.index], stringsAsFactors=FALSE)

keep.coords.2<-point.in.polygon(village.geog.df$X , village.geog.df$Y, bboxx(bbox(elev.dem2))[, 1], bboxx(bbox(elev.dem2))[, 2])==1

villages.elev.2.spatialpixels<-SpatialPixels(
  SpatialPoints(as.data.frame(village.geog.df[keep.coords.2, c("X", "Y")])),
  grid=elev.dem2@grid)
  
elevation.df<-rbind(elevation.df,
  data.frame(EID=village.geog.df[keep.coords.2, "EID"],
    elevation=elev.dem2$band1[villages.elev.2.spatialpixels@grid.index], stringsAsFactors=FALSE)
  )


village.geog.df <- merge(village.geog.df, elevation.df, all.x=TRUE)

rm(elev.dem1)
rm(elev.dem2)
gc()


with(village.geog.df[village.geog.df$Y!=0, ], plot(X, Y, col=rgb(red=elevation, green=0, blue=0, alpha=max(elevation, na.rm=TRUE), maxColorValue=max(elevation, na.rm=TRUE))))



system.time(
  villages.to.roads.ls <- apply(t(as.matrix(village.geog.df[, c("comunidad.id", "X", "Y")])), 2, function(center) {
#    cat(date(), "\n")
#    print(center)
    min.index<- which.min(
      colSums((t(as.matrix(roads.shp[, c("X", "Y")])) - as.numeric(center[ c("X", "Y")]))^2)^.5
    )
    
    data.frame(
      dist.to.road=colSums((t(as.matrix(roads.shp[min.index, c("X", "Y")])) - as.numeric(center[ c("X", "Y")]))^2)^.5,
      X.on.road = roads.shp$X[min.index],
      Y.on.road = roads.shp$Y[min.index]
    )
})
)

gc()

villages.to.roads.df<-do.call(rbind, villages.to.roads.ls)
village.geog.df<-cbind(village.geog.df, villages.to.roads.df)

system.time(
  cities.to.roads.ls <- apply(t(as.matrix(
    pob.shp[pob.shp$CAT_LOC %in% c("0-Capital", "1-Urbana", "2-Amanzanada"),
      c( "EID", "X", "Y")])), 2, function(center) {
#    cat(date(), "\n")
#    print(center)
    min.index<- which.min(
      colSums((t(as.matrix(roads.shp[, c("X", "Y")])) - center[c("X", "Y")])^2)^.5
    )
    
    data.frame(
      EID=center["EID"],
      dist.to.road=colSums((t(as.matrix(roads.shp[min.index, c("X", "Y")])) - center[c("X", "Y")])^2)^.5,
      X.on.road = roads.shp$X[min.index],
      Y.on.road = roads.shp$Y[min.index]
    )
})
)

cities.to.roads.df<-do.call(rbind, cities.to.roads.ls)

pob.shp.on.roads<-pob.shp

pob.shp.on.roads<-merge(pob.shp.on.roads, cities.to.roads.df)





# table(pob.shp$CAT_LOC)
# plot(pob.shp[pob.shp$CAT_LOC %in% ("2-Amanzanada"), c("X", "Y")])

#villages.df<-crop.wide.df[ !duplicated(crop.wide.df$comunidad.id) & crop.wide.df$X!=0, c("comunidad.id", "X", "Y")]

drive.time.data.ls<-vector(mode="list", length = nrow(villages.df))

gather.mapquest<-TRUE

for (village in 1:nrow(village.geog.df)) {

  if(!gather.mapquest) {break}

  target.village<-village.geog.df[village, c("X.on.road", "Y.on.road")]
  time.calcs.final.cols <- c()

  for (settlement.type in list(c("0-Capital", "1-Urbana"), "2-Amanzanada")) {
  
    if ( pob.shp$CAT_LOC[pob.shp$EID==village.geog.df$EID[village]] %in% settlement.type ) {
      time.calcs.final.cols <- c(time.calcs.final.cols, 0)
      next
    }
    
  
    urban.coords<-pob.shp.on.roads[pob.shp.on.roads$CAT_LOC %in% settlement.type, c("X.on.road", "Y.on.road")]
    
    target.knn<-knearneigh(as.matrix(rbind(target.village, urban.coords ) ), k=96,  longlat = TRUE)
    target.knn<-knn2nb(target.knn)[[1]] - 1
    
    time.calcs<-c()
    time.calcs.final.v<-c()
    
#    for (coord.chunk in list(1:24, 25:48, 49:72, 73:96)) {
    for (coord.chunk in list(1:24)) {
    
      target.coords<-rbind(target.village, urban.coords[target.knn[coord.chunk], ] )
      
      for (direction.option in c("false", "true")) {
      
        json.req<-toJSON(list(locations=paste(target.coords$Y.on.road, target.coords$X.on.road, sep="," ),
          options=list(allToAll="false", manyToOne=direction.option)))
          
        post.receipt<-POST(paste0("http://www.mapquestapi.com/directions/v1/routematrix?key=", 
          mapquest.api.key), config=accept_json(), body=json.req)
          
        time.calcs<-c(time.calcs, content(post.receipt)$time[-1])
        
       }
       
       time.calcs[ time.calcs==0] <- 99999999999
       
       time.calcs.final.v<- c(time.calcs.final.v, unlist(time.calcs[1:24] + time.calcs[25:48]))
       
    }
    
    time.calcs.final.cols <- c(time.calcs.final.cols, min(time.calcs.final.v))
    
  }
  
  drive.time.data.ls[[village]]<-data.frame(comunidad.id=village.geog.df$comunidad.id[village], 
    drive.time.urban=time.calcs.final.cols[1], drive.time.amanzanada=time.calcs.final.cols[2],
    stringsAsFactors=FALSE)
    
  cat(village.geog.df$comunidad.id[village], date(), content(post.receipt)$info$messages, sep="\n")
    

}


# Unable to calculate route matrix. - what does this mean?
#"Unable to use location #1 :Must have a valid GEFID." means too far away from a road
#"100 Unable to use location #15 :Must have a valid GEFID."
#"100 Unable to use location #0 :Must have a valid GEFID."
#manyToOne gives the direction that we are going in, i.e. to city or to village

#names(rev(sort(table(test))))[1]



drive.time.df<-do.call(rbind, drive.time.data.ls)
drive.time.df$drive.time.urban[drive.time.df$drive.time.urban>99999999998]<-NA
drive.time.df$drive.time.amanzanada[drive.time.df$drive.time.amanzanada>99999999998]<-NA



village.geog.df.f<-cbind(village.geog.df, drive.time.df[, c("drive.time.urban", "drive.time.amanzanada")] )




precip.coords<-read.fortran(paste0(work.dir, "Global2011P/precip.2000"), 
  format=c("F8.3", "F8.3", "12F8.1"))[, c(1,2)]
  
precip.coords[,1]<-precip.coords[,1]*1000
precip.coords[,2]<-precip.coords[,2]*1000



precip.ls<-list()

for ( i in as.character(1996:2001)) {
  precip.ls[[i]]<-read.fortran(paste0(work.dir, "Global2011P/precip.", i), 
    format=c("F8.3", "F8.3", "12F8.1"))[, -c(1,2)]
    
}

#TODO: REALLY UNCLEAR ABOUT THE SCALE of rainfall values. MAY HAVE TO MULTIPLY THesE VALUES TO GET RIGHT SCALE. http://climate.geog.udel.edu/~climate/html_pages/Global2011/Precip_revised_3.02/README.GlobalTsP2011.html

precip.years<-as.character(1996:2001)

precip.disag.ls<-list()

for ( i in 2:length(precip.years) ) {

  precip.temp.1<-precip.ls[[precip.years[i-1]]]
  precip.temp.2<-precip.ls[[precip.years[i]]]
  precip.disag.ls[[ paste0("rain.grow.season.", precip.years[i])]] <- 
    rowSums(precip.temp.1[, 10:12]) + rowSums(precip.temp.1[, 1:4])
}


precip.disag.df<-as.data.frame(do.call(cbind, precip.disag.ls))

precip.df<-do.call(cbind, precip.ls[as.character(1996:2000)])

precip.df<-rowSums(precip.df)/5

#all(precip.ls[[1]][,1]==precip.ls[[2]][,1] &
#  precip.ls[[1]][,1]==precip.ls[[3]][,1] &
#  precip.ls[[1]][,1]==precip.ls[[4]][,1] &
#  precip.ls[[1]][,1]==precip.ls[[5]][,1] 
#)

#all(precip.ls[[1]][,2]==precip.ls[[2]][,2] &
#  precip.ls[[1]][,2]==precip.ls[[3]][,2] &
#  precip.ls[[1]][,2]==precip.ls[[4]][,2] &
#  precip.ls[[1]][,2]==precip.ls[[5]][,2] 
#)
# This test is passed


rain.grid<-GridTopology(cellcentre.offset=c(-179.75, -89.75), cellsize=c(.5, .5), cells.dim=c(4*2*180, 4*2*90))

rain.pixels <- SpatialPixels(SpatialPoints(precip.coords) , grid=rain.grid )

villages.spatialpixels.rain<-SpatialPixels(
  SpatialPoints(as.data.frame(village.geog.df.f[, c("X", "Y")])),
  grid=rain.grid)

village.geog.df.f$rain.grid.index<-villages.spatialpixels.rain@grid.index

precip.df<-cbind(
  data.frame(rain.grid.index=rain.pixels@grid.index, mean.ann.rain.5yr=precip.df),
  precip.disag.df
)




village.geog.df.f<-merge(village.geog.df.f, precip.df, all.x=TRUE)

village.geog.df.f$rain.grid.index<-NULL




village.geog.ls <- by(village.geog.df.f[, !names(village.geog.df.f) %in% c("EID", "MU.GLOBAL", "comunidad.id", "X", "Y", "ID")],
  INDICES=village.geog.df.f$comunidad.id, FUN=function(x) {
  vivienda.weights <- x$VIVIENDA
  data.frame(
    lapply(x, FUN = function(x) {
     if (all(is.na(x))) return(NA)
     if (!is.numeric(x)) return( names(rev(sort(table(x))))[1] )
     else return( sum(x[!is.na(x)] * vivienda.weights[!is.na(x)])/sum(vivienda.weights[!is.na(x)]) )
    } )
  )
} )



village.geog.df.f<-do.call(rbind, village.geog.ls)

village.geog.df.f$comunidad.id<-names(village.geog.ls)

intersect(names(village.geog.df.f), names(crop.wide.df))


crop.wide.df<-merge(crop.wide.df, village.geog.df.f, all.x=TRUE)

# TODO
length(village.geog.ls )
[1] 348
why?, when
length(unique(crop.wide.df$comunidad.id))
[1] 320





# TODO: wut:




save(crop.wide.df, file=paste0(work.dir, "crop wide df2.Rdata"))
#load(file=paste0(work.dir, "crop wide df2.Rdata"))

crop.wide.df$credit.source<-factor(crop.wide.df$credit.source)
crop.wide.df$hhh.edu.measure<-factor(crop.wide.df$hhh.edu.measure)

crop.wide.df[] <- lapply(crop.wide.df,function(x) if(is.factor(x)) factor(x) else x)



















# IMPUTATION OF PRICES



#START NEW DATA


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



prod01.df$crop.r<-prod01.df$crop



cropname.corrections<-as.matrix( read.csv(paste0(work.dir, "unclean cropnames01 corrections.csv"), header=FALSE, stringsAsFactors=FALSE, encoding = "UTF-8") )

# MAKE SURE TO ENCODE THE csv in EXCEL AS csv (MS-DOS)

prod01.df$crop.name.cleaned<-FALSE

for (i in 1:nrow(cropname.corrections)) {
  if ( (sum(cropname.corrections[i, ]=="")+1)==ncol(cropname.corrections)) { next }
  clean.index<-prod01.df$crop.r %in% cropname.corrections[i, -1]
  prod01.df$crop.r[clean.index] <- cropname.corrections[i, 1]
  prod01.df$crop.name.cleaned[clean.index] <- TRUE
}
  

cumsum(rev(sort(table(prod01.df$crop.r))))/length(prod01.df$crop.r)





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

# 228 records could not be imputed
  
# TODO: 4 NA's in prod01.df$total.value - what to do?
# damn, this thing is so log-normal: hist(log(prod01.df$total.value))

save(prod01.df,  file=paste0(work.dir, "prod01.df imputed prices.Rdata"))







input.prices.df<-read.delim(paste0(work.dir, "2008 input prices.txt"), stringsAsFactors=FALSE, na.strings="-", dec=",")
lapply(test, FUN=summary)

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

prod01.df$department.caps<-toupper(prod01.df$department)

prod01.df<-merge(prod01.df, input.prices.df, by.x=c("department.caps", "crop.r"), by.y=c("department", "crop"), all.x=TRUE)

misc.input.prices.ls<-by(input.prices.df, INDICES=list(input.prices.df$department), FUN=function(x) {
  x<-x[ x$crop %in% c("PAPA", "MAIZ", "HABA", "TRIGO", "CEBADA", "ARROZ", 
    "YUCA", "OCA", "PLATANO", "CEBOLLA", "QUINUA", "ARVEJA"), !colnames(x) %in% c("department", "crop")]
  
    as.data.frame(lapply(x, FUN=median, na.rm=TRUE))
  }
)

misc.input.prices.df<-do.call(rbind, misc.input.prices.ls)
misc.input.prices.df$department<-names(misc.input.prices.ls)

for ( i in 1:ncol(misc.input.prices.df)) {
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

hh.input.prices.df$seed.price <- hh.input.prices.df$seed.price * (192.73/265.32)
hh.input.prices.df$abono.price <- hh.input.prices.df$abono.price * (192.73/265.32)
hh.input.prices.df$plaguicida.price.quintal <- hh.input.prices.df$plaguicida.price.quintal * (192.73/265.32)
hh.input.prices.df$plaguicida.price.liter <- hh.input.prices.df$plaguicida.price.liter * (192.73/265.32)
hh.input.prices.df$fert.price.quintal <- hh.input.prices.df$fert.price.quintal * (0.335527868 * 6.193302093) / (0.416741871 * 7.794167074)
hh.input.prices.df$fert.price.liter <- hh.input.prices.df$fert.price.liter * (0.335527868 * 6.193302093) / (0.416741871 * 7.794167074)


# 2007 agroindutry PPI: 265.32
# 2000 agroindutry PPI: 192.73
# http://www.ine.gob.bo/indice/visualizador.aspx?ah=PC4070402.HTM

# 2007 Bolivianos to USD: 7.794167074
# 2000 Bolivianos to USD: 6.193302093
# IMF WEO

# 2007 price of fertilizer per kg in dollars: 0.416741871
# 2000 price of fertilizer per kg in dollars: 0.335527868





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
    crop.wide.df$imputed.ag.wage[i] <- median(impute.aux[match.index])
    crop.wide.df$ag.wages.impute.level[i]<-impute.level
    crop.wide.df$ag.wages.impute.sample.size[i] <- impute.sample.size
    break
  }
  
}
  
}




land.agg<-aggregate(prod01.df$area.r, by=list(FOLIO=prod01.df$FOLIO), FUN=sum, na.rm=TRUE)
colnames(land.agg)[2] <- "land.area"

firm.df <- crop.wide.df[, c("FOLIO", "fert.exp", "seed.exp", "manure.exp", "pesticide.exp", 
  "hired.labor.exp", "imputed.ag.wage", "crop.labor", "crop.and.livestick.labor")]
  
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

firm.df$labor.hours <- firm.df$hired.labor.exp / firm.df$imputed.ag.wage +
  (firm.df$crop.labor + firm.df$crop.and.livestick.labor) * (4.345 * 6)
# 6 month growing season,and 4.3 weeks in a month
  
firm.df$fert.quintals <- firm.df$fert.exp / firm.df$fert.price.quintal
firm.df$plaguicida.liters <- firm.df$pesticide.exp / firm.df$plaguicida.price.liter
firm.df$seed.quintals <- firm.df$seed.exp / firm.df$seed.price
firm.df$abono.quintals <- firm.df$manure.exp / firm.df$abono.price


crop.price.wide.df<-reshape(prod01.df[ prod01.df$crop %in% names(rev(sort(table(prod01.df$crop))))[1:12], 
	names(prod01.df) %in% c("harvest.r", "price", "crop", "FOLIO" )], timevar="crop", idvar="FOLIO", direction="wide")

crop.price.wide.df<-data.frame(lapply(crop.price.wide.df, FUN=function(x) {
	x[!is.finite(x)]<-0 
	x
	}))


firm.df <- merge(firm.df, crop.price.wide.df)

firm.df<-data.frame(lapply(firm.df, FUN=function(x) {
	x[!is.finite(x)]<-0 
	x
	}))




















#load(file=paste0(work.dir, "prod01.df imputed prices.Rdata"))


crop.value.wide.df<-reshape(prod01.df[ prod01.df$crop %in% names(rev(sort(table(prod01.df$crop))))[1:12], 
	names(prod01.df) %in% c("total.value", "price", "crop", "FOLIO" )], timevar="crop", idvar="FOLIO", direction="wide")

crop.value.wide.df<-data.frame(lapply(crop.value.wide.df, FUN=function(x) {
	x[is.na(x)]<-0 
	x
	}))



#May be able to get land prices from ENA 2008 from renting out land

#how about the labor is just family labor that is "dedicated" to farming




eff.prod.df<-merge( crop.wide.df, crop.value.wide.df)

eff.prod.df<-eff.prod.df[eff.prod.df$total.value.MAIZ!=0 & eff.prod.df$total.value.PAPA!=0, ]

eff.prod.df$labor.input <- eff.prod.df$num.pers.agropecuaria
eff.prod.df$labor.price <- 90 * 7 
# 1 dollars per day times 90 days times 7 bolivinos per USD

eff.prod.df$fertilizer.input <-  eff.prod.df$fert.exp / 334
eff.prod.df$fertilizer.price <- 334
# from page 70 of ENA 2008, maize fert price



[270] "total.value.MAIZ"                       
[271] "price.MAIZ"                             
[272] "total.value.PAPA"                       
[273] "price.PAPA"    

[31,] "p1"         
[32,] "p2"         
[33,] "y1"         
[34,] "y2"         
[35,] "w1"         
[36,] "w2"         
[37,] "w3"         
[38,] "x1"         
[39,] "x2"         
[40,] "x3"  

p1=eff.prod.df$price.MAIZ, p2=price.PAPA, y1=total.value.MAIZ, y2=total.value.PAPA, w1=labor.price, w2=fertilizer.price, x1=labor.input, x2=fertilizer.input


