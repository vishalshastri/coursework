library("foreign")

work.dir <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/"

inputs.df<- read.spss(paste0(work.dir, "bd68/2-AGRICOLA/Cultivos (Preg-19)/2.-ENA08_BOLIVIA_CULTIVOS_PRODUCCION_INSUMOS(preg_19).sav"), to.data.frame=TRUE)

# TODO: Need to figure out this warning message:
# "Unrecognized record type 7, subtype 18 encountered in system file"

colnames(inputs.df) <- tolower( make.names(gsub("[()]|[.]", "", attr(inputs.df, "variable.labels")) ) )




exclude.index <- is.na(inputs.df$x19.abono.cantidad.quintal) | 
  inputs.df$x19.superficie.cultivada.hectareas==0  
# Small exclusion for now
inputs.df <- inputs.df[!exclude.index, ]

# Changing missing price data to zeros:
inputs.df$x19.fertilizante.bs.quintal[is.na(inputs.df$x19.fertilizante.bs.quintal)] <- 0
inputs.df$x19.plagicidas.bs.quintal[is.na(inputs.df$x19.plagicidas.bs.quintal)] <- 0
inputs.df$x19.sem.comprada.bs.quintal[is.na(inputs.df$x19.sem.comprada.bs.quintal)] <- 0
inputs.df$x19.abono.bs.quintal[is.na(inputs.df$x19.abono.bs.quintal)] <- 0

# summary(inputs.df[inputs.df[, "x19.fertilizante.bs.quintal"]>0, "x19.fertilizante.bs.quintal"])


for ( i in grep("quintal", colnames(inputs.df)) ) {
  inputs.df[, gsub("quintal", "kg", colnames(inputs.df)[i]) ] <- inputs.df[, i] / 46
}
# Ooops, before I was doing " * 46"
# p. 18 of ftp://ftp.fao.org/docrep/fao/010/ah868e/ah868e00.pdf
# "One Bolivian arroba is equivalent to 11.5 kg"
# "One Bolivian quintal is equivalent to 46 kg"






geog.df<- read.spss(paste0(work.dir, "bd68/1-UBIGEO PRODUCTOR/1.-ENA08_BOLIVIA_UBIGEO_CONDICION_JURIDICA_SUPERFICIE_UPA(preg_1-17).sav"), to.data.frame=TRUE)

colnames(geog.df) <- tolower( make.names(gsub("[()]|[.]", "", attr(geog.df, "variable.labels")) ) )

geog.df$provincia.full <- with(geog.df, { paste0(departamento, provincia) } )
geog.df$seccion.full <- with(geog.df, { paste0(departamento, provincia, seccion.provincial) } )
geog.df$canton.full <- with(geog.df, { paste0(departamento, provincia, seccion.provincial, canton) } )
geog.df$sector.full <- with(geog.df, { paste0(departamento, provincia, seccion.provincial, canton, x6.sector) } )
geog.df$segmento.full <- with(geog.df, { paste0(departamento, provincia, seccion.provincial, canton, x6.sector, x7.segmento) } )


inputs.df <- merge(inputs.df, geog.df[, c("folio", "provincia.full", 
  "seccion.full", "canton.full", "sector.full", "segmento.full"  )])


# Imputing input prices below

library("compiler")

enableJIT(3)


input.price.columns <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", "x19.plagicidas.bs.kg")

nation.input.averages <- apply(inputs.df[, input.price.columns], 2, FUN=function(x) median(x[x>0]) )

for (target.input in input.price.columns ) {

  inputs.df[, paste0(target.input, ".impute.level")] <- NA
  
  inputs.df[inputs.df[, target.input]!=0, paste0(target.input, ".impute.level")] <- "itself"

impute.levels <- c("household", "segmento.full", "sector.full", "canton.full", "seccion.full", "provincia.full", "departamento", "nation")

# Rprof()

imputed.data.ls<- vector(mode = "list", length = nrow(inputs.df))

for (i in 1:nrow(inputs.df)) {

if (inputs.df[i, paste0(target.input, ".impute.level")] %in% "itself") {next}

for (impute.level in impute.levels) {

  if (impute.level=="nation") { 
    imputed.data.ls[[i]] <- c(unname(nation.input.averages[target.input]), impute.level)
    break
  }
  
  if(impute.level=="household") {
    impute.data <- inputs.df[ inputs.df$folio==i & 
         inputs.df[, paste0(target.input, ".impute.level")] %in% "itself" , target.input]
  } else {
    impute.data <- inputs.df[ inputs.df[, impute.level] == inputs.df[i, impute.level] & 
         inputs.df[, paste0(target.input, ".impute.level")] %in% "itself" , target.input]
  }
  
  if (impute.level=="household" && length(impute.data)>0  ) {
    
#     inputs.df[i, target.input] <- median( impute.data )
#     inputs.df[i, c(target.input, paste0(target.input, ".impute.level") )] <- "household"
      imputed.data.ls[[i]] <- c(median( impute.data ), "household")
#    prod01.df$impute.sample.size[i]<-0
     break
  }
  if (impute.level=="household") {next}
  
  #target.crop<-prod01.df$crop[i]

#  match.index <- inputs.df[, impute.level] == inputs.df[i, impute.level] & 
#    inputs.df[, paste0(target.input, ".impute.level")] %in% "itself"
  
#  impute.sample.size <- sum(match.index)
  
  if (length(impute.data) >= 3) {
#    inputs.df[i, target.input] <- median(impute.data )
#    inputs.df[i, paste0(target.input, ".impute.level")] <- impute.level
#    prod01.df$impute.sample.size[i] <- impute.sample.size
    imputed.data.ls[[i]] <- c(median( impute.data ), impute.level)
    break
  }
  

  
}
 
 cat(i, "\n")
  
}

temp.imputed.df <- data.frame(matrix(unlist(imputed.data.ls), ncol=2, byrow=TRUE), stringsAsFactors=FALSE)
temp.imputed.df[, 1] <- as.numeric(temp.imputed.df[, 1])

inputs.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), target.input] <- 
  temp.imputed.df[, 1]

inputs.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), 
  paste0(target.input, ".impute.level") ] <- temp.imputed.df[, 2]

}


for (k in input.price.columns ) {
  print( table(inputs.df[, paste0(k, ".impute.level")], useNA="always") )
}

# table(inputs.df[, paste0(target.input, ".impute.level")], useNA="always")

# save(inputs.df, file=paste0(work.dir, "inputs df after 1st imputation.Rdata"))










mano.obra.df <- read.spss(paste0(work.dir, "bd68/6-MANO DE OBRA/24.-ENA08_BOLIVIA_MANO_DE_OBRA(preg_98-101).sav"), to.data.frame=TRUE) 


colnames(mano.obra.df) <- tolower( make.names(gsub("[()]|[.]", "", attr(mano.obra.df, "variable.labels")) ) )

mano.obra.df$paid.hours <- mano.obra.df$x1003.hrs.promedio.x.jornal * (
  mano.obra.df$x991.lab.agricolas...hombre...6mes * 9 * 4.345 * 5 +
  mano.obra.df$x991.lab.agricolas...mujer...6mes * 9 * 4.345 * 5 +
  mano.obra.df$x991.lab.agricolas...hombre...6mes * 3 * 4.345 * 5 + 
  mano.obra.df$x991.lab.agricolas...mujer...6mes * 3 * 4.345 * 5 )

# Assumption that less than 6 months means 3 months; greater than 6 months means 9 months,
# and 5 days in a workweek
# 4.345 weeks in a month according to http://www.convertunits.com/from/weeks/to/months

comida.dif <- mean(
  mano.obra.df$x1002.sin.comida[mano.obra.df$x1001.con.comida>0 & mano.obra.df$x1002.sin.comida>0] / 
  mano.obra.df$x1001.con.comida[mano.obra.df$x1001.con.comida>0 & mano.obra.df$x1002.sin.comida>0]
) 

# TODO: as a refinement, this should be geographically linked

mano.obra.df$x1001.con.comida[mano.obra.df$x1001.con.comida>0 & !mano.obra.df$x1002.sin.comida>0] <- comida.dif * 
  mano.obra.df$x1001.con.comida[mano.obra.df$x1001.con.comida>0 & !mano.obra.df$x1002.sin.comida>0] 

mano.obra.df$hourly.wage <- 0

mano.obra.df$hourly.wage[mano.obra.df$x1003.hrs.promedio.x.jornal>0] <- mano.obra.df$x1001.con.comida[mano.obra.df$x1003.hrs.promedio.x.jornal>0] /
   mano.obra.df$x1003.hrs.promedio.x.jornal[mano.obra.df$x1003.hrs.promedio.x.jornal>0]
   
mano.obra.df <- merge(mano.obra.df, geog.df[, c("folio", "provincia.full", 
  "seccion.full", "canton.full", "sector.full", "segmento.full"  )])
  
mano.obra.df$hourly.tractor.rental <- mano.obra.df$x101.cual.el.costo.de.la.hra.tractor.alquilado





input.price.columns <- c("hourly.wage", "hourly.tractor.rental")

nation.input.averages <- apply(inputs.df[, input.price.columns], 2, FUN=function(x) median(x[x>0]) )

for (target.input in input.price.columns ) {

  mano.obra.df[, paste0(target.input, ".impute.level")] <- NA
  
  mano.obra.df[mano.obra.df[, target.input]!=0, paste0(target.input, ".impute.level")] <- "itself"

impute.levels <- c("household", "segmento.full", "sector.full", "canton.full", "seccion.full", "provincia.full", "departamento", "nation")


imputed.data.ls<- vector(mode = "list", length = nrow(mano.obra.df))

for (i in 1:nrow(mano.obra.df)) {

if (mano.obra.df[i, paste0(target.input, ".impute.level")] %in% "itself") {next}

for (impute.level in impute.levels) {

  if (impute.level=="nation") { 
    imputed.data.ls[[i]] <- c(unname(nation.input.averages[target.input]), impute.level)
    break
  }
  
  if(impute.level=="household") {
    impute.data <- mano.obra.df[ mano.obra.df$folio==i & 
         mano.obra.df[, paste0(target.input, ".impute.level")] %in% "itself" , target.input]
  } else {
    impute.data <- mano.obra.df[ mano.obra.df[, impute.level] == mano.obra.df[i, impute.level] & 
         mano.obra.df[, paste0(target.input, ".impute.level")] %in% "itself" , target.input]
  }
  
  if (impute.level=="household" && length(impute.data)>0  ) {
    
    imputed.data.ls[[i]] <- c(median( impute.data ), "household")
     break
  }
  if (impute.level=="household") {next}

  
  if (length(impute.data) >= 3) {
    imputed.data.ls[[i]] <- c(median( impute.data ), impute.level)
    break
  }
  
}
 
 cat(i, "\n")
  
}

temp.imputed.df <- data.frame(matrix(unlist(imputed.data.ls), ncol=2, byrow=TRUE), stringsAsFactors=FALSE)
temp.imputed.df[, 1] <- as.numeric(temp.imputed.df[, 1])

mano.obra.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), target.input] <- 
  temp.imputed.df[, 1]

mano.obra.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), 
  paste0(target.input, ".impute.level") ] <- temp.imputed.df[, 2]
  
}

for (k in input.price.columns ) {
  print( table(mano.obra.df[, paste0(k, ".impute.level")], useNA="always") )
}





family.df <- read.spss(paste0(work.dir, "bd68/6-MANO DE OBRA/23.-ENA08_BOLIVIA_MIEMBROS_DEL_HOGAR(preg_97).sav"), to.data.frame=TRUE) 


colnames(family.df) <- tolower( make.names(gsub("[()]|[.]", "", attr(family.df, "variable.labels")) ) )

family.df$ag.fam.labor.equiv <- (family.df$x97.en.=="Agricultura") + (family.df$x97.en.=="Ambas") * .5
family.df$ag.fam.labor.equiv[is.na(family.df$ag.fam.labor.equiv)] <- 0

labor.aggregate <- aggregate(ag.fam.labor.equiv ~ folio, data=family.df, FUN=sum)

mano.obra.df <- merge( mano.obra.df, labor.aggregate)



tractor.df <- read.spss(paste0(work.dir, "bd68/8-JORNALES Y MAQUINARIA/26.-ENA08_BOLIVIA_JORNALES_MAQUINARIA EMPLEADA_1C (preg_104-107).sav"), to.data.frame=TRUE) 


colnames(tractor.df) <- tolower( make.names(gsub("[()]|[.]", "", attr(tractor.df, "variable.labels")) ) )


table(rowSums( table(tractor.df[, c(1,3)])>0))
# So seems like each folio has only one plot detailed here

summary(tractor.aggregate <- aggregate( x107.hrs.tractor ~ folio, data=tractor.df, FUN=sum))

# TODO: it's hard to determine what is sensible with distribution the tractor hours. 
# By land area, in Leontief style? I could get more specific with distributing it out since 
# I know which lands used tractors

mano.obra.df <- merge( mano.obra.df, tractor.aggregate)


inputs.df <- merge(inputs.df, mano.obra.df, by="folio")

area.agg <- aggregate(x19.superficie.cultivada.hectareas ~ folio, data=inputs.df, FUN=sum)

colnames(area.agg)[2] <- "firm.level.area"

inputs.df <- merge(inputs.df, area.agg )

inputs.df$plot.prop.of.firm.area <- inputs.df$x19.superficie.cultivada.hectareas /inputs.df$firm.level.area

inputs.df$ag.fam.labor.equiv.spread <- inputs.df$ag.fam.labor.equiv * inputs.df$plot.prop.of.firm.area

inputs.df$x107.hrs.tractor.spread <- inputs.df$x107.hrs.tractor * inputs.df$plot.prop.of.firm.area

inputs.df$paid.hours.spread <- inputs.df$paid.hours * inputs.df$plot.prop.of.firm.area



# TODO: we have something weird going on with hourly.tractor.rental and NA's






#[3] "x991.lab.agricolas...hombre...6mes"               
# [4] "x991.lab.agricolas...mujer...6mes"                
# [5] "x991.lab.agricolas...hombre...6mes"               
# [6] "x991.lab.agricolas...mujer...6mes"  

#[15] "x1001.con.comida"                                 
#[16] "x1002.sin.comida"                                 
#[17] "x1003.hrs.promedio.x.jornal"   











