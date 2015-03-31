library("foreign")

work.dir <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/"

inputs.df<- read.spss(paste0(work.dir, "bd68/2-AGRICOLA/Cultivos (Preg-19)/2.-ENA08_BOLIVIA_CULTIVOS_PRODUCCION_INSUMOS(preg_19).sav"), to.data.frame=TRUE)

# TODO: Need to figure out this warning message:
# "Unrecognized record type 7, subtype 18 encountered in system file"

colnames(inputs.df) <- tolower( make.names(gsub("[()]|[.]", "", attr(inputs.df, "variable.labels")) ) )


# tar -zcvf projdir10-28.tar.gz /home/c/cschmidt/TravisImInYourInternets/gamsdir/projdir

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


for ( i in grep("cantidad.quintal", colnames(inputs.df)) ) {
  inputs.df[, gsub("cantidad.quintal", "cantidad.kg", colnames(inputs.df)[i]) ] <- inputs.df[, i] * 46
}

for ( i in grep("obtenidad.quintal", colnames(inputs.df)) ) {
  inputs.df[, gsub("obtenidad.quintal", "obtenidad.kg", colnames(inputs.df)[i]) ] <- inputs.df[, i] * 46
}

# p. 18 of ftp://ftp.fao.org/docrep/fao/010/ah868e/ah868e00.pdf
# "One Bolivian arroba is equivalent to 11.5 kg"
# "One Bolivian quintal is equivalent to 46 kg"

for ( i in grep("bs.quintal", colnames(inputs.df)) ) {
  inputs.df[, gsub("bs.quintal", "bs.kg", colnames(inputs.df)[i]) ] <- inputs.df[, i] / 46
}
# Reciprocal since data is is Bolivianos per quintals







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

nation.input.averages <- apply(inputs.df[, input.price.columns], 2, FUN=function(x) median(x[x>0], na.rm=TRUE) )

for (target.input in input.price.columns ) {

  inputs.df[, paste0(target.input, ".impute.level")] <- NA
  
  inputs.df[inputs.df[, target.input]!=0, paste0(target.input, ".impute.level")] <- "itself"

impute.levels <- c("household", "segmento.full", "sector.full", "canton.full", "seccion.full", "provincia.full", "departamento", "nation")

# Rprof()

imputed.data.ls<- vector(mode = "list", length = nrow(inputs.df))

for (i in 1:nrow(inputs.df)) {

if (inputs.df[i, paste0(target.input, ".impute.level")] %in% "itself") {next}

for (impute.level in impute.levels) {
  
  if (target.input=="x19.sem.comprada.bs.kg") {
    seed.switcher <- inputs.df$x19.codigo==inputs.df$x19.codigo[i]
  } else {
    seed.switcher <- TRUE
  }

  if (impute.level=="nation") { 
    imputed.data.ls[[i]] <- c(unname(nation.input.averages[target.input]), impute.level)
    break
  }
  
  if(impute.level=="household") {
    impute.data <- inputs.df[ inputs.df$folio==inputs.df$folio[i] & 
         inputs.df[, paste0(target.input, ".impute.level")] %in% "itself" &
         seed.switcher , target.input]
  } else {
    impute.data <- inputs.df[ inputs.df[, impute.level] == inputs.df[i, impute.level] & 
         inputs.df[, paste0(target.input, ".impute.level")] %in% "itself" &
         seed.switcher, target.input]
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
 
 cat(target.input, i, impute.level, "\n")
  
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

mano.obra.df <- data.frame(mano.obra.df)

mano.obra.df$paid.hours <- mano.obra.df$x1003.hrs.promedio.x.jornal * (
  mano.obra.df$x991.lab.agricolas...hombre...6mes * 9 * 4.345 * 5 +
  mano.obra.df$x991.lab.agricolas...mujer...6mes.1 * 9 * 4.345 * 5 +
  mano.obra.df$x991.lab.agricolas...hombre...6mes * 3 * 4.345 * 5 + 
  mano.obra.df$x991.lab.agricolas...mujer...6mes.1 * 3 * 4.345 * 5 )

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

nation.input.averages <- apply(mano.obra.df[, input.price.columns], 2, FUN=function(x) median(x[x>0], na.rm=TRUE) )

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
    impute.data <- mano.obra.df[ mano.obra.df$folio==mano.obra.df$folio[i] & 
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

mano.obra.df <- merge( mano.obra.df, labor.aggregate, all=TRUE)



tractor.df <- read.spss(paste0(work.dir, "bd68/8-JORNALES Y MAQUINARIA/26.-ENA08_BOLIVIA_JORNALES_MAQUINARIA EMPLEADA_1C (preg_104-107).sav"), to.data.frame=TRUE) 


colnames(tractor.df) <- tolower( make.names(gsub("[()]|[.]", "", attr(tractor.df, "variable.labels")) ) )


table(rowSums( table(tractor.df[, c(1,3)])>0))
# So seems like each folio has only one plot detailed here

summary(tractor.aggregate <- aggregate( x107.hrs.tractor ~ folio, data=tractor.df, FUN=sum))

# TODO: it's hard to determine what is sensible with distribution the tractor hours. 
# By land area, in Leontief style? I could get more specific with distributing it out since 
# I know which lands used tractors

mano.obra.df <- merge( mano.obra.df, tractor.aggregate, all=TRUE)

intersect(names(mano.obra.df), names(inputs.df))

inputs.df <- merge(inputs.df, mano.obra.df[, !colnames(mano.obra.df) %in% c("zona.agroproductiva", 
  "factor.de.expansión", "departamento", "provincia.full", "seccion.full",  "canton.full", 
  "sector.full", "segmento.full")], by="folio", all.x=TRUE)




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

#  inputs.df.save <- inputs.df
#  inputs.df <- inputs.df.save


tractor.df$x107.detalle.recoded <- as.character(tractor.df$x107.detalle)

tractor.df$x107.detalle.recoded[ tractor.df$x107.detalle.recoded %in% 
  c("Aplicación de abono       ", "Aplicación de fertilizant ", "Aplicación de fungicidas  ",
  "Aplicación de herbicidas  ", "Aplicación de insecticida ", "Aplicación de Insecticidas", 
  "Aplicación de riego       ", "Labores culturales        ") ] <- "Labores culturales"


tractor.df$x107.detalle.recoded[ tractor.df$x107.detalle.recoded %in% 
  c("Desmonte                  ", "Lomas artificiales        ", "Picado                    ", 
  "Preparación del suelo     ", "Rotabatear                ") ] <- "Preparación del suelo"


tractor.df$x107.detalle.recoded[ tractor.df$x107.detalle.recoded %in% 
  c( "Embolsado,empacado, encaj ", "Labor de cosecha          ", "Selección del producto    ", 
  "Trillado                  ") ] <- "Labor de cosecha"

tractor.df$x107.detalle.recoded[ tractor.df$x107.detalle.recoded %in% 
  "Siembra                   " ] <- "Siembra"


table( tractor.df$x107.detalle.recoded)

tractor.df.big.agg <- aggregate(tractor.df[, "x107.hrs.tractor", drop=FALSE], 
  by=list(folio=tractor.df$folio, x107.detalle.recoded=tractor.df$x107.detalle.recoded), 
  FUN=sum, na.rm=TRUE) 

tractor.reshaped <- reshape(tractor.df.big.agg[, c("folio", "x107.detalle.recoded", "x107.hrs.tractor")], idvar="folio", timevar = "x107.detalle.recoded", direction="wide")

colnames(tractor.reshaped) <- gsub(" ", ".", colnames(tractor.reshaped))

tractor.reshaped <- tractor.reshaped[, c("folio", "x107.hrs.tractor.Labor.de.cosecha", 
  "x107.hrs.tractor.Labores.culturales", "x107.hrs.tractor.Preparación.del.suelo", 
  "x107.hrs.tractor.Siembra") ]
  
tractor.reshaped[is.na(tractor.reshaped)] <- 0

tractor.reshaped <- merge(tractor.reshaped, tractor.df[
!duplicated(tractor.df[, c("folio", "x104.indique.el.número.de.jornales.u.hrs.tractor")]), c("folio", "x104.indique.el.número.de.jornales.u.hrs.tractor")])

colnames(tractor.reshaped)[colnames(tractor.reshaped)=="x104.indique.el.número.de.jornales.u.hrs.tractor"] <- "nro"
# ****
# Ok, changing this to "nro" makes sense if you look at the survey text.

tractor.reshaped[is.na(tractor.reshaped)] <- 0




#inputs.df
#x19.prepara.el.suelo
#x19.siembra.planta 
#x19.labores.culturales
#x19.cosecha

table(inputs.df$x19.prepara.el.suelo )
table(inputs.df$x19.siembra.planta  )
table(inputs.df$x19.labores.culturales )
table(inputs.df$x19.cosecha )

intersect(names(tractor.reshaped), names(inputs.df))

inputs.df <- merge(inputs.df, tractor.reshaped, all.x=TRUE)


#inputs.df$x19.prepara.el.suelo.tractor.hrs <- ifelse(inputs.df$x19.prepara.el.suelo=="Maq. Agricola", NA, 0)
#inputs.df$x19.siembra.planta.tractor.hrs <- ifelse(inputs.df$x19.siembra.planta=="Maq. Agricola", NA, 0)
#inputs.df$x19.labores.culturales.tractor.hrs <- ifelse(inputs.df$x19.labores.culturales=="Maq. Agricola", NA, 0)
#inputs.df$x19.cosecha.tractor.hrs <- ifelse(inputs.df$x19.cosecha.tractor.hrs=="Maq. Agricola", NA, 0)





# x19.nro.lote == x104.indique.el.número.de.jornales.u.hrs.tractor


# It's correct to match on x19.nro.lote based on seeing what happens when there is a
# discrepancy between x19.nro.lote and nro
# Still, x19.nro.lote  seems to have some duplicates
# Hmm, I think I will go with nro due to no duplication
# The only things to change is the **** up above and below

table(duplicated(inputs.df[,  c("folio", "x19.nro.lote")], ))

table(duplicated(inputs.df[,  c("folio", "nro")], ))


tractor.agg<- aggregate(tractor.df$x107.hrs.tractor, by=list(tractor.df$folio, 
  tractor.df$x104.indique.el.número.de.jornales.u.hrs.tractor), FUN=sum, na.rm=TRUE)

colnames(tractor.agg) <-c("folio", "nro", "exact.plot.tractor.hrs")
# ****


#inputs.df.save <- inputs.df
#inputs.df <- inputs.df.save

inputs.df <- merge(inputs.df, tractor.agg, all.x=TRUE)

inputs.df$exact.plot.tractor.hrs[is.na(inputs.df$exact.plot.tractor.hrs)] <- 0



# TODO: an "itself" count of  1379  seems low
# TODO: it seems we lost a lot of observations


#tractor.columns <- c("x19.prepara.el.suelo.tractor.hrs", "x19.siembra.planta.tractor.hrs",
#  "x19.labores.culturales.tractor.hrs", "x19.cosecha.tractor.hrs")

tractor.columns <- c("x107.hrs.tractor.Labor.de.cosecha" , "x107.hrs.tractor.Labores.culturales",
  "x107.hrs.tractor.Preparación.del.suelo", "x107.hrs.tractor.Siembra" )
  


corresponding.input <- c("x19.cosecha", "x19.labores.culturales",
  "x19.prepara.el.suelo", "x19.siembra.planta")






nation.input.averages <- apply(inputs.df[, tractor.columns], 2, FUN=function(x) {
  median( (x[x>0])/inputs.df$x19.superficie.cultivada.hectareas[x>0], na.rm=TRUE)
  } )



for (target.input in tractor.columns ) {

  inputs.df[, paste0(target.input, ".impute.level")] <- NA
  
#  inputs.df[inputs.df[, target.input]!=0, paste0(target.input, ".impute.level")] <- "itself"

impute.levels <- c("household", "segmento.full", "sector.full", "canton.full", 
  "seccion.full", "provincia.full", "departamento", "nation")


imputed.data.ls<- vector(mode = "list", length = nrow(inputs.df))

inputs.df[ , target.input]

obs.to.include <- which(inputs.df$exact.plot.tractor.hrs==0 & 
  inputs.df[ , corresponding.input[target.input==tractor.columns] ] == "Maq. Agricola" )

inputs.df[inputs.df$exact.plot.tractor.hrs!=0, paste0(target.input, ".impute.level")] <- "itself"

for (i in obs.to.include) {
# 1:nrow(inputs.df)

# if (inputs.df[i, paste0(inputs.df, ".impute.level")] %in% "itself") {next}

for (impute.level in impute.levels) {


  if (impute.level=="nation") { 
#   stop("reached nation level")
    imputed.data.ls[[i]] <- c(unname(nation.input.averages[target.input]), impute.level)
    break
  }
  
#  if (impute.level=="itself") {
#    if ( inputs.df$x19.nro.lote[i]!= inputs.df[ 1, "x104.indique.el.número.de.jornales.u.hrs.tractor"] )
#      next
#    }
  
  
  
  
  if(impute.level=="household") {
    impute.data <- inputs.df[ inputs.df$folio==inputs.df$folio[i] & 
         inputs.df[, paste0(target.input, ".impute.level")] %in% "itself" , target.input]
    impute.data <- impute.data / inputs.df[ inputs.df$folio==inputs.df$folio[i] & 
         inputs.df[, paste0(target.input, ".impute.level")] %in% "itself" , "x19.superficie.cultivada.hectareas"]
         
  } else {
    impute.data <- inputs.df[ inputs.df[, impute.level] == inputs.df[i, impute.level] & 
         inputs.df[, paste0(target.input, ".impute.level")] %in% "itself" , target.input]
    impute.data <- impute.data / inputs.df[ inputs.df[, impute.level] == inputs.df[i, impute.level] & 
         inputs.df[, paste0(target.input, ".impute.level")] %in% "itself" , "x19.superficie.cultivada.hectareas"]
  }
  
  impute.data <- impute.data[impute.data!=0]
  
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

inputs.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), target.input] <- 
  temp.imputed.df[, 1]

inputs.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), 
  paste0(target.input, ".impute.level") ] <- temp.imputed.df[, 2]
  
}


for (target.input in tractor.columns ) {
  inputs.df[! inputs.df$exact.plot.tractor.hrs>0, target.input] <- 
    inputs.df[! inputs.df$exact.plot.tractor.hrs>0, target.input] * 
    inputs.df$x19.superficie.cultivada.hectareas[! inputs.df$exact.plot.tractor.hrs>0]
}

# re-scaling amount used by the number of hectares for
# all non-itself observations



for (k in tractor.columns ) {
  print( table(inputs.df[, paste0(k, ".impute.level")], useNA="always") )
}




inputs.df$tractor.hrs.final <- rowSums( inputs.df[, tractor.columns], na.rm=TRUE )

inputs.df$tractor.hrs.final[inputs.df$exact.plot.tractor.hrs>0] <- 
  inputs.df$exact.plot.tractor.hrs[inputs.df$exact.plot.tractor.hrs>0]



table( 
inputs.df[ , corresponding.input[1] ] == "Maq. Agricola" |
inputs.df[ , corresponding.input[2] ] == "Maq. Agricola" |
inputs.df[ , corresponding.input[3] ] == "Maq. Agricola" |
inputs.df[ , corresponding.input[4] ] == "Maq. Agricola" 
)

# inputs.df.save.2 <- inputs.df





summary(inputs.df[, c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", 
  "x19.plagicidas.bs.kg", "hourly.wage", "hourly.tractor.rental", 
  "x19.fertilizante.cantidad.kg", "x19.sem.comprada.cantidad.kg", "x19.abono.cantidad.kg", 
  "x19.plagicidas.cantidad.kg", "tractor.hrs.final", "x19.produccion.obtenidad.kg", 
  "x19.superficie.cultivada.hectareas", "x19.uso.riego", "ag.fam.labor.equiv.spread") ]
)



min(inputs.df$x19.produccion.obtenidad.kg)
table(is.na(inputs.df$x19.produccion.obtenidad.kg))


nation.input.averages.tractor <- apply(inputs.df[, c("hourly.tractor.rental"), drop=FALSE], 2, FUN=function(x) median(x[x>0], na.rm=TRUE) )

inputs.df$hourly.tractor.rental[is.na(inputs.df$hourly.tractor.rental)] <- nation.input.averages.tractor
# Ok, so this has no effect, since all of them were imputed at a higher level

for ( i in c("x19.fertilizante.cantidad.kg", "x19.sem.comprada.cantidad.kg", 
  "x19.abono.cantidad.kg", "x19.plagicidas.cantidad.kg", "tractor.hrs.final") ) {
  print(mean(is.na(inputs.df[, i])))
  inputs.df[ is.na(inputs.df[, i]) , i] <- 0
  
}

# inputs.df.save.2 <- inputs.df

inputs.df <- inputs.df[inputs.df$x19.produccion.obtenidad.kg>0 &
  !is.na(inputs.df$x19.produccion.obtenidad.kg), ] 
# This removes crop failures


summary(inputs.df[, c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", 
  "x19.plagicidas.bs.kg", "hourly.wage", "hourly.tractor.rental", 
  "x19.fertilizante.cantidad.kg", "x19.sem.comprada.cantidad.kg", "x19.abono.cantidad.kg", 
  "x19.plagicidas.cantidad.kg", "tractor.hrs.final", "x19.produccion.obtenidad.kg", 
  "x19.superficie.cultivada.hectareas", "x19.uso.riego", "ag.fam.labor.equiv.spread") ]
)




######### BEGIN GEOG/SOIL WORK



library("foreign")
library("PBSmapping")
# install.packages("rgdal", repos="http://cran.us.r-project.org", type = "source")
# Warnings: as of now, must install rgdal from source
# Ok, this is how i finally fixed it:
# from http://cran.r-project.org/web/packages/rgdal/, download "OS X Snow Leopard binaries"
# Then:
# sudo R CMD INSTALL /Users/travismcarthur/Downloads/rgdal_0.9-1.tgz

# May also have to do the following:
# Accord to http://cran.r-project.org/web/packages/rgdal/ , must download GDAL from http://www.kyngchaos.com/software/frameworks
# And then edit bash_profile in accordance with the GDAL readme file that
# comes wit the kyngchaos install. input "gdal-config" in Terminal to
# confirm GDAL has been installed
# --configure-args='--with-gdal-config=/travismcarthur/local/bin/gdal-config'

library("rgdal")

# install.packages("splancs", repos="http://cran.us.r-project.org")
library("splancs")

work.dir <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/"









pob.shp<-importShapefile(paste0(work.dir, "centros_poblados.zip Folder/centros_poblados.shp"))


#combine.localidades<-function(x) {
#  ret<-x[1, ]
#  ret$num.of.localidades<-nrow(x)
#  ret$X <- sum(x$X * x$VIVIENDA)/sum(x$VIVIENDA)
#  ret$Y <- sum(x$Y * x$VIVIENDA)/sum(x$VIVIENDA)
#  ret$LOCALIDAD<-"combined localidades"
#  ret$N_ZONLOC<-"combined localidades"
#  ret
#}


#dup.indices<- duplicated(substr(pob.shp$COD_BD_CEN, 1, 11)) |
#  duplicated(substr(pob.shp$COD_BD_CEN, 1, 11), fromLast=TRUE)

#combined.localidades.ls<-by(pob.shp[dup.indices, ], INDICES=substr(pob.shp$COD_BD_CEN[dup.indices], 1, 11), FUN=combine.localidades)

#combined.localidades.df<-do.call(rbind, combined.localidades.ls)

#pob.shp.rev <- rbind(combined.localidades.df,
#  cbind(pob.shp[!dup.indices, ], data.frame(num.of.localidades=rep(1, nrow(pob.shp[!dup.indices, ]))))
#)

#pob.shp.rev$comunidad.id<-substr(pob.shp.rev$COD_BD_CEN, 1, 11)


#table(pob.shp.rev$CAT_LOC)
#simplified.loc.cat <- as.character(pob.shp.rev$CAT_LOC)
#simplified.loc.cat[simplified.loc.cat == "0-Capital"] <- "1-Urbana"
#simplified.loc.cat[simplified.loc.cat == "3-Dispersa"] <- "3-7Dispersa"  
#aggregate( pob.shp.rev$POB2001, by=list(simplified.loc.cat), FUN=median, na.rm=TRUE)



pob.shp$comunidad.id<-substr(pob.shp$COD_BD_CEN, 1, 11)

pob.shp$canton.full <- with(pob.shp , paste0(DEPTO, PROVIN, SECCION, CANTON))

# village.geog.df<-pob.shp[pob.shp$comunidad.id %in% hogar01.df.ids, c("EID", "comunidad.id", "X", "Y", "VIVIENDA")]

#install.packages("SDMTools", repos="http://cran.us.r-project.org")
library("SDMTools")

soil.qual.asc <- read.asc("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/sq1.asc", gz = FALSE)


#cellcentre.offset: numeric; vector with the smallest coordinates for
#          each dimension
#cellsize: numeric; vector with the cell size in each dimension
#cells.dim: integer; vector with number of cells in each dimension

soil.qual.grid.defn <- GridTopology(cellcentre.offset=
  c(attr(soil.qual.asc,  "xll"), attr(soil.qual.asc,  "yll")), 
  cellsize=rep(attr(soil.qual.asc,  "cellsize"), 2), 
  cells.dim= rev(dim(soil.qual.asc))) # ok, this should be rev() by evidence in soil.bil@grid. This is crucial

soil.qual.SP <- SpatialPixels(
  SpatialPoints(as.data.frame(pob.shp[, c("X", "Y")])),
  grid=soil.qual.grid.defn )

# head(diff(soil.qual.SP@grid.index))

#pob.shp$soil.qual <- c(soil.qual.asc)[soil.qual.SP@grid.index]
# Reactivate this if we want the other measure




soil.mdb <- read.csv("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/HWSD.csv")
# Used mdb-export command line tool to make csv.
load(file=paste0( work.dir, "HWSD_RASTER/hwsd.Rdata"))


#soil.shp<-importShapefile(paste0(work.dir, "DSMW/DSMW.shp"))

#soil.bil<- readGDAL(paste0(work.dir, "HWSD_RASTER/hwsd.bil"))
#save(file=paste0( work.dir, "HWSD_RASTER/hwsd.Rdata"),soil.bil)
load(file=paste0( work.dir, "HWSD_RASTER/hwsd.Rdata"))

#soil.mdb<-mdb.get(paste0("'", work.dir, "HWSD.mdb'"))



villages.spatialpixels<-SpatialPixels(
  SpatialPoints(as.data.frame(pob.shp[, c("X", "Y")])),
  grid=soil.bil@grid)

# max(crop.wide.df$fert.exp)!=crop.wide.df$fert.exp


pob.shp$MU.GLOBAL<-soil.bil$band1[villages.spatialpixels@grid.index]

colnames(soil.mdb) <- gsub("_", ".", colnames(soil.mdb))

intersect(colnames(pob.shp), colnames(soil.mdb))

pob.shp<-merge(pob.shp, soil.mdb[soil.mdb$SEQ==1, ], all.x=TRUE) # soil.mdb$HWSD_DATA[soil.mdb$HWSD_DATA$SEQ==1

table(pob.shp$T.PH.H2O)
table(pob.shp$AWC.CLASS)

pob.shp$AWC.mm <- NA

pob.shp$AWC.mm[pob.shp$AWC.CLASS==1] <- 150
pob.shp$AWC.mm[pob.shp$AWC.CLASS==2] <- 125
pob.shp$AWC.mm[pob.shp$AWC.CLASS==3] <- 100
pob.shp$AWC.mm[pob.shp$AWC.CLASS==4] <-  75
pob.shp$AWC.mm[pob.shp$AWC.CLASS==5] <-  50
pob.shp$AWC.mm[pob.shp$AWC.CLASS==6] <-  15
pob.shp$AWC.mm[pob.shp$AWC.CLASS==7] <-   0

# see http://www.fao.org/docrep/018/aq361e/aq361e.pdf
#1 150
#2 125
#3 100
#4  75
#5  50
#6  15
#7   0

pob.shp$soil.qual <- with(pob.shp, 0.703450 + 0.420518 * AWC.mm/1000 - 0.008133 * (T.CEC.SOIL - T.TEB))

# AWC is basically a percentage in the index paper

#0.420518 water
#-0.008133  pH
#0.703450  constant

# Index paper: http://www.jstor.org.ezproxy.library.wisc.edu/stable/pdfplus/1244331.pdf?acceptTC=true


# The scientific community often expresses CEC of a soil as cmol/kg. This is centimoles (cmol) of charge per kilogram of soil. Many soil testing laboratories (and your textbook), however, express CEC as meq/100 g. This should not cause confusion since: 1 meq/100 g = 1 cmol/kg
#http://www.public.iastate.edu/~teloynac/354ppcecsol.html

# THIS IS THE KEY INSIGHT!:
# 3.  Calculate exchangeable acidity in meq/100 g as follows.
# Acidity (meq/100g) = CEC – (Ca + Mg + K + Na)
# www.clemson.edu/sera6/soilcec_sikora4.doc

# so acidity is CEC-TEB
# There is a minor issue in that the database has measures for "soil" and "clay", so maybe a weighted average of them?

summary(pob.shp$T.CEC.SOIL - pob.shp$T.TEB )


mean.soil.qual <- mean(pob.shp$soil.qual, na.rm=TRUE)

pob.shp$soil.qual[is.na(pob.shp$soil.qual)] <- mean.soil.qual

soil.agg.v <- by(pob.shp[, c("soil.qual", "VIVIENDA")], INDICES=list(pob.shp$canton.full), 
  FUN=function(x) {
    weighted.mean(x[, 1], x[, 2], na.rm=TRUE)
  }
) 

soil.agg.df <- data.frame(soil.quality = unclass(soil.agg.v), canton.full=attr(soil.agg.v, "dimnames")[[1]])

table(pob.shp$T.CEC.SOIL)


setdiff(inputs.df$canton.full, soil.agg.df$canton.full)


inputs.df <- merge(inputs.df, soil.agg.df, all.x=TRUE)

inputs.df$soil.quality[is.na(inputs.df$soil.quality)] <- mean.soil.qual
# impute by mean









pob.shp<-importShapefile(paste0(work.dir, "centros_poblados.zip Folder/centros_poblados.shp"))

pob.shp$canton.full <- with(pob.shp , paste0(DEPTO, PROVIN, SECCION, CANTON))


elev.dem1<-readGDAL(paste0(work.dir, "w060s10/W060S10.DEM"))
elev.dem2<-readGDAL(paste0(work.dir, "w100s10/W100S10.DEM"))

#hist(elev.dem2$band1)
#image(elev.dem2["band1"])
#max(village.geog.df$Y[village.geog.df$Y!=0])
# Since this is south of -10 latitude, we don;t have to download additional elevation panels
# TODO: Ok, now the max is actually -9.704484 and we are missing two villages, so we need to fix this

keep.coords.1<-point.in.polygon(pob.shp$X , pob.shp$Y, bboxx(bbox(elev.dem1))[, 1], bboxx(bbox(elev.dem1))[, 2])==1

villages.elev.1.spatialpixels<-SpatialPixels(
  SpatialPoints(as.data.frame(pob.shp[keep.coords.1, c("X", "Y")])),
  grid=elev.dem1@grid)

elevation.df<-data.frame(EID=pob.shp[keep.coords.1, "EID"],
  elevation=elev.dem1$band1[villages.elev.1.spatialpixels@grid.index], stringsAsFactors=FALSE)

keep.coords.2<-point.in.polygon(pob.shp$X , pob.shp$Y, bboxx(bbox(elev.dem2))[, 1], bboxx(bbox(elev.dem2))[, 2])==1

villages.elev.2.spatialpixels<-SpatialPixels(
  SpatialPoints(as.data.frame(pob.shp[keep.coords.2, c("X", "Y")])),
  grid=elev.dem2@grid)
  



elevation.df <- rbind(elevation.df,
  data.frame(EID=pob.shp[keep.coords.2, "EID"],
    elevation=elev.dem2$band1[villages.elev.2.spatialpixels@grid.index], stringsAsFactors=FALSE)
  )
# Weird textwrangler corruption is messing us up in the line above

pob.shp <- merge(pob.shp, elevation.df, all.x=TRUE)



mean.elevation <- mean(pob.shp$elevation, na.rm=TRUE)

pob.shp$elevation[is.na(pob.shp$elevation)] <- mean.elevation

elevation.agg.v <- by(pob.shp[, c("elevation", "VIVIENDA")], INDICES=list(pob.shp$canton.full), 
  FUN=function(x) {
    weighted.mean(x[, 1], x[, 2], na.rm=TRUE)
  }
) 

elevation.agg.df <- data.frame(elevation = unclass(elevation.agg.v), canton.full=attr(elevation.agg.v, "dimnames")[[1]])




setdiff(inputs.df$canton.full, elevation.agg.df$canton.full)


inputs.df <- merge(inputs.df, elevation.agg.df, all.x=TRUE)

inputs.df$elevation[is.na(inputs.df$elevation)] <- mean.elevation

inputs.df$elevation <- inputs.df$elevation/1000
# Change elevation to kilometers for the sake of scaling

#hist(inputs.df$elevation)


library("foreign")
library("PBSmapping")
library("sp")
#work.dir <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/"



precip.coords<-read.fortran(paste0(work.dir, "Global2011P/precip.2000"), 
  format=c("F8.3", "F8.3", "12F8.1"))[, c(1,2)]
  
precip.coords[,1]<-precip.coords[,1]*1000
precip.coords[,2]<-precip.coords[,2]*1000

# 2001-2006
target.precip.years <- 2001:2006
# SO get: 2001-2002, 2002-2003, 2003-2004, 2004-2005, 2005-2006 growing season

precip.ls<-list()

for ( i in as.character(target.precip.years)) {
  precip.ls[[i]]<-read.fortran(paste0(work.dir, "Global2011P/precip.", i), 
    format=c("F8.3", "F8.3", "12F8.1"))[, -c(1,2)]
    
}

#TODO: REALLY UNCLEAR ABOUT THE SCALE of rainfall values. MAY HAVE TO MULTIPLY THesE VALUES TO GET RIGHT SCALE. http://climate.geog.udel.edu/~climate/html_pages/Global2011/Precip_revised_3.02/README.GlobalTsP2011.html

precip.years<-as.character(target.precip.years)

precip.disag.ls<-list()

for ( i in 2:length(precip.years) ) {

  precip.temp.1<-precip.ls[[precip.years[i-1]]]
  precip.temp.2<-precip.ls[[precip.years[i]]]
  precip.disag.ls[[ paste0("rain.grow.season.", precip.years[i])]] <- 
    rowSums(precip.temp.1[, 10:12]) + rowSums(precip.temp.1[, 1:4])
  # NOTE: This only gets the rain during the growing season, which actually 
}


precip.disag.df<-as.data.frame(do.call(cbind, precip.disag.ls))

precip.df<-do.call(cbind, precip.ls[as.character(target.precip.years)])

precip.df<-rowSums(precip.df)/(length(target.precip.years))

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






#pob.shp<-importShapefile(paste0(work.dir, "centros_poblados.zip Folder/centros_poblados.shp"))

#pob.shp$canton.full <- with(pob.shp , paste0(DEPTO, PROVIN, SECCION, CANTON))


villages.spatialpixels.rain<-SpatialPixels(
  SpatialPoints(as.data.frame(pob.shp[, c("X", "Y")])),
  grid=rain.grid)

pob.shp$rain.grid.index<-villages.spatialpixels.rain@grid.index

precip.df<-cbind(
  data.frame(rain.grid.index=rain.pixels@grid.index, mean.ann.rain.5yr=precip.df),
  precip.disag.df
)


pob.shp <- merge(pob.shp, precip.df, all.x=TRUE)

mean.rainfall <- mean(pob.shp$mean.ann.rain.5yr, na.rm=TRUE)

pob.shp$mean.ann.rain.5yr[is.na(pob.shp$mean.ann.rain.5yr)] <- mean.rainfall

rainfall.agg.v <- by(pob.shp[, c("mean.ann.rain.5yr", "VIVIENDA")], INDICES=list(pob.shp$canton.full), 
  FUN=function(x) {
    weighted.mean(x[, 1], x[, 2], na.rm=TRUE)
  }
) 

rainfall.agg.df <- data.frame(mean.ann.rain.5yr = unclass(rainfall.agg.v), canton.full=attr(rainfall.agg.v, "dimnames")[[1]])


setdiff(inputs.df$canton.full, rainfall.agg.df$canton.full)


attributes(rainfall.agg.df$mean.ann.rain.5yr) <- NULL

inputs.df <- merge(inputs.df, rainfall.agg.df, all.x=TRUE)

inputs.df$mean.ann.rain.5yr[is.na(inputs.df$mean.ann.rain.5yr)] <- mean.rainfall

# inputs.df$elevation <- inputs.df$elevation/1000





######### END GEOG WORK





# rm(list=setdiff(ls(), keep.these))




#save.image("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF with soil.Rdata")

# save(inputs.df, file="/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF with soil.Rdata")

save(inputs.df, file="/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF with soil and rain.Rdata")

# save.image("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF.Rdata")


#x107.hrs.tractor.spread

#w01 = firm.df$x19.fertilizante.bs.kg
#w02 = firm.df$x19.sem.comprada.bs.kg
#w03 = firm.df$x19.abono.bs.kg
#w04 = firm.df$x19.plagicidas.bs.kg
#w05 = firm.df$hourly.wage
#w06 = firm.df$hourly.tractor.rental

#x01 = firm.df$x19.fertilizante.cantidad.kg
#x02 = firm.df$x19.sem.comprada.cantidad.kg
#x03 = firm.df$x19.abono.cantidad.kg
#x04 = firm.df$x19.plagicidas.cantidad.kg
#x05 = firm.df$paid.hours.spread 
#x06 = firm.df$x107.hrs.tractor.spread

#y01 <- log( firm.df$x19.produccion.obtenidad.kg )

#q01 = firm.df$x19.superficie.cultivada.hectareas
# q01[q01 ==0] = median(q01)

#q02 = ifelse(firm.df$x19.uso.riego!="Si",  1, exp(1))

#q03 = firm.df$ag.fam.labor.equiv.spread
#q03[q03 == 0] = .5














