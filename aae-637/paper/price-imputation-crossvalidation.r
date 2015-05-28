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
# Also here: http://sizes.com/units/quintal.htm

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




# Use below for Amelia
for ( i in grep("bs.quintal", colnames(inputs.df)) ) {
  inputs.df[, gsub("bs.quintal", "bs.kg", colnames(inputs.df)[i]) ][ 
  inputs.df[, gsub("bs.quintal", "bs.kg", colnames(inputs.df)[i]) ]==0] <- NA
}

#nominal.vars <- names(inputs.df)[sapply(inputs.df, FUN=function(x) is.character(x) | is.factor(x))]

nominal.vars <- c("departamento",  "provincia.full", "seccion.full", "canton.full", "sector.full", "segmento.full", "zona.agroproductiva")

library("Amelia")

nominal.vars <- c("provincia.full")


a.out <- amelia(inputs.df[, c("x19.fertilizante.bs.kg", nominal.vars)], m = 1, noms = nominal.vars, incheck=FALSE, p2s=2,
  logs="x19.fertilizante.bs.kg")
# Thanks to https://lists.gking.harvard.edu/pipermail/amelia/2015-January/001128.html

a.out$imputations$imp1

a.overimpute <- overimpute(a.out, var = "x19.fertilizante.bs.kg")






# Imputing input prices below

library("compiler")

enableJIT(3)




input.price.columns <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", "x19.plagicidas.bs.kg")

input.price.subsets.ls <- list()

for ( i in input.price.columns) {
  input.price.subsets.ls[[i]] <- inputs.df[inputs.df[, i] > 0 , ]
}

num.folds <- 10
target.fold <- 3

chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 
# THanks to http://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r
# all(1:113 %in% unlist(chunk2(sample(113), 10)))
# A-OK

nation.input.averages <- apply(inputs.df[, input.price.columns], 2, FUN=function(x) median(x[x>0], na.rm=TRUE) )
nation.input.nobs <- apply(inputs.df[, input.price.columns], 2, FUN=function(x) { x <- x[!is.na(x)]; length(x[x>0])} )
nation.input.sd <- apply(inputs.df[, input.price.columns], 2, FUN=function(x) sd(x[x>0], na.rm=TRUE) )

for (target.input in input.price.columns ) {
# Set to input.price.columns[1] for now

  set.seed(100)

  inputs.temp.df <- input.price.subsets.ls[[target.input]]
  folds.random.index <- sample.int(nrow(inputs.temp.df))
  folds.ind.ls <- chunk2(folds.random.index, num.folds)
  inputs.temp.df.save <- inputs.temp.df
  inputs.temp.df[unlist(folds.ind.ls[target.fold]), target.input] <- 0
  # folds.ind.ls[1]: Only do the first one for now

  inputs.temp.df[, paste0(target.input, ".impute.level")] <- NA
  
  inputs.temp.df[inputs.temp.df[, target.input]!=0, paste0(target.input, ".impute.level")] <- "itself"

impute.levels <- c("household", "segmento.full", "sector.full", "canton.full", "seccion.full", "provincia.full", "departamento", "nation")

# Rprof()

imputed.data.ls<- vector(mode = "list", length = nrow(inputs.temp.df))

for (i in 1:nrow(inputs.temp.df)) {

if (inputs.temp.df[i, paste0(target.input, ".impute.level")] %in% "itself") {next}

for (impute.level in impute.levels) {
  
  if (target.input=="x19.sem.comprada.bs.kg") {
    seed.switcher <- inputs.temp.df$x19.codigo==inputs.temp.df$x19.codigo[i]
  } else {
    seed.switcher <- TRUE
  }

  if (impute.level=="nation") { 
    imputed.data.ls[[i]] <- c(unname(nation.input.averages[target.input]), 
      impute.level, unname(nation.input.nobs[target.input]), unname(nation.input.sd[target.input]))
    break
  }
  
  if(impute.level=="household") {
    impute.data <- inputs.temp.df[ inputs.temp.df$folio==inputs.temp.df$folio[i] & 
         inputs.temp.df[, paste0(target.input, ".impute.level")] %in% "itself" &
         seed.switcher , target.input]
  } else {
    impute.data <- inputs.temp.df[ inputs.temp.df[, impute.level] == inputs.temp.df[i, impute.level] & 
         inputs.temp.df[, paste0(target.input, ".impute.level")] %in% "itself" &
         seed.switcher, target.input]
  }
  
  if (impute.level=="household" && length(impute.data)>0  ) {
    
#     inputs.temp.df[i, target.input] <- median( impute.data )
#     inputs.temp.df[i, c(target.input, paste0(target.input, ".impute.level") )] <- "household"
      imputed.data.ls[[i]] <- c(median( impute.data ), "household", length(impute.data), sd(impute.data))
#    prod01.df$impute.sample.size[i]<-0
     break
  }
  if (impute.level=="household") {next}
  
  #target.crop<-prod01.df$crop[i]

#  match.index <- inputs.temp.df[, impute.level] == inputs.temp.df[i, impute.level] & 
#    inputs.temp.df[, paste0(target.input, ".impute.level")] %in% "itself"
  
#  impute.sample.size <- sum(match.index)
  
  if (length(impute.data) >= 3) {
#    inputs.temp.df[i, target.input] <- median(impute.data )
#    inputs.temp.df[i, paste0(target.input, ".impute.level")] <- impute.level
#    prod01.df$impute.sample.size[i] <- impute.sample.size
    imputed.data.ls[[i]] <- c(median( impute.data ), impute.level, length(impute.data), sd(impute.data))
    break
  }
  

  
}
 
 cat(target.input, i, impute.level, "\n")
  
}

temp.imputed.df <- data.frame(matrix(unlist(imputed.data.ls), ncol=4, byrow=TRUE), stringsAsFactors=FALSE)
temp.imputed.df[, 1] <- as.numeric(temp.imputed.df[, 1])
temp.imputed.df[, 3] <- as.numeric(temp.imputed.df[, 3])
temp.imputed.df[, 4] <- as.numeric(temp.imputed.df[, 4])

inputs.temp.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), target.input] <- 
  temp.imputed.df[, 1]

inputs.temp.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), 
  paste0(target.input, ".impute.level") ] <- temp.imputed.df[, 2]
  
inputs.temp.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), 
  paste0(target.input, ".impute.n.obs") ] <- temp.imputed.df[, 3]

inputs.temp.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), 
  paste0(target.input, ".impute.sd") ] <- temp.imputed.df[, 4]
  
pred.error <- inputs.temp.df[unlist(folds.ind.ls[target.fold]), target.input] - 
  inputs.temp.df.save[unlist(folds.ind.ls[target.fold]), target.input]
  
mse.test <- mean(pred.error^2)



}

cor(inputs.temp.df[unlist(folds.ind.ls[target.fold]), target.input], 
  inputs.temp.df.save[unlist(folds.ind.ls[target.fold]), target.input])

cor(inputs.temp.df[unlist(folds.ind.ls[target.fold]), target.input], 
  inputs.temp.df.save[unlist(folds.ind.ls[target.fold]), target.input], method="spearman")


for (k in input.price.columns ) {
  print( table(inputs.temp.df[, paste0(k, ".impute.level")], useNA="always") )
}

# table(inputs.temp.df[, paste0(target.input, ".impute.level")], useNA="always")

# save(inputs.temp.df, file=paste0(work.dir, "inputs df after 1st imputation.Rdata"))


for (k in input.price.columns ) {
  print( summary(inputs.temp.df[, paste0(k, ".impute.n.obs")]) )
}




























saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF with soil and rain.Rdata"
load(saved.workspace.path)

inputs.df <- within(inputs.df, {

w01 = x19.fertilizante.bs.kg
w02 = x19.sem.comprada.bs.kg
w03 = hourly.tractor.rental
w04 = x19.plagicidas.bs.kg
w05 = hourly.wage
w06 = x19.abono.bs.kg
# w05 = imputed.ag.wage

x01 = x19.fertilizante.cantidad.kg
x02 = x19.sem.comprada.cantidad.kg
x03 = tractor.hrs.final
x04 = x19.plagicidas.cantidad.kg
x05 = paid.hours.spread 
x06 = x19.abono.cantidad.kg
# x107.hrs.tractor.spread

# x05 = labor.hours * crop.coverage

x01 <- unname(x01)
x02 <- unname(x02)
x03 <- unname(x03)
x04 <- unname(x04)
x05 <- unname(x05)
x06 <- unname(x06)
y01 <-  x19.produccion.obtenidad.kg 


q01 = x19.superficie.cultivada.hectareas
# q01[q01 ==0] = median(q01)

q02 = ifelse(x19.uso.riego!="Si",  0, 1)

q03 = ag.fam.labor.equiv.spread
q03[q03 == 0] = .5
q04 <- c(unname(soil.quality))
q05 <- c(unname(elevation))
q06 <- c(unname(mean.ann.rain.5yr)) / 100 # Rescaling rainfall 

w01[! x19.fertilizante.bs.kg.impute.level %in% c("itself", "household")] <- NA
w02[! x19.sem.comprada.bs.kg.impute.level %in% c("itself", "household")] <- NA
w03[! hourly.tractor.rental.impute.level %in% c("itself", "household")] <- NA
w04[! x19.plagicidas.bs.kg.impute.level %in% c("itself", "household")] <- NA
w05[! hourly.wage.impute.level %in% c("itself", "household")] <- NA
w06[! x19.abono.bs.kg.impute.level %in% c("itself", "household")] <- NA


} )



targ.vars <- c("w01", "w02", "w03", "w04", "w05", "w06", "x01", "x02", "x03", "x04", "x05",
  "x06", "q01", "q02", "q03", "q04", "q05", "q06", "y01")


library("Amelia")

#ff <- folio ~ (x01 + x02 + x03 + x04 + x05 + x06 + 
#  q01 + q02 + q03 + q04 + q05 + q06 + y01 + poly(y01,2))^2 + poly(x01,2) + poly(x02, 2) + poly(x03,2) + poly(x04,2) + poly(x05,2) +
#  poly(x06,2) + poly(q01,2)  + poly(q03,2) + poly(q04,2) + poly(q05,2) + poly(q06,2) + poly(y01,2)
  
ff <- folio ~ (w01 + w02 + w03 + w04 + w05 + w06 + 
  q01 + q02 + q03 + q04 + q05 + q06 + y01  )^2 + I(w01^2) + I(w02^2) + I(w03^2) + I(w04^2) + I(w05^2) +
  I(w06^2) + I(q01^2)  + I(q03^2) + I(q04^2) + I(q05^2) + I(q06^2)  + I(y01^2) +  x01 + x02 + x03 + x04 + x05 + x06

ff <- folio ~ w01 + w02 + w03 + w04 + w05 + w06 + 
  q01 + q02 + q03 + q04 + q05 + q06 + y01 + I(w01^2) + I(w02^2) + I(w03^2) + I(w04^2) + I(w05^2) +
  I(w06^2) + I(q01^2)  + I(q03^2) + I(q04^2) + I(q05^2) + I(q06^2)  + I(y01^2) +  x01 + x02 + x03 + x04 + x05 + x06
utils::str(m <- model.frame(ff, inputs.df, na.action=NULL)) # , na.action=NULL is important
mat <- model.matrix(ff, m)


cor.mat <- cor(mat[, -1], use="pairwise.complete.obs")
diag(cor.mat) <- 0
sort(c(cor.mat))

cor.mat[all.equal(cor.mat,1)]
which(cor.mat>.9999999, arr.ind=TRUE)
cor.mat[ which(cor.mat>.9999999, arr.ind=TRUE), drop=FALSE ]

targ.logs <- colnames(cor.mat)
targ.logs <- targ.logs[!grepl("q02", targ.logs)]

# inputs.df[, targ.vars]
a.out <- amelia(mat[, -1], m = 1,  p2s=2,
  logs=targ.logs) # , incheck=FALSE

a.overimpute <- overimpute(a.out, var = 2)



cor(a.overimpute[, 2], a.overimpute[, 3])
cor(a.overimpute[, 2], a.overimpute[, 3], method="spearman")











region <- toupper(inputs.df$zona.agroproductiva)
region[region=="VALLES CERRADAS     "] <- "VALLES CERRADOS     "

region <- gsub("Ú", "U", region)
region <- gsub(" *$", "", region)
region <- gsub(" ", ".", region)
region <- make.names(region)

region[region %in% c("CHACO.HUMEDO", "CHACO.SECO", "LLANOS.DE.SANTA.CRUZ", "PAMPAS.DE.MOXOS")] <- "CHACO"
region[region %in% c("YUNGAS.DEL.NORTE", "YUNGAS.DEL.SUR", "YUMGAS.DEL.NORTE", "YUNGAS.DE.NORTE")] <- "YUNGAS"

#if (target.crop=="Cebada combined") {
#  region[region %in% "ALTIPLANO.SUR"] <- "ALTIPLANO.CENTRAL"
#}

region[region %in% "ALTIPANO.CENTRAL"] <- "ALTIPLANO.CENTRAL"


region <- factor(region)
inputs.df$region <- region



num.folds <- 10
target.fold <- 3

chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 
# Thanks to http://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r
# all(1:113 %in% unlist(chunk2(sample(113), 10)))
# A-OK


#input.price.columns <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", "x19.plagicidas.bs.kg")
input.price.columns <- c("w01", "w02", "w03", "w04", "w05", "w06")


input.price.nonmissing.ls <- list()

for ( target.input in input.price.columns) {
  input.price.nonmissing.ls[[target.input]] <- which(!is.na(inputs.df[, target.input])) 
}

folds.random.index.by.input.ls <- list()
input.crossval.df <- inputs.df

set.seed(100)

for (target.input in input.price.columns ) {
# Set to input.price.columns[1] for now
  folds.random.index <- sample(input.price.nonmissing.ls[[target.input]])
  folds.ind.ls <- chunk2(folds.random.index, num.folds)
  input.crossval.df[unlist(folds.ind.ls[target.fold]), target.input] <- NA
  folds.random.index.by.input.ls[[target.input]] <- unlist(folds.ind.ls[target.fold])
}


ff <- folio ~ w01 + w02 + w03 + w04 + w05 + w06 + elevation + I(elevation^2) #   zona.agroproductiva +  region + 

ff <- folio ~ w01 + w02 + w03 + w04 + w05 + w06 + elevation + I(elevation^2) #   zona.agroproductiva +  region + 


ff <- folio ~ w01 + w02 + w03 + w04 + w05 + w06 + 
  q01 + q02 + q03 + q04 + q05 + q06 + y01 + I(w01^2) + I(w02^2) + I(w03^2) + I(w04^2) + I(w05^2) +
  I(w06^2) + I(q01^2)  + I(q03^2) + I(q04^2) + I(q05^2) + I(q06^2)  + I(y01^2) +  x01 + x02 + x03 + x04 + x05 + x06 +
  provincia.full  #   zona.agroproductiva +  region + 

input.crossval.df$x19.codigo <- factor(input.crossval.df$x19.codigo) # Drop unused levels
input.crossval.df$zona.agroproductiva <- factor(input.crossval.df$zona.agroproductiva)
median.output.df <- aggregate(y01 ~ x19.codigo, data=input.crossval.df, FUN=median, na.rm=TRUE)
colnames(median.output.df)[2] <- "y01.crop.median"
input.crossval.df$sort.ind <- 1:nrow(input.crossval.df)
input.crossval.df <- merge(input.crossval.df, median.output.df)

input.crossval.df <- input.crossval.df[order(input.crossval.df$sort.ind), ]

ff <- folio ~ w01 + w02 + w03 + w04 + w05 + w06 + 
  q01 + q02 + q03 + q04 + q05 + q06 + I(y01) + I(w01^2) + I(w02^2) + I(w03^2) + I(w04^2) + I(w05^2) +
  I(w06^2) + I(q01^2)  + I(q03^2) + I(q04^2) + I(q05^2) + I(q06^2)  + I((y01)^2) +  x01 + x02 + x03 + x04 + x05 + x06 

# /y01.crop.median /y01.crop.median

utils::str(m <- model.frame(ff, input.crossval.df, na.action=NULL)) # , na.action=NULL is important
mat <- model.matrix(ff, m)

targ.logs <- colnames(mat[, -1])
targ.logs <- targ.logs[!grepl("q02", targ.logs)]
#targ.logs <- targ.logs[grepl("w0", targ.logs)]

# inputs.df[, targ.vars]
a.out <- amelia(mat[, -1], m = 1,  p2s=2, logs=targ.logs, incheck=FALSE)



for (target.input in input.price.columns ) {
  deleted.index <- unlist(folds.random.index.by.input.ls[[target.input]])
  print( cor(inputs.df[deleted.index, target.input], a.out$imputations$imp1[deleted.index, target.input] ) )
  print( cor(log(inputs.df[deleted.index, target.input]), log(a.out$imputations$imp1[deleted.index, target.input]) ) )
  print( cor(inputs.df[deleted.index, target.input], a.out$imputations$imp1[deleted.index, target.input], method="spearman" ) )
  cat("\n")
}


  print( cor(inputs.df[deleted.index, target.input], a.out$imputations$imp1[deleted.index, target.input] ) )


summary(basic.lm <- lm(log(w01) ~ canton.full,
  data=input.crossval.df))

cor(log(inputs.df[deleted.index, target.input]), predict(basic.lm, newdata= droplevels(inputs.df[deleted.index, ])))



[1] 0.06365001
[1] 0.1629388
[1] 0.1607887

[1] 0.3120797
[1] 0.3255153
[1] 0.2180001

[1] 0.1641616
[1] 0.1400182
[1] 0.1169696

[1] 0.2100134
[1] 0.1632509
[1] 0.1284833

[1] 0.005116838
[1] 0.1141685
[1] 0.1263661

[1] 0.1320506
[1] 0.1125831
[1] 0.09041432

With provincia:
# Seems like a big improvement, like double the correlation

[1] 0.08602727
[1] 0.2649471
[1] 0.2313551

[1] 0.3356371
[1] 0.3445603
[1] 0.235755

[1] 0.2539582
[1] 0.3488313
[1] 0.2949304

[1] 0.1239673
[1] 0.188552
[1] 0.1556008

[1] 0.09584629
[1] 0.2750683
[1] 0.2916089

[1] 0.2126267
[1] 0.2123415
[1] 0.1797413



# "Simple" model, but dividing by median quanity of output by crop

[1] 0.03723061
[1] 0.07607127
[1] 0.05868398

[1] 0.2428013
[1] 0.239328
[1] 0.1507125

[1] 0.03795097
[1] 0.09319242
[1] 0.1123526

[1] 0.03001608
[1] 0.1093045
[1] 0.09281291

[1] -0.007413669
[1] 0.06150285
[1] 0.09488542

[1] 0.1800836
[1] 0.2000895
[1] 0.167157





















with(inputs.df,
table(x19.fertilizante.bs.kg.impute.level=="itself" & x19.fertilizante.bs.kg==0)
)
deletion.index.ls[[1]][[target.fold]]
# Checks out



















### BELOW ARE THE CROSSVALIDTION FUNCTIONS





load(file="/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF with soil and rain and advanced drive time fixed.Rdata")


#inputs.df[inputs.df$x19.fertilizante.cantidad.kg==0, "x19.fertilizante.bs.kg.impute.level"] <- "not.itself"
# This is to correct for some weirdness, i.e. all ofthe fert saying "itself". may be a problem with partially using
# The slower way to impute

input.price.columns <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", "x19.plagicidas.bs.kg")

# THIS PART BELOW KILLS ANYTHING THAT HAD BEEN IMPUTED BEFORE
for (target.input in input.price.columns) {
  inputs.df[inputs.df[, paste0(target.input, ".impute.level")] != "itself", target.input] <- 0
}



# NEW IMPUTE FN:

min.impute.sample.size <- 3
mean.or.median <- mean


geog.impute.fn <- function(inputs.df) {




impute.levels <- c("household", "segmento.full", "sector.full", "canton.full", "seccion.full", "provincia.full", "departamento", "nation")

input.price.columns <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", "x19.plagicidas.bs.kg")





geog.split.ls <- list()

for ( target.input in input.price.columns) {
  list.temp <- list()
  for (i in impute.levels) {
    inputs.df[, paste0(target.input, ".impute.level")] <- NA
    inputs.df[inputs.df[, target.input]!=0, paste0(target.input, ".impute.level")] <- "itself"
    
    if ( i=="household") { split.level <- "folio"} else { split.level <- i}
    if (i=="nation") {
      if (target.input != "x19.sem.comprada.bs.kg") {
        # Do nothing, since the overall average will get picked up below for anything that isn't seeds
      } else {
        inputs.for.split <- inputs.df[ inputs.df[, paste0(target.input, ".impute.level")] %in% "itself", 
          c(target.input, "x19.codigo")]
        list.temp[[i]] <- split(x=inputs.for.split , f=list( paste0("nation.", inputs.for.split[, "x19.codigo"])), drop=FALSE)
      }
      next
    }
    inputs.for.split <- inputs.df[ inputs.df[, paste0(target.input, ".impute.level")] %in% "itself", 
      c(split.level, target.input, "x19.codigo")]
      
    if (target.input != "x19.sem.comprada.bs.kg") {
      list.temp[[i]] <- split(x=inputs.for.split , f=inputs.for.split[, split.level])
    } else {
      list.temp[[i]] <- split(x=inputs.for.split , f=list(inputs.for.split[, split.level], inputs.for.split[, "x19.codigo"]), drop=TRUE)
    }
  }
  geog.split.ls[[target.input]] <- list.temp
}

#      list.temp[[i]] <- list()
#      for ( j in unique(inputs.for.split$x19.codigo) ) {





input.price.columns <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", "x19.plagicidas.bs.kg")

nation.input.averages <- apply(inputs.df[, input.price.columns], 2, FUN=function(x) mean.or.median(x[x>0], na.rm=TRUE) )
nation.input.nobs <- apply(inputs.df[, input.price.columns], 2, FUN=function(x) { x <- x[!is.na(x)]; length(x[x>0])} )
nation.input.sd <- apply(inputs.df[, input.price.columns], 2, FUN=function(x) sd(x[x>0], na.rm=TRUE) )

nation.input.averages.log <- apply(inputs.df[, input.price.columns], 2, FUN=function(x) mean.or.median(log(x[x>0]), na.rm=TRUE) )
nation.input.sd.log <- apply(inputs.df[, input.price.columns], 2, FUN=function(x) sd(log(x[x>0]), na.rm=TRUE) )

for (target.input in input.price.columns ) {

  inputs.df[, paste0(target.input, ".impute.level")] <- NA
  
  inputs.df[inputs.df[, target.input]!=0, paste0(target.input, ".impute.level")] <- "itself"

impute.levels <- c("household", "segmento.full", "sector.full", "canton.full", "seccion.full", "provincia.full", "departamento", "nation")

# Rprof()

imputed.data.ls<- vector(mode = "list", length = nrow(inputs.df))

geog.split.ls.targ.input <- geog.split.ls[[target.input]]

for (i in 1:nrow(inputs.df)) {

if (inputs.df[i, paste0(target.input, ".impute.level")] %in% "itself") {next}

for (impute.level in impute.levels) {

  if (impute.level=="nation") { 
    if (target.input=="x19.sem.comprada.bs.kg") {
    
      nation.impute.data <- geog.split.ls.targ.input[[ impute.level ]][[ paste("nation", inputs.df$x19.codigo[i], sep=".") ]][ , target.input]
      
      if ( length(nation.impute.data) < 1 ) {
        nation.impute.data <- nation.input.averages[target.input]
      }
    
      imputed.data.ls[[i]] <- c(unname(mean.or.median(nation.impute.data)), 
        impute.level, unname(length(nation.impute.data)), unname(sd(nation.impute.data)),
        unname(mean.or.median(log(nation.impute.data))), unname(sd(log(nation.impute.data))))
    
    } else {
      imputed.data.ls[[i]] <- c(unname(nation.input.averages[target.input]), 
        impute.level, unname(nation.input.nobs[target.input]), unname(nation.input.sd[target.input]),
        unname(nation.input.averages.log[target.input]), unname(nation.input.sd.log[target.input]))
    }
    break
  }
  
  if (target.input=="x19.sem.comprada.bs.kg") {
  
    if(impute.level=="household") {
      impute.data <-   geog.split.ls.targ.input[[ impute.level ]][[ paste(inputs.df$folio[i], inputs.df$x19.codigo[i], sep=".") ]][, target.input]
    } else {
      impute.data <- geog.split.ls.targ.input[[ impute.level ]][[ paste(inputs.df[i, impute.level], inputs.df$x19.codigo[i], sep=".") ]][ , target.input]
    }
  
  } else {
  
    if(impute.level=="household") {
      impute.data <-   geog.split.ls.targ.input[[ impute.level ]][[ inputs.df$folio[i] ]][ , target.input]
    } else {
      impute.data <- geog.split.ls.targ.input[[ impute.level ]][[ inputs.df[i, impute.level] ]][ , target.input]
    }
  
  }
  
  
  
  if (impute.level=="household" && !all(is.na(impute.data)) && length(impute.data)>0  ) {
     imputed.data.ls[[i]] <- c(mean.or.median( impute.data ), "household", length(impute.data), sd(impute.data),
     mean.or.median( log(impute.data) ), sd(log(impute.data)))
     break
  }
  if (impute.level=="household") {next}
  
  if (!all(is.na(impute.data)) && length(impute.data) >=  min.impute.sample.size ) {

    imputed.data.ls[[i]] <- c(mean.or.median( impute.data ), impute.level, length(impute.data), sd(impute.data),
    mean.or.median( log(impute.data) ), sd(log(impute.data)))
    break
  }
  

  
}
 
 cat(target.input, i, impute.level, "\n")
  
}

temp.imputed.df <- data.frame(matrix(unlist(imputed.data.ls), ncol=6, byrow=TRUE), stringsAsFactors=FALSE)
temp.imputed.df[, 1] <- as.numeric(temp.imputed.df[, 1])
temp.imputed.df[, 3] <- as.numeric(temp.imputed.df[, 3])
temp.imputed.df[, 4] <- as.numeric(temp.imputed.df[, 4])
temp.imputed.df[, 5] <- as.numeric(temp.imputed.df[, 5])
temp.imputed.df[, 6] <- as.numeric(temp.imputed.df[, 6])

inputs.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), target.input] <- 
  temp.imputed.df[, 1]

inputs.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), 
  paste0(target.input, ".impute.level") ] <- temp.imputed.df[, 2]
  
inputs.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), 
  paste0(target.input, ".impute.n.obs") ] <- temp.imputed.df[, 3]
  
inputs.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), 
  paste0(target.input, ".impute.sd") ] <- temp.imputed.df[, 4]
  
inputs.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), 
  paste0(target.input, ".impute.average.log") ] <- temp.imputed.df[, 5]

inputs.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), 
  paste0(target.input, ".impute.sd.log") ] <- temp.imputed.df[, 6]

}

inputs.df

}






# system.time( inputs.df.imputed <- geog.impute.fn(inputs.df) )



for (k in input.price.columns ) {
  print( table(inputs.df.imputed[, paste0(k, ".impute.level")], useNA="always") )
}

for (k in input.price.columns ) {
  print( summary(inputs.df.imputed[, paste0(k, ".impute.n.obs")]) )
}

for (k in input.price.columns ) {
  print( summary(inputs.df.imputed[, paste0(k, ".impute.sd")]) )
}










num.folds <- 10
# target.fold <- 3


# crossval.geog.fn <- function(inputs.df, num.folds) {

inputs.df.orig <- inputs.df

chunk2 <- function(x,n) split(x, cut(seq_along(x), n, labels = FALSE)) 
# THanks to http://stackoverflow.com/questions/3318333/split-a-vector-into-chunks-in-r
# all(1:113 %in% unlist(chunk2(sample(113), 10)))
# A-OK

input.price.columns <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", "x19.plagicidas.bs.kg")

set.seed(100)

deletion.index.ls <- list()

for (target.input in input.price.columns ) {

  # Ok, I am changing this to a household-level deletion of info

  nonmissing.index <- which(inputs.df[, target.input] > 0)
  
  nonmissing.folio.index <-unique(inputs.df$folio[nonmissing.index])
  
  nonmissing.index.to.remove <- sample(nonmissing.folio.index) # This just scrmables the indices
  deletion.index.ls[[target.input]] <- chunk2(nonmissing.index.to.remove, num.folds)

}

comparison.data.ls <- list()
imputed.comparison.data.ls <- list()
data.comparison.by.fold.ls <- list()
imputed.comparison.data.for.amelia.ls <- list()
which.level.imputed.comparison.ls <- list()

# NOTE: BELOW IS SOME MODERATELY-COMPLICATED CODE. IT INVOLVES 2 LEVELS OF LISTS.

for (target.fold in 1:num.folds ) { # 1:num.folds

  inputs.df <- inputs.df.orig
  comparison.data.ls[[target.fold]] <- list()

  for (target.input in input.price.columns ) {
  
    target.deletion <- deletion.index.ls[[target.input]][[target.fold]]

    comparison.data.ls[[target.fold]][[target.input]] <- inputs.df[ inputs.df$folio %in% target.deletion , target.input]

    inputs.df[inputs.df$folio %in% target.deletion , target.input] <- 0
  }
  
  inputs.df.imputed <- geog.impute.fn(inputs.df)
  
  imputed.comparison.data.ls[[target.fold]] <- list()
  which.level.imputed.comparison.ls[[target.fold]] <- list()
  
  comparison.dfs.list <- list()
  
  for (target.input in input.price.columns ) {
  
    target.deletion <- deletion.index.ls[[target.input]][[target.fold]]

    imputed.comparison.data.ls[[target.fold]][[target.input]] <- 
      inputs.df.imputed[ inputs.df$folio %in% target.deletion , target.input]
    
    which.level.imputed.comparison.ls[[target.fold]][[target.input]] <- 
      inputs.df.imputed[ inputs.df$folio %in% target.deletion ,  paste0(target.input, ".impute.level")]
    
    comparison.dfs.list[[target.input]] <- 
      data.frame(actual=comparison.data.ls[[target.fold]][[target.input]], 
        imputed=imputed.comparison.data.ls[[target.fold]][[target.input]])

  }
  
  data.comparison.by.fold.ls[[target.fold]] <- comparison.dfs.list
  imputed.comparison.data.for.amelia.ls[[target.fold]] <- inputs.df.imputed
  
}

# Ok, so below the basic list element is the fold
CV.stats.ls <- lapply(data.comparison.by.fold.ls, function(x, trim=0) {
  input.price.columns <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", "x19.plagicidas.bs.kg")
  stat.to.fill.v <- vector(mode="numeric", length=length(input.price.columns))
  names(stat.to.fill.v) <- input.price.columns
  for (target.input in input.price.columns ) {  
    x[[target.input]] <- x[[target.input]][ x[[target.input]][, 1] > 0    ,] 
    # Do above because now we have many zeros where a firm uses fert on some plots and not others
    cutoff.1 <- quantile(x[[target.input]][, 1], probs=c(trim, 1-trim))
    cutoff.2 <- quantile(x[[target.input]][, 2], probs=c(trim, 1-trim))
    x[[target.input]] <- x[[target.input]][ 
      x[[target.input]][, 1] >= cutoff.1[1] & x[[target.input]][, 1] <= cutoff.1[2] &
      x[[target.input]][, 2] >= cutoff.2[1] & x[[target.input]][, 2] <= cutoff.2[2] ,
      ]
    stat.to.fill.v[target.input] <- cor( x[[target.input]], use="complete.obs" )[1, 2]
  }
  data.frame(t(stat.to.fill.v))
} )

CV.stats.df <- do.call(rbind, CV.stats.ls)
colMeans(CV.stats.df)




rmse <- function(x) {  sqrt(mean( (x[, 1] - x[, 2])^2, na.rm=TRUE)) }


# Below is RMSE:

CV.stats.ls <- lapply(data.comparison.by.fold.ls, function(x, trim=0) {
  input.price.columns <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", "x19.plagicidas.bs.kg")
  stat.to.fill.v <- vector(mode="numeric", length=length(input.price.columns))
  names(stat.to.fill.v) <- input.price.columns
  for (target.input in input.price.columns ) {  
    x[[target.input]] <- x[[target.input]][ x[[target.input]][, 1] > 0    ,] 
    # Do above because now we have many zeros where a firm uses fert on some plots and not others
    cutoff.1 <- quantile(x[[target.input]][, 1], probs=c(trim, 1-trim))
    cutoff.2 <- quantile(x[[target.input]][, 2], probs=c(trim, 1-trim))
    x[[target.input]] <- x[[target.input]][ 
      x[[target.input]][, 1] >= cutoff.1[1] & x[[target.input]][, 1] <= cutoff.1[2] &
      x[[target.input]][, 2] >= cutoff.2[1] & x[[target.input]][, 2] <= cutoff.2[2] ,
      ]
    stat.to.fill.v[target.input] <- rmse(x[[target.input]]) # / sd(x[[target.input]][, 1])
  }
  data.frame(t(stat.to.fill.v))
} )

CV.stats.df <- do.call(rbind, CV.stats.ls)
colMeans(CV.stats.df)




CV.stats.ls <- lapply(data.comparison.by.fold.ls, function(x, trim=0) {
  input.price.columns <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", "x19.plagicidas.bs.kg")
  stat.to.fill.v <- vector(mode="numeric", length=length(input.price.columns))
  names(stat.to.fill.v) <- input.price.columns
  for (target.input in input.price.columns ) {  
    x[[target.input]] <- x[[target.input]][ x[[target.input]][, 1] > 0    ,] 
    # Do above because now we have many zeros where a firm uses fert on some plots and not others
    cutoff.1 <- quantile(x[[target.input]][, 1], probs=c(trim, 1-trim))
    cutoff.2 <- quantile(x[[target.input]][, 2], probs=c(trim, 1-trim))
    x[[target.input]] <- x[[target.input]][ 
      x[[target.input]][, 1] >= cutoff.1[1] & x[[target.input]][, 1] <= cutoff.1[2] &
      x[[target.input]][, 2] >= cutoff.2[1] & x[[target.input]][, 2] <= cutoff.2[2] ,
      ]
    stat.to.fill.v[target.input] <- mean(x[[target.input]][, 2] - x[[target.input]][, 1])
  }
  data.frame(t(stat.to.fill.v))
} )

CV.stats.df <- do.call(rbind, CV.stats.ls)
colMeans(CV.stats.df)
# This calculates bias



CV.stats.ls <- lapply(data.comparison.by.fold.ls, function(x, trim=0) {
  input.price.columns <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", "x19.plagicidas.bs.kg")
  stat.to.fill.v <- vector(mode="numeric", length=length(input.price.columns))
  names(stat.to.fill.v) <- input.price.columns
  for (target.input in input.price.columns ) {  
    x[[target.input]] <- x[[target.input]][ x[[target.input]][, 1] > 0    ,] 
    # Do above because now we have many zeros where a firm uses fert on some plots and not others
    cutoff.1 <- quantile(x[[target.input]][, 1], probs=c(trim, 1-trim))
    cutoff.2 <- quantile(x[[target.input]][, 2], probs=c(trim, 1-trim))
    x[[target.input]] <- x[[target.input]][ 
      x[[target.input]][, 1] >= cutoff.1[1] & x[[target.input]][, 1] <= cutoff.1[2] &
      x[[target.input]][, 2] >= cutoff.2[1] & x[[target.input]][, 2] <= cutoff.2[2] ,
      ]
    stat.to.fill.v[target.input] <- mean(x[[target.input]][, 1])
  }
  data.frame(t(stat.to.fill.v))
} )

CV.stats.df <- do.call(rbind, CV.stats.ls)
colMeans(CV.stats.df)
# This calculates mean of the original value
















#table( abs(comparison.dfs.list[[1]][1] - comparison.dfs.list[[1]][2]) < .001   )
# plot(data.comparison.by.fold.ls[[1]][[1]])


#which.level.imputed.comparison.ls[[1]][[1]][ (comparison.dfs.list[[1]][1] - comparison.dfs.list[[1]][2]) < .001]

#comparison.dfs.list[[1]]*46







library("Amelia")

target.fold <- 1
amelia.data.comparison.by.fold.ls <- list()

set.seed(100)

for (target.fold in c(1:num.folds)) {



cat("\n\n\n\n\nCURRENT FOLD: ", target.fold, "\n\n\n\n\n")

inputs.for.amelia.df <- imputed.comparison.data.for.amelia.ls[[target.fold]]

did.other.imputations <- TRUE

inputs.for.amelia.df <- within(inputs.for.amelia.df, {

w01 = x19.fertilizante.bs.kg
w02 = x19.sem.comprada.bs.kg
if (did.other.imputations) w03 = hourly.tractor.rental
w04 = x19.plagicidas.bs.kg
if (did.other.imputations) w05 = hourly.wage
w06 = x19.abono.bs.kg
# w05 = imputed.ag.wage

x01 = x19.fertilizante.cantidad.kg
x02 = x19.sem.comprada.cantidad.kg
if (did.other.imputations) x03 = tractor.hrs.final
x04 = x19.plagicidas.cantidad.kg
if (did.other.imputations) x05 = paid.hours.spread 
x06 = x19.abono.cantidad.kg
# x107.hrs.tractor.spread

# x05 = labor.hours * crop.coverage

x01 <- unname(x01)
x02 <- unname(x02)
if (did.other.imputations) x03 <- unname(x03)
x04 <- unname(x04)
if (did.other.imputations) x05 <- unname(x05)
x06 <- unname(x06)
y01 <-  x19.produccion.obtenidad.kg 


q01 = x19.superficie.cultivada.hectareas
# q01[q01 ==0] = median(q01)

q02 = ifelse(x19.uso.riego!="Si",  0, 1)

if (did.other.imputations) q03 = ag.fam.labor.equiv.spread
if (did.other.imputations) q03[q03 == 0] = .5
if (did.other.imputations) q04 <- c(unname(soil.quality))
if (did.other.imputations) q05 <- c(unname(elevation))
if (did.other.imputations) q06 <- c(unname(mean.ann.rain.5yr)) / 100 # Rescaling rainfall 

w01[! x19.fertilizante.bs.kg.impute.level %in% c("itself", "household")] <- NA
w02[! x19.sem.comprada.bs.kg.impute.level %in% c("itself", "household")] <- NA
if (did.other.imputations) w03[! hourly.tractor.rental.impute.level %in% c("itself", "household")] <- NA
w04[! x19.plagicidas.bs.kg.impute.level %in% c("itself", "household")] <- NA
if (did.other.imputations) w05[! hourly.wage.impute.level %in% c("itself", "household")] <- NA
w06[! x19.abono.bs.kg.impute.level %in% c("itself", "household")] <- NA


} )


  
input.price.columns <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", "x19.plagicidas.bs.kg")
# See "priors" at http://cran.r-project.org/web/packages/Amelia/vignettes/amelia.pdf
#In any row, the entry in the first column is the row of the observation and the entry
#is the second column is the column of the observation.  In the four column priors
#matrix the third and fourth columns are the mean and standard deviation of the
#prior distribution of the missing value.

priors.mat.ls <- list()  

#input.price.columns <- paste0(input.price.columns, ".impute.average.log")

for (target.input in input.price.columns) {
  which.missing <- which( ! inputs.for.amelia.df[, paste0(target.input, ".impute.level")] %in% c("itself", "household") )
  which.column <- which(target.input == input.price.columns)
  priors.mat.ls[[target.input]]<- data.frame( a=which.missing,  b=which.column, 
    c=as.numeric(as.character(inputs.for.amelia.df[which.missing, paste0(target.input, ".impute.average.log")])), 
    d=as.numeric(as.character(inputs.for.amelia.df[which.missing, paste0(target.input, ".impute.sd.log")])) )
}

priors.mat <- as.matrix(do.call(rbind, priors.mat.ls) )

# NOTE TO SELF: GIVING AMELIA A FREEBIE WITH THE HOUSEHOLD-LEVEL IMPUTATION

priors.mat[priors.mat[, 4]==0 | is.na(priors.mat[, 4]), 4] <- 
  jitter(rep(quantile(priors.mat[, 4], prob=.1, na.rm=TRUE), sum(priors.mat[, 4]==0 | is.na(priors.mat[, 4]))))
# setting when sd is zero



# NOTE: HAD TO RE-ARRANGE THE w's below to correspond to the above arrangement of the priors mat 

ff <- folio ~ w01 + w02 + w06 + w04 + 
  q01 + q02 + I(y01) + I(w01^2) + I(w02^2) + I(w04^2) +
  I(w06^2) + I(q01^2) + I((y01)^2) + x01 + x02  + x04  + x06 

ff <- folio ~ w01 + w02 + w06 + w04 + w03 + w05 + 
  q01 + q02 + q03 + q04 + q05 + q06 + I(y01) + I(w01^2) + I(w02^2) + I(w03^2) + I(w04^2) + I(w05^2) +
  I(w06^2) + I(q01^2)  + I(q03^2) + I(q04^2) + I(q05^2) + I(q06^2)  + I((y01)^2) +  x01 + x02 + x03 + x04 + x05 + x06 +
  cities.within.2.hrs + I(cities.within.4.hrs-cities.within.2.hrs)  +
  I(cities.within.8.hrs-cities.within.4.hrs) + I(cities.within.16.hrs-cities.within.8.hrs) +
  I(cities.within.32.hrs-cities.within.16.hrs) +
  dist.to.road + drive.time.la.paz +
  drive.time.amanzanada
  
ff <- folio ~ w01 + w02 + w06 + w04 + w03 + w05 + 
  q01 + q02 + q03 + q04 + q05 + q06 + I(y01) + 
  I(q01^2)  + I(q03^2) + I(q04^2) + I(q05^2) + I(q06^2)  + I((y01)^2) +  x01 + x02 + x03 + x04 + x05 + x06 +
  cities.within.2.hrs + I(cities.within.4.hrs-cities.within.2.hrs)  +
  I(cities.within.8.hrs-cities.within.4.hrs) + I(cities.within.16.hrs-cities.within.8.hrs) +
  I(cities.within.32.hrs-cities.within.16.hrs) +
  dist.to.road + drive.time.la.paz +
  drive.time.amanzanada
# Above is no price quadratic terms. This may speed convergence. 

#ff <- folio ~ w01 + w02 + w06 + w04 + w03 + w05 + 
#  provincia.full

#ff <- folio ~ w01 + w02 + w06 + w04 + w03 + w05 + 
#  q01 + q02 + q03 + q04 + q05 + q06 + I(y01) + I(w01^2) + I(w02^2) + I(w03^2) + I(w04^2) + I(w05^2) +
#  I(w06^2) + I(q01^2)  + I(q03^2) + I(q04^2) + I(q05^2) + I(q06^2)  + I((y01)^2) +  x01 + x02 + x03 + x04 + x05 + x06





#ff <- folio ~ w01 + w02 + w06 + w04 + w03 + w05 +
#  cities.within.2.hrs + I(cities.within.4.hrs-cities.within.2.hrs)  +
#  I(cities.within.8.hrs-cities.within.4.hrs) + I(cities.within.16.hrs-cities.within.8.hrs) +
#  I(cities.within.32.hrs-cities.within.16.hrs) +
#  dist.to.road + drive.time.la.paz +
#  drive.time.amanzanada

# Not bothering trying to put a prior on the "w" square terms
# /y01.crop.median /y01.crop.median

# utils::str(m <- model.frame(ff, inputs.for.amelia.df, na.action=NULL)) # , na.action=NULL is important
m <- model.frame(ff, inputs.for.amelia.df, na.action=NULL) # , na.action=NULL is important
mat <- model.matrix(ff, m)
mat <- mat[, -1] # Take out the intercept

targ.logs <- colnames(mat)
targ.logs <- targ.logs[!grepl("q02", targ.logs)]
#targ.logs <- targ.logs[grepl("w[0-9][0-9]", targ.logs)]
#targ.logs <- targ.logs[grepl("w0", targ.logs)]

# inputs.df[, targ.vars]

#priors.mat[, 3] <- log(priors.mat[, 3])
#priors.mat[, 4] <- log(priors.mat[, 4])

a.out <- amelia(mat, m = 1,  p2s=2, logs=targ.logs, incheck=TRUE, priors =priors.mat) #, empri = 0.01*nrow(mat))

input.price.columns <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", "x19.plagicidas.bs.kg")
input.price.columns.by.num <- c("w01", "w02", "w06", "w04")


  for (target.input in 1:length(input.price.columns) ) {
  
    target.deletion <- deletion.index.ls[[ input.price.columns[target.input] ]][[target.fold]]

    imputed.comparison.data.temp <- 
      a.out$imputations$imp1[ inputs.for.amelia.df[, "folio"] %in% target.deletion , input.price.columns.by.num[target.input] ]
    
    comparison.dfs.list[[target.input]] <- 
      data.frame(actual=comparison.data.ls[[target.fold]][[ input.price.columns[target.input] ]], 
        imputed=imputed.comparison.data.temp )

  }
  
amelia.data.comparison.by.fold.ls[[target.fold]] <- comparison.dfs.list

}


amelia.data.comparison.by.fold.ls <- amelia.data.comparison.by.fold.ls[!sapply(amelia.data.comparison.by.fold.ls, FUN=is.null)]

# amelia.data.comparison.by.fold.ls <- amelia.data.comparison.by.fold.ls[1:6]

CV.stats.ls <- lapply(amelia.data.comparison.by.fold.ls, function(x) {
  input.price.columns <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", "x19.plagicidas.bs.kg")
  stat.to.fill.v <- vector(mode="numeric", length=length(input.price.columns))
  names(stat.to.fill.v) <- input.price.columns
  for (target.input in input.price.columns ) {  
    x[[target.input]] <- x[[target.input]][ x[[target.input]][, 1] > 0    ,] 
    # Do above because now we have many zeros where a firm uses fert on some plots and not others
    stat.to.fill.v[target.input] <- cor(x[[target.input]], use="complete.obs")[1, 2]
  }
  data.frame(t(stat.to.fill.v))
} )

CV.stats.df <- do.call(rbind, CV.stats.ls)
colMeans(CV.stats.df)




priors.mat.ls[[target.input]]


inputs.for.amelia.df[, paste0(target.input, ".impute.level")] %in% c("itself", "household")

priors.mat.ls[[target.input]][ 10:20, ]

inputs.for.amelia.df[10:20, c(target.input, paste0(target.input, ".impute.level") )] 


prop.table(table(imputed.comparison.data.for.amelia.ls[[1]][, paste0(target.input, ".impute.level")] ))


priors.mat.ls[[target.input]][ 5442:5465, ]

  5442       5448       5458 
         0         14          0          1     501661   67999702     121138 
      5465       6020       6040       6049       6169       6180       6187 
1433606338 


Ok, before taking log of prior:
x19.fertilizante.bs.kg x19.sem.comprada.bs.kg        x19.abono.bs.kg 
          -0.006932621            0.128837761            0.067783561 
  x19.plagicidas.bs.kg 
           0.033783222 



With log prior:
x19.fertilizante.bs.kg x19.sem.comprada.bs.kg        x19.abono.bs.kg 
           0.008312882            0.742352388            0.089335413 
  x19.plagicidas.bs.kg 
           0.099686884 
> 

So, even log prior is not so good







rmse <- function(x) {  sqrt(mean( (x[, 1] - x[, 2])^2, na.rm=TRUE)) }


# Below is RMSE:

CV.stats.ls <- lapply(amelia.data.comparison.by.fold.ls, function(x, trim=0) {
  input.price.columns <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", "x19.plagicidas.bs.kg")
  stat.to.fill.v <- vector(mode="numeric", length=length(input.price.columns))
  names(stat.to.fill.v) <- input.price.columns
  for (target.input in input.price.columns ) {  
    x[[target.input]] <- x[[target.input]][ x[[target.input]][, 1] > 0    ,] 
    # Do above because now we have many zeros where a firm uses fert on some plots and not others
    cutoff.1 <- quantile(x[[target.input]][, 1], probs=c(trim, 1-trim))
    cutoff.2 <- quantile(x[[target.input]][, 2], probs=c(trim, 1-trim))
    x[[target.input]] <- x[[target.input]][ 
      x[[target.input]][, 1] >= cutoff.1[1] & x[[target.input]][, 1] <= cutoff.1[2] &
      x[[target.input]][, 2] >= cutoff.2[1] & x[[target.input]][, 2] <= cutoff.2[2] ,
      ]
    stat.to.fill.v[target.input] <- rmse(x[[target.input]])
  }
  data.frame(t(stat.to.fill.v))
} )

CV.stats.df <- do.call(rbind, CV.stats.ls)
colMeans(CV.stats.df)






CV.stats.ls <- lapply(amelia.data.comparison.by.fold.ls, function(x, trim=0) {
  input.price.columns <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", "x19.plagicidas.bs.kg")
  stat.to.fill.v <- vector(mode="numeric", length=length(input.price.columns))
  names(stat.to.fill.v) <- input.price.columns
  for (target.input in input.price.columns ) {  
    x[[target.input]] <- x[[target.input]][ x[[target.input]][, 1] > 0    ,] 
    # Do above because now we have many zeros where a firm uses fert on some plots and not others
    cutoff.1 <- quantile(x[[target.input]][, 1], probs=c(trim, 1-trim))
    cutoff.2 <- quantile(x[[target.input]][, 2], probs=c(trim, 1-trim))
    x[[target.input]] <- x[[target.input]][ 
      x[[target.input]][, 1] >= cutoff.1[1] & x[[target.input]][, 1] <= cutoff.1[2] &
      x[[target.input]][, 2] >= cutoff.2[1] & x[[target.input]][, 2] <= cutoff.2[2] ,
      ]
    stat.to.fill.v[target.input] <- mean(x[[target.input]][, 2] - x[[target.input]][, 1])
  }
  data.frame(t(stat.to.fill.v))
} )

CV.stats.df <- do.call(rbind, CV.stats.ls)
colMeans(CV.stats.df)
# This calculates bias












# NOTE: The Amelia imputation may be off ( in a broad, theoretical, non-fixable sense)
# a bit since I do not include the fact that there is zero inputs
# when the price is actually missing.


# amelia.data.comparison.by.fold.ls <- amelia.data.comparison.by.fold.ls[!sapply(amelia.data.comparison.by.fold.ls, FUN=is.null)]

# Hmm. Currently chokes at 8th fold

#3rd iter does not want to converge





amelia.data.comparison.by.fold.ls.save.2 <- amelia.data.comparison.by.fold.ls


amelia.data.comparison.by.fold.ls.save.ridge.prior <- amelia.data.comparison.by.fold.ls
# amelia.data.comparison.by.fold.ls.save
amelia.data.comparison.by.fold.ls.without.targ.logs.w01 <- amelia.data.comparison.by.fold.ls

CV.stats.ls <- lapply(amelia.data.comparison.by.fold.ls.save.ridge.prior, function(x) {
  input.price.columns <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", "x19.plagicidas.bs.kg")
  stat.to.fill.v <- vector(mode="numeric", length=length(input.price.columns))
  names(stat.to.fill.v) <- input.price.columns
  for (target.input in input.price.columns ) {  
    stat.to.fill.v[target.input] <- cor(x[[target.input]], use="complete.obs")[1, 2]
  }
  data.frame(t(stat.to.fill.v))
} )

CV.stats.df <- do.call(rbind, CV.stats.ls)
colMeans(CV.stats.df)
# Adding a  ridge prior seems like it may be a wash?



summary(lm(log(w01) ~ w02 + w06 + w04 + w03 + w05 + 
  q01 + q02 + q03 + q04 + q05 + q06 + I(y01) + I(w01^2) + I(w02^2) + I(w03^2) + I(w04^2) + I(w05^2) +
  I(w06^2) + I(q01^2)  + I(q03^2) + I(q04^2) + I(q05^2) + I(q06^2)  + I((y01)^2) +  x01 + x02 + x03 + x04 + x05 + x06 +
  cities.within.2.hrs + I(cities.within.4.hrs-cities.within.2.hrs)  +
  I(cities.within.8.hrs-cities.within.4.hrs) + I(cities.within.16.hrs-cities.within.8.hrs) +
  I(cities.within.32.hrs-cities.within.16.hrs) +
  dist.to.road + drive.time.la.paz +
  drive.time.amanzanada, data=inputs.for.amelia.df))



plot(a.out, which.vars=1:4, log="x")
overimpute(a.out, var=1, log="x")
disperse(a.out, dims = 1, m = 5)
# Seems like they converge to the same place



for 

deletion.index.ls[[target.input]][[target.fold]]

a.out$imputations$imp1


# a.out <- amelia(mat, m = 1,  p2s=2, logs=targ.logs, incheck=TRUE, priors =priors.mat)


hist(log(mat[,1]))
shapiro.test(log(mat[,1]))
# Ok, higher W stat is good
qqnorm(log(mat[,1]))
abline(0, 1)







# Start with correlation first 

data.frame(t(c(a=1, b=2)))


list.test <- list()

list.test[[1]][[1]] <- 5

  









geog.impute.fn






















































# NEW IMPUTE FN:




geog.impute.fn <- function(inputs.df) {




impute.levels <- c("household", "segmento.full", "sector.full", "canton.full", "seccion.full", "provincia.full", "departamento", "nation")

input.price.columns <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", "x19.plagicidas.bs.kg")





geog.split.ls <- list()

for ( target.input in input.price.columns) {
  list.temp <- list()
  for (i in impute.levels) {
    inputs.df[, paste0(target.input, ".impute.level")] <- NA
    inputs.df[inputs.df[, target.input]!=0, paste0(target.input, ".impute.level")] <- "itself"
    
    if ( i=="household") { split.level <- "folio"} else { split.level <- i}
    if (i=="nation") {geog.split.ls[[i]] <- inputs.df; next}
    
    inputs.for.split <- inputs.df[ inputs.df[, paste0(target.input, ".impute.level")] %in% "itself", 
      c(split.level, target.input, "x19.codigo")]
      
    if (target.input != "x19.sem.comprada.bs.kg") {
      list.temp[[i]] <- split(x=inputs.for.split , f=inputs.for.split[, split.level])
    } else {
      list.temp[[i]] <- split(x=inputs.for.split , f=list(inputs.for.split[, split.level], inputs.for.split[, "x19.codigo"]), drop=TRUE)
    }
  }
  geog.split.ls[[target.input]] <- list.temp
}

#      list.temp[[i]] <- list()
#      for ( j in unique(inputs.for.split$x19.codigo) ) {





input.price.columns <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", "x19.plagicidas.bs.kg")

nation.input.averages <- apply(inputs.df[, input.price.columns], 2, FUN=function(x) median(x[x>0], na.rm=TRUE) )
nation.input.nobs <- apply(inputs.df[, input.price.columns], 2, FUN=function(x) { x <- x[!is.na(x)]; length(x[x>0])} )
nation.input.sd <- apply(inputs.df[, input.price.columns], 2, FUN=function(x) sd(x[x>0], na.rm=TRUE) )

for (target.input in input.price.columns ) {

  inputs.df[, paste0(target.input, ".impute.level")] <- NA
  
  inputs.df[inputs.df[, target.input]!=0, paste0(target.input, ".impute.level")] <- "itself"

impute.levels <- c("household", "segmento.full", "sector.full", "canton.full", "seccion.full", "provincia.full", "departamento", "nation")

# Rprof()

imputed.data.ls<- vector(mode = "list", length = nrow(inputs.df))

geog.split.ls.targ.input <- geog.split.ls[[target.input]]

for (i in 1:nrow(inputs.df)) {

if (inputs.df[i, paste0(target.input, ".impute.level")] %in% "itself") {next}

for (impute.level in impute.levels) {

  if (impute.level=="nation") { 
    imputed.data.ls[[i]] <- c(unname(nation.input.averages[target.input]), 
      impute.level, unname(nation.input.nobs[target.input]), unname(nation.input.sd[target.input]))
    break
  }
  
  if (target.input=="x19.sem.comprada.bs.kg") {
  
    if(impute.level=="household") {
      impute.data <-   geog.split.ls.targ.input[[ impute.level ]][[ paste(inputs.df$folio[i], inputs.df$x19.codigo[i], sep=".") ]][, target.input]
    } else {
      impute.data <- geog.split.ls.targ.input[[ impute.level ]][[ paste(inputs.df[i, impute.level], inputs.df$x19.codigo[i], sep=".") ]][ , target.input]
    }
  
  } else {
  
    if(impute.level=="household") {
      impute.data <-   geog.split.ls.targ.input[[ impute.level ]][[ inputs.df$folio[i] ]][ , target.input]
    } else {
      impute.data <- geog.split.ls.targ.input[[ impute.level ]][[ inputs.df[i, impute.level] ]][ , target.input]
    }
  
  }
  
  
  
  if (impute.level=="household" && !all(is.na(impute.data)) && length(impute.data)>0  ) {
    
#     inputs.df[i, target.input] <- median( impute.data )
#     inputs.df[i, c(target.input, paste0(target.input, ".impute.level") )] <- "household"
      imputed.data.ls[[i]] <- c(median( impute.data ), "household", length(impute.data), sd(impute.data))
#    prod01.df$impute.sample.size[i]<-0
     break
  }
  if (impute.level=="household") {next}
  
  #target.crop<-prod01.df$crop[i]

#  match.index <- inputs.df[, impute.level] == inputs.df[i, impute.level] & 
#    inputs.df[, paste0(target.input, ".impute.level")] %in% "itself"
  
#  impute.sample.size <- sum(match.index)
  
  if (!all(is.na(impute.data)) && length(impute.data) >= 3) {
  # all(is.na(impute.data)) is just barely valid when impute.data is NULL since all() is there
#    inputs.df[i, target.input] <- median(impute.data )
#    inputs.df[i, paste0(target.input, ".impute.level")] <- impute.level
#    prod01.df$impute.sample.size[i] <- impute.sample.size
    imputed.data.ls[[i]] <- c(median( impute.data ), impute.level, length(impute.data), sd(impute.data))
    break
  }
  

  
}
 
 cat(target.input, i, impute.level, "\n")
  
}

temp.imputed.df <- data.frame(matrix(unlist(imputed.data.ls), ncol=4, byrow=TRUE), stringsAsFactors=FALSE)
temp.imputed.df[, 1] <- as.numeric(temp.imputed.df[, 1])
temp.imputed.df[, 3] <- as.numeric(temp.imputed.df[, 3])
temp.imputed.df[, 4] <- as.numeric(temp.imputed.df[, 4])

inputs.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), target.input] <- 
  temp.imputed.df[, 1]

inputs.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), 
  paste0(target.input, ".impute.level") ] <- temp.imputed.df[, 2]
  
inputs.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), 
  paste0(target.input, ".impute.n.obs") ] <- temp.imputed.df[, 3]
  
inputs.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), 
  paste0(target.input, ".impute.sd") ] <- temp.imputed.df[, 4]

}

inputs.df

}






system.time( inputs.df.imputed <- geog.impute.fn(inputs.df) )



for (k in input.price.columns ) {
  print( table(inputs.df.imputed[, paste0(k, ".impute.level")], useNA="always") )
}

for (k in input.price.columns ) {
  print( summary(inputs.df.imputed[, paste0(k, ".impute.n.obs")]) )
}

for (k in input.price.columns ) {
  print( summary(inputs.df.imputed[, paste0(k, ".impute.sd")]) )
}









#OLD IMPUTE FN:




geog.impute.fn <- function(inputs.df) {




impute.levels <- c("household", "segmento.full", "sector.full", "canton.full", "seccion.full", "provincia.full", "departamento", "nation")

input.price.columns <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", "x19.plagicidas.bs.kg")





geog.split.ls <- list()

for ( target.input in input.price.columns) {
  list.temp <- list()
  for (i in impute.levels) {
    inputs.df[, paste0(target.input, ".impute.level")] <- NA
    inputs.df[inputs.df[, target.input]!=0, paste0(target.input, ".impute.level")] <- "itself"
    
    if ( i=="household") { split.level <- "folio"} else { split.level <- i}
    if (i=="nation") {geog.split.ls[[i]] <- inputs.df; next}
    
    inputs.for.split <- inputs.df[ inputs.df[, paste0(target.input, ".impute.level")] %in% "itself", 
      c(split.level, target.input, "x19.codigo")]
      
    list.temp[[i]] <- split(x=inputs.for.split , f=inputs.for.split[, split.level])
  }
  geog.split.ls[[target.input]] <- list.temp
}




input.price.columns <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", "x19.plagicidas.bs.kg")

nation.input.averages <- apply(inputs.df[, input.price.columns], 2, FUN=function(x) median(x[x>0], na.rm=TRUE) )
nation.input.nobs <- apply(inputs.df[, input.price.columns], 2, FUN=function(x) { x <- x[!is.na(x)]; length(x[x>0])} )
nation.input.sd <- apply(inputs.df[, input.price.columns], 2, FUN=function(x) sd(x[x>0], na.rm=TRUE) )

for (target.input in input.price.columns ) {

  inputs.df[, paste0(target.input, ".impute.level")] <- NA
  
  inputs.df[inputs.df[, target.input]!=0, paste0(target.input, ".impute.level")] <- "itself"

impute.levels <- c("household", "segmento.full", "sector.full", "canton.full", "seccion.full", "provincia.full", "departamento", "nation")

# Rprof()

imputed.data.ls<- vector(mode = "list", length = nrow(inputs.df))

geog.split.ls.targ.input <- geog.split.ls[[target.input]]

for (i in 1:nrow(inputs.df)) {

if (inputs.df[i, paste0(target.input, ".impute.level")] %in% "itself") {next}

for (impute.level in impute.levels) {
  
  if (target.input=="x19.sem.comprada.bs.kg") {
    if(impute.level=="household") {
      seed.switcher <- geog.split.ls.targ.input[[ impute.level ]][[ inputs.df$folio[i] ]]$x19.codigo==inputs.df$x19.codigo[i]
    } else {
      seed.switcher <- geog.split.ls.targ.input[[ impute.level ]][[ inputs.df[i, impute.level] ]]$x19.codigo==inputs.df$x19.codigo[i]
    }
  } else {
    seed.switcher <- TRUE
  }

  if (impute.level=="nation") { 
    imputed.data.ls[[i]] <- c(unname(nation.input.averages[target.input]), 
      impute.level, unname(nation.input.nobs[target.input]), unname(nation.input.sd[target.input]))
    break
  }
  
  if(impute.level=="household") {
    impute.data <-   geog.split.ls.targ.input[[ impute.level ]][[ inputs.df$folio[i] ]][seed.switcher, target.input]
  } else {
    impute.data <- geog.split.ls.targ.input[[ impute.level ]][[ inputs.df[i, impute.level] ]][seed.switcher, target.input]
  }
  
  if (impute.level=="household" && !all(is.na(impute.data)) && length(impute.data)>0  ) {
    
#     inputs.df[i, target.input] <- median( impute.data )
#     inputs.df[i, c(target.input, paste0(target.input, ".impute.level") )] <- "household"
      imputed.data.ls[[i]] <- c(median( impute.data ), "household", length(impute.data), sd(impute.data))
#    prod01.df$impute.sample.size[i]<-0
     break
  }
  if (impute.level=="household") {next}
  
  #target.crop<-prod01.df$crop[i]

#  match.index <- inputs.df[, impute.level] == inputs.df[i, impute.level] & 
#    inputs.df[, paste0(target.input, ".impute.level")] %in% "itself"
  
#  impute.sample.size <- sum(match.index)
  
  if (!all(is.na(impute.data)) && length(impute.data) >= 3) {
  # all(is.na(impute.data)) is just barely valid when impute.data is NULL since all() is there
#    inputs.df[i, target.input] <- median(impute.data )
#    inputs.df[i, paste0(target.input, ".impute.level")] <- impute.level
#    prod01.df$impute.sample.size[i] <- impute.sample.size
    imputed.data.ls[[i]] <- c(median( impute.data ), impute.level, length(impute.data), sd(impute.data))
    break
  }
  

  
}
 
 cat(target.input, i, impute.level, "\n")
  
}

temp.imputed.df <- data.frame(matrix(unlist(imputed.data.ls), ncol=4, byrow=TRUE), stringsAsFactors=FALSE)
temp.imputed.df[, 1] <- as.numeric(temp.imputed.df[, 1])
temp.imputed.df[, 3] <- as.numeric(temp.imputed.df[, 3])
temp.imputed.df[, 4] <- as.numeric(temp.imputed.df[, 4])

inputs.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), target.input] <- 
  temp.imputed.df[, 1]

inputs.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), 
  paste0(target.input, ".impute.level") ] <- temp.imputed.df[, 2]
  
inputs.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), 
  paste0(target.input, ".impute.n.obs") ] <- temp.imputed.df[, 3]
  
inputs.df[sapply(imputed.data.ls, FUN= function(x) length(x)>0 ), 
  paste0(target.input, ".impute.sd") ] <- temp.imputed.df[, 4]

}

inputs.df

}

















load(file="/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF with soil and rain and advanced drive time fixed.Rdata")


#inputs.df[inputs.df$x19.fertilizante.cantidad.kg==0, "x19.fertilizante.bs.kg.impute.level"] <- "not.itself"
# This is to correct for some weirdness, i.e. all ofthe fert saying "itself". may be a problem with partially using
# The slower way to impute

input.price.columns <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg", "x19.plagicidas.bs.kg")

# THIS PART BELOW KILLS ANYTHING THAT HAD BEEN IMPUTED BEFORE
for (target.input in input.price.columns) {
  inputs.df[inputs.df[, paste0(target.input, ".impute.level")] != "itself", target.input] <- 0
}








inputs.reduced.df <- aggregate(x19.fertilizante.bs.kg ~ folio + canton.full + seccion.full + provincia.full + departamento ,  data=inputs.df, 
  FUN=function(x) {
    if (all(x==0) ) {
      return(0)
    } else {
      return(mean(x[x>0]) )
    }
})



summary(lm(log(x19.fertilizante.bs.kg) ~ canton.full, data=inputs.reduced.df , subset= x19.fertilizante.bs.kg>0 ))$adj.r.squared
summary(lm(log(x19.fertilizante.bs.kg) ~ seccion.full, data=inputs.reduced.df , subset= x19.fertilizante.bs.kg>0 ))$adj.r.squared
summary(lm(log(x19.fertilizante.bs.kg) ~ provincia.full, data=inputs.reduced.df , subset= x19.fertilizante.bs.kg>0 ))$adj.r.squared
summary(lm(log(x19.fertilizante.bs.kg) ~ departamento, data=inputs.reduced.df , subset= x19.fertilizante.bs.kg>0 ))$adj.r.squared


inputs.reduced.df <- merge(inputs.reduced.df, 
  unique(inputs.df[, c("canton.full", "cities.within.2.hrs", "cities.within.4.hrs", "cities.within.8.hrs",
     "cities.within.16.hrs", "cities.within.32.hrs", "dist.to.road", "drive.time.la.paz", "drive.time.amanzanada") ])
  )


summary(lm( log(x19.fertilizante.bs.kg ) ~ cities.within.2.hrs + I(cities.within.4.hrs-cities.within.2.hrs)  +
  I(cities.within.8.hrs-cities.within.4.hrs) + I(cities.within.16.hrs-cities.within.8.hrs) +
  I(cities.within.32.hrs-cities.within.16.hrs) +
  dist.to.road + drive.time.la.paz +
 # I(100*prop.canton.buying.fert) +
  drive.time.amanzanada,
  data=inputs.reduced.df, subset= x19.fertilizante.bs.kg>0 ))







colnames(inputs.df)!="x19.fertilizante.bs.kg"










