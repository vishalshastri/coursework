#saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF with soil and rain.Rdata"

#load(saved.workspace.path)

library("foreign")
work.dir <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/"
inputs.df<- read.spss(paste0(work.dir, "bd68/2-AGRICOLA/Cultivos (Preg-19)/2.-ENA08_BOLIVIA_CULTIVOS_PRODUCCION_INSUMOS(preg_19).sav"), to.data.frame=TRUE)


colnames(inputs.df) <- tolower( make.names(gsub("[()]|[.]", "", attr(inputs.df, "variable.labels")) ) )





crops.by.parcel.count <- sort(table(inputs.df$x19.codigo), decreasing=TRUE)
crops.by.parcel.count <- crops.by.parcel.count[crops.by.parcel.count>0]
rownames(crops.by.parcel.count) <- gsub("(^ +)|( +$)", "", rownames(crops.by.parcel.count))
crops.by.parcel.count <- as.data.frame(crops.by.parcel.count)
rownames(crops.by.parcel.count)
colnames(crops.by.parcel.count) <- "Parcel count"

inputs.df.no.crop.dups <- inputs.df[!duplicated(inputs.df[, c("folio", "x19.codigo")]), ]

crops.by.farm.count <- sort(table(inputs.df.no.crop.dups$x19.codigo), decreasing=TRUE)
crops.by.farm.count <- crops.by.farm.count[crops.by.farm.count>0]
rownames(crops.by.farm.count) <- gsub("(^ +)|( +$)", "", rownames(crops.by.farm.count))
crops.by.farm.count <- as.data.frame(crops.by.farm.count)
rownames(crops.by.farm.count)
colnames(crops.by.farm.count) <- "Farm count"

length(unique(factor(inputs.df$folio)))
nrow(inputs.df)


crops.by.weight <- aggregate(inputs.df$x19.produccion.obtenidad.quintal, by=list(factor(inputs.df$x19.codigo)), FUN=sum, na.rm=TRUE)
# NOTE: This is in quintals, so need to convert to KG
crops.by.weight[, 1] <- gsub("(^ +)|( +$)", "", crops.by.weight[, 1])
crops.by.weight <- crops.by.weight[order(crops.by.weight[, 2], decreasing=TRUE), ]
crops.by.weight[, 2] <- crops.by.weight[, 2] * 46 # to kg
crops.by.weight[, 2] <- crops.by.weight[, 2] / 1000 # to metric tons
crops.by.weight[, 2] <- round(crops.by.weight[, 2], digits=2)
names(crops.by.weight) <- c("Crop", "Aggregate quantity in metric tons")

write.csv(crops.by.parcel.count, 
  file="/Users/travismcarthur/Desktop/Proposal course/Materials for 4-1 meeting with Brad/crops-by-parcel-count.csv", 
  row.names=TRUE)
  
write.csv(crops.by.farm.count, 
  file="/Users/travismcarthur/Desktop/Proposal course/Materials for 4-1 meeting with Brad/crops-by-farm-count.csv", 
  row.names=TRUE)
  
write.csv(crops.by.weight, 
  file="/Users/travismcarthur/Desktop/Proposal course/Materials for 4-1 meeting with Brad/crops-by-weight.csv", 
  row.names=FALSE)
# Encoding is not so great



sort(table(inputs.df$x19.codigo))

code.check.mat <- as.matrix(table(inputs.df$x19.cultivo, inputs.df$x19.codigo))

image(code.check.mat>0)

colSums(code.check.mat>0)
rowSums(code.check.mat>0)

table(colSums(code.check.mat>0))
table(rowSums(code.check.mat>0))

"OTROS CULTIVOS DE FRUTAS" has grabbed 4 categories
"Plantaciones de palmitos" has grabbed 2 categories


code.check.mat[ ,"Plantaciones de palmitos         " ]
Here is what those are:
PLANTACIONES DE PALMITO      1
PLANTACIONES DE PALMITOS    73
So we are A-ok going with just "codigo"



output.df<- read.spss(paste0(work.dir, "bd68/2-AGRICOLA/Produccion (Preg-20)/3.-ENA08_BOLIVIA_DISTRIBUCION_PRODUCCION(preg_20).sav"), to.data.frame=TRUE)

# /Users/travismcarthur/Desktop/Metrics (637)/Final paper/bd68/7-INFRAESTRUCTURA Y MAQUINARIA/25.-ENA08_BOLIVIA_INFRAESTRUCTURA-MAQUINARIA(preg_103).sav

colnames(output.df) <- tolower( make.names(gsub("[()]|[.]", "", attr(output.df, "variable.labels")) ) )

colnames(output.df) <- make.names(colnames(output.df), unique = TRUE)
colnames(output.df)[colnames(output.df)=="x20.bs.unidad.quintal"] <- "x20.bs.unidad.quintal.mn"
# "mn" is mercado nacional
colnames(output.df)[colnames(output.df)=="x20.bs.unidad.quintal.1"] <- "x20.bs.unidad.quintal.fuera.pais"


sort(table(output.df$x20.producto))
sort(table(output.df$x20.codigo.de.producto))




# Oca is http://en.wikipedia.org/wiki/Oxalis_tuberosa
# Papaliza is http://en.wikipedia.org/wiki/Ullucus
# Camote (batata) is sweet potato
# Izaño (mashua)  is http://en.wikipedia.org/wiki/Tropaeolum_tuberosum



#### NOW START ANALYZING THE VARIETIES below


library("foreign")
work.dir <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/"
inputs.df<- read.spss(paste0(work.dir, "bd68/2-AGRICOLA/Cultivos (Preg-19)/2.-ENA08_BOLIVIA_CULTIVOS_PRODUCCION_INSUMOS(preg_19).sav"), to.data.frame=TRUE)


colnames(inputs.df) <- tolower( make.names(gsub("[()]|[.]", "", attr(inputs.df, "variable.labels")) ) )


inputs.df$x19.codigo <- gsub("(^ +)|( +$)", "", inputs.df$x19.codigo)


crop.categories.df <- read.csv("/Users/travismcarthur/Desktop/Proposal course/Materials for 4-1 meeting with Brad/Broad Bolivian crop categories.csv",
stringsAsFactors=FALSE, fileEncoding="UTF-8")
# NOTE: had to open the above Excel-saved file in textwrangler and re-save as UTF-8

crop.categories.df[1, 3] <- NA
# Removing papas as a cash crop for now

table(crop.categories.df$Crop %in% inputs.df$x19.codigo)
crop.categories.df$Crop[!crop.categories.df$Crop %in% inputs.df$x19.codigo]

crop.categories.df[is.na(crop.categories.df)] <- 0
crop.categories.df$Cash[crop.categories.df$Staple.grains==1] <- 0
# Ok, just make these two categories mutually exclusive

inputs.df <- merge(inputs.df, crop.categories.df[, -2], by.x="x19.codigo", by.y="Crop", all.x=TRUE)

for ( i in c("Cash", "Staple.grains", "Orchard.plantation")) {
  inputs.df[, paste0("fert.use.", i)] <- (inputs.df$x19.fertilizante.cantidad.quintal > 0) * inputs.df[, i]
}


crop.cat.count.agg <- aggregate(inputs.df[, c("Cash", "Staple.grains", "Orchard.plantation", 
  "fert.use.Cash", "fert.use.Staple.grains", "fert.use.Orchard.plantation")],
  by=list(folio=inputs.df$folio), 
  FUN=sum, na.rm=TRUE)

library("mfx")

# TODO: fix region names

crop.cat.count.agg <- merge(crop.cat.count.agg, unique(inputs.df[, c("folio", "zona.agroproductiva")]), all.x=TRUE)

probitmfx( I(fert.use.Staple.grains>0) ~ I(fert.use.Cash>0) + zona.agroproductiva,
  data=crop.cat.count.agg[(crop.cat.count.agg$Cash > 0) & (crop.cat.count.agg$Staple.grains > 0), ]
)

summary(glm( I(fert.use.Staple.grains>0) ~ I(fert.use.Cash>0) + zona.agroproductiva,
  family = binomial(link = "probit"),
  data=crop.cat.count.agg[(crop.cat.count.agg$Cash > 0) & (crop.cat.count.agg$Staple.grains > 0), ]
))

with(crop.cat.count.agg[(crop.cat.count.agg$Cash > 0) & (crop.cat.count.agg$Staple.grains > 0), ],
  cor.test(as.numeric(fert.use.Staple.grains>0), as.numeric(fert.use.Cash>0))
)

with(crop.cat.count.agg[(crop.cat.count.agg$Cash > 0) & (crop.cat.count.agg$Staple.grains > 0), ],
  table(as.numeric(fert.use.Staple.grains>0), as.numeric(fert.use.Cash>0))
)
 


for ( i in c("Cash", "Staple.grains", "Orchard.plantation")) {
  print(prop.table(table(inputs.df[inputs.df[, i] >0, paste0("fert.use.", i)])))
}

# TODO: OK, these are good crosstabs, but then have each crosstab counting only each farm once.
# And then crosstab the individual crops.


#for ( i in unique() {
#  print(prop.table(table(inputs.df[inputs.df[, i] >0, paste0("fert.use.", i)])))
#}


round(100*prop.table(table(inputs.df$x19.codigo, inputs.df$x19.fertilizante.cantidad.quintal > 0), margin=1), 1)

#<- round(100*prop.table(xtabs(~ as.factor(x19.codigo) + x19.prepara.el.suelo, data=inputs.df), margin=1), 1)

#                             + x19.siembra.planta + x19.labores.culturales + x19.cosecha

traction.table.ls <- list(table(N=inputs.df$x19.codigo))

for ( i in c("x19.prepara.el.suelo", "x19.siembra.planta", "x19.labores.culturales", "x19.cosecha")) {
  traction.table.ls[[i]] <- round(100*prop.table(table(inputs.df$x19.codigo,  inputs.df[, i], exclude=c("", "S/D")), margin=1), 1)
}

traction.table.f <- do.call(cbind, traction.table.ls)
colnames(traction.table.f)[1] <- "N"

colnames(traction.table.f)[colnames(traction.table.f)=="Maq. Agricola"] <- "Mach."
colnames(traction.table.f)[colnames(traction.table.f)=="Tracc. Animal"] <- "Animal"
colnames(traction.table.f)[colnames(traction.table.f)=="Fuerza humana"] <- "Human"


library("stargazer")


crops.sorted <- crop.categories.df$Crop

table.chunks <- list(1:17, 18:34)

traction.table.f.save <- traction.table.f
# target.chunk <- 1

for (target.chunk in 1:length(table.chunks)) { 


traction.table.f <- traction.table.f.save[crops.sorted[table.chunks[[target.chunk]]], ]

rownames(traction.table.f) <- substr(rownames(traction.table.f), 1, 20)

traction.stargazer <- stargazer(traction.table.f, float.env="sidewaystable", 
  font.size="footnotesize", digits=1, align=TRUE,
  title="Cultivation techniques by crop")


#library("xtable")
#traction.stargazer <- print(xtable(traction.table.f),
#  caption="",
#  caption.placement = "top")


traction.stargazer <- c(traction.stargazer[1:9], # 1:9
"    \\multicolumn{2}{c}{}  &
    \\multicolumn{3}{c}{Soil prep}  &
    \\multicolumn{3}{c}{Planting}  &
    \\multicolumn{3}{c}{Cultivation} & 
    \\multicolumn{3}{c}{Harvest} \\\\
    \\cmidrule(lr){3-5} \\cmidrule(lr){6-8} \\cmidrule(lr){9-11} \\cmidrule(lr){12-14} 
    \\\\",
  traction.stargazer[-(1:9)]
)

traction.stargazer <- gsub("Mach..[0-9]", "Mach.", traction.stargazer)
traction.stargazer <- gsub("Animal.[0-9]", "Animal", traction.stargazer)
traction.stargazer <- gsub("Human.[0-9]", "Human", traction.stargazer)
traction.stargazer <- gsub("^.multicolumn......", "\\\\multicolumn{1}{l}", traction.stargazer)



cat(traction.stargazer, file=paste0("/Users/travismcarthur/Desktop/Proposal course/Materials for 4-16 meeting with JP and Brad/tractiontable", target.chunk, ".tex"), sep="\n")

}






## BELOW IS THE elevation SPLIT


saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF with soil and rain.Rdata"
# with soil and rain and elevation

load(saved.workspace.path)
# OK, so this isn't 100% consistent with the above since the inputs.df from the workspace exclude zero-output observations.

inputs.df$x19.codigo <- gsub("(^ +)|( +$)", "", inputs.df$x19.codigo)

inputs.df$x19.codigo.low.high <- ifelse( inputs.df$elevation < median(inputs.df$elevation),  
  paste0("L: ", inputs.df$x19.codigo),
  paste0("H: ", inputs.df$x19.codigo))


traction.table.ls <- list(table(N=inputs.df$x19.codigo.low.high))

for ( i in c("x19.prepara.el.suelo", "x19.siembra.planta", "x19.labores.culturales", "x19.cosecha")) {
  traction.table.ls[[i]] <- round(100*prop.table(table(inputs.df$x19.codigo.low.high,  inputs.df[, i], exclude=c("", "S/D")), margin=1), 1)
}

traction.table.f <- do.call(cbind, traction.table.ls)
colnames(traction.table.f)[1] <- "N"

colnames(traction.table.f)[colnames(traction.table.f)=="Maq. Agricola"] <- "Mach."
colnames(traction.table.f)[colnames(traction.table.f)=="Tracc. Animal"] <- "Animal"
colnames(traction.table.f)[colnames(traction.table.f)=="Fuerza humana"] <- "Human"


#install.packages("gdata", repos="http://cran.us.r-project.org")
#library("gdata")


#rownames(traction.table.f)[ grepl("^H", rownames(traction.table.f))]

table.chunks <- list(1:8, 9:17, 18:26, 27:34)

traction.table.f.save <- traction.table.f
# target.chunk <- 1

for (target.chunk in 1:length(table.chunks)) {


crop.sort.H.L <- paste0(c("H: ", "L: "), rep(crops.sorted[table.chunks[[target.chunk]]], each=2))

traction.table.f <-  traction.table.f.save[
  match(crop.sort.H.L, rownames(traction.table.f.save)),]


# traction.table.f.save[substr(rownames(traction.table.f.save), 4, 1000) %in% crops.sorted[table.chunks[[target.chunk]]], ]
traction.table.f <- traction.table.f[!is.na(traction.table.f[, 1]),]

interleave.ls <- list()

for ( i in crops.sorted) {
  interleave.ls[[i]] <-  traction.table.f[substr(rownames(traction.table.f), 4, 1000)==i, , drop=FALSE]
}

traction.table.f <- do.call(rbind, interleave.ls)



library("stargazer")

#traction.table.f <- traction.table.f[1:10, ]

rownames(traction.table.f) <- substr(rownames(traction.table.f), 1, 20)

traction.stargazer <- stargazer(traction.table.f, float.env="sidewaystable", 
  font.size="footnotesize", digits=1, align=TRUE,
  title="Cultivation techniques by crop. H indicates plot is higher than overall median elevation; L is lower.")


#library("xtable")
#traction.stargazer <- print(xtable(traction.table.f),
#  caption="",
#  caption.placement = "top")


traction.stargazer <- c(traction.stargazer[1:9], # 1:9
"    \\multicolumn{2}{c}{}  &
    \\multicolumn{3}{c}{Soil prep}  &
    \\multicolumn{3}{c}{Planting}  &
    \\multicolumn{3}{c}{Cultivation} & 
    \\multicolumn{3}{c}{Harvest} \\\\
    \\cmidrule(lr){3-5} \\cmidrule(lr){6-8} \\cmidrule(lr){9-11} \\cmidrule(lr){12-14} 
    \\\\",
  traction.stargazer[-(1:9)]
)

traction.stargazer <- gsub("Mach..[0-9]", "Mach.", traction.stargazer)
traction.stargazer <- gsub("Animal.[0-9]", "Animal", traction.stargazer)
traction.stargazer <- gsub("Human.[0-9]", "Human", traction.stargazer)
traction.stargazer <- gsub("^.multicolumn......", "\\\\multicolumn{1}{l}", traction.stargazer)



cat(traction.stargazer, file=paste0("/Users/travismcarthur/Desktop/Proposal course/Materials for 4-16 meeting with JP and Brad/elevtractiontable", target.chunk, ".tex"), sep="\n")


}









## BELOW IS THE elevation SPLIT by own median


saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF with soil and rain.Rdata"
# with soil and rain and elevation

load(saved.workspace.path)
# OK, so this isn't 100% consistent with the above since the inputs.df from the workspace exclude zero-output observations.

inputs.df$x19.codigo <- gsub("(^ +)|( +$)", "", inputs.df$x19.codigo)


elev.own.median.agg <- aggregate(inputs.df$elevation, by=list(x19.codigo=inputs.df$x19.codigo), FUN=median)

colnames(elev.own.median.agg)[2] <- "elevation.own.median"

inputs.df <- merge(inputs.df, elev.own.median.agg, all.x=TRUE)


inputs.df$x19.codigo.low.high <- ifelse( inputs.df$elevation < inputs.df$elevation.own.median,  
  paste0("L: ", inputs.df$x19.codigo),
  paste0("H: ", inputs.df$x19.codigo))


traction.table.ls <- list(table(N=inputs.df$x19.codigo.low.high))

for ( i in c("x19.prepara.el.suelo", "x19.siembra.planta", "x19.labores.culturales", "x19.cosecha")) {
  traction.table.ls[[i]] <- round(100*prop.table(table(inputs.df$x19.codigo.low.high,  inputs.df[, i], exclude=c("", "S/D")), margin=1), 1)
}

traction.table.f <- do.call(cbind, traction.table.ls)
colnames(traction.table.f)[1] <- "N"

colnames(traction.table.f)[colnames(traction.table.f)=="Maq. Agricola"] <- "Mach."
colnames(traction.table.f)[colnames(traction.table.f)=="Tracc. Animal"] <- "Animal"
colnames(traction.table.f)[colnames(traction.table.f)=="Fuerza humana"] <- "Human"


#install.packages("gdata", repos="http://cran.us.r-project.org")
#library("gdata")


#rownames(traction.table.f)[ grepl("^H", rownames(traction.table.f))]

table.chunks <- list(1:8, 9:17, 18:26, 27:34)

traction.table.f.save <- traction.table.f
# target.chunk <- 1

for (target.chunk in 1:length(table.chunks)) {


crop.sort.H.L <- paste0(c("H: ", "L: "), rep(crops.sorted[table.chunks[[target.chunk]]], each=2))

traction.table.f <-  traction.table.f.save[
  match(crop.sort.H.L, rownames(traction.table.f.save)),]


# traction.table.f.save[substr(rownames(traction.table.f.save), 4, 1000) %in% crops.sorted[table.chunks[[target.chunk]]], ]
traction.table.f <- traction.table.f[!is.na(traction.table.f[, 1]),]

interleave.ls <- list()

for ( i in crops.sorted) {
  interleave.ls[[i]] <-  traction.table.f[substr(rownames(traction.table.f), 4, 1000)==i, , drop=FALSE]
}

traction.table.f <- do.call(rbind, interleave.ls)



library("stargazer")

#traction.table.f <- traction.table.f[1:10, ]

rownames(traction.table.f) <- substr(rownames(traction.table.f), 1, 20)

traction.stargazer <- stargazer(traction.table.f, float.env="sidewaystable", 
  font.size="footnotesize", digits=1, align=TRUE,
  title="Cultivation techniques by crop. H indicates plot is higher median elevation of the crop; L is lower.")


#library("xtable")
#traction.stargazer <- print(xtable(traction.table.f),
#  caption="",
#  caption.placement = "top")


traction.stargazer <- c(traction.stargazer[1:9], # 1:9
"    \\multicolumn{2}{c}{}  &
    \\multicolumn{3}{c}{Soil prep}  &
    \\multicolumn{3}{c}{Planting}  &
    \\multicolumn{3}{c}{Cultivation} & 
    \\multicolumn{3}{c}{Harvest} \\\\
    \\cmidrule(lr){3-5} \\cmidrule(lr){6-8} \\cmidrule(lr){9-11} \\cmidrule(lr){12-14} 
    \\\\",
  traction.stargazer[-(1:9)]
)

traction.stargazer <- gsub("Mach..[0-9]", "Mach.", traction.stargazer)
traction.stargazer <- gsub("Animal.[0-9]", "Animal", traction.stargazer)
traction.stargazer <- gsub("Human.[0-9]", "Human", traction.stargazer)
traction.stargazer <- gsub("^.multicolumn......", "\\\\multicolumn{1}{l}", traction.stargazer)



cat(traction.stargazer, file=paste0("/Users/travismcarthur/Desktop/Proposal course/Materials for 4-16 meeting with JP and Brad/ownelevtractiontable", target.chunk, ".tex"), sep="\n")


}







#### FERTILIZER USE CROSSTABS



saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF with soil and rain.Rdata"
# with soil and rain and elevation

load(saved.workspace.path)
# OK, so this isn't 100% consistent with the above since the inputs.df from the workspace exclude zero-output observations.

inputs.df$x19.codigo <- gsub("(^ +)|( +$)", "", inputs.df$x19.codigo)


fert.use.table.ls <- list(table(N=inputs.df$x19.codigo))

fert.use.table.ls[[2]] <- round(100*prop.table(table(inputs.df$x19.codigo,  inputs.df$x19.fertilizante.cantidad.kg>0, exclude=c("", "S/D")), margin=1), 1)

fert.use.table.ls[[2]] <- fert.use.table.ls[[2]][, 2, drop=FALSE]
colnames(fert.use.table.ls[[2]]) <- "Perc. that use fertilizer"

inputs.df$fert.intensity.kg.ha <- inputs.df$x19.fertilizante.cantidad.kg / inputs.df$x19.superficie.cultivada.hectareas

mean.fert.agg <- aggregate(inputs.df$fert.intensity.kg.ha, by=list(x19.codigo=inputs.df$x19.codigo), 
  FUN=function(x) ifelse( any(x>0), median(x[x>0]), 0 ) )

fert.use.table.ls[[3]] <- mean.fert.agg

fert.use.table.ls[[3]] <- fert.use.table.ls[[3]][, 2, drop=FALSE]
colnames(fert.use.table.ls[[3]]) <- "Median kg/ha, given positive amount"

fert.use.table.f <- do.call(cbind, fert.use.table.ls)
fert.use.table.f <- fert.use.table.f[, -1]
colnames(fert.use.table.f)[1] <- "N"


crops.sorted <- crop.categories.df$Crop

#table.chunks <- list(1:17, 18:34)
table.chunks <- list(1:34)

fert.use.table.f.save <- fert.use.table.f
# target.chunk <- 1

for (target.chunk in 1:length(table.chunks)) { 


fert.use.table.f <- fert.use.table.f.save[crops.sorted[table.chunks[[target.chunk]]], ]

#rownames(traction.table.f) <- substr(rownames(traction.table.f), 1, 20)

fert.use.stargazer <- stargazer(fert.use.table.f, # float.env="sidewaystable", 
  font.size="footnotesize", digits=1, align=TRUE,
  title="Inorganic fertilizer use by crop", summary=FALSE)


#library("xtable")
#traction.stargazer <- print(xtable(traction.table.f),
#  caption="",
#  caption.placement = "top")


#traction.stargazer <- c(traction.stargazer[1:9], # 1:9
#"    \\multicolumn{2}{c}{}  &
#    \\multicolumn{3}{c}{Soil prep}  &
#    \\multicolumn{3}{c}{Planting}  &
#    \\multicolumn{3}{c}{Cultivation} & 
#    \\multicolumn{3}{c}{Harvest} \\\\
#    \\cmidrule(lr){3-5} \\cmidrule(lr){6-8} \\cmidrule(lr){9-11} \\cmidrule(lr){12-14} 
#    \\\\",
#  traction.stargazer[-(1:9)]
#)

#traction.stargazer <- gsub("Mach..[0-9]", "Mach.", traction.stargazer)
#traction.stargazer <- gsub("Animal.[0-9]", "Animal", traction.stargazer)
#traction.stargazer <- gsub("Human.[0-9]", "Human", traction.stargazer)
fert.use.stargazer <- gsub("^.multicolumn......", "\\\\multicolumn{1}{l}", fert.use.stargazer)



cat(fert.use.stargazer, file=paste0("/Users/travismcarthur/Desktop/Proposal course/Materials for 4-16 meeting with JP and Brad/fertusetable", target.chunk, ".tex"), sep="\n")

}







######



#### NOW START ANALYZING THE VARIETIES below

for ( potato.cash in c(TRUE, FALSE) ) {
library("stargazer")

library("foreign")
work.dir <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/"
inputs.df<- read.spss(paste0(work.dir, "bd68/2-AGRICOLA/Cultivos (Preg-19)/2.-ENA08_BOLIVIA_CULTIVOS_PRODUCCION_INSUMOS(preg_19).sav"), to.data.frame=TRUE)



colnames(inputs.df) <- tolower( make.names(gsub("[()]|[.]", "", attr(inputs.df, "variable.labels")) ) )


inputs.df$x19.codigo <- gsub("(^ +)|( +$)", "", inputs.df$x19.codigo)


crop.categories.df <- read.csv("/Users/travismcarthur/Desktop/Proposal course/Materials for 4-1 meeting with Brad/Broad Bolivian crop categories.csv",
stringsAsFactors=FALSE, fileEncoding="UTF-8")
# NOTE: had to open the above Excel-saved file in textwrangler and re-save as UTF-8

if (!potato.cash) {
  crop.categories.df[1, 3] <- NA
  # Removing papas as a cash crop for now
}

crop.categories.df[is.na(crop.categories.df)] <- 0

inputs.df <- merge(inputs.df, crop.categories.df[, -2], by.x="x19.codigo", by.y="Crop", all.x=TRUE)



fert.use.table.ls <- list(data.frame(N=c(sum(inputs.df$Staple.grains, na.rm=TRUE), sum(inputs.df$Cash, na.rm=TRUE), sum(inputs.df$Orchard.plantation, na.rm=TRUE) )))

fert.use.table.ls[[2]] <- data.frame(`Perc. that use fertilizer`=c(
  round(100*prop.table(table(inputs.df$Staple.grains,  inputs.df$x19.fertilizante.cantidad.quintal>0), margin=1), 1)["1", "TRUE"],
  round(100*prop.table(table(inputs.df$Cash,  inputs.df$x19.fertilizante.cantidad.quintal>0), margin=1), 1)["1", 2],
  round(100*prop.table(table(inputs.df$Orchard.plantation,  inputs.df$x19.fertilizante.cantidad.quintal>0), margin=1), 1)["1", 2]
))

colnames(fert.use.table.ls[[2]]) <- "Perc. that use fertilizer"


inputs.df$fert.intensity.kg.ha <- (inputs.df$x19.fertilizante.cantidad.quintal * 46) / inputs.df$x19.superficie.cultivada.hectareas

inputs.df$fert.intensity.kg.ha[is.na(inputs.df$fert.intensity.kg.ha)] <- 0

# Below is a total hack

median.vec<- c()

for ( i in c("Staple.grains", "Cash", "Orchard.plantation")) {

  median.temp <- aggregate(inputs.df$fert.intensity.kg.ha, by=list(inputs.df[, i]), 
    FUN=function(x) ifelse( any(x>0), median(x[x>0]), 0 ) )
  
  median.vec[i] <-  median.temp[median.temp[,1]==1, 2]
}

fert.use.table.ls[[3]] <- as.data.frame(median.vec)

colnames(fert.use.table.ls[[3]]) <- "Median kg/ha, given positive amount"

fert.use.table.f <- do.call(cbind, fert.use.table.ls)
#fert.use.table.f <- fert.use.table.f[, -1]
colnames(fert.use.table.f)[1] <- "N"


intended.title <- ifelse(potato.cash, "Inorganic fertilizer use by crop category, potatoes as cash and staple",
  "Inorganic fertilizer use by crop category, potatoes as staple only")


fert.use.stargazer <- stargazer(fert.use.table.f, # float.env="sidewaystable", 
  font.size="footnotesize", digits=1, align=TRUE,
  title=intended.title, summary=FALSE)

fert.use.stargazer <- gsub("^.multicolumn......", "\\\\multicolumn{1}{l}", fert.use.stargazer)



cat(fert.use.stargazer, file=paste0("/Users/travismcarthur/Desktop/Proposal course/Materials for 4-16 meeting with JP and Brad/fertusetable_cat_potatoes_as_cash_", potato.cash, ".tex"), sep="\n")

}










saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF with soil and rain.Rdata"
# with soil and rain and elevation

load(saved.workspace.path)
# OK, so this isn't 100% consistent with the above since the inputs.df from the workspace exclude zero-output observations.



crop.categories.df <- read.csv("/Users/travismcarthur/Desktop/Proposal course/Materials for 4-1 meeting with Brad/Broad Bolivian crop categories.csv",
stringsAsFactors=FALSE, fileEncoding="UTF-8")
# NOTE: had to open the above Excel-saved file in textwrangler and re-save as UTF-8

crop.categories.df[1, 3] <- NA
# Removing papas as a cash crop for now

table(crop.categories.df$Crop %in% inputs.df$x19.codigo)
crop.categories.df$Crop[!crop.categories.df$Crop %in% inputs.df$x19.codigo]

crop.categories.df[is.na(crop.categories.df)] <- 0
crop.categories.df$Cash[crop.categories.df$Staple.grains==1] <- 0
# Ok, just make these two categories mutually exclusive

inputs.df$x19.codigo <- gsub("(^ +)|( +$)", "", inputs.df$x19.codigo)

inputs.df <- merge(inputs.df, crop.categories.df[, -2], by.x="x19.codigo", by.y="Crop", all.x=TRUE)

for ( i in c("Cash", "Staple.grains", "Orchard.plantation")) {
  inputs.df[, paste0("fert.use.", i)] <- (inputs.df$x19.fertilizante.cantidad.quintal > 0) * inputs.df[, i]
}



crop.cat.count.agg <- aggregate(inputs.df[, c("Cash", "Staple.grains", "Orchard.plantation", 
  "fert.use.Cash", "fert.use.Staple.grains", "fert.use.Orchard.plantation")],
  by=list(folio=inputs.df$folio), 
  FUN=sum, na.rm=TRUE)

library("mfx")

# TODO: fix region names

region <- toupper(inputs.df$zona.agroproductiva)
region[region=="VALLES CERRADAS     "] <- "VALLES CERRADOS     "

region <- gsub("Ú", "U", region)
region <- gsub(" *$", "", region)
region <- gsub(" ", ".", region)
region <- make.names(region)

region[region %in% c("CHACO.HUMEDO", "CHACO.SECO", "LLANOS.DE.SANTA.CRUZ", "PAMPAS.DE.MOXOS")] <- "CHACO"
region[region %in% c("YUNGAS.DEL.NORTE", "YUNGAS.DEL.SUR")] <- "YUNGAS"


inputs.df$zona.agroproductiva.r <- region

crop.cat.count.agg <- merge(crop.cat.count.agg, inputs.df[!duplicated(inputs.df$folio), c("folio", "zona.agroproductiva", 
  "zona.agroproductiva.r", "elevation")], all.x=TRUE)







probitmfx( I(fert.use.Staple.grains>0) ~ I(fert.use.Cash>0) + zona.agroproductiva.r,
  data=crop.cat.count.agg[(crop.cat.count.agg$Cash > 0) & (crop.cat.count.agg$Staple.grains > 0), ]
)


probitmfx( I(fert.use.Staple.grains>0) ~ I(fert.use.Cash>0) + elevation ,
  data=crop.cat.count.agg[(crop.cat.count.agg$Cash > 0) & (crop.cat.count.agg$Staple.grains > 0), ]
)



























    \\multicolumn{2}{c}{Developing countries} &
    \\multicolumn{2}{c}{World} \\\\
    \\\\
\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}




fit.SUR.texreg <- c(fit.SUR.texreg[1:6], "\\toprule
     &
    \\multicolumn{4}{c}{Sample} \\\\
    \\cmidrule(lr){2-5} 
    &
    \\multicolumn{2}{c}{Developing countries} &
    \\multicolumn{2}{c}{World} \\\\
    \\\\
\\cmidrule(lr){2-3} \\cmidrule(lr){4-5}
     &
    \\multicolumn{4}{c}{Dependent variable} \\\\
    \\cmidrule(lr){2-5} \\\\
  & \\multicolumn{1}{c}{1st eqn} & \\multicolumn{1}{c}{2nd eqn} & \\multicolumn{1}{c}{1st eqn} & \\multicolumn{1}{c}{2nd eqn} \\\\
  " ,
  fit.SUR.texreg[-(1:6)])
# Thanks to http://tex.stackexchange.com/questions/59478/multi-column-problem







"Maq. Agricola" "Tracc. Animal" "Fuerza humana"




cbind( round(100*prop.table(table(inputs.df$x19.codigo,  inputs.df$x19.prepara.el.suelo, exclude=c("", "S/D"), margin=1), 1),
round(100*prop.table(table(inputs.df$x19.codigo,  inputs.df$x19.prepara.el.suelo), margin=1), 1)
)






