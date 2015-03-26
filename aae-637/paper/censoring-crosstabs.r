

 synthetic.data <-FALSE
if (!exists("global.max.seed")) { global.max.seed <- 0}
do.SUR <- FALSE
include.cost.fn <- TRUE
only.cost.fn <- TRUE
generate.synth.data.from.cost.fn <- TRUE
start.at.true.xi <- FALSE
start.nonlin.from.ignorance <- TRUE
convex.in.f.inputs <- FALSE
concave.in.prices <- TRUE

M <- 1
N <- 6
J <- 6


#saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF with soil.Rdata"
saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF with soil and rain.Rdata"
# with soil and rain

library("foreign")
work.dir <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/"
geog.df<- read.spss(paste0(work.dir, "bd68/1-UBIGEO PRODUCTOR/1.-ENA08_BOLIVIA_UBIGEO_CONDICION_JURIDICA_SUPERFICIE_UPA(preg_1-17).sav"), to.data.frame=TRUE)
colnames(geog.df) <- tolower( make.names(gsub("[()]|[.]", "", attr(geog.df, "variable.labels")) ) )


code.dir <- "/Users/travismcarthur/git/coursework/aae-637/paper/"

load(saved.workspace.path)
functional.form <- "SGM"

if (functional.form =="SGM") {
  include.censored.cost <- TRUE
}

price.trim.quantile <- 0.99
demand.var.trim.quantile <- 0.95
#demand.var.trim.quantile <- 1


local.source.evaluation <- FALSE
dropped.cost.share.eq <- 10
# anything >6 means that no equation gets dropped


target.top.crop.number <- 1
# inputs.df

log.plus.one.cost <- FALSE

bootstrap.iter <- 1
# NOTE: Bootstrap iter = 0 means actual estimate
bootstrap.selection.v <- TRUE
source(paste0(code.dir, "build-model-extract-parcels.r"))

# Maybe just get each crop in a loop and then restack in a DF


#plot(cars, main = "lowess(cars)")
#lines(lowess(cars), col = 2)
#lines(lowess(cars, f = .2), col = 3)
#legend(5, 120, c(paste("f = ", c("2/3", ".2"))), lty = 1, col = 2:3)

#with(cars, scatter.smooth(speed, dist))

# NOTE: scatter.smooth() below can be useful
#scatter.smooth(firm.df$elevation, firm.df$x19.fertilizante.cantidad > 0)
#scatter.smooth(firm.df$soil.quality, firm.df$x19.fertilizante.cantidad > 0)


#firm.level.area



livestock.files <- list.files(paste0(work.dir, "bd68/3-GANADERIA/"), recursive=TRUE)
livestock.files <- paste0("bd68/3-GANADERIA/", livestock.files)

livestock.files <- c(livestock.files, "bd68/4-OTRAS ESPECIES ANIMALES/21.-ENA08_BOLIVIA_OTRAS_ESPECIES_ANIMALES(preg_93).sav",
  "bd68/5-AVICULTURA/22.-ENA08_BOLIVIA_AVICULTURA_CORRAL(preg_94).sav")
  
livestock.files <- livestock.files[!grepl("CARACTERISTICAS", livestock.files)]
# Above removes the characterics files since those have all the folios listed
livestock.files <- livestock.files[!grepl("BOLIVIA_AVICULTURA_CORRAL", livestock.files)]
# Taking out the birds since everyone and their mother is growing them


livestock.folio <- c()

for ( i in livestock.files) {
  livestock.df<- read.spss(paste0(work.dir, i), to.data.frame=TRUE)
  colnames(livestock.df) <- tolower( make.names(gsub("[()]|[.]", "", attr(livestock.df, "variable.labels")) ) )
  cat(i, "\n")
  cat(length(unique(livestock.df$folio)), "\n")
#  stopifnot( length(unique(livestock.df$folio))!=8022) 
  livestock.folio <- union(livestock.folio, livestock.df$folio)
}




additional.chars.df <-geog.df[, c("folio", "x10.cual.su.condicion.juridica", 
  "x11.el.productor.es", "x12.su.ocupacion.principal.es.agropecuaria")]


target.top.crop.number <- 1
source(paste0(code.dir, "build-model-extract-parcels.r"))
firm.df$region <- region
firm.df$which.crop <- firm.df$x19.codigo
firm.df <- merge(firm.df, additional.chars.df, all.x=TRUE)
stacked.firm.df <- firm.df

for ( target.top.crop.number in 2:5 ) {
  source(paste0(code.dir, "build-model-extract-parcels.r"))
  firm.df$which.crop <- firm.df$x19.codigo
  firm.df$region <- region
  firm.df <- merge(firm.df, additional.chars.df, all.x=TRUE)
  stacked.firm.df <- rbind(stacked.firm.df, firm.df)
}

stacked.firm.df$which.crop <- factor(stacked.firm.df$which.crop)
stacked.firm.df$region <- factor(stacked.firm.df$region)
# Get rid of factor levels that don't exist
stacked.firm.df$has.livestock <- ifelse( stacked.firm.df$folio %in% livestock.folio, TRUE, FALSE)



stacked.firm.df$which.crop <- as.character(stacked.firm.df$which.crop)
stacked.firm.df$which.crop[stacked.firm.df$which.crop=="Papa (patatas) "] <- "Potatoes"
stacked.firm.df$which.crop[stacked.firm.df$which.crop=="Maiz combined"] <- "Maize"
stacked.firm.df$which.crop[stacked.firm.df$which.crop=="Cebada combined"] <- "Barley"
stacked.firm.df$which.crop[stacked.firm.df$which.crop=="Trigo          "] <- "Wheat"
stacked.firm.df$which.crop[stacked.firm.df$which.crop=="Haba (verde)    "] <- "Fava Beans"


top.crops.for.plotting <- c("Potatoes", "Maize", "Barley", "Wheat", "Fava Beans")


library(ggplot2)
# http://docs.ggplot2.org/0.9.3.1/stat_smooth.html
# Default is 95% C.I.

output.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Descriptive plots/"


temp.plot <- ggplot(stacked.firm.df, aes(mean.ann.rain.5yr, as.numeric(x19.fertilizante.cantidad > 0))) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by mean growing season rainfall, 5 crops aggregated") +
  theme( axis.title.y = element_blank())
  
ggsave(filename = paste0(output.path, "Fert by rainfall aggregated.pdf"), plot = temp.plot)

temp.plot <- ggplot(stacked.firm.df, aes(mean.ann.rain.5yr, as.numeric(x19.fertilizante.cantidad > 0), colour=which.crop)) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by mean growing season rainfall") +
  theme( axis.title.y = element_blank())

#method="loess"
# I guess I should just keep the default

ggsave(filename = paste0(output.path, "Fert by rainfall disaggregated.pdf"), plot = temp.plot)






temp.plot <- ggplot(stacked.firm.df, aes(elevation, as.numeric(x19.fertilizante.cantidad > 0))) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by elevation (km), 5 crops aggregated") +
  theme( axis.title.y = element_blank())
  
ggsave(filename = paste0(output.path, "Fert by elevation aggregated.pdf"), plot = temp.plot)

temp.plot <- ggplot(stacked.firm.df, aes(elevation, as.numeric(x19.fertilizante.cantidad > 0), colour=which.crop)) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by elevation (km)") +
  theme( axis.title.y = element_blank())

#method="loess"
# I guess I should just keep the default

ggsave(filename = paste0(output.path, "Fert by elevation disaggregated.pdf"), plot = temp.plot)





temp.plot <- ggplot(stacked.firm.df, aes(log(firm.level.area), as.numeric(x19.fertilizante.cantidad > 0))) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by total cultivated area of firm, 5 crops aggregated") +
  theme( axis.title.y = element_blank())
  
ggsave(filename = paste0(output.path, "Fert by firm size aggregated.pdf"), plot = temp.plot)

temp.plot <- ggplot(stacked.firm.df, aes(log(firm.level.area), as.numeric(x19.fertilizante.cantidad > 0), colour=which.crop)) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by total cultivated area of firm") +
  theme( axis.title.y = element_blank())

ggsave(filename = paste0(output.path, "Fert by firm size disaggregated.pdf"), plot = temp.plot)




temp.plot <- ggplot(stacked.firm.df, aes(log(x19.superficie.cultivada.hectareas), as.numeric(x19.fertilizante.cantidad > 0))) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by plot area, 5 crops aggregated") +
  theme( axis.title.y = element_blank())
  
ggsave(filename = paste0(output.path, "Fert by plot area aggregated.pdf"), plot = temp.plot)

temp.plot <- ggplot(stacked.firm.df, aes(log(x19.superficie.cultivada.hectareas), as.numeric(x19.fertilizante.cantidad > 0), colour=which.crop)) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by plot area") +
  theme( axis.title.y = element_blank())

ggsave(filename = paste0(output.path, "Fert by plot area disaggregated.pdf"), plot = temp.plot)





temp.plot <- ggplot(stacked.firm.df[stacked.firm.df$soil.quality != min(stacked.firm.df$soil.quality), ], 
  aes(soil.quality, as.numeric(x19.fertilizante.cantidad > 0))) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by soil quality index, 5 crops aggregated") +
  theme( axis.title.y = element_blank())
  
ggsave(filename = paste0(output.path, "Fert by soil quality aggregated.pdf"), plot = temp.plot)

temp.plot <- ggplot(stacked.firm.df[stacked.firm.df$soil.quality != min(stacked.firm.df$soil.quality), ], 
  aes(soil.quality, as.numeric(x19.fertilizante.cantidad > 0), colour=which.crop)) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by soil quality index") +
  theme( axis.title.y = element_blank())

ggsave(filename = paste0(output.path, "Fert by soil quality disaggregated.pdf"), plot = temp.plot)




temp.plot <- ggplot(stacked.firm.df, aes(x19.fertilizante.bs.kg, as.numeric(x19.fertilizante.cantidad > 0))) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by fert price (Bolivianos/kg), 5 crops aggregated") +
  theme( axis.title.y = element_blank())

ggsave(filename = paste0(output.path, "Fert by fert price aggregated.pdf"), plot = temp.plot)

temp.plot <- ggplot(stacked.firm.df, aes(x19.fertilizante.bs.kg, 
  as.numeric(x19.fertilizante.cantidad > 0), colour=which.crop)) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by fert price (Bolivianos/kg)") +
  theme( axis.title.y = element_blank())

ggsave(filename = paste0(output.path, "Fert by fert price disaggregated.pdf"), plot = temp.plot)


temp.plot <- ggplot(stacked.firm.df, aes(x19.sem.comprada.bs.kg, as.numeric(x19.fertilizante.cantidad > 0))) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by seed price (Bolivianos/kg), 5 crops aggregated") +
  theme( axis.title.y = element_blank())

ggsave(filename = paste0(output.path, "Fert by seed price aggregated.pdf"), plot = temp.plot)

temp.plot <- ggplot(stacked.firm.df, aes(x19.sem.comprada.bs.kg, 
  as.numeric(x19.fertilizante.cantidad > 0), colour=which.crop)) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by seed price (Bolivianos/kg)") +
  theme( axis.title.y = element_blank())

ggsave(filename = paste0(output.path, "Fert by seed price disaggregated.pdf"), plot = temp.plot)


temp.plot <- ggplot(stacked.firm.df, aes(x19.abono.bs.kg, as.numeric(x19.fertilizante.cantidad > 0))) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by organic fert price (Bolivianos/kg), 5 crops aggregated") +
  theme( axis.title.y = element_blank())

ggsave(filename = paste0(output.path, "Fert by organic fert price aggregated.pdf"), plot = temp.plot)

temp.plot <- ggplot(stacked.firm.df, aes(x19.abono.bs.kg, 
  as.numeric(x19.fertilizante.cantidad > 0), colour=which.crop)) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by organic fert price (Bolivianos/kg)") +
  theme( axis.title.y = element_blank())

ggsave(filename = paste0(output.path, "Fert by organic fert price disaggregated.pdf"), plot = temp.plot)


temp.plot <- ggplot(stacked.firm.df, aes(x19.plagicidas.bs.kg, as.numeric(x19.fertilizante.cantidad > 0))) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by plagicida price (Bolivianos/kg), 5 crops aggregated") +
  theme( axis.title.y = element_blank())

ggsave(filename = paste0(output.path, "Fert by plagicida price aggregated.pdf"), plot = temp.plot)


temp.plot <- ggplot(stacked.firm.df, aes(x19.plagicidas.bs.kg, 
  as.numeric(x19.fertilizante.cantidad > 0), colour=which.crop)) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by plagicida price (Bolivianos/kg)") +
  theme( axis.title.y = element_blank())

ggsave(filename = paste0(output.path, "Fert by plagicida price disaggregated.pdf"), plot = temp.plot)


temp.plot <- ggplot(stacked.firm.df, aes(hourly.wage, as.numeric(x19.fertilizante.cantidad > 0))) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by hourly wage (Bolivianos), 5 crops aggregated") +
  theme( axis.title.y = element_blank())

ggsave(filename = paste0(output.path, "Fert by wage aggregated.pdf"), plot = temp.plot)

temp.plot <- ggplot(stacked.firm.df, aes(hourly.wage, 
  as.numeric(x19.fertilizante.cantidad > 0), colour=which.crop)) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by hourly wage (Bolivianos)") +
  theme( axis.title.y = element_blank())

ggsave(filename = paste0(output.path, "Fert by wage disaggregated.pdf"), plot = temp.plot)



temp.plot <- ggplot(stacked.firm.df, aes(hourly.tractor.rental, as.numeric(x19.fertilizante.cantidad > 0))) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by hourly tractor rental rate (Bolivianos), 5 crops aggregated") +
  theme( axis.title.y = element_blank())

ggsave(filename = paste0(output.path, "Fert by tractor rate aggregated.pdf"), plot = temp.plot)


temp.plot <- ggplot(stacked.firm.df, aes(hourly.tractor.rental, 
  as.numeric(x19.fertilizante.cantidad > 0), colour=which.crop)) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by hourly tractor rental rate (Bolivianos)") +
  theme( axis.title.y = element_blank())

ggsave(filename = paste0(output.path, "Fert by tractor rate disaggregated.pdf"), plot = temp.plot)




temp.plot <- ggplot(stacked.firm.df, aes(ag.fam.labor.equiv, as.numeric(x19.fertilizante.cantidad > 0))) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by num of HH members working on farm, 5 crops aggregated") +
  theme( axis.title.y = element_blank())

ggsave(filename = paste0(output.path, "Fert by HH size aggregated.pdf"), plot = temp.plot)


temp.plot <- ggplot(stacked.firm.df, aes(ag.fam.labor.equiv, 
  as.numeric(x19.fertilizante.cantidad > 0), colour=which.crop)) +
  stat_smooth() + geom_point() +
  ggtitle("Proportion of fert use by num of HH members working on farm") +
  theme( axis.title.y = element_blank())


ggsave(filename = paste0(output.path, "Fert by HH size disaggregated.pdf"), plot = temp.plot)












stacked.firm.df$x19.fertilizante.cantidad.kg.posi <- stacked.firm.df$x19.fertilizante.cantidad.kg > 0 
stacked.firm.df$x19.sem.comprada.cantidad.kg.posi <- stacked.firm.df$x19.sem.comprada.cantidad.kg > 0
stacked.firm.df$tractor.hrs.final.posi <- stacked.firm.df$tractor.hrs.final > 0
stacked.firm.df$x19.plagicidas.cantidad.kg.posi <- stacked.firm.df$x19.plagicidas.cantidad.kg > 0
stacked.firm.df$paid.hours.spread.posi <- stacked.firm.df$paid.hours.spread > 0
stacked.firm.df$x19.abono.cantidad.kg.posi <- stacked.firm.df$x19.abono.cantidad.kg > 0



possible.crosstabs <- c( "region",
"x19.uso.riego",
"x10.cual.su.condicion.juridica",
"x11.el.productor.es",
"x12.su.ocupacion.principal.es.agropecuaria",
"has.livestock",
"x19.mes.siembra",
"x19.sem.comprada.cantidad.kg.posi",
"tractor.hrs.final.posi",
"x19.plagicidas.cantidad.kg.posi",
"paid.hours.spread.posi",
"x19.abono.cantidad.kg.posi"
)

crosstab.titles <-
c( "agroproductive zone",
"has irrigation",
"legal status of firm",
"gender of firm owner",
"whether farming is the owner's principal occupation",
"has livestock",
"planting month",
"positive amount of purchased seed",
"used tractor",
"positive amount of plagicidas",
"positive amount of hired labor",
"positive amount of organic fertilizer"
)



for ( target.crosstab in 1:length(possible.crosstabs)) {



target.cross.tab <- possible.crosstabs[target.crosstab]
# "region"
# "x19.uso.riego"
# "x10.cual.su.condicion.juridica"
# "x11.el.productor.es"
# "x12.su.ocupacion.principal.es.agropecuaria"
# "has.livestock"
# "x19.mes.siembra"

# "x19.sem.comprada.cantidad.kg.posi"
# "tractor.hrs.final.posi"
# "x19.plagicidas.cantidad.kg.posi"
# "paid.hours.spread.posi"
# "x19.abono.cantidad.kg.posi"


fert.prop.table.ls <- list()

for ( i in 1:5 ) {
  current.crop <- top.crops.for.plotting[i]
  fert.prop.table<- table(factor(stacked.firm.df[, target.cross.tab]),  
    stacked.firm.df$x19.fertilizante.cantidad > 0,
    stacked.firm.df$which.crop)[ , , current.crop]
    
  
  
  fert.prop.table <- prop.table(fert.prop.table, margin=1)
  fert.prop.table <- as.data.frame(fert.prop.table[, "TRUE", drop=FALSE])
  names(fert.prop.table) <- "proportion"
  fert.prop.table$N.obs <- table(factor(stacked.firm.df[, target.cross.tab]),  
       stacked.firm.df$which.crop)[ , i]
       
  fert.prop.table$SE <- with(fert.prop.table, sqrt(proportion * (1-proportion)/ N.obs))
  fert.prop.table$lower <- with(fert.prop.table, proportion - 1.96 * SE - 0.5/N.obs)
  fert.prop.table$upper <- with(fert.prop.table, proportion + 1.96 * SE + 0.5/N.obs)
  fert.prop.table$lower <- ifelse(fert.prop.table$lower < 0 , 0, fert.prop.table$lower)
  fert.prop.table$upper <- ifelse(fert.prop.table$upper > 1 , 1, fert.prop.table$upper)
  # For CI calculation for proportion:
  # http://onlinestatbook.com/2/estimation/proportion_ci.html
  fert.prop.table$ID <- rownames(fert.prop.table)
  fert.prop.table$crop <- current.crop
  fert.prop.table.ls[[i]] <- fert.prop.table
  rm(fert.prop.table)
}

fert.prop.table <- do.call(rbind, fert.prop.table.ls)


crosstab.plot <- ggplot(data = fert.prop.table, 
    aes(x = ID, y = proportion, ymin = lower, ymax = upper, colour = crop)) +
  geom_point(position = position_dodge(width = 0.7)) +
  geom_errorbar(position = position_dodge(width = 0.7), width = 0.1) +
  coord_flip() +
  guides(colour = guide_legend(reverse=TRUE)) +
  ggtitle(paste0("Proportion of fert use by ", crosstab.titles[target.crosstab]) )
  
  
  # , order = -as.numeric(crop))
  
ggsave(filename = paste0(output.path, "Fert by ",
  crosstab.titles[target.crosstab], ".pdf"), plot = crosstab.plot)


}






# Thanks to https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles
# setwd("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/1155/")
#nation.shp = readOGR(dsn=".", layer="bolivia")
#nation.shp@data$id = rownames(nation.shp@data)
#nation.points = fortify(nation.shp, region="id")
#fert.price.agg <- aggregate( x19.fertilizante.bs.kg ~ provincia.full, data=inputs.df, FUN=median)
#wage.agg <- aggregate( hourly.wage ~ provincia.full, data=inputs.df, FUN=median)
#tractor.price.agg <- aggregate( hourly.tractor.rental ~ provincia.full, data=inputs.df, FUN=median)
# TODO: may want to restrict this to "itself" imputation levels


require("rgdal") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("gridExtra")
require("plyr")
  

library("foreign")
library("PBSmapping")
library("rgdal")

# install.packages("splancs", repos="http://cran.us.r-project.org")
library("splancs")

work.dir <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/"

#agro.zone.shp<-importShapefile(paste0(work.dir, "Zonas_agroproductivas19/Zonas_agroproductivas19.shp"))


#table(gsub("[.]", " ", stacked.firm.df$region, ) %in% attr(agro.zone.shp, "PolyData")$ZONA_AGROP)
#table(stacked.firm.df$region[!gsub("[.]", " ", stacked.firm.df$region, ) %in% attr(agro.zone.shp, "PolyData")$ZONA_AGROP])

#agro.zone.df <- agro.zone.shp
#agro.zone.df <- merge(agro.zone.df, attr(agro.zone.shp, "PolyData"))
#names(agro.zone.df)[names(agro.zone.df)=="X"] <- "long"
#names(agro.zone.df)[names(agro.zone.df)=="Y"] <- "lat"


stacked.firm.df$region.map <- as.character(stacked.firm.df$region)
stacked.firm.df$region.map[stacked.firm.df$region.map=="AMAZÓNICA"] <- "AMAZONIA"


stacked.firm.df$region.map[stacked.firm.df$region.map=="GUARAYO...CHIQUITANA"] <- "GUARAYO.CHIQUITANO" # A vs O ending

stacked.firm.df$region.map[stacked.firm.df$region.map=="YUNGAS"] <- "YUNGAS DEL NORTE"
stacked.firm.df$region.map <- gsub("[.]", " ", stacked.firm.df$region.map)

setwd("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Zonas_agroproductivas19/")
agro.zone.shp = readOGR(dsn=".", layer="Zonas_agroproductivas19")
agro.zone.shp@data$id = rownames(agro.zone.shp@data)
agro.zone.points = fortify(agro.zone.shp, region="id")




agro.zone.shp@data$ZONA_AGROP <- as.character(agro.zone.shp@data$ZONA_AGROP)
agro.zone.shp@data$ZONA_AGROP[agro.zone.shp@data$REGION=="GRAN CHACO"] <- "CHACO"
# Need to aggregate a bit

fert.posi.agg.max <- max(aggregate( x19.fertilizante.cantidad.kg.posi ~ region.map + which.crop, 
  data=stacked.firm.df, FUN=mean)[, 3] )


fert.ggplots.ls <- list()
fert.ggplots.own.scale.ls <- list()

for ( i in 1:5 ) {

current.crop <- top.crops.for.plotting[i]

fert.posi.agg <- aggregate( x19.fertilizante.cantidad.kg.posi ~ region.map, 
  data=stacked.firm.df[stacked.firm.df$which.crop==current.crop, ], FUN=mean)

stopifnot(all(fert.posi.agg$region.map %in% agro.zone.shp@data$ZONA_AGROP))

agro.zone.shp.for.agg <- agro.zone.shp

agro.zone.shp.for.agg@data <- merge(agro.zone.shp.for.agg@data, fert.posi.agg, by.x="ZONA_AGROP", by.y="region.map", all.x=TRUE)

agro.zone.df = join(agro.zone.points, agro.zone.shp.for.agg@data, by="id")

fert.ggplots.ls[[i]] <- ggplot(agro.zone.df) + 
  aes(long,lat,group=group,fill=x19.fertilizante.cantidad.kg.posi) + 
  geom_polygon() +
#  geom_path(color="white") +
  coord_equal() +
  ggtitle(current.crop) +
  theme(axis.ticks = element_blank(), legend.position="none",
    axis.text = element_blank(), axis.title=element_blank(),
    plot.margin= unit(rep(0, 4), "lines"),
    legend.title=element_blank()) +
  scale_fill_gradient(limits=c(0,fert.posi.agg.max))
  # from http://stackoverflow.com/questions/24265652/label-minimum-and-maximum-of-scale-fill-gradient-legend-with-text-ggplot2
  
fert.ggplots.own.scale.ls[[i]] <- ggplot(agro.zone.df) + 
  aes(long,lat,group=group,fill=x19.fertilizante.cantidad.kg.posi) + 
  geom_polygon() +
#  geom_path(color="white") +
  coord_equal() +
  ggtitle(current.crop) +
  theme(axis.ticks = element_blank(),  
    axis.text = element_blank(), axis.title=element_blank(),
    plot.margin= unit(rep(0, 4), "lines"),
    legend.title=element_blank()) 

}



legended.plot <- ggplot(agro.zone.df) + 
  aes(long,lat,group=group,fill=x19.fertilizante.cantidad.kg.posi) + 
  geom_polygon() +
#  geom_path(color="white") +
  coord_equal()  +
  scale_fill_gradient(limits=c(0,fert.posi.agg.max)) +
   theme(legend.title=element_blank())



g_legend<-function(a.gplot){
    tmp <- ggplot_gtable(ggplot_build(a.gplot))
    leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
    legend <- tmp$grobs[[leg]]
    legend
}
# Thanks to http://stackoverflow.com/questions/12539348/ggplot-separate-legend-and-plot

fert.ggplots.ls[[length(fert.ggplots.ls) + 1]] <- g_legend(legended.plot)

fert.ggplots.ls$ncol <- 3
fert.ggplots.ls$main <- "Proportion of plots using fertilizer by agroproductive zone, common scale"

fert.map.unif.scale <- do.call(arrangeGrob, fert.ggplots.ls )
# do grid.arrange to directly plot

fert.ggplots.own.scale.ls$ncol <- 3
fert.ggplots.own.scale.ls$main <- "Proportion of plots using fertilizer by agroproductive zone, crop-specific scale"

fert.map.spec.scale  <- do.call(arrangeGrob, fert.ggplots.own.scale.ls )

ggsave(filename = paste0(output.path, "Fert map common scale.pdf"), plot = fert.map.unif.scale)
ggsave(filename = paste0(output.path, "Fert map crop-specific scale.pdf"), plot = fert.map.spec.scale)




set.seed(13)

col.par = function(n) sample(seq(0.3, 1, length.out=50),n); cols = rainbow(26, s=col.par(26), v=col.par(26))[sample(1:26,26)]

n.zonas <- length(unique(agro.zone.df$ZONA_AGROP))

rainbow.map.plot<- ggplot(agro.zone.df) + 
  aes(long,lat,group=group,fill=ZONA_AGROP) + 
  geom_polygon() +
#  geom_path(color="white") +
  coord_equal() +
  ggtitle("Agroproductive zones") +
  theme(axis.ticks = element_blank(), 
    axis.text = element_blank(), axis.title=element_blank(),
    plot.margin= unit(rep(0, 4), "lines"),
    legend.title=element_blank())  +
    scale_fill_manual(values=rainbow(n.zonas, s=col.par(n.zonas), v=col.par(n.zonas)))
    
# For colors, thanks to http://stackoverflow.com/questions/21352683/randomising-qualitative-colours-for-large-sets-in-ggplot

#  scale_fill_brewer(brewer.pal(length(unique(agro.zone.df$ZONA_AGROP)), "Set1"))

ggsave(filename = paste0(output.path, "Rainbow region map.pdf"), plot = rainbow.map.plot)


#palette = "spectral"



crop.prod.by.occupation <- aggregate(x19.produccion.obtenidad.kg ~  x12.su.ocupacion.principal.es.agropecuaria + which.crop  , 
  data=stacked.firm.df, FUN=sum, na.rm=TRUE)

crop.prod.by.occupation <- crop.prod.by.occupation[crop.prod.by.occupation[, 1]!="S/D", ]

crop.prod.by.occupation <- reshape(crop.prod.by.occupation, timevar="x12.su.ocupacion.principal.es.agropecuaria",idvar="which.crop", direction="wide")

crop.prod.by.occupation$perc.grown.by.farmers.by.profession <- with(crop.prod.by.occupation, 
  100 * crop.prod.by.occupation$x19.produccion.obtenidad.kg.Si / 
    (x19.produccion.obtenidad.kg.Si + x19.produccion.obtenidad.kg.No)
  )

crop.prod.by.occupation

# Now do the same thing with survey weights:


crop.prod.by.occupation <- aggregate(I(x19.produccion.obtenidad.kg*factor.de.expansión) ~  x12.su.ocupacion.principal.es.agropecuaria + which.crop  , 
  data=stacked.firm.df, FUN=sum, na.rm=TRUE)
# I hope I() works

crop.prod.by.occupation <- crop.prod.by.occupation[crop.prod.by.occupation[, 1]!="S/D", ]
names(crop.prod.by.occupation)[3] <- "x19.produccion.obtenidad.kg"

crop.prod.by.occupation <- reshape(crop.prod.by.occupation, timevar="x12.su.ocupacion.principal.es.agropecuaria",idvar="which.crop", direction="wide")

crop.prod.by.occupation$perc.grown.by.farmers.by.profession <- with(crop.prod.by.occupation, 
  100 * crop.prod.by.occupation$x19.produccion.obtenidad.kg.Si / 
    (x19.produccion.obtenidad.kg.Si + x19.produccion.obtenidad.kg.No)
  )

crop.prod.by.occupation
















# Thin crosstabs:



fert.ftable <-ftable( stacked.firm.df$x19.sem.comprada.cantidad.kg.posi,
  stacked.firm.df$tractor.hrs.final.posi,
  stacked.firm.df$x19.plagicidas.cantidad.kg.posi,
  stacked.firm.df$paid.hours.spread.posi,
  stacked.firm.df$x19.abono.cantidad.kg.posi,
  stacked.firm.df$x19.fertilizante.cantidad.kg.posi,
  dnn = c("Seed", "Tractor", "Plag", "Labor", "Abono", "Fert"))


fert.ftable <- as.data.frame(fert.ftable)

fert.ftable <- cbind(fert.ftable[fert.ftable$Fert==TRUE, ], 
  fert.ftable[fert.ftable$Fert==FALSE, "Freq"])

colnames(fert.ftable)[7:8] <- c("Uses", "Does.not.use")

fert.ftable$percentage.use <- round(100*fert.ftable$Uses/(fert.ftable$Uses + fert.ftable$Does.not.use), 1)



str(test.ftable)






target.top.crop.number <- 1
source(paste0(code.dir, "build-model-extract-parcels.r"))

library("effects")

price.lm <- lm( x01 ~ (w01 + w02 + w03 + w04 + w05 + w06 + y01 + I(y01^2))^2 + 
  I(w01^2) + I(w02^2) + I(w03^2) + I(w04^2) + I(w05^2) + I(w06^2) +
  (q01 + q02 + q03)^2  , subset=x01>0   
   )
summary(price.lm)

plot(Effect("w01", price.lm), ask = FALSE, rescale.axis = FALSE)


plot(Effect("w06", price.lm), ask = FALSE, rescale.axis = FALSE)


price.lm <- lm( x19.fertilizante.bs.kg ~ elevation, data=stacked.firm.df)
summary(price.lm )

target.price <- "x19.abono.bs.kg"

ggplot(stacked.firm.df, aes(elevation, get(target.price))) +
  geom_point()  + stat_smooth() +
  ggtitle("") +
  theme( axis.title.y = element_blank())

#w01 = firm.df$x19.fertilizante.bs.kg
#w02 = firm.df$x19.sem.comprada.bs.kg
#w03 = firm.df$hourly.tractor.rental
#w04 = firm.df$x19.plagicidas.bs.kg
#w05 = firm.df$hourly.wage
#w06 = firm.df$x19.abono.bs.kg







glm( ~ s(x0)+ s(x1)+s(x2)+s(x3),family=Gamma(link=log)

price.lm <- lm( x19.fertilizante.cantidad.kg ~ x19.fertilizante.bs.kg + elevation, data=stacked.firm.df)
summary(price.lm )


price.lm <- lm( x19.plagicidas.cantidad.kg ~ x19.plagicidas.bs.kg + elevation, data=stacked.firm.df)
summary(price.lm )


price.lm <- lm( x19.sem.comprada.cantidad.kg ~ x19.sem.comprada.bs.kg + elevation, data=stacked.firm.df)
summary(price.lm )


price.lm <- glm( x19.fertilizante.cantidad.kg>0 ~ x19.fertilizante.bs.kg*which.crop + elevation , family=binomial(link=probit), data=stacked.firm.df)
summary(price.lm )


price.lm <- glm( x19.fertilizante.cantidad.kg>0 ~ x19.fertilizante.bs.kg*which.crop + elevation , family=binomial(link=probit), data=stacked.firm.df)
summary(price.lm )


price.lm <- lm( x19.sem.comprada.cantidad.kg ~ which.crop + elevation + mean.ann.rain.5yr, data=stacked.firm.df)
summary(price.lm )


stacked.firm.df$which.crop <- as.factor(stacked.firm.df$which.crop)

price.lm <- glm( x19.fertilizante.cantidad.kg>0 ~ which.crop *(poly(elevation, 2)  + 
  poly(mean.ann.rain.5yr, 2) + poly(soil.quality, 2)) , family=binomial(link=probit), data=stacked.firm.df)
summary(price.lm )

cor(predict(price.lm), stacked.firm.df$x19.fertilizante.cantidad.kg>0)

library("effects")
plot(Effect("mean.ann.rain.5yr", price.lm), ask = FALSE, rescale.axis = FALSE)
plot(Effect("soil.quality", price.lm), ask = FALSE, rescale.axis = FALSE)
plot(Effect("elevation", price.lm), ask = FALSE, rescale.axis = FALSE)


price.lm <- glm( x19.fertilizante.cantidad.kg>0 ~ which.crop  , family=binomial(link=probit), data=stacked.firm.df)
summary(price.lm )

cor(predict(price.lm), stacked.firm.df$x19.fertilizante.cantidad.kg>0)





mean(stacked.firm.df$mean.ann.rain.5yr)


price.lm <- glm( x19.fertilizante.cantidad.kg>0 ~ (x19.fertilizante.bs.kg + 
  I(x19.fertilizante.bs.kg^2))* which.crop  , 
  family=binomial(link=probit), data=stacked.firm.df)
summary(price.lm )


price.lm <- glm( x19.fertilizante.cantidad.kg>0 ~ x19.fertilizante.bs.kg + 
  I(x19.fertilizante.bs.kg^2)  , 
  family=binomial(link=probit), data=stacked.firm.df)
summary(price.lm )


# install.packages("mfx")
library("mfx")

#price.mfx <-  probitmfx(formula=x19.fertilizante.cantidad.kg>0 ~  
#  poly(x19.fertilizante.bs.kg, 2, raw = TRUE) , data=stacked.firm.df)
#price.mfx
# Above is no good

# install.packages("effects")
library("effects")

stacked.firm.df$which.crop.factor <- factor(stacked.firm.df$which.crop)

price.mfx <- glm( x19.fertilizante.cantidad.kg>0 ~
  poly(x19.fertilizante.bs.kg, 2, raw = TRUE)* which.crop.factor  , 
  family=binomial(link=probit), data=stacked.firm.df)

plot(Effect("x19.fertilizante.bs.kg", price.mfx), ask = FALSE, rescale.axis = FALSE)



plot(allEffects(price.mfx), ask = FALSE, rescale.axis = FALSE)





#price.lm <- glm( x19.fertilizante.cantidad.kg>0 ~ (x19.fertilizante.bs.kg + 
#  I(x19.fertilizante.bs.kg^2))* I(gsub("Bar", "X", which.crop))  ,  # This is good for quickly reveling the factor
#  family=binomial(link=probit), data=stacked.firm.df)
#summary(price.lm )




price.lm <- lm( x19.fertilizante.cantidad.kg ~ (x19.fertilizante.bs.kg + I(x19.fertilizante.bs.kg^2))*which.crop   ,data=stacked.firm.df[stacked.firm.df$x19.fertilizante.cantidad.kg>0, ])
summary(price.lm )



price.lm <- lm( x19.fertilizante.cantidad.kg >0 ~ (x19.fertilizante.bs.kg + I(x19.fertilizante.bs.kg^2))*which.crop   ,data=stacked.firm.df)
summary(price.lm )


price.lm <- lm( x19.fertilizante.cantidad.kg >0 ~ x19.fertilizante.bs.kg ,data=stacked.firm.df)
summary(price.lm )





prop.table(table(stacked.firm.df$x12.su.ocupacion.principal.es.agropecuaria))




#Join PDF's:
# http://gotofritz.net/blog/howto/joining-pdf-files-in-os-x-from-the-command-line/


# Input in terminal: 
cd '/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Descriptive plots/'
"/System/Library/Automator/Combine PDF Pages.action/Contents/Resources/join.py" -o 'Fert crosstabs combined.pdf' *.pdf







summary(lm( y01 ~ (x01 + x02 + x03 + x04 + x05 + x06 + q01 + q02 + q03)^2 
 + I(x01^2) + I(x02^2) + I(x03^2) + I(x04^2) + I(x05^2) + I(x06^2) + I(q01^2) + I(q02^2) + I(q03^2) 
# + I(x01>0) + I(x02>0) + I(x03>0) + I(x04>0) + I(x05>0) + I(x06>0) + I(q01>0) + I(q02>0) + I(q03>0) 
))




summary(lm( y01 ~ (I(x01>0) + I(x02>0) + I(x03>0) + I(x04>0) + I(x05>0) + I(x06>0) )^6
))










# DELETE BELOW:













#grid.arrange(arrangeGrob(p1,p3,p2,p4, ncol=2, nrow=2), main = "Daily QC: Blue",nrow=1)
#main=textGrob("Daily QC: Blue",gp=gpar(fontsize=20,font=3))
# If need to makle additional changes to title fonts: 
# http://stackoverflow.com/questions/14726078/changing-title-in-multiplot-ggplot2-using-grid-arrange

# mean will give proportion

#nation.shp@data$provincia.full <- paste0(substr( nation.shp@data$DCODE, 4, 5), substr( nation.shp@data$DCODE, 7,8))

#nation.shp@data <- merge(nation.shp@data, wage.agg, all.x=TRUE)
#nation.shp@data <- merge(nation.shp@data, fert.price.agg, all.x=TRUE)
#nation.shp@data <- merge(nation.shp@data, tractor.price.agg, all.x=TRUE)

# group=group,


#nation.df = join(nation.points, nation.shp@data, by="id")



plot1 <- ggplot(nation.df) + 
  aes(long,lat,group=group,fill=hourly.tractor.rental) + 
  geom_polygon() +
#  geom_path(color="white") +
  coord_equal()
  
plot2 <-ggplot(nation.df) + 
  aes(long,lat,group=group,fill=x19.fertilizante.bs.kg) + 
  geom_polygon() +
#  geom_path(color="white") +
  coord_equal()

plot3 <-ggplot(nation.df) + 
  aes(long,lat,group=group,fill=hourly.wage) + 
  geom_polygon() +
#  geom_path(color="white") +
  coord_equal()

grid.arrange(plot1, plot2, plot3, ncol=2)




























bovino.df<- read.spss(paste0(work.dir, "bd68/3-GANADERIA/Balance Ganadero/5.-ENA08_BOLIVIA_BALANCE_GANADERO_BOVINOS(preg_23).sav"), to.data.frame=TRUE)
colnames(bovino.df) <- tolower( make.names(gsub("[()]|[.]", "", attr(bovino.df, "variable.labels")) ) )

bovino.df$folio





  +
  theme(legend.position = "none") 
  
  
  + 
  scale_colour_manual(values = c("blue"))





, colour = sex

      ID avg lower upper sex
1 Study1 2.0  1.80  2.20   m
2 Study2 3.0  2.70  3.30   m
3 Study3 3.5  3.15  3.85   m
4 Study1 2.5  2.25  2.75   f
5 Study2 3.3  2.97  3.63   f
6 Study3 4.0  3.60  4.40   f

# plot
ggplot(data = df, aes(x = ID, y = avg, ymin = lower, ymax = upper, colour = sex)) +
  geom_point(position = position_dodge(width = 0.2)) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.1) +
  coord_flip() +
  scale_colour_manual(values = c("blue", "red")) +
  theme_classic()


/Users/travismcarthur/Downloads/rar/rar x '/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Zonas_agroproductivas19.zip'

/Users/travismcarthur/Downloads/rar/rar e '/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Zonas_agroproductivas19.zip'


# About half of abono is not purchased:

table( stacked.firm.df$x19.abono.cantidad > 0,  stacked.firm.df$x19.abono.bs >0 )
table( stacked.firm.df$x19.fertilizante.cantidad > 0,  stacked.firm.df$x19.fertilizante.bs.unid >0 )
table( stacked.firm.df$x19.plagicidas.cantidad > 0,  stacked.firm.df$x19.plagicidas.bs.unid >0 )
table( stacked.firm.df$x19.sem.comprada.cantidad > 0,  stacked.firm.df$x19.semilla.comprada.bs >0 )



test.unif <- runif(10000, 0, 2)
cor(test.unif, test.unif^2)
# Result is 0.967993 !
test.unif <- runif(10000, 0, 1000)
cor(test.unif, test.unif^2)

test.unif <- runif(10000, -1, 1)
cor(test.unif, test.unif^2)

