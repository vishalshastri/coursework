#results.ls <- extract.gams.est()



target.top.crop.number <- 1

#Including zero cost:
#Potatoes	4,058
#Maize	3,440
#Barley	2,048
#Wheat	1,647
#Fava Beans	1,484

M <- 1
N <- 6
#J <- 3
J <- 3
 

do.data.prep <- TRUE
source("/Users/travismcarthur/git/coursework/aae-637/paper/postestimation-functions.r")

text.crop <- "potatoes"
text.crop <- "maize"
#text.crop <- "wheat"



results.ls <- extract.gams.est(file.name = "sgmGMEnonlinearMaiz00000 3 fixed inputs.lst")
# results.ls <- extract.gams.est(file.name = "sgmGMEnonlinearTrigo00000 6 fixed inputs.lst")
# "sgmGMEnonlinearPapa00000 6 fixed inputs.lst"

gc(); gc(); gc(); gc(); gc(); gc(); gc(); gc(); gc(); gc(); gc(); gc(); 


library("stargazer")
library("xtable")



xtab.output <- print(xtable(shadow.price.params.extracted(params=results.ls$params),
  caption=paste0("Shadow price parameters for ", text.crop)),
  caption.placement = "top")
  
cat(xtab.output, sep="n",
      file=paste0("/Users/travismcarthur/Desktop/Proposal course/Materials for 3-31 meeting with Brad/shadow-price-",
      text.crop, "-", J, "-fixed-inputs.tex")
      )




inef.cost.df<- as.data.frame(inef.cost.est(params=results.ls$params, data=combined.df))

inef.cost.df <- inef.cost.df[, 3:4]
names(inef.cost.df) <- c("Increase in cost, in Bolivianos", "Percent increase in cost")
inef.cost.df[, 2] <- inef.cost.df[, 2]*100

stargazer(inef.cost.df, summary.stat=c("p25", "median", "mean", "p75"),
  title="Summary statistics for each parcel's increase in cost due to allocative inefficiency \\newline Note: The exchange rate is approximately 7 Bolivianos per USD", 
  out=paste0("/Users/travismcarthur/Desktop/Proposal course/Materials for 3-31 meeting with Brad/inef-cost-",
      text.crop, "-", J, "-fixed-inputs.tex"),
      align=TRUE)


inef.demand.ls <- inef.demand.est(params=results.ls$params, data=combined.df)

inef.demand.ls$ch.optimal.demand.levels.non.neg <- inef.demand.ls$ch.optimal.demand.levels.non.neg * (-1)


stargazer(inef.demand.ls$ch.optimal.demand.levels.non.neg, summary.stat=c("p25", "median", "mean", "p75"),
  title="Summary statistics for each parcel's change in input demand when shifting \\newline from an inefficient allocation to an efficient allocation \\newline Units: Fert in kg; Seed in kg; Tractor in hours; Plagicidas in kg; Labor in hours; Organic fert in kg", 
  out=paste0("/Users/travismcarthur/Desktop/Proposal course/Materials for 3-31 meeting with Brad/inef-demand-levels-",
      text.crop, "-", J, "-fixed-inputs.tex"),
      align=TRUE)





inef.demand.ls$ch.optimal.demand.perc.non.neg <- inef.demand.ls$ch.optimal.demand.perc.non.neg * (-1)
inef.demand.ls$ch.optimal.demand.perc.non.neg <- inef.demand.ls$ch.optimal.demand.perc.non.neg * 100

stargazer(inef.demand.ls$ch.optimal.demand.perc.non.neg, summary.stat=c("p25", "median", "mean", "p75"),
  title="Summary statistics for each parcel's \\textbf{percent} change in input demand when shifting \\newline from an inefficient allocation to an efficient allocation ", 
  out=paste0("/Users/travismcarthur/Desktop/Proposal course/Materials for 3-31 meeting with Brad/inef-demand-perc-",
      text.crop, "-", J, "-fixed-inputs.tex"),
      align=TRUE)


xtab.output <- print(xtable(allen.uzawa.e.s.mat(params=results.ls$params),
  caption=paste0("Allen-Uzawa elasticties of substitution for ", text.crop,", evaluated at mean of data")),
  caption.placement = "top")
  
cat(xtab.output, sep="n",
      file=paste0("/Users/travismcarthur/Desktop/Proposal course/Materials for 3-31 meeting with Brad/allen-elast-table-",
      text.crop, "-", J, "-fixed-inputs.tex")
      )


goodness.of.fit.mat <- cor.pred.actual.posi(params=results.ls$params)

xtab.output <- print(xtable(goodness.of.fit.mat,
  caption=paste0("Correlation between actual and predicted values for cost function and each  \\newline demand equation (restricted to observations with positive values) for ", text.crop," model")),
  caption.placement = "top",
  sanitize.text.function=function(x){x})
# Thanks to http://stackoverflow.com/questions/14877305/using-xtable-with-r-and-latex-math-mode-in-column-names

cat(xtab.output, sep="n",
      file=paste0("/Users/travismcarthur/Desktop/Proposal course/Materials for 3-31 meeting with Brad/goodness-of-fit-table-",
      text.crop, "-", J, "-fixed-inputs.tex")
      )

## END





### Can do below after doing the prep in the censoring-crosstabs.r script

library("stargazer")
library("xtable")
library("plyr") # requires plyr for rbind.fill()
cbind.fill <- function(...) {                                                                                                                                                       
  transpoted <- lapply(list(...),t)                                                                                                                                                 
  transpoted_dataframe <- lapply(transpoted, as.data.frame)                                                                                                                         
  return (data.frame(t(rbind.fill(transpoted_dataframe))))                                                                                                                          
} 
# Thanks to http://stackoverflow.com/questions/7962267/cbind-a-df-with-an-empty-df-cbind-fill

for (impute.level.crop in c("Potatoes", "Maize", "Wheat", "Fava Beans") ) {


impute.level.ls <- lapply(stacked.firm.df[stacked.firm.df$which.crop==impute.level.crop, 
  grepl("impute", colnames(stacked.firm.df)) & ! grepl("hrs.tractor", colnames(stacked.firm.df))],
  FUN=function(x) {
    ret <- as.data.frame(prop.table(table(x)))
    rownames(ret) <- ret[, 1]
    ret[, 2] <- ret[, 2]*100
    row.order <- c("itself", "household", "segmento.full", "sector.full", "canton.full", "seccion.full", "provincia.full", "departamento", "nation")
    ret <- ret[row.order, ]
    ret <- ret[!is.na(ret[, 2]), , drop=FALSE] # This removes any impute levels tat don't appear. This is "nation", basically.
    ret[, 2, drop=FALSE]
  })


impute.level.df <- do.call(cbind.fill, impute.level.ls )

impute.level.df[is.na(impute.level.df)] <- 0
 
colnames(impute.level.df) <- names(impute.level.ls)

colnames(impute.level.df) <- c("Fert", "Seed", "Organic fert", "Plagicidas", "Hired labor", "Tractor")

stargazer(impute.level.df, summary=FALSE, digits=2, align=TRUE,
  title=paste0(impute.level.crop, ": Geographic imputation level for each input prices in percent"  ), 
  out=paste0("/Users/travismcarthur/Desktop/Proposal course/Materials for 3-31 meeting with Brad/impute-level-",
      strsplit(impute.level.crop, " ")[[1]][1], ".tex"))

}


#mean(stacked.firm.df$x19.fertilizante.cantidad.kg[stacked.firm.df$which.crop=="Wheat"]>0)
#mean(stacked.firm.df$x19.fertilizante.cantidad.kg[stacked.firm.df$which.crop=="Barley"]>0)





## CHECKING THE POPULATION SIZE FOR EACH LEVEL
library("PBSmapping")
pob.shp.check <- importShapefile(paste0("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/", "centros_poblados.zip Folder/centros_poblados.shp"))

pob.shp.check$ciuorg.full <- with(pob.shp.check , paste0(DEPTO, PROVIN, SECCION, CANTON, CIUORG))
pob.shp.check$canton.full <- with(pob.shp.check , paste0(DEPTO, PROVIN, SECCION, CANTON))
pob.shp.check$seccion.full <- with(pob.shp.check , paste0(DEPTO, PROVIN, SECCION))
pob.shp.check$provincia.full <- with(pob.shp.check , paste0(DEPTO, PROVIN))
pob.shp.check$departamento <- with(pob.shp.check , paste0(DEPTO))

#POB2001

median(pob.shp$VIVIENDA)

for ( target.geog.unit.check in c("ciuorg.full", "canton.full", "seccion.full", "provincia.full", "departamento")) {
  pob.agg <- aggregate(pob.shp.check$VIVIENDA, by=list(pob.shp.check[, target.geog.unit.check]), FUN=sum, na.rm=TRUE)
  print(target.geog.unit.check)
  print(c(median=median(pob.agg$x), mean=mean(pob.agg$x)))
}




## BELOW COMPUTE SUMMARY STATS
library("stargazer")

summary.stats.ls <- list()

stacked.firm.df$x19.uso.riego.logical <- as.numeric(stacked.firm.df$x19.uso.riego=="Si" )

summary.stats.ls[[1]] <- do.call(rbind, lapply(stacked.firm.df[, c(
  "x19.produccion.obtenidad.kg",
  "x19.fertilizante.cantidad.kg",    
  "x19.sem.comprada.cantidad.kg", 
  "x19.abono.cantidad.kg", 
  "x19.plagicidas.cantidad.kg",
  "paid.hours.spread", 
  "tractor.hrs.final",
  "x19.fertilizante.bs.kg",
  "x19.sem.comprada.bs.kg",
  "hourly.tractor.rental",
  "x19.plagicidas.bs.kg",
  "hourly.wage",
  "x19.abono.bs.kg",
  "x19.superficie.cultivada.hectareas",
  "x19.uso.riego.logical",
  "ag.fam.labor.equiv.spread",
  "soil.quality",
  "elevation",
  "mean.ann.rain.5yr"
  )  ], FUN=summary)
  )
  
names(summary.stats.ls) <- "All crops"

for  ( i in c("Potatoes", "Maize", "Wheat", "Fava Beans")) {

summary.stats.ls[[i]] <- do.call(rbind, lapply(stacked.firm.df[stacked.firm.df$which.crop==i, c(
  "x19.produccion.obtenidad.kg",
  "x19.fertilizante.cantidad.kg",    
  "x19.sem.comprada.cantidad.kg", 
  "x19.abono.cantidad.kg", 
  "x19.plagicidas.cantidad.kg",
  "paid.hours.spread", 
  "tractor.hrs.final",
  "x19.fertilizante.bs.kg",
  "x19.sem.comprada.bs.kg",
  "hourly.tractor.rental",
  "x19.plagicidas.bs.kg",
  "hourly.wage",
  "x19.abono.bs.kg",
  "x19.superficie.cultivada.hectareas",
  "x19.uso.riego.logical",
  "ag.fam.labor.equiv.spread",
  "soil.quality",
  "elevation",
  "mean.ann.rain.5yr"
  )  ], FUN=summary)
  )

}

for ( i in 1:length(summary.stats.ls)) {
  stargazer(
    summary.stats.ls[[i]] , 
    summary=FALSE,#  type="html", out.header = FALSE,  
    rownames=TRUE,
    title=paste0("Summary stats for ", names(summary.stats.ls)[i]), align=TRUE,
    out=paste0("/Users/travismcarthur/Desktop/Proposal course/Materials for 3-31 meeting with Brad/summary-stats-", 
    gsub(" ", "-", names(summary.stats.ls)[i]), ".tex"),
    font.size="small"
  )
}




price.data.columns <- c("x19.fertilizante.bs.kg",
  "x19.sem.comprada.bs.kg",
  "x19.abono.bs.kg",
  "x19.plagicidas.bs.kg",
  "hourly.wage",  
  "hourly.tractor.rental")
  
clean.price.names <- c("Fert", "Seed", "Organic fert", "Plagicidas", "Hired labor", "Tractor")

for ( targ.price.data.column in 1:6) {
  pdf(file=paste0("/Users/travismcarthur/Desktop/Proposal course/Materials for 3-31 meeting with Brad/price-hist-",
   gsub(" ", "-", clean.price.names[targ.price.data.column]), ".pdf"), width=4, height=4)
   
  hist(stacked.firm.df[
      stacked.firm.df[, paste0(price.data.columns[targ.price.data.column], ".impute.level")] == "itself", 
      price.data.columns[targ.price.data.column] ], 
    breaks=20,
    xlab="Price in Bolivianos",
    main=paste0("Price of ", clean.price.names[targ.price.data.column])
     )

  dev.off()
}



## Andean yield comparison


library("ggplot2")

cross.country.yield.df <- read.csv("/Users/travismcarthur/Desktop/Proposal course/Materials for 3-31 meeting with Brad/andean-yield-comparison.csv",
  stringsAsFactors=FALSE)
  
cross.country.yield.df <- cross.country.yield.df[, !grepl("lag", colnames(cross.country.yield.df))]
cross.country.yield.df <- cross.country.yield.df[, !colnames(cross.country.yield.df) %in% c("X", "Element")]
cross.country.yield.df$Area <- gsub(" .+$", "", cross.country.yield.df$Area) 

library("reshape2")
#require(data.table)
df_m <- melt(cross.country.yield.df, id.var=c("Item","Area"))
df_m$variable<-as.numeric(gsub("X", "", df_m$variable))

ggplot(df_m, aes(x=variable, y=value, group=Area, colour=Area)) + 
    geom_line()   + 
    ggtitle("Yield (kg per hectare)") +
    facet_grid(Item ~ ., scales="free_y") +
    expand_limits(y=0)
# Thanks to http://stackoverflow.com/questions/11214012/set-only-lower-bound-of-a-limit-for-ggplot
  


## What prop[ortion of land is under fert?

# What proportion of land within each farm is under fertilizer use?

prop.land.fert.use <- by(inputs.df, INDICES=list(factor(inputs.df$folio)), FUN= function(x) {
  sum((x$x19.fertilizante.cantidad.kg>0)*x$x19.superficie.cultivada.hectareas)/sum(x$x19.superficie.cultivada.hectareas)
  }
)
# use factor() to clean away any levels that don't appear in stacked.firm.df

summary(prop.land.fert.use)

pdf(file=paste0("/Users/travismcarthur/Desktop/Proposal course/Materials for 3-31 meeting with Brad/prop-land-under-fert.pdf"), width=4, height=4)
hist(prop.land.fert.use, main="Proportion of each farmer's \nland that uses fertilizer")
dev.off()




mean(prop.land.fert.use==0)
mean(prop.land.fert.use==1)
mean(prop.land.fert.use!=0 & prop.land.fert.use!=1)
hist(prop.land.fert.use[prop.land.fert.use!=0 & prop.land.fert.use!=1])

table.multi.plot.folio <- by(stacked.firm.df, INDICES=list(factor(stacked.firm.df$which.crop)), FUN= function(x) {
  list(table(table(factor(x$folio))), round(100*prop.table(table(table(factor(x$folio))))))
  }
)







































#unique(stacked.firm.df$which.crop)
#[1] "Potatoes"   "Maize"      "Barley"     "Wheat"      "Fava Beans"


# Below was just double checking some things:


impute.level.ls <- lapply(inputs.df[, grepl("impute", colnames(inputs.df)) & ! grepl("hrs.tractor", colnames(inputs.df))],
  FUN=function(x) {
    ret <- as.data.frame(prop.table(table(x)))
 #   rownames(ret) <- ret[, 1]
 #   ret[, 2] <- ret[, 2]*100
#    row.order <- c("itself", "segmento.full", "sector.full", "seccion.full", "provincia.full", "departamento")
#    ret <- ret[row.order, ]
#    ret[, 2, drop=FALSE]
  })

table(inputs.df$x19.codigo[inputs.df$hourly.wage.impute.level=="canton.full"]) 
sort(table(stacked.firm.df$x19.codigo[stacked.firm.df$hourly.wage.impute.level=="canton.full"]))


unique(stacked.firm.df$hourly.tractor.rental.impute.level)


Departamento
 [57] "provincia.full"                                     
 [58] "seccion.full"  
 "canton.full"                                     
 [59] "sector.full"                                        
 [60] "segmento.full" 






inef.cost.est(params=results.ls$params, data=as.data.frame(t(colMeans(combined.df))))
inef.demand.est(params=results.ls$params, data=as.data.frame(t(colMeans(combined.df))))


lapply(inef.cost.est(results.ls$params), FUN=summary)


inef.demand.est(results.ls$params)

lapply(inef.demand.est(results.ls$params)[[1]], FUN=summary)

lapply(inef.demand.est(results.ls$params)[[3]], FUN=summary)
lapply(inef.demand.est(results.ls$params)[[4]], FUN=function(x) summary(x[is.finite(x)]))

library("xtable")






inef.cost.est(params=results.ls$params, data=as.data.frame(t(colMeans(combined.df))))
inef.demand.est(params=results.ls$params, data=as.data.frame(t(colMeans(combined.df))))









"max"
maximum
"mean"
mean
"median"
median
"min"
minimum
"n"
number of observations
"p25"
25th percentile
"p75"
75th percentile
"sd"
standard deviation












demand.curve.slope(1, params=results.ls$params)
demand.curve.slope(2, params=results.ls$params)
demand.curve.slope(3, params=results.ls$params)
demand.curve.slope(4, params=results.ls$params)
demand.curve.slope(5, params=results.ls$params)
demand.curve.slope(6, params=results.ls$params)


cost.elast.output(eval.point=median(combined.df$y01),  params=results.ls$params)

for ( targ.quantile in seq(.1, .9, by=.1)) {
  print(
    cost.elast.output(eval.point=quantile(combined.df$y01, probs=targ.quantile),  params=results.ls$params)
  )
}

for ( targ.quantile in seq(.1, .9, by=.1)) {
  print(
    cost.elast.output(eval.point=quantile(combined.df$y01, probs=targ.quantile),  
      params=results.ls$params,
      data=apply(combined.df, 2, FUN=quantile, probs=targ.quantile))
  )
}


for ( targ.quantile in seq(.1, .9, by=.1)) {
  print(
    cost.elast.output(eval.point=quantile(combined.df$y01, probs=targ.quantile),  
      params=results.ls$params,
      data=combined.df)
  )
}


all.obs.cost.elast<- cost.elast.output.all.obs(params=results.ls$params)
summary(all.obs.cost.elast[is.finite(all.obs.cost.elast)])
quantile(all.obs.cost.elast[is.finite(all.obs.cost.elast)], probs=seq(0, 1, by=.1))

for ( targ.eqn in 1:7) {

all.obs.input.elast <- cost.elast.fixed.input.all.obs(i=3, which.eqn=targ.eqn, params=results.ls$params)

summary(all.obs.input.elast[is.finite(all.obs.input.elast)])
print(round(quantile(all.obs.input.elast[is.finite(all.obs.input.elast)], probs=seq(0, 1, by=.1)), digits=2))
}


# Many observations have zero cost
#TOOD should we use predicted cost or actual cost? Probbaly predicted. we are using actual cost now.



lapply(inef.cost.est(results.ls$params), FUN=summary)


inef.demand.est(results.ls$params)

lapply(inef.demand.est(results.ls$params)[[1]], FUN=summary)

lapply(inef.demand.est(results.ls$params)[[3]], FUN=summary)
lapply(inef.demand.est(results.ls$params)[[4]], FUN=function(x) summary(x[is.finite(x)]))


inef.demand.est(results.ls$params)[[4]][[4]]





#######


eigen(hessian.cost.fn(params=results.ls$params))



