

#saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/GAMS work/saved workspace.Rdata"

saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF.Rdata"



load(saved.workspace.path)


inputs.df$x19.codigo.saved <- inputs.df$x19.codigo
#inputs.df$x19.codigo <- as.character(inputs.df$x19.codigo.saved)

inputs.df$x19.codigo <- as.character(inputs.df$x19.codigo)

inputs.df$x19.codigo[inputs.df$x19.codigo %in% 
  c("Maiz Choclo", "Maiz blando (dulce, blanco, chuspillo)", 
  "Maiz duro (cristalino, cubano)        ")] <- "Maiz combined"


inputs.df$x19.codigo[inputs.df$x19.codigo %in% 
  c("Cebada (berza) ", "Cebada en grano         " )] <- "Cebada combined"

inputs.df$x19.codigo <- factor(inputs.df$x19.codigo)

top.crops <- names(sort(table(inputs.df$x19.codigo), decreasing=TRUE))[1:10]
sort(table(inputs.df$x19.codigo), decreasing=TRUE)[1:10]

# for (target.crop in top.crops) {




region.inputs <- toupper(inputs.df$zona.agroproductiva)
region.inputs[region.inputs=="VALLES CERRADAS     "] <- "VALLES CERRADOS     "

region.inputs <- gsub("Ú", "U", region.inputs)
region.inputs <- gsub(" *$", "", region.inputs)
region.inputs <- gsub(" ", ".", region.inputs)
region.inputs <- make.names(region.inputs)

#region.inputs[region.inputs %in% c("CHACO.HUMEDO", "CHACO.SECO", "LLANOS.DE.SANTA.CRUZ", "PAMPAS.DE.MOXOS")] <- "CHACO"
#region.inputs[region.inputs %in% c("YUNGAS.DEL.NORTE", "YUNGAS.DEL.SUR", "YUMGAS.DEL.NORTE", "YUNGAS.DE.NORTE")] <- "YUNGAS"

# OK, we are going to avoid the groupings that we had for the actual estimation in
# order to get a better disaggregation


region.inputs[region.inputs %in% c("YUMGAS.DEL.NORTE", "YUNGAS.DE.NORTE")] <- "YUNGAS.DEL.NORTE"

region.inputs[region.inputs =="ALTIPANO.CENTRAL"] <- "ALTIPLANO.CENTRAL"

region.inputs <- factor(region.inputs)

inputs.df$zona.agroproductiva.cleaned <- region.inputs

full.sample.fert.use.df <- aggregate(inputs.df$x19.fertilizante.cantidad.kg, by=list(folio=inputs.df$folio), FUN=sum) 

colnames(full.sample.fert.use.df)[2] <- "fert.use.kg"

full.sample.fert.use.df <- merge(full.sample.fert.use.df, 
  inputs.df[!duplicated(inputs.df[, "folio"]), c("folio", "zona.agroproductiva.cleaned")])
# For some reason we still have 2 "bad" values for agroproductive zone, where 
# a given firm is listed as having two distinct zones, but we have dealt
# with that by having duplicated(inputs.df[, "folio"]) instead of 
# duplicated(inputs.df[, c("folio", "zona.agroproductiva.cleaned")])


table(region.inputs)


fert.region.use.tab <-  data.frame(round(prop.table(table(
  full.sample.fert.use.df$zona.agroproductiva.cleaned,
  full.sample.fert.use.df$fert.use.kg > 0), margin=1), digits=2)[, 2])
names(fert.region.use.tab) <- "prop.firms.use.fert"

fert.region.use.tab[, "num.firms"] <- 
  table(full.sample.fert.use.df$zona.agroproductiva.cleaned)

fert.region.use.tab




num.of.top.crops <- 5

top.crops <- names(sort(table(inputs.df$x19.codigo), decreasing=TRUE))[1:10]
sort(table(inputs.df$x19.codigo), decreasing=TRUE)[1:10]

# for (target.crop in top.crops) {

censored.cols.ls <- list()
fert.intensity.unconditional.ls <- list()
fert.intensity.conditional.ls <- list()
full.summary.stats.ls <- list()
number.obs.ls <- list()



for ( i in 1:num.of.top.crops) {


target.crop <- top.crops[i]
print(target.crop)

firm.df <- inputs.df[inputs.df$x19.codigo == target.crop & inputs.df$x19.produccion.obtenidad.kg>0 &
!is.na(inputs.df$x19.produccion.obtenidad.kg), ]

# Above, we are eliminating zero-harvest cases, mostly so we can take log

price.to.trim <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg",
   "x19.plagicidas.bs.kg", "hourly.wage",  "hourly.tractor.rental" )

# firm.df <- firm.df[!is.na(firm.df$hourly.tractor.rental), ]
# only kills 2 obseravtions for maiz and zero for Barley

price.trim.criteria <- apply(firm.df[, price.to.trim], 2, FUN=function(x) x < quantile(x, probs=0.99) )
price.trim.criteria <- apply(price.trim.criteria, 1, FUN=all)
firm.df <- firm.df[price.trim.criteria, ]

uncensored.cost <- apply(firm.df[, c(
  "x19.fertilizante.cantidad.kg",    
  "x19.sem.comprada.cantidad.kg", 
  "x19.abono.cantidad.kg", 
  "x19.plagicidas.cantidad.kg",
  "paid.hours.spread", "tractor.hrs.final")], 
  # x107.hrs.tractor.spread
  1, FUN=function(x) {sum(x)!=0}
)

print(table(uncensored.cost))

firm.df<- firm.df[uncensored.cost, ]
# try to see what happens when we eliminate censoring

number.obs.ls[[target.crop]] <- nrow(firm.df)

censored.cols <- c(sum(firm.df$x19.fertilizante.cantidad.kg!=0)/nrow(firm.df),
sum(firm.df$x19.sem.comprada.cantidad.kg!=0)/nrow(firm.df),
sum(firm.df$x19.abono.cantidad.kg!=0)/nrow(firm.df),
sum(firm.df$x19.plagicidas.cantidad.kg!=0)/nrow(firm.df),
sum(firm.df$paid.hours.spread!=0)/nrow(firm.df),
sum(firm.df$tractor.hrs.final!=0)/nrow(firm.df)
)

censored.cols.ls[[i]] <- censored.cols

fert.intensity.unconditional.ls[[i]] <- 
  mean(firm.df$x19.fertilizante.cantidad.kg/firm.df$x19.superficie.cultivada.hectareas, na.rm=TRUE)

fert.intensity.conditional.ls[[i]] <- 
  median(firm.df$x19.fertilizante.cantidad.kg[firm.df$x19.fertilizante.cantidad.kg>0] / 
    firm.df$x19.superficie.cultivada.hectareas[firm.df$x19.fertilizante.cantidad.kg>0], na.rm=TRUE)
    
cat("number of firms with same crop in multiple plots", "\n")
print(table(table(firm.df$folio)))

w01 = firm.df$x19.fertilizante.bs.kg
w02 = firm.df$x19.sem.comprada.bs.kg
w03 = firm.df$hourly.tractor.rental
w04 = firm.df$x19.plagicidas.bs.kg
w05 = firm.df$hourly.wage
w06 = firm.df$x19.abono.bs.kg
y01 <- log( firm.df$x19.produccion.obtenidad.kg )

q01 = firm.df$x19.superficie.cultivada.hectareas
# q01[q01 ==0] = median(q01)

firm.df$x19.uso.riego.r = ifelse(firm.df$x19.uso.riego!="Si",  0, 1)

firm.df$ag.fam.labor.equiv.spread.r = firm.df$ag.fam.labor.equiv.spread
firm.df$ag.fam.labor.equiv.spread.r[firm.df$ag.fam.labor.equiv.spread.r == 0] = .5

full.summary.stats.ls[[target.crop]]<- do.call(rbind, lapply(firm.df[, c(
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
  "x19.uso.riego.r",
  "ag.fam.labor.equiv.spread.r"
  )  ], FUN=summary)
  )


}

censored.df <- do.call(rbind, censored.cols.ls)

colnames(censored.df) <- c("Inorganic Fert", "Seed", "Organic Fert", "Plaguicidas", "Labor", "Tractor Hrs")

barplot(as.matrix(censored.df), beside=TRUE, col=terrain.colors(num.of.top.crops),
  main="Proportion of uncensored observations")

legend("top", top.crops[1:num.of.top.crops], cex=1, 
       fill=terrain.colors(num.of.top.crops))
#cex=0.6

names(fert.intensity.unconditional.ls) <- top.crops[1:num.of.top.crops]
names(fert.intensity.conditional.ls) <- top.crops[1:num.of.top.crops]

# NOTE: Below is mean
unlist(fert.intensity.unconditional.ls)
# NOTE: Below is median
unlist(fert.intensity.conditional.ls)


save.image(file="/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/2008 summary stats.Rdata")


# number of farms that have the same crop in multiple plots
       






