

saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/GAMS work/saved workspace.Rdata"





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




num.of.top.crops <- 5

top.crops <- names(sort(table(inputs.df$x19.codigo), decreasing=TRUE))[1:10]
sort(table(inputs.df$x19.codigo), decreasing=TRUE)[1:10]

# for (target.crop in top.crops) {

censored.cols.ls <- list()
fert.intensity.unconditional.ls <- list()
fert.intensity.conditional.ls <- list()



for ( i in 1:num.of.top.crops) {


target.crop <- top.crops[i]

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


}

censored.df <- do.call(rbind, censored.cols.ls)

colnames(censored.df) <- c("Fert", "Seed", "Abono", "Plag", "Labor", "Tractor")

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


       






