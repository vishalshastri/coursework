
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






target.crop <- top.crops[target.top.crop.number]

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

firm.df<- firm.df[uncensored.cost, ]
# try to see what happens when we eliminate censoring




# NOTE: BELOW IS WHERE BOOTSTRAPPING HAPPENS

firm.df <- firm.df[bootstrap.selection.v, ]


# sur-var-building
# linear-sur-building
source(paste0(code.dir, "sur-var-building.r"), local=TRUE)
