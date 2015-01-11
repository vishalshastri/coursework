
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

price.trim.criteria <- apply(firm.df[, price.to.trim], 2, FUN=function(x) x < quantile(x, probs=price.trim.quantile) )
price.trim.criteria <- apply(price.trim.criteria, 1, FUN=all)
firm.df <- firm.df[price.trim.criteria, ]


if (functional.form =="SGM") {

demand.var.to.trim <- c(
  "x19.fertilizante.cantidad.kg",    
  "x19.sem.comprada.cantidad.kg", 
  "x19.abono.cantidad.kg", 
  "x19.plagicidas.cantidad.kg",
  "paid.hours.spread", "tractor.hrs.final")

demand.var.trim.criteria <- apply(firm.df[, demand.var.to.trim]/firm.df$x19.produccion.obtenidad.kg, 2, 
  FUN=function(x) x < quantile(x[x>0], probs=demand.var.trim.quantile) )
# data.frame(a=1:10, b=101:110)/(1:10) is ok, so the above operation works
demand.var.trim.criteria <- apply(demand.var.trim.criteria, 1, FUN=all)
firm.df <- firm.df[demand.var.trim.criteria, ]

}


if (functional.form =="TRANSLOG") {

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
}
# try to see what happens when we eliminate censoring




# NOTE: BELOW IS WHERE BOOTSTRAPPING HAPPENS

firm.df <- firm.df[bootstrap.selection.v, ]

if (functional.form =="TRANSLOG") {
# sur-var-building
# linear-sur-building
source(paste0(code.dir, "sur-var-building.r"), local=local.source.evaluation)
source(paste0(code.dir, "linear-sur-building.r"), local=local.source.evaluation)
source(paste0(code.dir, "nonlinear-sur-building.r"), local=local.source.evaluation)




region <- toupper(firm.df$zona.agroproductiva)
region[region=="VALLES CERRADAS     "] <- "VALLES CERRADOS     "

region <- gsub("Ú", "U", region)
region <- gsub(" *$", "", region)
region <- gsub(" ", ".", region)
region <- make.names(region)

region[region %in% c("CHACO.HUMEDO", "CHACO.SECO", "LLANOS.DE.SANTA.CRUZ", "PAMPAS.DE.MOXOS")] <- "CHACO"
region[region %in% c("YUNGAS.DEL.NORTE", "YUNGAS.DEL.SUR")] <- "YUNGAS"

if (target.crop=="Cebada combined") {
  region[region %in% "ALTIPLANO.SUR"] <- "ALTIPLANO.CENTRAL"
}

region <- factor(region)

table(region)

# Make region01 be the actual variable, and then the name of the region be the parameter name 

region.matrix <- model.matrix(ln.E.data ~ region, data=firm.df)[, -1] # take out intercept

# need to stick this matrix with the full dataframe

region.tackon <- paste0("region",  lead.zero(1:ncol(region.matrix)), " * ", colnames(region.matrix), collapse=" + ")

# sort(table(region))


#CHACO.HUMEDO 
#CHACO.SECO
#LLANOS.DE.SANTA.CRUZ 

# LLANOS.DE.SANTA.CRUZ should go in Chaco because:
# http://es.wikipedia.org/wiki/Llanos_de_Chiquitos
#regionYUNGAS.DEL.NORTE       regionYUNGAS.DEL.SUR 
# Pampas in Chaco
# http://en.wikipedia.org/wiki/Gran_Chaco


S.n.H.region <- S.n.H

S.n.H.region[[length(S.n.H.region)]] <- 
  as.formula( paste0("ln.E.data ~ ", as.character(S.n.H[[length(S.n.H)]])[[3]], " + region" ) )
  
linear.sur.est.region <- systemfit( S.n.H.region, "SUR", restrict.matrix = lm.param.restrictions,  maxit = 5000 )

region.rearrange <- coef(linear.sur.est.region)[grepl("region", names(coef(linear.sur.est.region)))]
linear.sur.est.region$coefficients <- c(coef(linear.sur.est.region)[
  !grepl("region", names(coef(linear.sur.est.region)))], region.rearrange)


ln.E.vars <- lin.to.nonlin.crossref.df$nonlinear


ln.E.start.vals <- vector(mode="numeric", length=length(coef(linear.sur.est.region)))
ln.E.start.vals <- coef(linear.sur.est.region)[grepl("cost.fn_", names(coef(linear.sur.est.region)))]
names(ln.E.start.vals) <- ln.E.vars
names(ln.E.start.vals)[is.na(names(ln.E.start.vals)) ] <- 
  paste0("region",  lead.zero(1:ncol(region.matrix)))

# This is to handle the fact that some of these drop out with adding-up restrictions:
ln.E.start.vals <- ln.E.start.vals[!grepl("(beta01)|(beta....01)|(gamma....01)", 
  names(ln.E.start.vals))]


theta.starts <- rep(1, times=N-1)
names(theta.starts) <- paste0("theta", lead.zero(1:(N-1)))
theta06 <- 1

ln.E.start.vals <-c(ln.E.start.vals, theta.starts)




#ln.E <- paste0("nls.formula.ln.E.region <- ln.E.data ~ ", ln.E.string, " + ", region.tackon)

# changing this a bit so we only have  the string
ln.E <- paste0(ln.E.string, " + ", region.tackon)

ln.E <- iconv(ln.E, to="ASCII//TRANSLIT")

ln.E <- gsub("'", "", ln.E )

nls.formula.ln.E.region <- ln.E 

#eval(parse(text=ln.E))

}

if (functional.form =="SGM") {

  source(paste0(code.dir, "sgm-linear-sur-building.r"), local=local.source.evaluation)  
  source(paste0(code.dir, "sur-var-building.r"), local=local.source.evaluation)  
  source(paste0(code.dir, "sgm-tobit.r"), local=local.source.evaluation) 

}












