library("foreign")

saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF with soil and rain.Rdata"

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



work.dir <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/"

infr.df<- read.spss(paste0(work.dir, "bd68/7-INFRAESTRUCTURA Y MAQUINARIA/25.-ENA08_BOLIVIA_INFRAESTRUCTURA-MAQUINARIA(preg_103).sav"), to.data.frame=TRUE)

# New Zealand tax source for depreciation: https://www.ird.govt.nz/resources/6/5/6576ff004ba3cf748844bd9ef8e4b077/ir265.pdf
# Silos: 8% depreciation rate
# diminishing value (DV)
# Not accounting for inflation, but I think that is OK
# But really, a silo can hold the same amount regardless of age, unless it is unusable, in which case it wouldn't be counted in the data

colnames(infr.df) <- tolower( make.names(gsub("[()]|[.]", "", attr(infr.df, "variable.labels")) ) )

colnames(infr.df) <- make.names(colnames(infr.df), unique = TRUE)

output.df<- read.spss(paste0(work.dir, "bd68/2-AGRICOLA/Produccion (Preg-20)/3.-ENA08_BOLIVIA_DISTRIBUCION_PRODUCCION(preg_20).sav"), to.data.frame=TRUE)

colnames(output.df) <- tolower( make.names(gsub("[()]|[.]", "", attr(output.df, "variable.labels")) ) )

colnames(output.df) <- make.names(colnames(output.df), unique = TRUE)
colnames(output.df)[colnames(output.df)=="x20.bs.unidad.quintal"] <- "x20.bs.unidad.quintal.mn"
# "mn" is mercado nacional
colnames(output.df)[colnames(output.df)=="x20.bs.unidad.quintal.1"] <- "x20.bs.unidad.quintal.fuera.pais"

output.df$x20.codigo.de.producto <- as.character(output.df$x20.codigo.de.producto)
output.df$x20.codigo.de.producto[output.df$x20.codigo.de.producto %in% 
  c("Maiz Choclo", "Maiz blando (dulce, blanco, chuspillo)", 
  "Maiz duro (cristalino, cubano)        ")] <- "Maiz combined"
output.df$x20.codigo.de.producto[output.df$x20.codigo.de.producto %in% 
  c("Cebada (berza) ", "Cebada en grano         " )] <- "Cebada combined"
output.df$x20.codigo.de.producto <- factor(output.df$x20.codigo.de.producto)

infr.df$x103.descripcion <- gsub("(^ +)|( +$)", "", infr.df$x103.descripcion)

infr.df$value.imputed <- infr.df$x103.valor.de.compra

# Applying value imputation to all:
for ( i in unique(infr.df$x103.descripcion)) {
  temp.infr.item <- infr.df$x103.valor.de.compra[infr.df$x103.descripcion == i]
  temp.valor <- mean(temp.infr.item[temp.infr.item>0], na.rm=TRUE)
  if (is.na(temp.valor)) {temp.valor <- 0}
  infr.df[infr.df$x103.descripcion == i & infr.df$x103.valor.de.compra==0, "value.imputed"] <- temp.valor 
}

  



infra.agg <- aggregate(x103.numero ~ x103.descripcion + folio,  data=infr.df, FUN=sum, na.rm=TRUE)

infra.agg <- reshape(infra.agg, timevar="x103.descripcion",idvar="folio", direction="wide")
names(infra.agg) <- make.names(names(infra.agg))

infra.agg[is.na(infra.agg)] <- 0

output.infra.df <- merge(output.df[, c("folio", "x20.codigo.de.producto", "x20.produccion.cantidad.quintal")] , infra.agg, all.x=TRUE)

output.infra.df[is.na(output.infra.df)] <- 0



infra.value.agg <- aggregate(value.imputed ~ x103.descripcion + folio,  data=infr.df, FUN=sum, na.rm=TRUE)

infra.value.agg <- reshape(infra.value.agg, timevar="x103.descripcion",idvar="folio", direction="wide")
names(infra.value.agg) <- make.names(names(infra.value.agg))

infra.value.agg[is.na(infra.value.agg)] <- 0


output.infra.df <- merge(output.df[, c("folio", "x20.codigo.de.producto", "x20.produccion.cantidad.quintal")] , infra.value.agg, all.x=TRUE)

output.infra.df[is.na(output.infra.df)] <- 0



#output.infra.df$x103.numero[is.na(output.infra.df$x103.numero)] <- 0
#x103.numero.Silos.rústicos
#x103.numero.Silos.metálicos

# Chatas means: "Eran vehículos de carga muy grandes que podían transportar hasta 7 toneladas de carga, tirados por nun número variable de caballos percherones, en general"
# http://forum.wordreference.com/showthread.php?t=2471237
# Tinglado: shed
# Carreton: Wheelbarrow
# BUGUI: "Buggy", like sand buggy
# Baños.antisárnicos : Anti-mange baths (for animals, I imagine)
# TRAZADO = "plan"?

# Another set f exclusion restrictions: any equipment dealing with animals (e.g. TANQUE.ENFRIADOR.DE.LECHE), and any animals, in
# the case that a large proportion of the produce goes to animals (although, what about 
# the fact that people can buy the output crop in the market? Why then would they produce more of
# the crop for their animals? This would probably only work in the case that the output market
# does not work well.

targ.silo.crop <- "Maiz combined"
# "Haba (verde)    "   "Papa (patatas) "   "Trigo          "   "Maiz combined"
# Seems biggest effect of silos is for trigo. And in particular, the rustic silos matter for trigo.
# "Se recomienda el uso de silos rústicos para almacenar la papa bajo condiciones de aireación e iluminación adecuadas, para obtener una semilla de buena"
# Smaller effects for papa, and smaller still for maiz
# No effect for Haba
# Ordered from strongest effect to largest: Maiz trigo haba papa
# So maybe about 20% of maiz-growers have some type of silo




with(output.infra.df, prop.table(table( (x103.numero.Silos.rústicos + x103.numero.Silos.metálicos)[x20.codigo.de.producto ==targ.silo.crop])))

table(output.infra.df$x103.numero.Silos.rústicos[output.infra.df$x20.codigo.de.producto ==targ.silo.crop])
table(output.infra.df$x103.numero.Silos.metálicos[output.infra.df$x20.codigo.de.producto ==targ.silo.crop])
table(output.infra.df$x103.numero.ALMACEN[output.infra.df$x20.codigo.de.producto ==targ.silo.crop])

#table(infra.agg$x103.numero.AVIONETAS)


summary(lm(x20.produccion.cantidad.quintal ~ value.imputed.Silos.rústicos + value.imputed.Silos.metálicos +
 I(value.imputed.Silos.rústicos>0) + I(value.imputed.Silos.metálicos>0),
  data=output.infra.df[output.infra.df$x20.codigo.de.producto ==targ.silo.crop, ],
  subset=x20.produccion.cantidad.quintal>0))  


summary(lm(log(x20.produccion.cantidad.quintal) ~ value.imputed.Silos.rústicos + value.imputed.Silos.metálicos +
 I(value.imputed.Silos.rústicos>0) + I(value.imputed.Silos.metálicos>0),
  data=output.infra.df[output.infra.df$x20.codigo.de.producto ==targ.silo.crop, ],
  subset=x20.produccion.cantidad.quintal>0))  



summary(lm(x20.produccion.cantidad.quintal ~ x103.numero.Silos.rústicos + x103.numero.Silos.metálicos +
 I(x103.numero.Silos.rústicos>0) + I(x103.numero.Silos.metálicos>0),
  data=output.infra.df[output.infra.df$x20.codigo.de.producto ==targ.silo.crop, ],
  subset=x20.produccion.cantidad.quintal>0))  

summary(lm(log(x20.produccion.cantidad.quintal) ~ x103.numero.Silos.rústicos + x103.numero.Silos.metálicos +
 I(x103.numero.Silos.rústicos>0) + I(x103.numero.Silos.metálicos>0),
  data=output.infra.df[output.infra.df$x20.codigo.de.producto ==targ.silo.crop, ],
  subset=x20.produccion.cantidad.quintal>0))  

# One way to achieve the "log fit" is to take the exponential of the RHS, if we need to have the Y in output form



summary(lm(log(x20.produccion.cantidad.quintal) ~ x103.numero.Silos.rústicos,
  data=output.infra.df[output.infra.df$x20.codigo.de.producto ==targ.silo.crop, ],
  subset=x20.produccion.cantidad.quintal>0))


summary(lm(log(x20.produccion.cantidad.quintal) ~ I(x103.numero.Silos.rústicos>0),
  data=output.infra.df[output.infra.df$x20.codigo.de.producto ==targ.silo.crop, ],
  subset=x20.produccion.cantidad.quintal>0))

summary(lm(log(x20.produccion.cantidad.quintal) ~ x103.numero.Silos.rústicos + x103.numero.Silos.metálicos,
  data=output.infra.df[output.infra.df$x20.codigo.de.producto ==targ.silo.crop, ],
  subset=x20.produccion.cantidad.quintal>0))
  
summary(lm(x20.produccion.cantidad.quintal ~ x103.numero.Silos.rústicos + x103.numero.Silos.metálicos,
  data=output.infra.df[output.infra.df$x20.codigo.de.producto ==targ.silo.crop, ],
  subset=x20.produccion.cantidad.quintal>0))
  

summary(lm(x20.produccion.cantidad.quintal ~ x103.numero.Silos.rústicos + x103.numero.Silos.metálicos +
 I(x103.numero.Silos.rústicos>0) + I(x103.numero.Silos.metálicos>0),
  data=output.infra.df[output.infra.df$x20.codigo.de.producto ==targ.silo.crop, ],
  subset=x20.produccion.cantidad.quintal>0))  


  
summary(lm(x20.produccion.cantidad.quintal ~ . ,
  data=output.infra.df[output.infra.df$x20.codigo.de.producto ==targ.silo.crop, 
  ! colnames(output.infra.df) %in% c("folio", "x20.codigo.de.producto")],
  subset=x20.produccion.cantidad.quintal>0))

summary(lm(log(x20.produccion.cantidad.quintal) ~ . ,
  data=output.infra.df[output.infra.df$x20.codigo.de.producto ==targ.silo.crop, 
  ! colnames(output.infra.df) %in% c("folio", "x20.codigo.de.producto")],
  subset=x20.produccion.cantidad.quintal>0))



summary(lm(log(x20.produccion.cantidad.quintal) ~ I(x103.numero.Silos.rústicos>0),
  data=output.infra.df[output.infra.df$x20.codigo.de.producto ==targ.silo.crop, ]))





"Silos rústicos           "
















