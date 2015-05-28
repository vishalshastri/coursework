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

output.df<- read.spss(paste0(work.dir, "bd68/2-AGRICOLA/Produccion (Preg-20)/3.-ENA08_BOLIVIA_DISTRIBUCION_PRODUCCION(preg_20).sav"), to.data.frame=TRUE)

# /Users/travismcarthur/Desktop/Metrics (637)/Final paper/bd68/7-INFRAESTRUCTURA Y MAQUINARIA/25.-ENA08_BOLIVIA_INFRAESTRUCTURA-MAQUINARIA(preg_103).sav

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


land.agg <- aggregate( x19.superficie.cultivada.hectareas ~ folio + x19.codigo, data=inputs.df, FUN=sum, na.rm=TRUE)
names(land.agg)[names(land.agg)=="x19.codigo"] <- "x20.codigo.de.producto"



nrow(output.df)
output.df <- merge(output.df, land.agg)
nrow(output.df)
# Hmm. one quarter of the data was dropped. This may be due to inconsistent coding of the crop names
# ok, now only ~3% of data is dropped after the coding fix



sold.perc.by <- by(output.df, INDICES=list(output.df$x20.codigo.de.producto), FUN=function(x) {
  x$x20.cuanto.vende.mn...cantidad.quintal[is.na(x$x20.cuanto.vende.mn...cantidad.quintal )] <- 0 
  x$x20.vende.fuera.pais...cantidad.quintal[is.na(x$x20.vende.fuera.pais...cantidad.quintal )] <- 0 
  mean(x$x20.cuanto.vende.mn...cantidad.quintal >0 | x$x20.vende.fuera.pais...cantidad.quintal>0)
})

sold.perc.by <- unclass(sold.perc.by)
sold.perc.df <- as.data.frame(sold.perc.by)
most.grown.tab <- sort(table(output.df$x20.codigo.de.producto), decreasing=TRUE)

sold.perc.df.save <- sold.perc.df

sold.perc.df <- sold.perc.df[match(names(most.grown.tab), rownames(sold.perc.df)), , drop=FALSE] * 100

rownames(sold.perc.df) <- gsub("(^ +)|( +$)", "", rownames(sold.perc.df))
colnames(sold.perc.df) <- "Percentage"

library("stargazer")

stargazer(sold.perc.df, digits=1, align=TRUE,
  out="/Users/travismcarthur/Desktop/Proposal course/Materials for 4-16 meeting with JP and Brad/market_sale.tex",
  title="Proportion of farmers growing each crop who sell that crop in the market",
  summary=FALSE)
  

fert.sold.ctable.p.starg <- gsub("multicolumn[{]1[}][{]c[}]", "multicolumn{1}{l}", fert.sold.ctable.p.starg)

cat(fert.sold.ctable.p.starg, file="/Users/travismcarthur/Desktop/Proposal course/Materials for 4-16 meeting with JP and Brad/market_sale_by_fert_use.tex", sep="\n")





crop.fert.agg<- aggregate(inputs.df$x19.fertilizante.cantidad.kg, 
  by=inputs.df[, c("folio", "x19.codigo")], 
  FUN=sum, na.rm=TRUE)
  
unique(crop.fert.agg$x19.codigo)[!as.character(unique(crop.fert.agg$x19.codigo)) %in% output.df$x20.codigo.de.producto]
# Seems ok, except for a few unusual crops
colnames(crop.fert.agg)[2:3] <- c("x20.codigo.de.producto",  "fert.kg")

output.df.aug <- merge(output.df, crop.fert.agg, )

output.df.aug$x20.cuanto.vende.mn...cantidad.quintal[is.na(output.df.aug$x20.cuanto.vende.mn...cantidad.quintal )] <- 0 
output.df.aug$x20.vende.fuera.pais...cantidad.quintal[is.na(output.df.aug$x20.vende.fuera.pais...cantidad.quintal )] <- 0 

output.df.aug$sold.any <- output.df.aug$x20.cuanto.vende.mn...cantidad.quintal >0 | output.df.aug$x20.vende.fuera.pais...cantidad.quintal>0

#round(prop.table(ftable(output.df.aug$x20.codigo.de.producto, output.df.aug$sold.any, output.df.aug$fert.kg>0), margin=3)*100)

library("catspec")

fert.sold.table <- table(output.df.aug$x20.codigo.de.producto, output.df.aug$sold.any, output.df.aug$fert.kg>0)

fert.sold.ctable <- ctab(fert.sold.table, type = "column", percentages = TRUE) 
# Thanks to http://r.789695.n4.nabble.com/prop-table-on-three-way-table-td798226.html



#fert.sold.ctable[, fert.sold.ctable$ctab=="TRUE", ]

fert.sold.ctable.p <- print(fert.sold.ctable)
fert.sold.ctable.p <- fert.sold.ctable.p[c(FALSE, TRUE), , drop=FALSE]
# This removes everyother row, which are the rows with the "FALSE" for having sold
fert.sold.ctable.p <- as.data.frame(fert.sold.ctable.p)
rownames(fert.sold.ctable.p ) <- attr(fert.sold.ctable$table, "dimnames")[[1]]
colnames(fert.sold.ctable.p) <- c("Did not use fert", "Did use")

fert.sold.ctable.p <- cbind(fert.sold.ctable.p, sold.perc.df.save*100)
colnames(fert.sold.ctable.p)[3] <- "Overall proportion who sold"

fert.sold.ctable.p <- fert.sold.ctable.p[match(names(most.grown.tab), rownames(fert.sold.ctable.p)), , drop=FALSE] 
rownames(fert.sold.ctable.p) <- gsub("(^ +)|( +$)", "", rownames(fert.sold.ctable.p))


fert.sold.ctable.p.starg <- stargazer(fert.sold.ctable.p, digits=1, align=TRUE,
  title="Proportion of farmers growing each crop who sell that crop in the market, according to whether use fert",
  summary=FALSE)
  

fert.sold.ctable.p.starg <- gsub("multicolumn[{]1[}][{]c[}]", "multicolumn{1}{l}", fert.sold.ctable.p.starg)

cat(fert.sold.ctable.p.starg, file="/Users/travismcarthur/Desktop/Proposal course/Materials for 4-16 meeting with JP and Brad/market_sale_by_fert_use.tex", sep="\n")



as.data.frame(print(fert.sold.ctable))



Maiz combined                                              FALSE   1903  137
                                                           TRUE    1061  112


Papa (patatas)                                             FALSE   2178  425
                                                           TRUE     860  582



### BELOW IS NOT COMPATIBLE WITH ABOVE


inputs.fixed.df <- inputs.df[ , c("folio", "soil.quality", "elevation" , "mean.ann.rain.5yr",  
  "provincia.full", "seccion.full", "sector.full", "segmento.full", "departamento" )]
  


output.df <- merge(output.df, inputs.fixed.df[!duplicated(inputs.fixed.df), ])


#top.crops <- names(sort(table(inputs.df$x19.codigo), decreasing=TRUE))[1:10]
#sort(table(inputs.df$x19.codigo), decreasing=TRUE)[1:10]


#output.df.spec.crop <- output.df[output.df$x20.codigo.de.producto =="Maiz combined", ]
output.df.spec.crop <- output.df[output.df$x20.codigo.de.producto =="Papa (patatas) ", ]
#output.df.spec.crop <- output.df[output.df$x20.codigo.de.producto =="Trigo          ", ]
# "Haba (verde)    " "Papa (patatas) " "Trigo          "


summary(lm(log(x20.produccion.cantidad.quintal) ~ log(x20.bs.unidad.quintal.mn) + soil.quality + elevation + mean.ann.rain.5yr + log(x19.superficie.cultivada.hectareas), 
  data=output.df.spec.crop[is.finite(log(output.df.spec.crop$x20.bs.unidad.quintal.mn) + log(output.df.spec.crop$x20.produccion.cantidad.quintal)), ] ))
# So this is actually the sample of farmers who end up selling
#  + seccion.full


summary(lm(log(x20.produccion.cantidad.quintal) ~ log(x20.bs.unidad.quintal.mn) + log(soil.quality) + log(elevation) + log(mean.ann.rain.5yr) , 
  data=output.df.spec.crop[is.finite(log(output.df.spec.crop$x20.bs.unidad.quintal.mn) + log(output.df.spec.crop$x20.produccion.cantidad.quintal)), ] ))
# So this is actually the sample of farmers who end up selling
# not a big difference taking logs





# + x20.donde.vende
summary(lm(log(I(x20.produccion.cantidad.quintal/x19.superficie.cultivada.hectareas)) ~ log(x20.bs.unidad.quintal.mn) , 
  data=output.df.spec.crop[is.finite(log(output.df.spec.crop$x20.bs.unidad.quintal.mn) + log(output.df.spec.crop$x20.produccion.cantidad.quintal)), ] ))
# So this is actually the sample of farmers who end up selling
# This is yield
# *zona.agroproductiva

summary(lm(log(x20.produccion.cantidad.quintal) ~ log(x20.bs.unidad.quintal.mn) + log(x19.superficie.cultivada.hectareas) , 
  data=output.df.spec.crop[is.finite(log(output.df.spec.crop$x20.bs.unidad.quintal.mn) + log(output.df.spec.crop$x20.produccion.cantidad.quintal)), ] ))
# So this is actually the sample of farmers who end up selling
# *zona.agroproductiva


library(ggplot2)
# http://docs.ggplot2.org/0.9.3.1/stat_smooth.html
# Default is 95% C.I.

#output.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Descriptive plots/"

# temp.plot <-
 ggplot(output.df.spec.crop[is.finite(log(output.df.spec.crop$x20.bs.unidad.quintal.mn) + log(output.df.spec.crop$x20.produccion.cantidad.quintal)), ] , 
    aes(log(x20.bs.unidad.quintal.mn), log(x20.produccion.cantidad.quintal))) +
  stat_smooth() + geom_point() +
  ggtitle("<>") +
  theme( axis.title.y = element_blank())
#  A bit of a hack to get only posi, nonmissing values in there




# Ok, try to do this with the original units:


summary(lm(log(x20.produccion.cantidad ) ~ log(x20.cuanto.vende.mn...bs ) + zona.agroproductiva , 
  data=output.df.spec.crop[is.finite(log(output.df.spec.crop$x20.cuanto.vende.mn...bs ) + log(output.df.spec.crop$x20.produccion.cantidad )), ] ))
# Ok, ignore this result. The two regressions below are OK

summary(lm(x20.produccion.cantidad.quintal ~ x20.produccion.unidad:x20.produccion.cantidad , 
  data=output.df.spec.crop ))

summary(lm(x20.bs.unidad.quintal.mn ~ x20.produccion.unidad:x20.cuanto.vende.mn...bs , 
  data=output.df.spec.crop ))










summary(lm(x19.fertilizante.bs.kg ~ x19.fertilizante.bs.unid:x19.fertilizante.unidad,  subset=x19.fertilizante.bs.unid >0, data=inputs.df))


summary(lm(x19.fertilizante.bs.kg ~ x19.fertilizante.bs.unid, subset= x19.fertilizante.unidad=="QUINTAL (qq)            ",  data=inputs.df))

with(inputs.df[inputs.df$x19.fertilizante.unidad=="QUINTAL (qq)            " & inputs.df$x19.fertilizante.bs.unid<.1,], 
  plot(x19.fertilizante.bs.kg, x19.fertilizante.bs.unid))


inputs.df[inputs.df$x19.fertilizante.unidad=="QUINTAL (qq)            " & inputs.df$x19.fertilizante.bs.unid<.1, c("x19.fertilizante.cantidad.kg", "x19.fertilizante.bs.kg.impute.level", "x19.fertilizante.bs.unid", "x19.fertilizante.bs.kg")]



summary(lm(x19.fertilizante.bs.quintal ~ x19.fertilizante.bs.unid, subset= x19.fertilizante.unidad=="QUINTAL (qq)            ",  data=inputs.df))






unique(as.character(inputs.df$x19.fertilizante.unidad))














x20.produccion.cantidad


expand.grid(data.frame(a=0:1, b=0:1, c=0:1, d=0:1, e=0:1, f=0:1)[2:1, ])
sum(as.matrix(expand.grid(data.frame(a=0:1, b=0:1, c=0:1, d=0:1, e=0:1, f=0:1)[2:1, ])))

choose(6,1) + choose(6,2) + choose(6,3) + choose(6,4) + choose(6,5) + choose(6,6) 



choose(6,1)*1 + choose(6,2)*2 + choose(6,3)*3 + choose(6,4)*4 + choose(6,5)*5 + choose(6,6)*6

# Seems I don't estimate the no-input-use case






