
library("foreign")

work.dir <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/"

peru.df<- read.spss("/Users/travismcarthur/Downloads/337-Modulo229/01_IVCENAGRO_REC01.sav", to.data.frame=TRUE)

# Consult http://iinei.inei.gob.pe/microdatos/Consulta_por_Encuesta.asp   to know which module
# to load to examine each section of the questionaiire

# /Users/travismcarthur/Desktop/Metrics (637)/Final paper/bd68/7-INFRAESTRUCTURA Y MAQUINARIA/25.-ENA08_BOLIVIA_INFRAESTRUCTURA-MAQUINARIA(preg_103).sav

colnames(peru.df) <- tolower( make.names(gsub("[()]|[.]", "", attr(peru.df, "variable.labels")) ) )





colnames(output.df) <- make.names(colnames(output.df), unique = TRUE)
colnames(output.df)[colnames(output.df)=="x20.bs.unidad.quintal"] <- "x20.bs.unidad.quintal.mn"
# "mn" is mercado nacional
colnames(output.df)[colnames(output.df)=="x20.bs.unidad.quintal.1"] <- "x20.bs.unidad.quintal.fuera.pais"



http://iinei.inei.gob.pe/iinei/srienaho/descarga/SPSS/355-Modulo229.zip
http://iinei.inei.gob.pe/iinei/srienaho/descarga/DBF/355-Modulo229.zip


http://iinei.inei.gob.pe/iinei/srienaho/descarga/SPSS/337-Modulo229.zip
http://iinei.inei.gob.pe/iinei/srienaho/descarga/SPSS/361-Modulo229.zip

239

dump.dir <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Peru Ag Census/"

for (target.format in c("SPSS", "DBF")) {
  for ( target.departamento in 337:361) {  
    for ( target.module in 229:239) {    
      try.output <- NA
      while(is.na(try.output)) {
        try.output<- tryCatch( download.file(url=paste0("http://iinei.inei.gob.pe/iinei/srienaho/descarga/", 
            target.format, "/", target.departamento, "-Modulo", target.module, ".zip"),
          destfile=paste0(dump.dir, target.format, "/", target.departamento, "-Modulo", target.module, ".zip")),
          error=function(e) {NA}
        )
        Sys.sleep(1)
      }  
    }
  }
}


for (target.format in c("SPSS", "DBF")) {
  for ( target.departamento in 337:361) {  
    for ( target.module in 229:239) {    
        unzip(zipfile=paste0(dump.dir, target.format, "/", target.departamento, "-Modulo", target.module, ".zip"),
          exdir=paste0(dump.dir, target.format, "/"))  
    }
  }
}


library("foreign")
peru.fert.ls <- list()
# /Users/travismcarthur/Desktop/Metrics (637)/Final paper/Peru Ag Census/SPSS/337-Modulo235/07_IVCENAGRO_REC04.sav

for ( target.departamento in 337:361) {  
      peru.fert.ls[[ target.departamento ]]<- read.spss(
      paste0("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Peru Ag Census/SPSS/",
        target.departamento, "-Modulo235/07_IVCENAGRO_REC04.sav"), to.data.frame=TRUE)
     colnames(peru.fert.ls[[ target.departamento ]]) <- 
       tolower( make.names(gsub("[()]|[.]", "", attr(peru.fert.ls[[ target.departamento ]], "variable.labels")) ) )
}


peru.fert.df <- do.call(rbind, peru.fert.ls)
rm(peru.fert.ls)
gc()

prop.table(table(peru.fert.df$x.aplica.fertilizantes.químicos))


peru.credit.ls <- list()
# /Users/travismcarthur/Desktop/Metrics (637)/Final paper/Peru Ag Census/SPSS/337-Modulo235/07_IVCENAGRO_REC04.sav

for ( target.departamento in 337:361) {  
      peru.credit.ls[[ target.departamento ]]<- read.spss(
      paste0("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Peru Ag Census/SPSS/",
        target.departamento, "-Modulo237/09_IVCENAGRO_REC04B.sav"), to.data.frame=TRUE)
     colnames(peru.credit.ls[[ target.departamento ]]) <- 
       tolower( make.names(gsub("[()]|[.]", "", attr(peru.credit.ls[[ target.departamento ]], "variable.labels")) ) )
}

peru.credit.df <- do.call(rbind, peru.credit.ls)
rm(peru.credit.ls)
gc()



peru.geog.ls <- list()
# /Users/travismcarthur/Desktop/Metrics (637)/Final paper/Peru Ag Census/SPSS/337-Modulo235/07_IVCENAGRO_REC04.sav

for ( target.departamento in 337:361) {  
      peru.geog.ls[[ target.departamento ]]<- read.spss(
      paste0("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Peru Ag Census/SPSS/",
        target.departamento, "-Modulo229/01_IVCENAGRO_REC01.sav"), to.data.frame=TRUE)
     colnames(peru.geog.ls[[ target.departamento ]]) <- 
       tolower( make.names(gsub("[()]|[.]", "", attr(peru.geog.ls[[ target.departamento ]], "variable.labels")) ) )
}

peru.geog.df <- do.call(rbind, peru.geog.ls)
rm(peru.geog.ls)
gc()



peru.transit.crop.ls <- list()
# /Users/travismcarthur/Desktop/Metrics (637)/Final paper/Peru Ag Census/SPSS/337-Modulo235/07_IVCENAGRO_REC04.sav

for ( target.departamento in 337:361) {  
      peru.transit.crop.ls[[ target.departamento ]]<- read.spss(
      paste0("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Peru Ag Census/SPSS/",
        target.departamento, "-Modulo234/06_IVCENAGRO_REC03.sav"), to.data.frame=TRUE)
     colnames(peru.transit.crop.ls[[ target.departamento ]]) <- 
       tolower( make.names(gsub("[()]|[.]", "", attr(peru.transit.crop.ls[[ target.departamento ]], "variable.labels")) ) )
}

peru.transit.crop.df <- do.call(rbind, peru.transit.crop.ls)
rm(peru.geog.ls)
gc()


crop.codebook <- read.delim("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Peru Ag Census/peru-crop-codes.tab")
# Copied from "cultivo permanente" part of http://webinei.inei.gob.pe:8080/sisconcode/publico.htm#  after much searching
crop.codebook <- crop.codebook[-1, c(2, 4)]

peru.transit.crop.df$crop.type <- droplevels(factor(peru.transit.crop.df$cultivo.transitorio..código,
levels = crop.codebook$CÓDIGO,
labels = crop.codebook$NOMBRE.DE.CULTIVO.PERMANENTE) )
# Thanks to http://www.statmethods.net/input/valuelabels.html




peru.transit.crop.df.no.dups <- peru.transit.crop.df[!duplicated(peru.transit.crop.df[ , c("número.de.cédula.principal", "cultivo.transitorio..código") ]), ]

peru.crop.fert.df <- merge(peru.transit.crop.df.no.dups[, c("número.de.cédula.principal", "crop.type")], 
  peru.fert.df[, c("número.de.cédula.principal", "x.aplica.fertilizantes.químicos.")])

top.crops.peru <- names(sort(table(peru.transit.crop.df$crop.type), decreasing=TRUE))[1:10]

extensive.margin.by.crop.mat <- as.matrix(with(peru.crop.fert.df[peru.crop.fert.df$crop.type %in% top.crops.peru, ],
  prop.table(table(factor(crop.type), x.aplica.fertilizantes.químicos.), margin=1)) * 100
)

library("stargazer")
library("xtable")

xtab.output <- print(xtable(extensive.margin.by.crop.mat,
  caption="Percentage of farmers using fertilizer by type of crop planted in Peru", digits=1),
  caption.placement = "top")
  
cat(xtab.output, sep="n",
      file=paste0("/Users/travismcarthur/Desktop/Proposal course/Materials for 4-1 meeting with Brad/peru-fert-use-by-crop.tex")
      )






intersect(names(peru.geog.df), names(peru.fert.df))

peru.merged.df <- merge(peru.fert.df, peru.geog.df[, -(1:6)])

region.fert.use.mat <- as.matrix(prop.table(table(peru.merged.df$código.de.la.región.natural,
  peru.merged.df$x.aplica.fertilizantes.químicos ), margin=1)) * 100

library("stargazer")
library("xtable")
xtab.output <- print(xtable(region.fert.use.mat,
  caption="Percentage of farmers using fertilizer by Peruvian region", digits=1),
  caption.placement = "top")
  
cat(xtab.output, sep="n",
      file=paste0("/Users/travismcarthur/Desktop/Proposal course/Materials for 4-1 meeting with Brad/peru-fert-use-by-region.tex")
      )

overall.fert.use.mat <- t(as.matrix(prop.table(table(
  peru.merged.df$x.aplica.fertilizantes.químicos ), margin=NULL)) * 100)

xtab.output <- print(xtable(overall.fert.use.mat,
  caption="Percentage of farmers using fertilizer in Peru, overall", digits=1),
  caption.placement = "top")
  
cat(xtab.output, sep="n",
      file=paste0("/Users/travismcarthur/Desktop/Proposal course/Materials for 4-1 meeting with Brad/peru-fert-overall.tex")
      )








natural.persons <- peru.geog.df$número.de.cédula.principal[peru.geog.df$condición.jurídica %in% "Persona natural"] 

peru.transit.crop.df.no.dups <- peru.transit.crop.df[!duplicated(peru.transit.crop.df[ , c("número.de.cédula.principal", "cultivo.transitorio..código") ]), ]

sort(table(peru.transit.crop.df$cultivo.transitorio..código[peru.transit.crop.df$número.de.cédula.principal %in% natural.persons]))

test.dbf <- read.dbf("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Peru Ag Census/DBF/337-Modulo234/06_IVCENAGRO_REC03.dbf")


test.spss <- read.spss("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Peru Ag Census/SPSS/337-Modulo231/03_IVCENAGRO_REC02.sav", to.data.frame=TRUE)











Extensive margin for 





prop.table(table(peru.credit.df$x.obtuvo.el..préstamo.o.crédito.que.gestionó., useNA="ifany"))

peru.merged.df <- merge(peru.fert.df, peru.credit.df[, -(1:6)])

peru.merged.df$x.obtuvo.el..préstamo.o.crédito.que.gestionó. <- as.character(peru.merged.df$x.obtuvo.el..préstamo.o.crédito.que.gestionó.)
peru.merged.df$x.obtuvo.el..préstamo.o.crédito.que.gestionó.[is.na(peru.merged.df$x.obtuvo.el..préstamo.o.crédito.que.gestionó.)] <- "did.not.ask.for.credit"

prop.table(table(peru.merged.df$x.aplica.fertilizantes.químicos))

summary(lm(I(peru.merged.df$x.aplica.fertilizantes.químicos!="No aplica") ~ x.obtuvo.el..préstamo.o.crédito.que.gestionó., data=peru.merged.df))


peru.merged.df$x.cuál.es.la.razón.principal.por.la.que.no.solicitó.el.crédito. <- as.character(peru.merged.df$x.cuál.es.la.razón.principal.por.la.que.no.solicitó.el.crédito.)

peru.merged.df$x.cuál.es.la.razón.principal.por.la.que.no.solicitó.el.crédito.[is.na(peru.merged.df$x.cuál.es.la.razón.principal.por.la.que.no.solicitó.el.crédito.)] <- "asked.for.credit"


summary(lm(I(peru.merged.df$x.aplica.fertilizantes.químicos!="No aplica") ~ x.cuál.es.la.razón.principal.por.la.que.no.solicitó.el.crédito., data=peru.merged.df))


prop.table(table(peru.merged.df$x.cuál.es.la.razón.principal.por.la.que.no.solicitó.el.crédito.))

peru.merged.df <- within(peru.merged.df, {
  para.que.pidió.el.crédito..1.adquisición.de.insumos.para.la.producción  <- 
    ifelse(is.na(para.que.pidió.el.crédito..1.adquisición.de.insumos.para.la.producción ), 0, 1)       
  para.que.pidió.el.crédito..2.compra.de.maquinaria.pesada...equipo  <- 
    ifelse(is.na(para.que.pidió.el.crédito..2.compra.de.maquinaria.pesada...equipo ), 0, 1)     
  para.que.pidió.el.crédito..3.compra.de.herramientas  <- 
    ifelse(is.na(para.que.pidió.el.crédito..3.compra.de.herramientas ), 0, 1)     
  para.que.pidió.el.crédito..4.para.la.comercialización.de.sus.productos  <- 
    ifelse(is.na(para.que.pidió.el.crédito..4.para.la.comercialización.de.sus.productos ), 0, 1)     
  para.que.pidió.el.crédito..5.otro.motivo  <- 
    ifelse(is.na(para.que.pidió.el.crédito..5.otro.motivo ), 0, 1)         
})

summary(lm(I(peru.merged.df$x.aplica.fertilizantes.químicos!="No aplica") ~   
  (para.que.pidió.el.crédito..1.adquisición.de.insumos.para.la.producción +
  para.que.pidió.el.crédito..2.compra.de.maquinaria.pesada...equipo +
  para.que.pidió.el.crédito..3.compra.de.herramientas +
  para.que.pidió.el.crédito..4.para.la.comercialización.de.sus.productos +
  para.que.pidió.el.crédito..5.otro.motivo), data=peru.merged.df,
  subset=x.obtuvo.el..préstamo.o.crédito.que.gestionó.=="Si"))

mean(peru.merged.df$para.que.pidió.el.crédito..1.adquisición.de.insumos.para.la.producción)
mean(peru.merged.df$para.que.pidió.el.crédito..2.compra.de.maquinaria.pesada...equipo)
mean(peru.merged.df$para.que.pidió.el.crédito..3.compra.de.herramientas )
mean(peru.merged.df$para.que.pidió.el.crédito..4.para.la.comercialización.de.sus.productos)
mean(peru.merged.df$para.que.pidió.el.crédito..5.otro.motivo)

prop.table(table(peru.merged.df$x.aplica.fertilizantes.químicos, peru.merged.df$x.obtuvo.el..préstamo.o.crédito.que.gestionó.))

                            
 [83] "para.que.pidió.el.crédito..2.compra.de.maquinaria.pesada...equipo"                                                
 [84] "para.que.pidió.el.crédito..3.compra.de.herramientas"                                                              
 [85] "para.que.pidió.el.crédito..4.para.la.comercialización.de.sus.productos"                                           
 [86] "para.que.pidió.el.crédito..5.otro.motivo"                                 





  [1] "tipo.registro"                                                                                                    
  [2] "departamento"                                                                                                     
  [3] "provincia"                                                                                                        
  [4] "distrito"                                                                                                         
  [5] "nro.sea"                                                                                                          
  [6] "unidad.agropecuaria.ua"                                                                                           
  [7] "número.de.cédula.principal" 








library("foreign")
peru.fert.df<- read.spss("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Peru Ag Census/SPSS/337-Modulo235/07_IVCENAGRO_REC04.sav", to.data.frame=TRUE)

colnames(peru.fert.df) <- tolower( make.names(gsub("[()]|[.]", "", attr(peru.fert.df, "variable.labels")) ) )






