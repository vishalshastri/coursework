
library("foreign")

work.dir <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/"

peru.df<- read.spss("/Users/travismcarthur/Downloads/337-Modulo229/01_IVCENAGRO_REC01.sav", to.data.frame=TRUE)

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






