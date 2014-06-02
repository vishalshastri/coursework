

# Need to include these stats too
prop.table(table(aggregate(x19.fertilizante.cantidad ~ folio,  data=inputs.df, FUN=function(x) any(x>0))$x19.fertilizante.cantidad))

prop.table(table(aggregate(x19.plagicidas.cantidad ~ folio,  data=inputs.df, FUN=function(x) any(x>0))$x19.plagicidas.cantidad))

prop.table(table(aggregate(x19.abono.cantidad ~ folio,  data=inputs.df, FUN=function(x) any(x>0))$x19.abono.cantidad))

prop.table(table(aggregate(x19.sem.comprada.cantidad ~ folio,  data=inputs.df, FUN=function(x) any(x>0))$x19.sem.comprada.cantidad))


possible.intercropping <- duplicated(inputs.df[, c("FOLIO", "S4_P19_1")] ) | duplicated(inputs.df[, c("FOLIO", "S4_P19_1")], fromLast=TRUE )

# inputs.df[ possible.intercropping, c( 1:4,7)]

# Not really much evidence of intercropping




library("stargazer")

# table(inputs.df$S4_P19_6)

fert.summary<-by(inputs.df, INDICES=inputs.df$x19.codigo, FUN=function(x) {
  pos.fert.per.land <- x$x19.abono.cantidad.kg[x$x19.abono.cantidad.kg>0] /
    x$x19.superficie.cultivada.hectareas[x$x19.abono.cantidad.kg>0]
  data.frame( as.character(x$x19.codigo[1]), 
     nrow(x), 
     100 * sum(x$x19.fertilizante.cantidad>0) / nrow(x),
     mean( pos.fert.per.land/1000 ),
     median(pos.fert.per.land/1000),
     sd(pos.fert.per.land/1000),
     max(pos.fert.per.land/1000),
     min(pos.fert.per.land/1000)
  )
  }
)
# So these stats are in metric tons per Hectare

fert.summary <- do.call(rbind, fert.summary)

fert.summary <- fert.summary[order(fert.summary$nrow.x. , decreasing=TRUE), ]


all.crops.addition<- by(inputs.df, INDICES=rep(1, nrow(inputs.df)), FUN=function(x) {
  pos.fert.per.land <- x$x19.abono.cantidad.kg[x$x19.abono.cantidad.kg>0] /
    x$x19.superficie.cultivada.hectareas[x$x19.abono.cantidad.kg>0]
  data.frame( as.character(x$x19.codigo[1]), 
     nrow(x), 
     100 * sum(x$x19.fertilizante.cantidad>0) / nrow(x),
     mean( pos.fert.per.land/1000 ),
     median(pos.fert.per.land/1000),
     sd(pos.fert.per.land/1000),
     max(pos.fert.per.land/1000),
     min(pos.fert.per.land/1000),
     stringsAsFactors=FALSE
  )
  }
)

all.crops.addition[[1]][1,1] <- "All crops"

fert.summary <- rbind(all.crops.addition[[1]], fert.summary)



colnames(fert.summary) <- c("Crop", "Plots", "Percent with fertilizer", 
"Mean", "Median", "Std. dev.", "Max", "Min")



#summary(inputs.df$x19.abono.cantidad.kg)
#summary(inputs.df$x19.superficie.cultivada.hectareas)






stargazer(fert.summary[1:10,] , summary=FALSE, out.header = FALSE, 
  out=paste0(work.dir, "tex tables/fertsummary.tex"), 
  rownames=FALSE,  column.separate=c(4,4), column.labels =c("test1", "test2"), float.env = "sidewaystable", font.size="small", title="Summary statistics of fertilizer",
  align=TRUE,
  digits=1
    )




plot(density(log(inputs.df$x19.fertilizante.bs.quintal[inputs.df$x19.fertilizante.bs.quintal>0])))

posi.fert <- inputs.df$x19.fertilizante.bs.quintal[inputs.df$x19.fertilizante.bs.quintal>0]

posi.fert <- posi.fert[posi.fert < quantile(posi.fert, probs=.95)]

plot(density(posi.fert))


plot(density(inputs.df$x19.fertilizante.bs.quintal[inputs.df$x19.fertilizante.bs.quintal>0]), 
xlim=quantile(inputs.df$x19.fertilizante.bs.quintal[inputs.df$x19.fertilizante.bs.quintal>0], probs=c(.00, .95))
)
