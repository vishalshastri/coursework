

w01 = firm.df$x19.fertilizante.bs.kg
w02 = firm.df$x19.sem.comprada.bs.kg
w03 = firm.df$x19.abono.bs.kg
w04 = firm.df$x19.plagicidas.bs.kg
# w05 = firm.df$imputed.ag.wage


x01 = firm.df$x19.fertilizante.cantidad.kg
x02 = firm.df$x19.sem.comprada.cantidad.kg
x03 = firm.df$x19.abono.cantidad.kg
x04 = firm.df$x19.plagicidas.cantidad.kg
# x05 = firm.df$labor.hours * firm.df$crop.coverage

x01 <- unname(x01)
x02 <- unname(x02)
x03 <- unname(x03)
x04 <- unname(x04)
# x05 <- unname(x05)

# crops.to.include comes from the setbuilding.r script

lead.zero <- function(x) {formatC(x, width = 2, flag = "0")}

# Ok, the below is an easier way to do this 
#firm.df <- firm.df[, sapply(firm.df,  FUN=function(x) { ! (is.character(x) | is.factor(x))  } ) ]
#firm.df <- firm.df[, apply(firm.df, 2, FUN=function(x) sum(x)>0) ]
# Below takes out one-crop stragglers
#firm.df <- firm.df[, apply(firm.df, 2, FUN=function(x) sum(x>0)-1>0) ]

# TODO: why don't we have price.r, as in revised price??? switching it to just "price" now.

#for (i in 1:sum(grepl("harvest.r", colnames(firm.df)))) {

#  targ.column <- firm.df[, grepl("price", colnames(firm.df)), drop=FALSE][, i ]
#  assign(paste0("p", lead.zero(i)), targ.column)
  
#  targ.column <- firm.df[, grepl("harvest.r", colnames(firm.df)), drop=FALSE][, i ]
#  assign(paste0("y", lead.zero(i)), targ.column)
  
#}

# TODO: NOTE: changing this below to log for now

y01 <- log( firm.df$x19.produccion.obtenidad.kg )

# quick fix below

#w02[is.na(w02)] <- 0
#w03[is.na(w03)] <- 0


#w01[w01==0] <- mean(w01[w01!=0]) + mean(w01[w01!=0])* rnorm(length(w01[w01==0]), mean = 0, sd = .1)
#w02[w02==0] <- mean(w02[w02!=0]) + mean(w02[w02!=0])* rnorm(length(w02[w02==0]), mean = 0, sd = .1)
#w03[w03==0] <- mean(w03[w03!=0]) + mean(w03[w03!=0])* rnorm(length(w03[w03==0]), mean = 0, sd = .1)
#w04[w04==0] <- mean(w04[w04!=0]) + mean(w04[w04!=0])* rnorm(length(w04[w04==0]), mean = 0, sd = .1)
# w05[w05==0] <- mean(w05[w05!=0]) + mean(w05[w05!=0])* rnorm(length(w05[w05==0]), mean = 0, sd = .1)

# Fix for translation of prices - and now we're incorporating it into the standard run:
# w03[w03==min(w03)] <- min(w03[w03!=min(w03)])

min(w01)
min(w02)
min(w03)
min(w04)

#> min(w01)
#[1] 70.3731
#> min(w02)
#[1] 45.90055
#> min(w03)
#[1] 7.268086
#> min(w04)
#[1] 33.1459

#try( p01[p01==0] <- mean(p01[p01!=0]) + mean(p01[p01!=0])* rnorm(length(p01[p01==0]), mean = 0, sd = .1) )
#try( p02[p02==0] <- mean(p02[p02!=0]) + mean(p02[p02!=0])* rnorm(length(p02[p02==0]), mean = 0, sd = .1) )
#try( p03[p03==0] <- mean(p03[p03!=0]) + mean(p03[p03!=0])* rnorm(length(p03[p03==0]), mean = 0, sd = .1) )
#try( p04[p04==0] <- mean(p04[p04!=0]) + mean(p04[p04!=0])* rnorm(length(p04[p04==0]), mean = 0, sd = .1) )
#try( p05[p05==0] <- mean(p05[p05!=0]) + mean(p05[p05!=0])* rnorm(length(p05[p05==0]), mean = 0, sd = .1) )
#try( p06[p06==0] <- mean(p06[p06!=0]) + mean(p06[p06!=0])* rnorm(length(p06[p06==0]), mean = 0, sd = .1) )
#try( p07[p07==0] <- mean(p07[p07!=0]) + mean(p07[p07!=0])* rnorm(length(p07[p07==0]), mean = 0, sd = .1) )
#try( p08[p08==0] <- mean(p08[p08!=0]) + mean(p08[p08!=0])* rnorm(length(p08[p08==0]), mean = 0, sd = .1) )
#try( p09[p09==0] <- mean(p09[p09!=0]) + mean(p09[p09!=0])* rnorm(length(p09[p09==0]), mean = 0, sd = .1) )
#try( p10[p10==0] <- mean(p10[p10!=0]) + mean(p10[p10!=0])* rnorm(length(p10[p10==0]), mean = 0, sd = .1) )
#try( p11[p11==0] <- mean(p11[p11!=0]) + mean(p11[p11!=0])* rnorm(length(p11[p11==0]), mean = 0, sd = .1) )
#try( p12[p12==0] <- mean(p12[p12!=0]) + mean(p12[p12!=0])* rnorm(length(p12[p12==0]), mean = 0, sd = .1) )
#try( p13[p13==0] <- mean(p13[p13!=0]) + mean(p13[p13!=0])* rnorm(length(p13[p13==0]), mean = 0, sd = .1) )
#try( p14[p14==0] <- mean(p14[p14!=0]) + mean(p14[p14!=0])* rnorm(length(p14[p14==0]), mean = 0, sd = .1) )
#try( p15[p15==0] <- mean(p15[p15!=0]) + mean(p15[p15!=0])* rnorm(length(p15[p15==0]), mean = 0, sd = .1) )


q01 = firm.df$x19.superficie.cultivada.hectareas
# q01[q01 ==0] = median(q01)

q02 = ifelse(firm.df$x19.uso.riego!="Si",  1, exp(1))

#q02 = ifelse(firm.df$x19.uso.riego!="Si",  exp(1), exp(2))

# q02[q02 == 0] = .5
# TODO: two observations have zero land input - this is for a subset, so probably more

industrialization.index <- rep(0, nrow(firm.df))
indust.columns <- c("x19.prepara.el.suelo", "x19.siembra.planta", "x19.labores.culturales", "x19.cosecha")

for ( i in indust.columns) {
  firm.df[is.na(firm.df[, i]), i] <- "Fuerza humana"

  industrialization.index <- industrialization.index + 
    ifelse(firm.df[, i]=="Maq. Agricola", 3, 0)
  industrialization.index <- industrialization.index + 
    ifelse(firm.df[, i]=="Tracc. Animal", 2, 0)
  industrialization.index <- industrialization.index + 
    ifelse(firm.df[, i]=="Fuerza humana", 1, 0)
}

# Fixing the NA's

industrialization.index[industrialization.index==0] <- 1

q03 = industrialization.index



 ln.E.data <- log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + 1  )
#ln.E.data <- log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + w05*x05 + 1 )

ln.E.data <- unname(ln.E.data )
 
 # TODO: why are there some duplicated row names here (above)?
 
 