

# TODO FIX: Ok, we should have had this way earlier in the process, but we'll put it herre for now:


firm.df<-data.frame(lapply(firm.df, FUN=function(x) {
	x[!is.finite(x)]<-0 
	x
	}))




# firm.df<-firm.df[apply(firm.df, 1, FUN=function(x) !any(is.na(x))), ]

#firm.df<-firm.df[firm.df$harvest.r.ARROZ!=0 | firm.df$harvest.r.MAIZ!=0 |  firm.df$harvest.r.PLATANO!=0 |  firm.df$harvest.r.YUCA!=0 |   firm.df$harvest.r.ARVEJA!=0 |   firm.df$harvest.r.CEBADA!=0 |   firm.df$harvest.r.CEBOLLA!=0 |   firm.df$harvest.r.HABA!=0 |   firm.df$harvest.r.OCA!=0 |   firm.df$harvest.r.PAPA!=0 |   firm.df$harvest.r.QUINUA!=0 |   firm.df$harvest.r.TRIGO!=0,  ]


#firm.df$revenue <- rowSums( 
#  as.matrix(firm.df[, grepl("^price", colnames(firm.df))]) *
#  as.matrix(firm.df[, grepl("^harvest", colnames(firm.df))])
#  )
  
# trim.set <- quantile(firm.df$revenue, probs = c(.1, .9))
# do this once

# Going to keep in all the obs. -- Actually, we get a much better fit from this
#  firm.df<-firm.df[firm.df$revenue >= trim.set[1] & firm.df$revenue <= trim.set[2], ]

firm.df<-firm.df[!(firm.df$harvest.r.PAPA!=0 & firm.df$harvest.r.PLATANO!=0), ]
# TODO: killing platano x papa mix for now

# TODO: need to fix

#firm.df<-firm.df[-which.max(profit),]

#firm.df<-firm.df[!is.na(profit) & profit!=0 & firm.df$land.area>0,]

#w1=w1, w2=w2, w3=w3, w4=w4, w5=w5, x1=x1, x2=x2, x3=x3, x4=x4, x5=x5, 
#p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6, p7=p7, p8=p8, p9=p9, p10=p10, 
#p11=p11, p12=p12, y1=y1, y2=y2, y3=y3, y4=y4, y5=y5, y6=y6, y7=y7, y8=y8,
#y9=y9, y10=y10, y11=y11, y12=y12, profit=profit




w01 = firm.df$fert.price.quintal
w02 = firm.df$seed.price
w03 = firm.df$abono.price
w04 = firm.df$plaguicida.price.liter
w05 = firm.df$imputed.ag.wage

# Below is now the "special" cutting out costs based on percentage of miscellaneous crops

x01 = firm.df$fert.quintals * firm.df$crop.coverage
x02 = firm.df$seed.quintals * firm.df$crop.coverage
x03 = firm.df$abono.quintals * firm.df$crop.coverage
x04 = firm.df$plaguicida.liters * firm.df$crop.coverage
x05 = firm.df$labor.hours * firm.df$crop.coverage

# crops.to.include comes from the setbuilding.r script

lead.zero <- function(x) {formatC(x, width = 2, flag = "0")}

# Ok, the below is an easier way to do this 
firm.df <- firm.df[, sapply(firm.df,  FUN=function(x) { ! (is.character(x) | is.factor(x))  } ) ]
firm.df <- firm.df[, apply(firm.df, 2, FUN=function(x) sum(x)>0) ]
# Below takes out one-crop stragglers
firm.df <- firm.df[, apply(firm.df, 2, FUN=function(x) sum(x>0)-1>0) ]

# TODO: why don't we have price.r, as in revised price??? switching it to just "price" now.

for (i in 1:sum(grepl("harvest.r", colnames(firm.df)))) {

  targ.column <- firm.df[, grepl("price", colnames(firm.df)), drop=FALSE][, i ]
  assign(paste0("p", lead.zero(i)), targ.column)
  
  targ.column <- firm.df[, grepl("harvest.r", colnames(firm.df)), drop=FALSE][, i ]
  assign(paste0("y", lead.zero(i)), targ.column)
  
}


w01[w01==0] <- mean(w01[w01!=0]) + mean(w01[w01!=0])* rnorm(length(w01[w01==0]), mean = 0, sd = .1)
w02[w02==0] <- mean(w02[w02!=0]) + mean(w02[w02!=0])* rnorm(length(w02[w02==0]), mean = 0, sd = .1)
w03[w03==0] <- mean(w03[w03!=0]) + mean(w03[w03!=0])* rnorm(length(w03[w03==0]), mean = 0, sd = .1)
w04[w04==0] <- mean(w04[w04!=0]) + mean(w04[w04!=0])* rnorm(length(w04[w04==0]), mean = 0, sd = .1)
w05[w05==0] <- mean(w05[w05!=0]) + mean(w05[w05!=0])* rnorm(length(w05[w05==0]), mean = 0, sd = .1)

# Fix for translation of prices - and now we're incorporating it into the standard run:
w03[w03==min(w03)] <- min(w03[w03!=min(w03)])

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

try( p01[p01==0] <- mean(p01[p01!=0]) + mean(p01[p01!=0])* rnorm(length(p01[p01==0]), mean = 0, sd = .1) )
try( p02[p02==0] <- mean(p02[p02!=0]) + mean(p02[p02!=0])* rnorm(length(p02[p02==0]), mean = 0, sd = .1) )
try( p03[p03==0] <- mean(p03[p03!=0]) + mean(p03[p03!=0])* rnorm(length(p03[p03==0]), mean = 0, sd = .1) )
try( p04[p04==0] <- mean(p04[p04!=0]) + mean(p04[p04!=0])* rnorm(length(p04[p04==0]), mean = 0, sd = .1) )
try( p05[p05==0] <- mean(p05[p05!=0]) + mean(p05[p05!=0])* rnorm(length(p05[p05==0]), mean = 0, sd = .1) )
try( p06[p06==0] <- mean(p06[p06!=0]) + mean(p06[p06!=0])* rnorm(length(p06[p06==0]), mean = 0, sd = .1) )
try( p07[p07==0] <- mean(p07[p07!=0]) + mean(p07[p07!=0])* rnorm(length(p07[p07==0]), mean = 0, sd = .1) )
try( p08[p08==0] <- mean(p08[p08!=0]) + mean(p08[p08!=0])* rnorm(length(p08[p08==0]), mean = 0, sd = .1) )
try( p09[p09==0] <- mean(p09[p09!=0]) + mean(p09[p09!=0])* rnorm(length(p09[p09==0]), mean = 0, sd = .1) )
try( p10[p10==0] <- mean(p10[p10!=0]) + mean(p10[p10!=0])* rnorm(length(p10[p10==0]), mean = 0, sd = .1) )
try( p11[p11==0] <- mean(p11[p11!=0]) + mean(p11[p11!=0])* rnorm(length(p11[p11==0]), mean = 0, sd = .1) )
try( p12[p12==0] <- mean(p12[p12!=0]) + mean(p12[p12!=0])* rnorm(length(p12[p12==0]), mean = 0, sd = .1) )
try( p13[p13==0] <- mean(p13[p13!=0]) + mean(p13[p13!=0])* rnorm(length(p13[p13==0]), mean = 0, sd = .1) )
try( p14[p14==0] <- mean(p14[p14!=0]) + mean(p14[p14!=0])* rnorm(length(p14[p14==0]), mean = 0, sd = .1) )
try( p15[p15==0] <- mean(p15[p15!=0]) + mean(p15[p15!=0])* rnorm(length(p15[p15==0]), mean = 0, sd = .1) )


q01 = firm.df$land.area
q01[q01 ==0] = median(q01)
q02 = firm.df$num.pers.agropecuaria 
q02[q02 == 0] = .5
# TODO: two observations have zero land input - this is for a subset, so probably more




# ln.E.data <- log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + 1  )
ln.E.data <- log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + w05*x05 + 1 )

ln.E.data <- unname(ln.E.data )
 
 # TODO: why are there some duplicated row names here (above)?
 
 
 


