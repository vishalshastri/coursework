
# TOBIT OF THE DETERMINANTS OF FERTILIZER USE


crop.wide.df$drive.time.amanzanada[is.na(crop.wide.df$drive.time.amanzanada)] <-
  mean(crop.wide.df$drive.time.amanzanada, na.rm=TRUE)
  
crop.wide.df$drive.time.urban[is.na(crop.wide.df$drive.time.urban)] <-
  mean(crop.wide.df$drive.time.urban, na.rm=TRUE)



fmla <- as.formula(paste("fert.exp ~ ", 
	paste0(names(crop.wide.df)[grepl("area.r", names(crop.wide.df))], collapse= " + "), 
	"+", paste0(names(livestock.wide.df)[-1], collapse= " + "),	
	"+ indig.prop + seed.exp + credit.source + department + hhh.edu.measure + hhh.sex + REMPAIS + REMEXT + num.pers.agropecuaria + AWC.CLASS + T.GRAVEL + T.SILT + T.CLAY + T.BULK.DENSITY + T.OC + T.PH.H2O + T.CEC.CLAY + T.CEC.SOIL + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + elevation + drive.time.amanzanada + drive.time.urban + mean.ann.rain.5yr"))
	
	department + 
	
fmla <- as.formula(paste("I(log(fert.exp+1)) ~ ", 
	paste0(names(crop.wide.df)[grepl("area.r", names(crop.wide.df))], collapse= " + "), 
	"+", paste0(names(livestock.wide.df)[-1], collapse= " + "),	
	"+ indig.prop + indig.practices + seed.exp + credit.source + hhh.edu.measure + hhh.sex + REMPAIS + REMEXT + num.pers.agropecuaria + elevation + drive.time.amanzanada + drive.time.urban + mean.ann.rain.5yr + + AWC.CLASS + T.GRAVEL + T.SILT + T.CLAY + T.OC + T.CEC.CLAY + T.CEC.SOIL "))
# AWC.CLASS + T.GRAVEL + T.SILT + T.CLAY + T.BULK.DENSITY + T.OC + T.PH.H2O + T.CEC.CLAY + T.CEC.SOIL + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE
	
#TODO: May also want to apply log tranfortmation to other log-normal variables like crop area and seed.exp

	
t(apply(model.matrix(fmla, data=crop.wide.df[max(crop.wide.df$fert.exp)!=crop.wide.df$fert.exp,]),
  2, FUN=max, na.rm=TRUE))
  
t(apply(model.matrix(fmla, data=crop.wide.df[max(crop.wide.df$fert.exp)!=crop.wide.df$fert.exp,]),
  2, FUN=mean, na.rm=TRUE))
  
mean(crop.wide.df$fert.exp[max(crop.wide.df$fert.exp)!=crop.wide.df$fert.exp])
max(crop.wide.df$fert.exp[max(crop.wide.df$fert.exp)!=crop.wide.df$fert.exp])
  
fml.test<-as.formula("fert.exp ~  department + drive.time.amanzanada + drive.time.urban ")
test<-solve(t(model.matrix(fmla, data=crop.wide.df.for.model)) %*% model.matrix(fmla, data=crop.wide.df.for.model))

crop.wide.df.for.model<-crop.wide.df[max(crop.wide.df$fert.exp)!=crop.wide.df$fert.exp, ]

crop.wide.df.for.model<-crop.wide.df.for.model[as.numeric(row.names(model.matrix(fmla, data=crop.wide.df))), ]

crop.wide.df.for.model[] <- lapply(crop.wide.df.for.model,function(x) if(is.factor(x)) factor(x) else x)

vars.to.scale<-colnames(model.matrix(fmla, data=crop.wide.df.for.model))
vars.to.scale<-vars.to.scale[vars.to.scale %in% names(crop.wide.df.for.model)]
vars.to.scale<-c("fert.exp", vars.to.scale)

for ( i in vars.to.scale) {
  while (max(crop.wide.df.for.model[, i], na.rm=TRUE)>100) {
    crop.wide.df.for.model[, i]<-crop.wide.df.for.model[, i]/10
  }
}

row.names(crop.wide.df.for.model)<-1:nrow(crop.wide.df.for.model)

 

#fmla <- as.formula("fert.exp ~ AWC.CLASS + T.GRAVEL + T.SILT + T.CLAY + T.BULK.DENSITY + T.OC + T.PH.H2O + T.CEC.CLAY + T.CEC.SOIL + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE" )
summary(crop.wide.df[, c("AWC.CLASS", "ADD.PROP", "T.GRAVEL", "T.SAND", "T.SILT", "T.CLAY", "T.BULK.DENSITY", "T.OC", "T.PH.H2O", "T.CEC.CLAY", "T.CEC.SOIL", "T.TEB", "T.CACO3", "T.CASO4", "T.ESP", "T.ECE")])

area.expl.tob2 <- censReg(fmla, data = crop.wide.df.for.model, iterlim = 1500, print.level=3, method="BFGS")
area.expl.tob3 <- censReg(fmla, data = crop.wide.df[max(crop.wide.df$fert.exp)!=crop.wide.df$fert.exp,], iterlim = 5000, print.level=3, method="BFGS")

# method="BFGS" is "more robust" http://cran.r-project.org/web/packages/mlogit/vignettes/mlogit.pdf
# area.expl.tob2 <- censReg(fmla, data = crop.wide.df[max(crop.wide.df$fert.exp)!=crop.wide.df$fert.exp,], iterlim = 500, start=coef(area.expl.tob2), print.level=3 )
# BHHH log like: -5200.391 (iteration limit exceeded) method="BHHH"
# iterlim = 500
# eigentol=1e-12
#maximisation method, currently either "Newton-Raphson", "BFGS", "BFGSR", "BHHH", "SANN" or "NM" (for Nelder-Mead). Lower-case letters and shortcuts (as 'nr' for Newton-Raphson) allowed.

summary( margEff( area.expl.tob2 ) )
# summary(area.expl.tob2 , eigentol=1e-20)



area.expl.tob0 <- censReg(fert.exp ~ 1, data = crop.wide.df.for.model, print.level=0)
area.expl.tob2$nObs==area.expl.tob0$nObs


area.expl.tob0 <- censReg(fert.exp ~ 1, data = crop.wide.df[row.names(model.matrix(fmla, data=crop.wide.df[max(crop.wide.df$fert.exp)!=crop.wide.df$fert.exp,])), ], print.level=0, method="BFGS")
area.expl.tob3$nObs==area.expl.tob0$nObs
area.expl.tob3$nObs
area.expl.tob0$nObs


# BGBHKJBHJ function value: -944.256 , LR: 0.1563822
# BFGS value:-937.841159 , LR: 0.1617443
#Last step could not find a value above the current.
#Boundary of parameter space?  
#Consider switching to a more robust optimisation method temporarily. 
# infinite standard errors: http://faculty.washington.edu/ezivot/econ424/maxLik.pdf

1 - as.vector( (logLik(area.expl.tob2) - length(coef(area.expl.tob2)) +2)/logLik(area.expl.tob0))
# 0.08605832
# .16

lr_test_output <- 2 * ( logLik(area.expl.tob3) - logLik(area.expl.tob2))

lr_test_p_val <- 1 - pchisq(lr_test_output, attr(logLik(area.expl.tob3), "df") - attr(logLik(area.expl.tob2), "df"))

heatmap(cor(model.matrix(fmla, data=crop.wide.df.for.model)[,-1], use="complete.obs"))

tobit.test<-tobit(fmla, data = crop.wide.df[max(crop.wide.df$fert.exp)!=crop.wide.df$fert.exp,])
tobit.test<-tobit(fmla, data = crop.wide.df.for.model)


tobit.predict <- function(formula, model, data) {
  x.mat <- model.matrix(formula, data=data)  
  x.mat <- x.mat[, !apply(x.mat, 2, FUN=function(x) all(x==0))]
  betas <- model$estimate[-length(model$estimate)]
  sigma <- exp(model$estimate[length(model$estimate)])
  argue <- x.mat %*% betas
  pnorm(argue/sigma) * (argue + sigma * dnorm(argue/sigma) / pnorm(argue/sigma))
}

dimnames(model.matrix(fmla, data=crop.wide.df) )
names(area.expl.tob2$estimate)

sapply(model.matrix(fmla, data=crop.wide.df), 2, FUN=function(x) all(x==0))

fert.predicted<-tobit.predict(fmla, area.expl.tob2, crop.wide.df.for.model)

fert.predicted<-tobit.predict(fmla, area.expl.tob3, crop.wide.df[row.names(model.matrix(fmla, data=crop.wide.df[max(crop.wide.df$fert.exp)!=crop.wide.df$fert.exp,])), ])

fert.observed <- log(
  crop.wide.df[row.names(model.matrix(fmla,   data=crop.wide.df[max(crop.wide.df$fert.exp)!=crop.wide.df$fert.exp,])),  "fert.exp"]
  +1 )

fert.residuals<- fert.observed  - fert.predicted



qqPlot(fert.residuals)

cor(fert.predicted, fert.observed)


plot(fert.predicted, fert.observed)



#row.names(model.matrix(fmla , data=crop.wide.df)))

#keep.folio<-crop.wide.df.for.model$FOLIO[as.numeric(row.names(fert.predicted))]

fert.residuals<-
crop.wide.df.for.model[as.numeric(row.names(fert.predicted)), "fert.exp"] - fert.predicted

#cor(fert.predicted, crop.wide.df.for.model[crop.wide.df$FOLIO %in% keep.folio, "fert.exp"])
cor(fert.predicted, crop.wide.df.for.model[as.numeric(row.names(fert.predicted)), "fert.exp"])


plot(fert.predicted, crop.wide.df[crop.wide.df$FOLIO %in% keep.folio, "fert.exp"])

min(crop.wide.df$fert.exp[crop.wide.df$fert.exp>0])


test.d<-dnearneigh(as.matrix(crop.wide.df[crop.wide.df$FOLIO %in% keep.folio, c("X", "Y")]), -1, 200, row.names = NULL, longlat = TRUE)

#test.nb <- knn2nb(test.d, row.names = NULL, sym = FALSE)

test.listw<-nb2listw(test.d, zero.policy=TRUE)



qqPlot(fert.residuals)


dim(model.matrix(fmla, data=crop.wide.df) )
area.expl.tob2$estimate

area.expl.tob2

test.moran<-lm.morantest(area.expl.tob2, listw=test.listw, zero.policy=TRUE, resfun=residuals)

test.moran<-moran.attempt(area.expl.tob2, test.listw, zero.policy = TRUE, alternative = "greater", 
    spChk = NULL, resfun = fert.residuals)
test.moran<-moran

fert.lm.LMtests<- lm.LMtests(fert.residuals, test.listw, zero.policy=TRUE, test="LMerr", spChk=NULL, naSubset=TRUE)

fert.lm.LMtests<- lm.LMtests(fert.residuals, test.listw, zero.policy=TRUE, test="all", spChk=NULL, naSubset=TRUE)

fert.moran.test<- moran.test(fert.residuals, test.listw, zero.policy=TRUE, randomisation=FALSE)

moran.mc.test<-moran.mc(fert.residuals, test.listw, nsim=99, zero.policy=TRUE)


corD <- correlog(as.matrix(crop.wide.df[crop.wide.df$FOLIO %in% keep.folio, c("X", "Y")]), fert.residuals, method = "Moran")

plot(corD )

fert.residuals.df<-data.frame(fert.residuals=fert.residuals)

fert.residuals.spdf<-SpatialPointsDataFrame(crop.wide.df[crop.wide.df$FOLIO %in% keep.folio, c("X", "Y")], fert.residuals.df)

plot(variogram(fert.residuals ~ 1, locations = coordinates(fert.residuals.spdf), data=fert.residuals.spdf,  cloud = F),  type = "b", main = "Variogram of fert.residuals") 

plot(sp.correlogram(test.d, as.vector(fert.residuals), order=2) )

test.variogram<-variogram(fert.residuals ~ 1, locations = coordinates(fert.residuals.spdf), data=fert.residuals.spdf,  cloud = T)

loess(gamma ~ dist, test.variogram)

test<- by(crop.wide.df$fert.exp, crop.wide.df$comunidad.id, FUN= function(x) { sum(x>0, na.rm=TRUE)/length(x[!is.na(x)]) } )




f<- function(n, X, M) { N<- sum(n) ; r<- ceiling(rank(X))
P<- (1:N-.5)/N ; W<- N*(P*log(P)+(1-P)*log(1-P))
g<- function(n,r,N,W) { u<- rep(0,N); k<- length(n); m<- 0:k
for (i in 1:k) { m[i+1]<- sum(n[1:i]); d<- (m[i]+1):m[i+1]
      d<- sort(r[d]) ; D<- c(1, d, N+1)
 p<- rep(0:n[i], D[2:(n[i]+2)]-D[1:(n[i]+1)])
 p[d]<- p[d]-.5 ; p<- p/n[i]
 u<- u+n[i]*(p*log(p+.000000001)+(1-p)*log(1-p+.000000001)) }
 return( max(u-W) )}
Zk<- g(n,r,N,W); z<- 0
 for (m in 1:M) z<- z + (g(n,rank(sample(N)),N,W) > Zk)
      p.value<- z/M ; return(list(Zk, p.value))}


n is number, so it should just be the output of by() with length

n <- by(crop.wide.df$fert.exp, crop.wide.df$comunidad.id, FUN= function(x) {length(x[!is.na(x)]) } )

X <- crop.wide.df$fert.exp[!is.na(crop.wide.df$fert.exp)]


try.k.sample <- f( n, X, 1000)





f<- function(n, X, M) { N<- sum(n) ; r<- ceiling(rank(X))
P<- (1:N-.5)/N ; W<- N*(P*log(P)+(1-P)*log(1-P))
g<- function(n,r,N,W) { u<- rep(0,N); k<- length(n); m<- 0:k
for (i in 1:k) { m[i+1]<- sum(n[1:i]); d<- (m[i]+1):m[i+1]
      d<- sort(r[d]) ; D<- c(1, d, N+1)
 p<- rep(0:n[i], D[2:(n[i]+2)]-D[1:(n[i]+1)])
 p[d]<- p[d]-.5 ; p<- p/n[i]
 u<- u-n[i]*(p*log(p+.000000001)+(1-p)*log(1-p+.000000001))}
 return( sum(u/(1:N-.5)/(N:1-.5)) )}
Za<- g(n,r,N,W) ; z<- 0
 for (m in 1:M) z<- z + (g(n,rank(sample(N)),N,W)<Za)
      p.value<- z/M ; return(list(Za, p.value)) }
    
      
      f<- function(n, X, M) { N<- sum(n); r<- ceiling(rank(X))
 P<- (1:N-.5)/N ; W<- N*(P*log(P)+(1-P)*log(1-P))
 g<- function(n,r,N,W) { u<- 0 ; k<- length(n); m<- 0:k
 for (i in 1:k) { m[i+1]<- sum(n[1:i]); d<- (m[i]+1):m[i+1]
 u<- u+sum(log(n[i]/(1:n[i]-.5)-1)*log(N/(sort(r[d])-.5)-1)) }
 return(u/N)}
Zc<- g(n,r,N,W); z<-0
 for (m in 1:M) z<-z + (g(n,rank(sample(N)),N,W)<Zc)
  p.value<-z/M ; return(list(Zc, p.value)) }



comunidad.id.free <-factor(crop.wide.df[row.names(model.matrix(fmla,   data=crop.wide.df[max(crop.wide.df$fert.exp)!=crop.wide.df$fert.exp,])),  "comunidad.id"])

n <- by(fert.residuals, comunidad.id.free, FUN= function(x) {length(x[!is.na(x)]) } )

X <- fert.residuals[!is.na(fert.residuals) & comunidad.id.free %in% dimnames(n)$INDICES[n>4]]
# X <- as.numeric(fert.residuals[!is.na(fert.residuals)]>0)


try.k.sample <- f( n[n>4], X, 1000)
try.k.sample


oneway.test(X ~ comunidad.id.free[!is.na(fert.residuals) & comunidad.id.free %in% dimnames(n)$INDICES[n>4]])

boxplot(X ~ comunidad.id.free[!is.na(fert.residuals) & comunidad.id.free %in% dimnames(n)$INDICES[n>4]])

plot(sort(by(X, comunidad.id.free[!is.na(fert.residuals) & comunidad.id.free %in% dimnames(n)$INDICES[n>4]], FUN=mean)))

