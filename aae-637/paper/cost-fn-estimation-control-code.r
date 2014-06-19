


# command:

# initial-data-setup

# Then this: 


top.crops <- names(sort(table(inputs.df$x19.codigo), decreasing=TRUE))[1:10]

# for (target.crop in top.crops) {

target.crop <- top.crops[1]

firm.df <- inputs.df[inputs.df$x19.codigo == target.crop & inputs.df$x19.produccion.obtenidad.kg>0 &
!is.na(inputs.df$x19.produccion.obtenidad.kg), ]

# Above, we are eliminating zero-harvest cases, mostly so we can take log

# TODO: Why no I(0.5 * log(q02)^2)    in the linear SUR?
# It's because it is collinear with log(q02) 


price.to.trim <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg",
   "x19.plagicidas.bs.kg", "hourly.wage",  "hourly.tractor.rental" )


price.trim.criteria <- apply(firm.df[, price.to.trim], 2, FUN=function(x) x < quantile(x, probs=0.99) )

price.trim.criteria <- apply(price.trim.criteria, 1, FUN=all)

firm.df <- firm.df[price.trim.criteria, ]

# sur-var-building
# linear-sur-building
# nonlinear-sur-building.r



# TODO: say that we need to keep in mind that the parameter equalling zero is not necessarily the appropriate null hypothesis

linear.sur.est <- systemfit( S.n.H, "SUR", restrict.matrix = lm.param.restrictions,  maxit = 5000 )

summary(linear.sur.est )$coefficients

summary(linear.sur.est )$coefficients[
  grepl("cost.fn_", rownames(summary(linear.sur.est )$coefficients)), "Pr(>|t|)"]

summary(lm(S.n.H[[length(S.n.H)]]))

summary(lm(S.n.H[[length(S.n.H)]]))$coefficients[, "Pr(>|t|)"]


region <- toupper(firm.df$zona.agroproductiva)
# TODO: need to fixed the problem with merging that forces us to do this:
region <- toupper(firm.df$zona.agroproductiva.y)
region[region=="VALLES CERRADAS     "] <- "VALLES CERRADOS     "

region <- gsub("Ú", "U", region)
region <- gsub(" *$", "", region)
region <- gsub(" ", ".", region)

region[region %in% c("CHACO.HUMEDO", "CHACO.SECO", "LLANOS.DE.SANTA.CRUZ")] <- "CHACO"
region[region %in% c("YUNGAS.DEL.NORTE", "YUNGAS.DEL.SUR")] <- "YUNGAS"
region <- factor(region)

# Make region01 be the actual variable, and then the name of the region be the parameter name 

region.matrix <- model.matrix(ln.E.data ~ region, data=firm.df)[, -1] # take out intercept

# need to stick this matrix with the full dataframe

region.tackon <- paste0("region",  lead.zero(1:ncol(region.matrix)), " * ", colnames(region.matrix), collapse=" + ")



#CHACO.HUMEDO 
#CHACO.SECO
#LLANOS.DE.SANTA.CRUZ 

# LLANOS.DE.SANTA.CRUZ should go in Chaco because:
# http://es.wikipedia.org/wiki/Llanos_de_Chiquitos
#regionYUNGAS.DEL.NORTE       regionYUNGAS.DEL.SUR 


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





######


S.n.H.region <- S.n.H

S.n.H.region[[length(S.n.H.region)]] <- 
  as.formula( paste0("ln.E.data ~ ", as.character(S.n.H[[length(S.n.H)]])[[3]], " + region" ) )
  
linear.sur.est.region <- systemfit( S.n.H.region, "SUR", restrict.matrix = lm.param.restrictions,  maxit = 5000 )

summary(linear.sur.est.region )

summary(lm(S.n.H.region[[length(S.n.H.region)]]))

summary(lm(S.n.H[[length(S.n.H)]]))



linear.sur.est.region <- systemfit( S.n.H.region, "SUR", restrict.matrix = lm.param.restrictions,  maxit = 5000 )

summary(linear.sur.est.region )





# S.n.H[[length(S.n.H)]] <- 









# Need to make sure to run this:

ln.E <- paste0("nls.formula.ln.E <- ln.E.data ~ ", ln.E.string)
eval(parse(text=ln.E))

for ( j in 1:length(S.n) ) {
  S.n[[j]] <- as.formula(S.n[[j]])
}



full.system.ls <- S.n
full.system.ls[[length(full.system.ls)+1]] <- nls.formula.ln.E
names(full.system.ls)[length(full.system.ls)] <-"cost.fn"

# TODO: Do we have to consider the environment when forming the formula?


#try.nls.group.2.SUR.fix <- nlsystemfit(method="SUR", eqns= full.system.ls, startvals=ln.E.start.vals, #data=as.data.frame(args.list[-1]), print.level=2, maxiter=100000)

try.nls.group.2.SUR <- nlsystemfit(method="SUR", eqns= full.system.ls, startvals=ln.E.start.vals, data=as.data.frame(args.list[-1]), print.level=2, maxiter=100000)

# Don't need to do "as.data.frame(args.list[-1])", since we did not do this:  args.list <- c(list(x=ln.E.start.vals), args.list)

# try.params



# Test of only cost function

#summary(lm(S.n.H[[length(S.n.H)]]))

linear.cost.fn.est <- lm(S.n.H[[length(S.n.H)]])


linear.cost.fn.est 

try.nls.cost.fn <- nls(nls.formula.ln.E, data=as.data.frame(args.list[-1]), 
  start=ln.E.start.vals, trace=TRUE, algorithm="port")

library("minpack.lm")

fm1DNase1 <- nlsLM(nls.formula.ln.E, start=ln.E.start.vals, data=as.data.frame(args.list[-1]),
trace=TRUE, lower=ifelse(grepl("theta", names(ln.E.start.vals)), 0, -Inf), control=list( maxiter=500) )


#eval(parse(text=paste0("mod <- function(x) {", first.line, 
#"  ret <- sum((ln.E.data - ", ln.E.string, ")^2); ifelse(is.finite(ret), ret, 10^300) }")))

eval(parse(text=paste0("mod.nlmrt <- function(x) {", first.line, 
"  ret <- ", ln.E.string, "- ln.E.data ; ifelse(is.finite(ret), ret, 10^300) }")))



# install.packages("nlmrt")
library(nlmrt)

fm1DNase1 <- nlfb(start=ln.E.start.vals, resfn=mod.nlmrt, trace=TRUE )
# mod.predicted

try.nlsLM.cost.fn <- nlsLM(nls.formula.ln.E, start=fm1DNase1$coefficients, data=as.data.frame(args.list[-1]),
trace=TRUE, lower=ifelse(grepl("theta", names(ln.E.start.vals)), 0, -Inf), 
control = nls.lm.control(maxiter=1000, maxfev = 100000) )



try.nlsLM.cost.fn <- nlsLM(nls.formula.ln.E, start=coef(try.nlsLM.cost.fn), data=as.data.frame(args.list[-1]),
trace=TRUE, lower=ifelse(grepl("theta", names(ln.E.start.vals)), 0, -Inf), 
control = nls.lm.control(maxiter=5000, maxfev = 100000) )

summary(try.nlsLM.cost.fn)


try.nls.cost.fn <- nls(nls.formula.ln.E, data=as.data.frame(args.list[-1]), 
  start=coef(try.nlsLM.cost.fn), trace=TRUE , algorithm="port", control=nls.control(maxiter = 1000, warnOnly=TRUE) )
  
# http://stackoverflow.com/questions/23474766/how-to-get-beautiful-latex-output-of-nls-object

summary(try.nls.cost.fn)

# mod.predicted(ln.E.start.vals)

fm1DNase1 <- nlsLM(as.formula(paste0("ln.E.data ~ ", ln.E.string)), start=pa, trace=TRUE, lower=ifelse(grepl("theta", names(ln.E.low.vals)), 0, -Inf), control=list( maxiter=500) )
# install.packages("nlmrt")
library(nlmrt)

fm1DNase1 <- nlfb(nls.formula.ln.E, data=.GlobalEnv, start=pa, trace=TRUE )
fm1DNase1 <- nlfb(start=pa, resfn=mod , trace=TRUE )
fm1DNase1 <- nlxb(nls.formula.ln.E, data=.GlobalEnv, start=pa, trace=TRUE )  
  




method="SUR", eqns= full.system.ls, start=ln.E.start.vals, data=as.data.frame(args.list[-1]), print.level=2, maxiter=100000)


full.system.ls[[length(full.system.ls)]]






# TODO: NOTE: issue with the cost share equations, since dependent var is actually censored from below at zero.














Barley estimation:
theta01      1.181e+06  3.907e+07   0.030  0.97590    
theta02      1.126e+00  2.002e+00   0.562  0.57417    
theta03      1.121e-01  1.424e-01   0.787  0.43142    
theta04      5.441e-01  7.511e-01   0.724  0.46914    
theta05      5.301e-01  5.922e-01   0.895  0.37105 



# ************************************************************************
# ************************************************************************
# ************************************************************************
# ************************************************************************
# ************************************************************************
# ************************************************************************

# OK, here is the code that I will attempt to estimate the model. This is the internal
# one-crop run:

# sort(as.character(levels(inputs.df$x19.codigo)))

#[124] "Maiz blando (dulce, blanco, chuspillo)"                        
#[125] "Maiz Choclo"                                                   
#[126] "Maiz duro (cristalino, cubano)        "  


#for ( i in grep("quintal", colnames(inputs.df)) ) {
#  inputs.df[, gsub("quintal", "kg", colnames(inputs.df)[i]) ] <- inputs.df[, i] / 46
#}
                      


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

# for (target.crop in top.crops) {





target.crop <- top.crops[5]

firm.df <- inputs.df[inputs.df$x19.codigo == target.crop & inputs.df$x19.produccion.obtenidad.kg>0 &
!is.na(inputs.df$x19.produccion.obtenidad.kg), ]

# Above, we are eliminating zero-harvest cases, mostly so we can take log

price.to.trim <- c("x19.fertilizante.bs.kg", "x19.sem.comprada.bs.kg", "x19.abono.bs.kg",
   "x19.plagicidas.bs.kg", "hourly.wage",  "hourly.tractor.rental" )

firm.df <- firm.df[!is.na(firm.df$hourly.tractor.rental), ]
# only kills 2 obseravtions for maiz and zero for Barley

price.trim.criteria <- apply(firm.df[, price.to.trim], 2, FUN=function(x) x < quantile(x, probs=0.99) )
price.trim.criteria <- apply(price.trim.criteria, 1, FUN=all)
firm.df <- firm.df[price.trim.criteria, ]

# sur-var-building
# linear-sur-building
source("/Users/travismcarthur/git/coursework/aae-637/paper/sur-var-building.r")
source("/Users/travismcarthur/git/coursework/aae-637/paper/linear-sur-building.r")
source("/Users/travismcarthur/git/coursework/aae-637/paper/nonlinear-sur-building.r")





region <- toupper(firm.df$zona.agroproductiva)
# TODO: need to fixed the problem with merging that forces us to do this:
region <- toupper(firm.df$zona.agroproductiva.y)
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



######



ln.E <- paste0("nls.formula.ln.E.region <- ln.E.data ~ ", ln.E.string, " + ", region.tackon)
eval(parse(text=ln.E))


library("minpack.lm")
library("nlmrt")
# nlmrt is for nlfb I think


eval(parse(text=paste0("mod.nlmrt <- function(x) {", first.line, 
"  ret <- ", ln.E.string, " + ", region.tackon, "- ln.E.data ; ifelse(is.finite(ret), ret, 10^300) }")))

#for (i in 1:ncol(region.matrix)) {
#  assign(paste0("region", lead.zero(i)), region.matrix[, i])
#}

for (i in 1:ncol(region.matrix)) {
  assign(colnames(region.matrix)[i], region.matrix[, i])
}


#rm("region01")
#rm("region02")
#rm("region03")
#rm("region04")
#rm("region05")
#rm("region06")
#rm("region07")
#rm("region08")





try.nlsLM.cost.fn <- nlsLM(nls.formula.ln.E.region, start=ln.E.start.vals+.001, 
data  = cbind( as.data.frame(args.list), as.data.frame(region.matrix) ),
trace=TRUE, lower=ifelse(grepl("theta", names(ln.E.start.vals)), 0, -Inf), 
control = nls.lm.control(maxiter=1000, maxfev = 100000),
weights=firm.df[, "factor.de.expansión.x"] )


try.nlsLM.cost.fn <- nlsLM(nls.formula.ln.E.region, start=ln.E.start.vals+.001, 
data  = cbind( as.data.frame(args.list), as.data.frame(region.matrix) ),
trace=TRUE, lower=ifelse(grepl("theta", names(ln.E.start.vals)), 0, -Inf), 
control = nls.lm.control(maxiter=1000, maxfev = 100000) )
# start=ln.E.start.vals+.001, 

nls.fitted.models <- list()
num.iters <- 100

while (num.iters > 5) {

try.nlsLM.cost.fn.2 <- nlsLM(nls.formula.ln.E.region, start=coef(try.nlsLM.cost.fn), 
data  = cbind( as.data.frame(args.list), as.data.frame(region.matrix) ),
trace=TRUE, lower=ifelse(grepl("theta", names(ln.E.start.vals)), 0, -Inf), 
control = nls.lm.control(maxiter=5000, maxfev = 100000) )

nls.fitted.models[[length(nls.fitted.models)+1]] <- try.nlsLM.cost.fn.2

try.nlsLM.cost.fn <- try.nlsLM.cost.fn.2

num.iters <- try.nlsLM.cost.fn.2$convInfo$finIter

cat(length(nls.fitted.models), " ", num.iters, 
  "\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n\n")

}

# TODO: I think I can do heteroskedasticity too

# Barley positive fertilizer use is extremely low:
#FALSE  TRUE 
#  726    13 

13/739
# # TODO: double hurdle stars





try.nlsLM.cost.fn <- nlsLM(nls.formula.ln.E.region, start=ln.E.start.vals, 
data  = cbind( as.data.frame(args.list), as.data.frame(region.matrix) ),
trace=TRUE, lower=ifelse(grepl("theta", names(ln.E.start.vals)), 0, -Inf), 
control = nls.lm.control(maxiter=1000, maxfev = 100000, factor=.1) )
# , factor=.1

try.nlsLM.cost.fn.6 <- nlsLM(nls.formula.ln.E.region, start=coef(try.nlsLM.cost.fn.5), 
data  = cbind( as.data.frame(args.list), as.data.frame(region.matrix) ),
trace=TRUE, lower=ifelse(grepl("theta", names(ln.E.start.vals)), 0, -Inf), 
control = nls.lm.control(maxiter=5000, maxfev = 100000) )

nls.fitted.models <- list()
num.iters <- 100

while (num.iters > 5) {

try.nlsLM.cost.fn.6 <- nlsLM(nls.formula.ln.E.region, start=coef(try.nlsLM.cost.fn.5), 
data  = cbind( as.data.frame(args.list), as.data.frame(region.matrix) ),
trace=TRUE, lower=ifelse(grepl("theta", names(ln.E.start.vals)), 0, -Inf), 
control = nls.lm.control(maxiter=5000, maxfev = 100000) )

nls.fitted.models[[length(nls.fitted.models)+1]] <- try.nlsLM.cost.fn.6

try.nlsLM.cost.fn.5 <- try.nlsLM.cost.fn.6

num.iters <- try.nlsLM.cost.fn.6$convInfo$finIter

cat(length(nls.fitted.models), " ", num.iters, "\n")

}





try.nlsLM.cost.fn <- nlsLM(nls.formula.ln.E.region, start=coef(try.nlsLM.cost.fn), 
data  = cbind( as.data.frame(args.list), as.data.frame(region.matrix) ),
trace=TRUE, lower=ifelse(grepl("theta", names(ln.E.start.vals)), 0, -Inf), 
control = nls.lm.control(maxiter=1000, maxfev = 100000) )

# nlsLM actually has a weights argument


fm1DNase1 <- nlfb(start=ln.E.start.vals, resfn=mod.nlmrt, trace=TRUE )
# mod.predicted

try.nlsLM.cost.fn <- nlsLM(nls.formula.ln.E, start=fm1DNase1$coefficients, data=as.data.frame(args.list[-1]),
trace=TRUE, lower=ifelse(grepl("theta", names(ln.E.start.vals)), 0, -Inf), 
control = nls.lm.control(maxiter=1000, maxfev = 100000) )




library("car")
library("aod")





# load("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/nls fitted models potato.Rdata")
# load("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/nls fitted models maiz.Rdata")
# load("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/nls fitted models Haba.Rdata")
# load("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/nls fitted models Trigo.Rdata")

fitted.lm <- nls.fitted.models[[length(nls.fitted.models)]]
summary(fitted.lm)
length(resid(fitted.lm))

hypothesis <- c("theta01 = 1",
    "theta02 = 1",
    "theta03 = 1",
    "theta04 = 1",
    "theta05 = 1")

linearHypothesis(fitted.lm, hypothesis.matrix=hypothesis)

# save(nls.fitted.models, file="/Users/travismcarthur/Desktop/Metrics (637)/Final paper/nls fitted models Trigo.Rdata")

#hypothesis.mat <- car:::makeHypothesis(rownames(vcov(fitted.lm)), 
#    hypothesis=hypothesis, rhs=NULL)
#hypothesis.mat <- hypothesis.mat[, -ncol(hypothesis.mat)]
# I'm not sure why we do this above. I guess it is because the last column is "*rhs*"
  
#wald.test(Sigma=vcov(fitted.lm),  # vcovHC(fitted.lm, type = "HC0")
#  b=coef(fitted.lm), 
#  L=hypothesis.mat )



  

  
hypothesis <- c(
    "theta02 = 1",
    "theta03 = 1",
    "theta04 = 1",
    "theta05 = 1")
    


#hypothesis.mat <- car:::makeHypothesis(rownames(vcov(fitted.lm)), 
#    hypothesis=hypothesis, rhs=NULL)
#hypothesis.mat <- hypothesis.mat[, -ncol(hypothesis.mat)]
# I'm not sure why we do this above. I guess it is because the last column is "*rhs*"
  
#wald.test(Sigma=vcov(fitted.lm),  # vcovHC(fitted.lm, type = "HC0")
#  b=coef(fitted.lm), 
#  L=hypothesis.mat )

linearHypothesis(fitted.lm, hypothesis.matrix=hypothesis)
linearHypothesis(fitted.lm, hypothesis.matrix="theta01 = 1")

hypothesis.vec <- c("theta01 = 1",
  "theta01 = theta02",
  "theta01 = theta03",
  "theta01 = theta04",
  "theta01 = theta05")
#  "theta02 = 1",
#  "theta03 = 1",
#  "theta04 = 1",
#  "theta05 = 1")

for ( i in hypothesis.vec) {

print(i)

hypothesis <- i

#hypothesis.mat <- car:::makeHypothesis(rownames(vcov(fitted.lm)), 
#    hypothesis=hypothesis, rhs=NULL)
#hypothesis.mat <- t(as.matrix(hypothesis.mat))
#hypothesis.mat <- hypothesis.mat[, -ncol(hypothesis.mat), drop=FALSE]
# I'm not sure why we do this above. I guess it is because the last column is "*rhs*"

print(  
#wald.test(Sigma=vcov(fitted.lm),  # vcovHC(fitted.lm, type = "HC0")
#  b=coef(fitted.lm), 
#  L=hypothesis.mat,
#  verbose=TRUE )
linearHypothesis(fitted.lm, hypothesis.matrix=hypothesis)
)

cat("\n\n")

}











fm1DNase1 <- nlsLM(nls.formula.ln.E.region, start=ln.E.start.vals, data=as.data.frame(args.list[-1]),
trace=TRUE, lower=ifelse(grepl("theta", names(ln.E.start.vals)), 0, -Inf), control=list( maxiter=500) )





try.nlsLM.cost.fn <- nlsLM(nls.formula.ln.E, start=coef(try.nlsLM.cost.fn), data=as.data.frame(args.list[-1]),
trace=TRUE, lower=ifelse(grepl("theta", names(ln.E.start.vals)), 0, -Inf), 
control = nls.lm.control(maxiter=5000, maxfev = 100000) )

summary(try.nlsLM.cost.fn)

























































































test.geog <- with(geog.df, {paste0(departamento , provincia, seccion.provincial , 
  canton    ,  x6.sector, x7.segmento)})
  # x5.organizacion.comunitaria,
  table(duplicated(test.geog))

#http://www.ine.gob.bo/anda/index.php/ddibrowser/31/questionnaires
# http://www.ine.gob.bo/anda/index.php/ddibrowser/38
 
 
 
test.geog <- with(geog.df, {paste0(departamento , provincia, seccion.provincial , 
  canton, zona.agroproductiva    ,  x6.sector, x7.segmento)})
  table(duplicated(test.geog))














