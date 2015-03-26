
# do.data.prep <- TRUE
# source("/Users/travismcarthur/git/coursework/aae-637/paper/postestimation-functions.r")



########################
###### EXTRACT ESTIMATES FROM A GAMS FILE
########################




extract.gams.est <- function(file.name =
  paste0("sgmGMEnonlinear", strsplit(target.crop, " ")[[1]][1], formatC(bootstrap.iter, width = 5, flag = "0"), ".lst") ) {


# GAMS.nonlinear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytest.lst")
# GAMS.projdir <- "/Users/travismcarthur/Desktop/gamsdir/projdir/"

all.eqns <- paste0("dem", 1:length(demand.eqns))

GAMS.nonlinear.results<- readLines(paste0(GAMS.projdir, file.name))


GAMS.nonlinear.results.params<- GAMS.nonlinear.results[grep("parameters to be estimated$", GAMS.nonlinear.results)]

GAMS.nonlinear.results.params <- GAMS.nonlinear.results.params[grep("VARIABLE", GAMS.nonlinear.results.params)]

GAMS.nonlinear.results.params.names <- gsub("[.]L", "", str_extract(GAMS.nonlinear.results.params, "[^ ]*[.]L") )


GAMS.nonlinear.results.params.numbers <- as.numeric(gsub("  parameters to be estimated", "",
  str_extract(GAMS.nonlinear.results.params, "[^ ]*  parameters to be estimated") ) )

GAMS.nonlinear.results.params.full <- GAMS.nonlinear.results.params.numbers
names(GAMS.nonlinear.results.params.full) <- GAMS.nonlinear.results.params.names








# GAMS.nonlinear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytest.lst")

#GAMS.nonlinear.results<- readLines(paste0(GAMS.projdir, "GMEnonlinear", strsplit(target.crop, " ")[[1]][1], 
#   formatC(bootstrap.iter, width = 5, flag = "0"), ".lst"))

#home.mac <- TRUE

#if (home.mac) {

#  annoying.lines<- grep(
#    "GAMS 24.1.3  r41464 Released Jul 26, 2013 XXX-DEG Mac x86_64/Darwin", 
#    GAMS.nonlinear.results)
  
#  annoying.lines <- eval(parse(text=paste0("c(", paste0(annoying.lines, ":", annoying.lines+8, collapse=", "), ")")))
  
#  cat(GAMS.nonlinear.results[-annoying.lines], 
#    file="/Users/travismcarthur/Desktop/Dropbox/entropytesttemp.lst",
#    sep="\n")
  
  
  
#}

#GAMS.nonlinear.results <- GAMS.nonlinear.results[
#  -(1:grep("S O L V E      S U M M A R Y", GAMS.nonlinear.results)) ]

begin.err.weight <- grep("L  probability corresponding error term", GAMS.nonlinear.results)
end.err.weight  <- grep(paste0("^", nrow(combined.df), " "), GAMS.nonlinear.results)

error.weight.eq.ls<-list()

for ( i in 1:length(all.eqns) ) {
  
#  err.weight.temp.df <- read.table(paste0(GAMS.projdir, "GMElineartesttest.lst"), 
#    skip = begin.err.weight[i] + 3,  nrows= nrow(combined.df))
    # "/Users/travismcarthur/Desktop/Dropbox/entropytest.lst"
    err.weight.temp.df <- read.table(
  paste0(GAMS.projdir, "sgmGMEnonlinear",  # "GMEnonlinear", 
  strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".lst"), 
#paste0(GAMS.projdir,"sgmGMEnonlinearHaba00000 before soil and elev.lst"),
   skip = begin.err.weight[i] + 3 ,  nrows= nrow(combined.df))  
   #    skip = begin.err.weight[i] + 3,  nrows= nrow(combined.df))  

  error.weight.eq.ls[[i]] <- err.weight.temp.df[, -1 ]

}

names(error.weight.eq.ls) <- all.eqns


names(err.support.dem.eqns) <- paste0("dem", 1:length(err.support.dem.eqns))


error.collapsed.eq.ls <- list()

for ( i in all.eqns ) {

  if (i=="cost") {
     error.collapsed.eq.ls[[i]] <- as.matrix(error.weight.eq.ls[[i]]) %*% 
     cost.err.support
      # c(-round( max(ln.E.data) * 5 ), 0, round( max(ln.E.data) * 5 ))
  } else {
     error.collapsed.eq.ls[[i]] <- as.matrix(error.weight.eq.ls[[i]]) %*% 
     err.support.dem.eqns[[i]]
    # demand.err.support
    # share.err.support
      # c(-10, 0, 10) 
  }
  
}


big.sigma <- cov(do.call( cbind, error.collapsed.eq.ls))

error.collapsed.eq.df <- do.call( cbind, error.collapsed.eq.ls)


for ( i in 1:N) {
  cat(paste0(i, ": ", max(get(paste0("x", lead.zero(i)))/y01), "\n"))
}

summary(error.collapsed.eq.df)


#summary(rowSums(error.collapsed.eq.df[, 2:7]))
summary(rowSums(error.collapsed.eq.df))






# Now do asymptotic SE's for SGM:




#GAMS.nonlinear.results.params.full[ paste0("theta", lead.zero(N)) ] <- 1
assign(paste0("xi", lead.zero(N)), 1)
assign(paste0("theta", lead.zero(N)), 1)



# TRANSLOG modified.ln.E.string <- ln.E.string # str_extract( ln.E.string, "log[(] [(]w01 / [(]w01 [*] theta01[)][)].*")

# TRANSLOG region.tackon.clean <- iconv(region.tackon, to="ASCII//TRANSLIT")
# TRANSLOG region.tackon.clean <- gsub("'", "", region.tackon.clean )
# TRANSLOG region.tackon.clean <- gsub("[.]", "", region.tackon.clean )


# TRANSLOG modified.ln.E.string <- gsub(pattern="[.]", replacement="", x=modified.ln.E.string )

# TRANSLOG modified.ln.E.string<- paste0(modified.ln.E.string, " + ", region.tackon.clean)

# modified.ln.E.string <- sub("log[(]", "", modified.ln.E.string)
# modified.ln.E.string <- sub("[)]$", "", modified.ln.E.string) 

#modified.ln.E.string.evaled.deriv <- (with(combined.w.params.df, eval(parse(text=modified.ln.E.string ))) -
#  with(combined.w.params.for.deriv.df, eval(parse(text=modified.ln.E.string ))) ) / 1e-8
  
  
  




temp.deriv.fn <- function(x, data) { 
    x <- c(as.list(x), as.list(data))
    with(x, eval(parse(text=modified.ln.E.string )) )
  }
  
#temp.deriv.fn(  GAMS.nonlinear.results.params.full, as.data.frame(combined.df))

#modified.ln.E.string.evaled.deriv <- jacobian(temp.deriv.fn, 
#    x=GAMS.nonlinear.results.params.full, method="complex", 
#    data=as.data.frame(combined.df) )



for ( i in 1:N) {
  assign( paste0("inputmean", lead.zero(i)), mean.of.inputs[i])
}

modified.S.n.string.evaled.deriv <- list()

# install.packages("numDeriv")
library("numDeriv")



for ( i in 1:(length(all.eqns)) ) {

#  modified.S.n.string <- str_extract( S.n[[i]], "~.*")

#  modified.S.n.string <- gsub(pattern="[.]", replacement="", x=modified.S.n.string )

#  modified.S.n.string <- sub("~", "", modified.S.n.string)
  
  temp.deriv.fn <- function(x, data) { 
    x <- c(as.list(x), as.list(data))
    with(x, eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]] #modified.S.n.string #demand.eqns.nonlinear[[i]]
    ) )) )
    # This line seemed to have a serious bug from the translog form,
    # where is ws just plugging in cost fn instaed of cost share eqns
  }

  modified.S.n.string.evaled.deriv[[i]] <- jacobian(temp.deriv.fn, 
    x=GAMS.nonlinear.results.params.full, method="complex", 
    data=as.data.frame(combined.df) )
  
  #(with(combined.w.params.df, eval(parse(text=modified.S.n.string ))) -
  #with(combined.w.params.for.deriv.df, eval(parse(text=modified.S.n.string ))) ) / 1e-8


}


temp.deriv.fn(  GAMS.nonlinear.results.params.full, as.data.frame(combined.df))


stacked.jacobian <- do.call(rbind, modified.S.n.string.evaled.deriv) 

#stacked.jacobian <- rbind(  modified.ln.E.string.evaled.deriv, 
#  do.call(rbind, modified.S.n.string.evaled.deriv) )


# install.packages("matrixcalc")
library("matrixcalc")

is.positive.definite(big.sigma)
#is.positive.definite(test.matrix)


#param.covar.mat <-  solve(
#  t( stacked.jacobian ) %*% 
#    kronecker( solve(big.sigma), diag(nrow(combined.df)) ) %*% 
#    stacked.jacobian
#  )


#stacked.jacobian.last.theta.dropped <- stacked.jacobian[, -ncol(stacked.jacobian)]


param.covar.mat <-  solve(
  t( stacked.jacobian ) %*% 
    kronecker( solve(big.sigma/nrow(combined.df)), diag(nrow(combined.df)) ) %*% 
    stacked.jacobian
  , tol=.Machine$double.eps^2)
# NOTE: reducing singularity tolerance to make it work
# is it solve(big.sigma/nrow(combined.df)) or solve(big.sigma)?


ret <- list(params=GAMS.nonlinear.results.params.full,  vcov=param.covar.mat )

ret

}




########################
###### ESTIMATE DISTORTION COST
########################

inef.cost.est <- function(GAMS.nonlinear.results.params.full) {

distort.cost.input.mat <- matrix(NA, ncol=(length(all.eqns)-1), nrow=nrow(combined.df))

for ( i in 1:(length(all.eqns)-1)) {
  distort.cost.input.mat[, i] <- get(paste0("w", lead.zero(i))) * with(as.list(GAMS.nonlinear.results.params.full), 
    eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]]))))
    # From eqn 9 of Kumbhakar 1992, the airline one

}

non.distort.cost.input.mat <- matrix(NA, ncol=(length(all.eqns)-1), nrow=nrow(combined.df))

for ( i in 1:(length(all.eqns)-1)) {
  non.distort.cost.input.mat[, i] <-  get(paste0("w", lead.zero(i))) * with(as.list(GAMS.nonlinear.results.params.full), 
    eval(parse(text=gsub("[.]", "", demand.eqns[[i]]))))
    # From eqn 9 of Kumbhakar 1992, the airline one
}


level.inc.cost <-  y01 * rowSums(distort.cost.input.mat) - y01 * rowSums(non.distort.cost.input.mat) 
# Multiply by y01 since the output of demand.eqns is the demand divided by y01. We want quantity demanded.
# Cost in Bolivianos

perc.inc.cost <-  (y01 * rowSums(distort.cost.input.mat) - y01 * rowSums(non.distort.cost.input.mat) ) /
  y01 * rowSums(non.distort.cost.input.mat)   
# Percent increase in (predicted) cost

ret <- list(level.inc.cost=level.inc.cost, perc.inc.cost=perc.inc.cost)

ret

}


########################
###### MORISHIMA ELAST OF SUBSTITUTION
########################



library("numDeriv")


morishima.e.s <- function(i, j, data, params, cost.fn.string, shadow=FALSE) {

  # All this is from eqn 8 of http://www.jstor.org/stable/pdf/1827940.pdf
  
  which.price <- ifelse(shadow, "xi", "w")
  
  if (shadow) {
    w_i <- params[ paste0(which.price, lead.zero(i)) ]
    w_j <- params[ paste0(which.price, lead.zero(j)) ]
    names(w_i) <- paste0(which.price, lead.zero(i)) 
    names(w_j) <- paste0(which.price, lead.zero(j)) 
    
  } else {
    w_i <- data[, paste0(which.price, lead.zero(i)) ]
    w_j <- data[, paste0(which.price, lead.zero(j)) ]
    names(w_i) <- paste0(which.price, lead.zero(i)) 
    names(w_j) <- paste0(which.price, lead.zero(j)) 
  }
  
#  cat(names(w_i), "\n") 
  
  assign(paste0("xi", lead.zero(N)), 1)
  assign(paste0("theta", lead.zero(N)), 1)

  temp.deriv.fn <- function(x, data) { 
    x <- c(as.list(x), as.list(data))
    with(x, eval(parse(text=gsub("[.]", "", paste0("y01 * (", cost.fn.string, ")")
    # Must multiply by y01 since estimated cost function divided by y01
    ) )) )
  }

  C_i <- grad(
    temp.deriv.fn, 
    x=w_i, method="complex", 
    data=c(params[names(params)!= paste0(which.price, lead.zero(i))], 
      data[names(data)!= paste0(which.price, lead.zero(i))] ))
      
#  print(C_i)
#  print(names(w_i))
    
  C_j <- grad(
    temp.deriv.fn, 
    x=w_j, method="complex", 
    data=c(params[names(params)!= paste0(which.price, lead.zero(j))], 
      data[names(data)!= paste0(which.price, lead.zero(j))] ))  
    
  C_hess <- hessian(
    temp.deriv.fn, 
    x=c(w_i, w_j),
    data=c(params[! names(params) %in% c(paste0(which.price, lead.zero(i)), paste0(which.price, lead.zero(j)))], 
      data[! names(data) %in% c(paste0(which.price, lead.zero(i)), paste0(which.price, lead.zero(j)))] ))
    
  C_ij <- C_hess[1, 2]
  C_ii <- C_hess[1, 1]
  print(C_hess)
  
  w_i * C_ij / C_j - w_i * C_ii / C_i
  
}



########################
###### MORISHIMA ELAST OF SUBSTITUTION MATRIX
########################

morishima.e.s.mat <- function(data=as.data.frame(t(colMeans(combined.df))), 
  params=GAMS.nonlinear.results.params.full, 
  cost.fn.string=demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]], shadow=FALSE) {

morishima.mat <- matrix(NA, ncol=N, nrow=N)

for ( i in 1:N) {
  for (j in 1:N) {
    morishima.mat[i, j] <- morishima.e.s(i, j, 
      data=data, 
      params=params, 
      cost.fn.string=cost.fn.string)
  }
}

colnames(morishima.mat) <-  c("Fert", "Seed", "Tractor", "Plagicidas", "Hired labor", "Organic fert")
rownames(morishima.mat) <-  c("Fert", "Seed", "Tractor", "Plagicidas", "Hired labor", "Organic fert")

morishima.mat


}



########################
###### ALLEN-UZAWA ELAST OF SUBSTITUTION
########################


allen.uzawa.e.s <- function(i, j, data, params, cost.fn.string, shadow=FALSE) {

  # All this is from eqn 1 of http://www.jstor.org/stable/pdf/1827940.pdf
  
  which.price <- ifelse(shadow, "xi", "w")
  
  if (shadow) {
    w_i <- params[ paste0(which.price, lead.zero(i)) ]
    w_j <- params[ paste0(which.price, lead.zero(j)) ]
    names(w_i) <- paste0(which.price, lead.zero(i)) 
    names(w_j) <- paste0(which.price, lead.zero(j)) 
    
  } else {
    w_i <- data[, paste0(which.price, lead.zero(i)) ]
    w_j <- data[, paste0(which.price, lead.zero(j)) ]
    names(w_i) <- paste0(which.price, lead.zero(i)) 
    names(w_j) <- paste0(which.price, lead.zero(j)) 
  }
  
#  cat(names(w_i), "\n") 
  
  assign(paste0("xi", lead.zero(N)), 1)
  assign(paste0("theta", lead.zero(N)), 1)

  temp.deriv.fn <- function(x, data) { 
    x <- c(as.list(x), as.list(data))
    with(x, eval(parse(text=gsub("[.]", "", paste0("y01 * (", cost.fn.string, ")")
    # Must multiply by y01 since estimated cost function divided by y01
    ) )) )
  }

  C_i <- grad(
    temp.deriv.fn, 
    x=w_i, method="complex", 
    data=c(params[names(params)!= paste0(which.price, lead.zero(i))], 
      data[names(data)!= paste0(which.price, lead.zero(i))] ))
      
#  print(C_i)
#  print(names(w_i))
    
  C_j <- grad(
    temp.deriv.fn, 
    x=w_j, method="complex", 
    data=c(params[names(params)!= paste0(which.price, lead.zero(j))], 
      data[names(data)!= paste0(which.price, lead.zero(j))] ))  
    
  C_hess <- hessian(
    temp.deriv.fn, 
    x=c(w_i, w_j),
    data=c(params[! names(params) %in% c(paste0(which.price, lead.zero(i)), paste0(which.price, lead.zero(j)))], 
      data[! names(data) %in% c(paste0(which.price, lead.zero(i)), paste0(which.price, lead.zero(j)))] ))
    
  C_ij <- C_hess[1, 2]
  
  Cost.fn.value <- 
    with(c(as.list(params), as.list(data)), 
      eval(parse(text=gsub("[.]", "", paste0("y01 * (", cost.fn.string, ")")))))
      
 # cat(Cost.fn.value, "\n")
  
  Cost.fn.value * C_ij / (C_i * C_j) 
  
}


########################
###### ALLEN-UZAWA ELAST OF SUBSTITUTION MATRIX
########################



allen.uzawa.e.s.mat <- function(data=as.data.frame(t(colMeans(combined.df))), 
  params=GAMS.nonlinear.results.params.full, 
  cost.fn.string=demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]], shadow=FALSE) {

allen.uzawa.matt <- matrix(NA, ncol=N, nrow=N)

for ( i in 1:N) {
  for (j in 1:N) {
    allen.uzawa.mat[i, j] <- allen.uzawa.e.s(i, j, 
      data=data, 
      params=params, 
      cost.fn.string=cost.fn.string)
  }
}

diag(allen.uzawa.mat) <- 0

colnames(allen.uzawa.mat) <-  c("Fert", "Seed", "Tractor", "Plagicidas", "Hired labor", "Organic fert")
rownames(allen.uzawa.mat) <-  c("Fert", "Seed", "Tractor", "Plagicidas", "Hired labor", "Organic fert")

allen.uzawa.mat


}


########################
###### SLOPE OF DEMAND CURVE
########################

demand.curve.slope <- function(i, data=as.data.frame(t(colMeans(combined.df))), 
  params=GAMS.nonlinear.results.params.full, 
  demand.fn.string=demand.eqns.nonlinear[[i]], shadow=FALSE) {

  # All this is from eqn 1 of http://www.jstor.org/stable/pdf/1827940.pdf
  
  which.price <- ifelse(shadow, "xi", "w")
  
  if (shadow) {
    w_i <- params[ paste0(which.price, lead.zero(i)) ]
    names(w_i) <- paste0(which.price, lead.zero(i)) 
    
  } else {
    w_i <- data[, paste0(which.price, lead.zero(i)) ]
    names(w_i) <- paste0(which.price, lead.zero(i)) 
  }
  
#  cat(names(w_i), "\n") 
  
  assign(paste0("xi", lead.zero(N)), 1)
  assign(paste0("theta", lead.zero(N)), 1)

  temp.deriv.fn <- function(x, data) { 
    x <- c(as.list(x), as.list(data))
    with(x, eval(parse(text=gsub("[.]", "", paste0("y01 * (", demand.fn.string, ")")
    # Must multiply by y01 since estimated cost function divided by y01
    ) )) )
  }

  C_i <- grad(
    temp.deriv.fn, 
    x=w_i, method="complex", 
    data=c(params[names(params)!= paste0(which.price, lead.zero(i))], 
      data[names(data)!= paste0(which.price, lead.zero(i))] ))

  C_i
}








########################
###### ELASTICITY OF COST W.R.T. OUTPUT
########################

cost.elast.output <- function(eval.point=mean(combined.df$y01), 
  data=as.data.frame(t(colMeans(combined.df))), 
  params=GAMS.nonlinear.results.params.full, 
  demand.fn.string=demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]]) {

  # All this is from eqn 1 of http://www.jstor.org/stable/pdf/1827940.pdf
  
  names(eval.point) <- "y01"
  
#  cat(names(w_i), "\n") 
  
  assign(paste0("xi", lead.zero(N)), 1)
  assign(paste0("theta", lead.zero(N)), 1)

  temp.deriv.fn <- function(x, data) { 
    x <- c(as.list(x), as.list(data))
    with(x, eval(parse(text=gsub("[.]", "", paste0("y01 * (", demand.fn.string, ")")
    # Must multiply by y01 since estimated cost function divided by y01
    ) )) )
  }

  C_y <- grad(
    temp.deriv.fn, 
    x=eval.point, method="complex", 
    data=c(params[names(params)!= "y01"], 
      data[names(data)!= "y01"]))

  C_y * (mean(y01)/mean(E.y01.data*y01))
}
      



########################
###### ELASTICITY OF COST W.R.T. OUTPUT for all observations
########################

cost.elast.output.all.obs <- function(eval.point=combined.df[, "y01", drop=FALSE], 
  data=combined.df, 
  params=GAMS.nonlinear.results.params.full, 
  demand.fn.string=demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]]) {

  # All this is from eqn 1 of http://www.jstor.org/stable/pdf/1827940.pdf
  
#  names(eval.point) <- "y01"
  
#  cat(names(w_i), "\n") 
  
  assign(paste0("xi", lead.zero(N)), 1)
  assign(paste0("theta", lead.zero(N)), 1)

  temp.deriv.fn <- function(x, data) { 
    x <- c(as.list(x), as.list(data))
    with(x, eval(parse(text=gsub("[.]", "", paste0("y01 * (", demand.fn.string, ")")
    # Must multiply by y01 since estimated cost function divided by y01
    ) )) )
  }

  C_y <- jacobian(
    temp.deriv.fn, 
    x=eval.point, method="complex", 
    data=c(params[names(params)!= "y01"], 
      data[names(data)!= "y01"]))

  C_y * y01/(E.y01.data*y01)
}
      












########################
###### STANDARD DATA PREP (THIS IS RUN IN THE GLOBAL ENVIRONMENT so that all the objects can be output there)
########################


if (!exists("do.data.prep")) {do.data.prep <- FALSE}

if (do.data.prep) {




target.top.crop.number <- 1

#Including zero cost:
#Potatoes	4,058
#Maize	3,440
#Barley	2,048
#Wheat	1,647
#Fava Beans	1,484

M <- 1
N <- 6
J <- 3
# J <- 6





functional.form <- "SGM" # OR TRANSLOG
#functional.form <- "TRANSLOG"


#synthetic.data <-TRUE
 synthetic.data <-FALSE
if (!exists("global.max.seed")) { global.max.seed <- 0}
do.SUR <- FALSE
include.cost.fn <- TRUE
only.cost.fn <- TRUE
generate.synth.data.from.cost.fn <- TRUE
start.at.true.xi <- FALSE
start.nonlin.from.ignorance <- TRUE
convex.in.f.inputs <- FALSE
concave.in.prices <- TRUE
# NOTE: J, i.e. number of fixed inputs, is set via sgm-linear-sur-building.r



if (!synthetic.data) { 
  intended.seed <- 100 
  start.nonlin.from.ignorance <- FALSE
#  start.nonlin.from.ignorance <- TRUE
  global.max.seed <- 4
  do.SUR <- TRUE
  include.cost.fn <- TRUE
  only.cost.fn <- FALSE
  generate.synth.data.from.cost.fn <- FALSE
  start.at.true.xi <- FALSE
}



# Only posi cost observations:

#Papa (patatas)    3155 
#Maiz combined   1838 
#Cebada combined   950 
#Trigo             475 
#Haba (verde)       641 
#Oca               240 
#Arveja (verde)     217 
#Hoja de coca       363 
#Arroz con cascara          264
#Quinua            284 




# do.SUR <- TRUE

#functional.form <- "TRANSLOG"

if (functional.form =="SGM") {
  include.censored.cost <- TRUE
}

price.trim.quantile <- 0.99
demand.var.trim.quantile <- 0.95
#demand.var.trim.quantile <- 1


local.source.evaluation <- FALSE
dropped.cost.share.eq <- 10
# anything >6 means that no equation gets dropped


saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/GAMS work/saved workspace.Rdata"

saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF with soil.Rdata"
# with soil

saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF with soil and rain.Rdata"
# with soil and rain and elevation


GAMS.projdir <-  "/Users/travismcarthur/Desktop/gamsdir/projdir2/"

GAMS.exe.path <- "/Applications/GAMS/gams24.1_osx_x64_64_sfx/gams"

code.dir <- "/Users/travismcarthur/git/coursework/aae-637/paper/"

# GAMS.projdir.subdir <-  "/Users/travismcarthur/Desktop/gamsdir/projdir/bootstrap/"

if (Sys.info()['sysname']=="Linux") {

saved.workspace.path <- "" # "/home/c/cschmidt/TravisImInYourInternets/bootstrap-output/saved workspace.Rdata" NEED TO FIX # saved workspace only inputsDF with soil.Rdata

GAMS.projdir <-  "/home/c/cschmidt/TravisImInYourInternets/gamsdir/projdir/"

GAMS.exe.path <- "/home/c/cschmidt/TravisImInYourInternets/gams24.1_linux_x64_64_sfx/gams"

code.dir <- "/home/c/cschmidt/TravisImInYourInternets/bootstrap-R-code/"

.libPaths("/home/c/cschmidt/TravisImInYourInternets/Rlib")

#detach("package:Matrix", unload = TRUE, force=TRUE)
#detach("package:lattice", unload = TRUE, force=TRUE)

#unloadNamespace("lattice")

#install.packages("lattice", repos="http://cran.us.r-project.org", 
#        lib="/home/c/cschmidt/TravisImInYourInternets/Rlib")

library(lattice, lib.loc ="/home/c/cschmidt/TravisImInYourInternets/Rlib")

library(Matrix)

  for ( i in c("gdata", "stringr", "systemfit") ) {
    if(!require(i, character.only=TRUE, lib.loc ="/home/c/cschmidt/TravisImInYourInternets/Rlib")) {
      install.packages(i, repos="http://cran.us.r-project.org", 
        lib="/home/c/cschmidt/TravisImInYourInternets/Rlib")
      while(!require(i, character.only=TRUE, lib.loc ="/home/c/cschmidt/TravisImInYourInternets/Rlib")) {
        Sys.sleep(1)
  	    require(i, character.only=TRUE, lib.loc ="/home/c/cschmidt/TravisImInYourInternets/Rlib")
  	  }
    }
  }

}






load(saved.workspace.path)




log.plus.one.cost <- FALSE

bootstrap.iter <- 1
# NOTE: Bootstrap iter = 0 means actual estimate
bootstrap.selection.v <- TRUE
source(paste0(code.dir, "build-model-extract-parcels.r"))
# Above is a bit hacky

combined.df <- data.frame(mget(c("y01", paste0("x", lead.zero(1:N)), 
  paste0("w", lead.zero(1:N)),  paste0("q", lead.zero(1:J)) )))

if (functional.form =="TRANSLOG") {

region.matrix.df <-   as.data.frame(region.matrix)


colnames(region.matrix.df) <- iconv(colnames(region.matrix.df), to="ASCII//TRANSLIT")
colnames(region.matrix.df) <- gsub("'", "", colnames(region.matrix.df) )
colnames(region.matrix.df) <- gsub("[.]", "", colnames(region.matrix.df) )
  
combined.df <- cbind(combined.df, region.matrix.df)


# Below makes use of the fact that we have the original dataframe floating in workspace
# from the above source()

log10_ceiling <- function(x) {
    10^(ceiling(log10(x)))
}
# Thanks to http://stackoverflow.com/questions/7906996/algorithm-to-round-to-the-next-order-of-magnitude-in-r

input.scaling.orig <- c()
for ( i in 1:N) {

  input.scaling.orig  <- c( input.scaling.orig, log10_ceiling(
    sqrt(sum((c(combined.df[, paste0("x", lead.zero(i))], 
    combined.df[, paste0("w", lead.zero(i))])^2)/(nrow(combined.df)-1)))
  )
  )
  # Got this idea from scale() function

}

scale.vars.on.orig.data <- TRUE

}





set.seed(100)

#bootstrap.replications <- 1
#bootstrap.replications <- 1500
nrow(firm.df)
nrow(inputs.df)
length(unique(inputs.df$folio))


bootstrap.replications.v <- 1:1500
# 0:300 301:600 601:900 901:1200 1201:1500
# condor_R max-entropy-bootstrap.r bootmaiz1.log &

bootstrap.replications <- max(bootstrap.replications.v)


bootstrap.selection.mat<- matrix(sample( x=nrow(firm.df), size=nrow(firm.df)*bootstrap.replications, 
  replace=TRUE), nrow=nrow(firm.df))

time.counter <- c()

# 1:bootstrap.replications

bootstrap.iter <- 0



#for ( bootstrap.iter in c(0, bootstrap.replications.v)) {
#for ( bootstrap.iter in 0) {

if( bootstrap.iter==0 ) {
  bootstrap.selection.v <- TRUE
} else {
  bootstrap.selection.v <- bootstrap.selection.mat[, bootstrap.iter]
}


#for (target.top.crop.number in c(2,4,5)) {

#if (functional.form =="TRANSLOG") {
  source(paste0(code.dir, "build-model-extract-parcels.r"))
#}
#if (functional.form =="SGM") {
#  source(paste0(code.dir, "sur-var-building.r"), local=local.source.evaluation)  
    if (synthetic.data) {
    source(paste0(code.dir, "synthetic-data.r"), local=local.source.evaluation)
  }
#}

# If want to make censoring plots:
# source("/Users/travismcarthur/git/coursework/aae-637/paper/analyze-summary-stats.r")


source(paste0(code.dir, "GAMS-construction-functions.r"))


if (functional.form =="TRANSLOG") {

cost.err.endpoint <- round(max(abs(resid(linear.sur.est.region)[grepl("cost", 
  names(resid(linear.sur.est.region)))])) * 1.4, digits=1)


#share.err.endpoint <- round(max(abs(resid(linear.sur.est.region)[!grepl("cost", 
#  names(resid(linear.sur.est.region)))])) * 1.4, digits=1)

#share.err.endpoint <- 1.5 
share.err.endpoint <- 2

cost.err.support <- seq(from = -cost.err.endpoint, to = cost.err.endpoint, length.out=3)

# -round( max(combined.df$cost) * 5 )

share.err.support <- seq(from = -share.err.endpoint, to = share.err.endpoint, length.out=3)

other.param.endpoint <- round( max(abs(coef(linear.sur.est.region))) * 3 , digits=1)

other.param.support <- seq(from = -other.param.endpoint, to = other.param.endpoint, length.out=5)

}


if (functional.form =="SGM") {
  other.param.endpoint <- round( max.abs.other.param * 2 , digits=1)

  other.param.support <- seq(from = -other.param.endpoint, to = other.param.endpoint, length.out=3)
  # NOTE: I changed this to 3
}


# linear.GAMS.output <- TRUE
linear.GAMS.output <- FALSE



if (only.cost.fn) {
  demand.eqns <- demand.eqns[length(demand.eqns)]
  demand.eqns.nonlinear <- demand.eqns.nonlinear[length(demand.eqns.nonlinear)]
}




if (functional.form =="TRANSLOG") {
  source(paste0(code.dir, "GAMS-linear-construction.r"))
}

if (functional.form =="SGM" & !start.nonlin.from.ignorance) {
  source(paste0(code.dir, "sgm-GAMS-linear-construction.r"))
}


} # End do.data.prep


rm(i)
rm(j)
























