



#theta.param.support <- c(
#-4, 
#1, 
#6
#)


#other.param.support <- c(
#-round( max(abs(coef(linear.sur.est))) * 3 ), 
#0, 
#round( max(abs(coef(linear.sur.est))) * 3 )
#)


#cost.err.support <- c(
#-12,
#0,
#12)
# -round( max(combined.df$cost) * 5 )

#share.err.support <- c(
#-1.2,
#0,
#1.2
#)

#cost.err.support share.err.support


#  bootstrap.iter <- 0


library("numDeriv")


# GAMS.nonlinear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytest.lst")

GAMS.nonlinear.results<- readLines(paste0(GAMS.projdir, "GMEnonlinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".lst"))



GAMS.nonlinear.results<- readLines(paste0(GAMS.projdir, "GMElineartesttest.lst"))






GAMS.nonlinear.results<- readLines(paste0(GAMS.projdir, "sgmGMEnonlinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".lst"))


#GAMS.nonlinear.results<- readLines(paste0(GAMS.projdir,"sgmGMEnonlinearHaba00000 before soil and elev.lst"))


all.eqns <- paste0("dem", 1:length(demand.eqns))






GAMS.nonlinear.results <- GAMS.nonlinear.results[
  -(1:grep("S O L V E      S U M M A R Y", GAMS.nonlinear.results)) ]
  
param.gather.regex<- paste0("(^1)|", paste0("(   ", 2:length(other.param.support), " )", collapse="|"))

GAMS.nonlinear.results <- gsub(param.gather.regex, "", GAMS.nonlinear.results)
  
#GAMS.nonlinear.results <- gsub("(^1)|(   2 )|(   3 )", "", GAMS.nonlinear.results)

GAMS.nonlinear.results.extracted <- str_extract(GAMS.nonlinear.results, " p.*[.]L")

prob.names <- GAMS.nonlinear.results.extracted[!is.na(GAMS.nonlinear.results.extracted)]

prob.numbers <- GAMS.nonlinear.results[which(!is.na(GAMS.nonlinear.results.extracted)) + 2]

#start.vals.lines <- paste0("theta", lead.zero(1:(N-1)), ".l = 1;")
# added this to have correct (non-zero) starting vals for theta

#for ( i in 1:length(prob.names)) {

#  start.vals.lines <- c( start.vals.lines, 
#    paste0( prob.names[1], "(\"", 1:3, "\") = ",  strsplit(prob.numbers[i], ",")[[1]], ";")
#  )

#}





########################
###### ACTUALLY, START HERE
########################

# GAMS.nonlinear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytest.lst")
# GAMS.projdir <- "/Users/travismcarthur/Desktop/gamsdir/projdir/"

all.eqns <- paste0("dem", 1:length(demand.eqns))

GAMS.nonlinear.results<- readLines(paste0(GAMS.projdir, "sgmGMEnonlinear" # "GMEnonlinear",  
strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".lst"))


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

stacked.jacobian <- rbind(  modified.ln.E.string.evaled.deriv, 
  do.call(rbind, modified.S.n.string.evaled.deriv) )


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


round(t(t(GAMS.nonlinear.results.params.full /  sqrt(diag(param.covar.mat)) ) ), digits=2)

round(t(t((GAMS.nonlinear.results.params.full-1) /  sqrt(diag(param.covar.mat) )) ), digits=2)[grepl("xi", names(GAMS.nonlinear.results.params.full))]


round(t(t((GAMS.nonlinear.results.params.full-1) /  sqrt(diag(param.covar.mat) )) ), digits=2)[grepl("theta", names(GAMS.nonlinear.results.params.full))]



param.covar.mat[grepl("xi", names(GAMS.nonlinear.results.params.full)),
grepl("xi", names(GAMS.nonlinear.results.params.full))]

sqrt(diag(param.covar.mat[grepl("xi", names(GAMS.nonlinear.results.params.full)),
grepl("xi", names(GAMS.nonlinear.results.params.full))]))

param.covar.mat[grepl("theta", names(GAMS.nonlinear.results.params.full)),
grepl("theta", names(GAMS.nonlinear.results.params.full))]

cov2cor(param.covar.mat[grepl("xi", names(GAMS.nonlinear.results.params.full)),
grepl("xi", names(GAMS.nonlinear.results.params.full))])

library("aod")

for ( i in 2:(N-1)) {

linear.combo <- rep(0, length(GAMS.nonlinear.results.params.full))
linear.combo[names(GAMS.nonlinear.results.params.full)=="xi01"] <- 1
linear.combo[names(GAMS.nonlinear.results.params.full)==paste0("xi", lead.zero(i))] <- -1

print(wald.test(param.covar.mat, GAMS.nonlinear.results.params.full, L = t(matrix(linear.combo))))

}


# BELOW IS TEST OF WHETHER A GIVEN FIXED INPUT BELONGS IN THE REGRESSION

which.fixed.input <- 6

param.set <- names(GAMS.nonlinear.results.params.full)[
  grepl(paste0("(d[0-9][0-9]0", which.fixed.input,
    ")|(c0", which.fixed.input, 
    "$)|(c0", which.fixed.input, 
    "[0-9][0-9])|(c[0-9][0-9]0", which.fixed.input, 
    ")"), names(GAMS.nonlinear.results.params.full))]


fixed.input.wald.ls <- list()

for ( i in 1:length(param.set)) {
  linear.combo <- rep(0, length(GAMS.nonlinear.results.params.full))
  linear.combo[names(GAMS.nonlinear.results.params.full)==param.set[i] ] <- 1
  fixed.input.wald.ls[[i]] <- t(matrix(linear.combo))
}
fixed.input.wald.mat <- do.call(rbind, fixed.input.wald.ls)

print(wald.test(param.covar.mat, GAMS.nonlinear.results.params.full, L = fixed.input.wald.mat))









linear.combo <- rep(0, length(GAMS.nonlinear.results.params.full))
linear.combo[names(GAMS.nonlinear.results.params.full)=="theta01"] <- 1
linear.combo[names(GAMS.nonlinear.results.params.full)=="theta05"] <- -1

wald.test(param.covar.mat, GAMS.nonlinear.results.params.full, L = t(matrix(linear.combo)))






GAMS.nonlinear.results.params.full[grepl("xi", names(GAMS.nonlinear.results.params.full))]

GAMS.nonlinear.results.params.full[grepl("theta", names(GAMS.nonlinear.results.params.full))]



colMeans(modified.S.n.string.evaled.deriv[[1]][, grepl("xi", names(GAMS.nonlinear.results.params.full))])
colMeans(modified.S.n.string.evaled.deriv[[2]][, grepl("xi", names(GAMS.nonlinear.results.params.full))])
colMeans(modified.S.n.string.evaled.deriv[[3]][, grepl("xi", names(GAMS.nonlinear.results.params.full))])
colMeans(modified.S.n.string.evaled.deriv[[4]][, grepl("xi", names(GAMS.nonlinear.results.params.full))])
colMeans(modified.S.n.string.evaled.deriv[[5]][, grepl("xi", names(GAMS.nonlinear.results.params.full))])
colMeans(modified.S.n.string.evaled.deriv[[6]][, grepl("xi", names(GAMS.nonlinear.results.params.full))])
colMeans(modified.S.n.string.evaled.deriv[[7]][, grepl("xi", names(GAMS.nonlinear.results.params.full))])


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


summary(  y01 * rowSums(distort.cost.input.mat) - y01 * rowSums(non.distort.cost.input.mat) )
# Multiply by y01 since the output of demand.eqns is the demand divided by y01. We want quantity demanded.
# Cost in Bolivianos

summary( (y01 * rowSums(distort.cost.input.mat) - y01 * rowSums(non.distort.cost.input.mat) ) /
  y01 * rowSums(non.distort.cost.input.mat)    ) 
# Percent increase in (predicted) cost




summary(E.y01.data * y01)
# total cost based on actual data



#GAMS.nonlinear.results.params.full[ paste0("theta", lead.zero(N)) ] <- 1
assign(paste0("xi", lead.zero(N)), 1)
assign(paste0("theta", lead.zero(N)), 1)

# Below is same thing, but non-neg constraints are imposed:

distort.cost.input.mat <- matrix(NA, ncol=(length(all.eqns)-1), nrow=nrow(combined.df))

for ( i in 1:(length(all.eqns)-1)) {
  distort.cost.input.mat[, i] <- get(paste0("w", lead.zero(i))) * with(as.list(GAMS.nonlinear.results.params.full), 
    eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]]))))
    # From eqn 9 of Kumbhakar 1992, the airline one
  distort.cost.input.mat[, i] <- ifelse(distort.cost.input.mat[, i]>0, distort.cost.input.mat[, i], 0)

}

non.distort.cost.input.mat <- matrix(NA, ncol=(length(all.eqns)-1), nrow=nrow(combined.df))

for ( i in 1:(length(all.eqns)-1)) {
  non.distort.cost.input.mat[, i] <-  get(paste0("w", lead.zero(i))) * with(as.list(GAMS.nonlinear.results.params.full), 
    eval(parse(text=gsub("[.]", "", demand.eqns[[i]]))))
    # From eqn 9 of Kumbhakar 1992, the airline one
   non.distort.cost.input.mat[, i] <- ifelse( non.distort.cost.input.mat[, i]>0 ,  non.distort.cost.input.mat[, i], 0)
}


summary(  y01 * rowSums(distort.cost.input.mat) - y01 * rowSums(non.distort.cost.input.mat) )
# Multiply by y01 since the output of demand.eqns is the demand divided by y01. We want quantity demanded.
# Cost in Bolivianos
# rowSums(distort.cost.input.mat) - rowSums(non.distort.cost.input.mat)

summary( (y01 * rowSums(distort.cost.input.mat) - y01 * rowSums(non.distort.cost.input.mat) ) /
  y01 * rowSums(non.distort.cost.input.mat)    ) 
# Percent increase in (predicted) cost
# Ok, so these values are not all posi since the non-negativity constraints mess up the 
# thing. We can have a few inputs hit non-neg constraints, and then the remaining 
# inputs happen to not result in the non-distort cost being lower than the distort cost.



#### Now just do input demand, not cost:

distort.cost.input.mat <- matrix(NA, ncol=(length(all.eqns)-1), nrow=nrow(combined.df))

for ( i in 1:(length(all.eqns)-1)) {
  distort.cost.input.mat[, i] <-  y01 * with(as.list(GAMS.nonlinear.results.params.full), 
    eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]]))))
    # From eqn 9 of Kumbhakar 1992, the airline one
  distort.cost.input.mat[, i] <- ifelse(distort.cost.input.mat[, i]>0, distort.cost.input.mat[, i], 0)

}

non.distort.cost.input.mat <- matrix(NA, ncol=(length(all.eqns)-1), nrow=nrow(combined.df))

for ( i in 1:(length(all.eqns)-1)) {
  non.distort.cost.input.mat[, i] <-   y01 * with(as.list(GAMS.nonlinear.results.params.full), 
    eval(parse(text=gsub("[.]", "", demand.eqns[[i]]))))
    # From eqn 9 of Kumbhakar 1992, the airline one
   non.distort.cost.input.mat[, i] <- ifelse( non.distort.cost.input.mat[, i]>0 ,  non.distort.cost.input.mat[, i], 0)
}


summary(  as.data.frame(distort.cost.input.mat - non.distort.cost.input.mat) )
# Multiply by y01 since the output of demand.eqns is the demand divided by y01. We want quantity demanded.
# Cost in Bolivianos

summary(  as.data.frame(distort.cost.input.mat - non.distort.cost.input.mat)/as.data.frame(distort.cost.input.mat) )
# Percent increase in (predicted) cost




# Correlation between actual and predicted values

for ( i in 1:(length(demand.eqns.nonlinear)-1)) {
  print(
  cor(get(paste0("x", lead.zero(i))) / y01, # Actual
    with(as.list(GAMS.nonlinear.results.params.full), 
      eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]])))) # Predicted
  ))
}
print(
 cor(E.y01.data, # Actual
   with(as.list(GAMS.nonlinear.results.params.full), 
     eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]])))) # Predicted
))



# Among posi observations

for ( i in 1:(length(demand.eqns.nonlinear)-1)) {
  actual.temp <-  get(paste0("x", lead.zero(i))) / y01 # Actual
  predicted.temp <-  with(as.list(GAMS.nonlinear.results.params.full), 
      eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]])))) # Predicted
  print( cor(  actual.temp[actual.temp>0] ,  predicted.temp[actual.temp>0]))
}

actual.temp <-  E.y01.data  # Actual
predicted.temp <-  with(as.list(GAMS.nonlinear.results.params.full),
    eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]])))) # Predicted
print( cor(  actual.temp[actual.temp>0] ,  predicted.temp[actual.temp>0]))

## Now taking logs of posi observations so that large outliers don't rule:


# Among posi observations

for ( i in 1:(length(demand.eqns.nonlinear)-1)) {
  actual.temp <-  get(paste0("x", lead.zero(i))) / y01 # Actual
  predicted.temp <-  with(as.list(GAMS.nonlinear.results.params.full), 
      eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]])))) # Predicted
  print( cor(  log(actual.temp[actual.temp>0]) ,  log(predicted.temp[actual.temp>0]), use="complete.obs"))
}

actual.temp <-  E.y01.data  # Actual
predicted.temp <-  with(as.list(GAMS.nonlinear.results.params.full),
    eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]])))) # Predicted
print( cor(  log(actual.temp[actual.temp>0]) ,  log(predicted.temp[actual.temp>0]), use="complete.obs"))




# Ch-sq on whether good prediction for posi vs. zero


for ( i in 1:(length(demand.eqns.nonlinear)-1)) {
  actual.temp <-  get(paste0("x", lead.zero(i))) / y01 # Actual
  predicted.temp <-  with(as.list(GAMS.nonlinear.results.params.full), 
      eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]])))) # Predicted
  print(mean(predicted.temp>0))
  print(table(actual.temp>0 ,  predicted.temp>0))
  print( chisq.test(  table(actual.temp>0 ,  predicted.temp>0)))
}

actual.temp <-  E.y01.data  # Actual
predicted.temp <-  with(as.list(GAMS.nonlinear.results.params.full),
    eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]])))) # Predicted
print(mean(predicted.temp>0))
print(table(actual.temp>0 ,  predicted.temp>0))
print( chisq.test(  table(actual.temp>0 ,  predicted.temp>0)))






















# OK, it is this one below that is not right, actually

#linear.combo <- matrix(0, ncol=2, nrow=length(GAMS.nonlinear.results.params.full))
#linear.combo[names(GAMS.nonlinear.results.params.full)=="xi01", 1] <- 1
#linear.combo[names(GAMS.nonlinear.results.params.full)=="xi05", 2] <- -1


#wald.test(param.covar.mat, GAMS.nonlinear.results.params.full, L = t(linear.combo))

#wald.test(param.covar.mat, GAMS.nonlinear.results.params.full, 
#  Terms=which(names(GAMS.nonlinear.results.params.full)=="xi05"))







wald.test(param.covar.mat, GAMS.nonlinear.results.params.full, Terms=1)


diag(param.covar.mat)


eigen(t(stacked.jacobian ) %*% stacked.jacobian )

solve(param.covar.mat, tol=.Machine$double.eps^2)



GAMS.nonlinear.results.params.full[-length(GAMS.nonlinear.results.params.full)] /  diag(param.covar.mat)

t(t(GAMS.nonlinear.results.params.full ))

cat(diag(param.covar.mat), sep="\n")



 testcols <- function(ee) {
       ## split eigenvector matrix into a list, by columns
       evecs <- split(zapsmall(ee$vectors),col(ee$vectors))
       ## for non-zero eigenvalues, list non-zero evec components
       mapply(function(val,vec) {
           if (val!=0) NULL else which(vec!=0)
       },zapsmall(ee$values),evecs)
   }

testcols(eigen(param.covar.mat))

testcols(eigen(crossprod(stacked.jacobian )))






































      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
-0.5579000 -0.0777800 -0.0223300 -0.0504800 -0.0000002  0.0000051 

summary(error.collapsed.eq.df)
hist(error.collapsed.eq.df)



combined.df <- data.frame(mget(c("y01", paste0("x", lead.zero(1:N)), 
  paste0("w", lead.zero(1:N)),  paste0("q", lead.zero(1:J)) )))
  
region.matrix.df <-   as.data.frame(region.matrix)


colnames(region.matrix.df) <- iconv(colnames(region.matrix.df), to="ASCII//TRANSLIT")
colnames(region.matrix.df) <- gsub("'", "", colnames(region.matrix.df) )
colnames(region.matrix.df) <- gsub("[.]", "", colnames(region.matrix.df) )
  
combined.df <- cbind(combined.df, region.matrix.df)



for ( i in 1:N) {

  input.scaling  <- log10_ceiling(
    sqrt(sum((c(combined.df[, paste0("x", lead.zero(i))], 
    combined.df[, paste0("w", lead.zero(i))])^2)/(nrow(combined.df)-1)))
  )
  # Got this idea from scale() function
  
  input.scaling <- input.scaling / 100
  
  combined.df[, paste0("x", lead.zero(i))] <- combined.df[, paste0("x", lead.zero(i))] / input.scaling
  combined.df[, paste0("w", lead.zero(i))] <- combined.df[, paste0("w", lead.zero(i))] / input.scaling

}

if (log.plus.one.cost) {
  ln.E.data.scaled <- with(combined.df, 
    log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + w05*x05 + w06*x06 + 1  )
  )
} else {
  ln.E.data.scaled <- with(combined.df, 
    log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + w05*x05 + w06*x06  )
  )
}

combined.df <- cbind(ln.E.data.scaled, combined.df)


colnames(combined.df)[colnames(combined.df)=="ln.E.data.scaled" ] <- "ln.E.data"
 


for (i in 1:length(S.n)) {

  combined.df[, paste0("s", i)] <- with(combined.df, eval( parse(text=gsub("~.*$", "", S.n[[i]]) ) ))

}




combined.w.params.df <- as.list(as.data.frame(combined.df))

for ( i in 1:length(GAMS.nonlinear.results.params.names)) {
  combined.w.params.df[[ GAMS.nonlinear.results.params.names[i] ]] <- GAMS.nonlinear.results.params.numbers[i]
}

combined.w.params.df[[ paste0("theta", lead.zero(N)) ]] <- 1








#combined.w.params.for.deriv.df <- as.list(as.data.frame(combined.df))

#for ( i in 1:length(GAMS.nonlinear.results.params.names)) {
#  combined.w.params.for.deriv.df[[ GAMS.nonlinear.results.params.names[i] ]] <- GAMS.nonlinear.results.params.numbers[i] + 1e-8
#}

#combined.w.params.for.deriv.df[[ paste0("theta", lead.zero(N)) ]] <- 1 + 1e-8


GAMS.nonlinear.results.params.full <- GAMS.nonlinear.results.params.numbers
names(GAMS.nonlinear.results.params.full) <- GAMS.nonlinear.results.params.names
GAMS.nonlinear.results.params.full[ paste0("theta", lead.zero(N)) ] <- 1



modified.ln.E.string <- ln.E.string # str_extract( ln.E.string, "log[(] [(]w01 / [(]w01 [*] theta01[)][)].*")

region.tackon.clean <- iconv(region.tackon, to="ASCII//TRANSLIT")
region.tackon.clean <- gsub("'", "", region.tackon.clean )
region.tackon.clean <- gsub("[.]", "", region.tackon.clean )


modified.ln.E.string <- gsub(pattern="[.]", replacement="", x=modified.ln.E.string )

modified.ln.E.string<- paste0(modified.ln.E.string, " + ", region.tackon.clean)

# modified.ln.E.string <- sub("log[(]", "", modified.ln.E.string)
# modified.ln.E.string <- sub("[)]$", "", modified.ln.E.string) 

#modified.ln.E.string.evaled.deriv <- (with(combined.w.params.df, eval(parse(text=modified.ln.E.string ))) -
#  with(combined.w.params.for.deriv.df, eval(parse(text=modified.ln.E.string ))) ) / 1e-8
  
  
  
## This is to test if we can get rid of noninvertibility by having more reasonable vals
# GAMS.nonlinear.results.params.full[grepl("theta", names(GAMS.nonlinear.results.params.full))] <- 1
  
temp.deriv.fn <- function(x, data) { 
    x <- c(as.list(x), as.list(data))
    with(x, eval(parse(text=modified.ln.E.string )) )
  }
  
temp.deriv.fn(  GAMS.nonlinear.results.params.full, as.data.frame(combined.df))

modified.ln.E.string.evaled.deriv <- jacobian(temp.deriv.fn, 
    x=GAMS.nonlinear.results.params.full, method="complex", 
    data=as.data.frame(combined.df) )

modified.S.n.string.evaled.deriv <- list()

# install.packages("numDeriv")
library("numDeriv")

for ( i in 1:(length(all.eqns)-1) ) {

  modified.S.n.string <- str_extract( S.n[[i]], "~.*")

  modified.S.n.string <- gsub(pattern="[.]", replacement="", x=modified.S.n.string )

  modified.S.n.string <- sub("~", "", modified.S.n.string)
  
  temp.deriv.fn <- function(x, data) { 
    x <- c(as.list(x), as.list(data))
    with(x, eval(parse(text=modified.ln.E.string )) )
  }

  modified.S.n.string.evaled.deriv[[i]] <- jacobian(temp.deriv.fn, 
    x=GAMS.nonlinear.results.params.full, method="complex", 
    data=as.data.frame(combined.df) )
  
  #(with(combined.w.params.df, eval(parse(text=modified.S.n.string ))) -
  #with(combined.w.params.for.deriv.df, eval(parse(text=modified.S.n.string ))) ) / 1e-8


}


temp.deriv.fn(  GAMS.nonlinear.results.params.full, as.data.frame(combined.df))


stacked.jacobian <- rbind(  modified.ln.E.string.evaled.deriv, 
  do.call(rbind, modified.S.n.string.evaled.deriv) )


# install.packages("matrixcalc")
library("matrixcalc")

is.positive.definite(big.sigma)
#is.positive.definite(test.matrix)


#param.covar.mat <-  solve(
#  t( stacked.jacobian ) %*% 
#    kronecker( solve(big.sigma), diag(nrow(combined.df)) ) %*% 
#    stacked.jacobian
#  )


stacked.jacobian.last.theta.dropped <- stacked.jacobian[, -ncol(stacked.jacobian)]


param.covar.mat <-  solve(
  t( stacked.jacobian.last.theta.dropped ) %*% 
    kronecker( solve(big.sigma), diag(nrow(combined.df)) ) %*% 
    stacked.jacobian.last.theta.dropped
  )

diag(param.covar.mat)




GAMS.nonlinear.results.params.full[-length(GAMS.nonlinear.results.params.full)] /  diag(param.covar.mat)

t(t(GAMS.nonlinear.results.params.full ))

cat(diag(param.covar.mat), sep="\n")

















GAMS.nonlinear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytest.lst")

# GAMS.linear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytestlinear.lst")

GAMS.nonlinear.results <- GAMS.nonlinear.results[
  -(1:grep("S O L V E      S U M M A R Y", GAMS.nonlinear.results)) ]
  
  
param.gather.regex<- paste0("(^1)|", paste0("(   ", 2:length(other.param.support), " )", collapse="|"))

GAMS.nonlinear.results <- gsub(param.gather.regex, "", GAMS.nonlinear.results)

#GAMS.nonlinear.results <- gsub("(^1)|(   2 )|(   3 )", "", GAMS.nonlinear.results)

GAMS.nonlinear.results.extracted <- str_extract(GAMS.nonlinear.results, " p.*[.]L")

prob.names <- GAMS.nonlinear.results.extracted[!is.na(GAMS.nonlinear.results.extracted)]

prob.numbers <- GAMS.nonlinear.results[which(!is.na(GAMS.nonlinear.results.extracted)) + 2]


# TODO: Do we need to add Theta06 to this goodness-of-fit measure?

resultant.param.probs.v <- eval(parse(text=paste0("c(", paste0(prob.numbers, collapse=", "), ")")))


(-resultant.param.probs.v %*% log(resultant.param.probs.v)) / 
  (length(prob.numbers) * log(3))
# Goodness-of-fit measure from GJM (1996) p. 93, eqn 6.4.26




# rvhess = maxdouble

# max(abs(do.call( cbind, error.collapsed.eq.ls)))


coef.of.variation <- function(x) sd(x)/mean(x)

coef.of.variation(w01)
coef.of.variation(w02)
coef.of.variation(w03)
coef.of.variation(w04)
coef.of.variation(w05)
coef.of.variation(w06)











