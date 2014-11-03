
library("stringr")


local.source.evaluation <- TRUE
dropped.cost.share.eq <- 10
# anything >6 means that no equation gets dropped


#condor.gams.dir <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Condor/10-19-projdir/home/c/cschmidt/TravisImInYourInternets/gamsdir/projdir/"

#condor.gams.dir <- "/Users/travismcarthur/Desktop/gamsdir/projdir/"

condor.gams.dir <- "/Users/travismcarthur/Desktop/Condor/10-28-projdir/home/c/cschmidt/TravisImInYourInternets/gamsdir/projdir/"

target.top.crop.number <- 2


listed.files <- list.files(condor.gams.dir )


theta.ci.ls <- list()
cost.ci.ls <- list()
share.ci.ls <- list()


for ( target.top.crop.number in c(2, 4, 5)) {


# target.top.crop.number <- 5

target.estimation <- c( "GMEnonlinearPapa", "GMEnonlinearMaiz", "GMEnonlinearCebada",  "GMEnonlinearTrigo", "GMEnonlinearHaba")[target.top.crop.number]


target.files <- listed.files[grepl( paste0(target.estimation, "[0-9]{5}[.]lst") , listed.files)]

# check to make sure that we have the 00000 estimation

target.files <- paste0(condor.gams.dir ,  target.files)


bootstrapped.thetas.ls <- vector(mode="list", length=0) # last.bootstrap+1
bootstrapped.all.params.ls <- vector(mode="list", length=0) # last.bootstrap+1

# as.character(0:last.bootstrap)

for ( bootstrap.iter in 1:length(target.files) ) {


# GAMS.nonlinear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytest.lst")
# paste0(GAMS.projdir, "GMEnonlinear", strsplit(target.crop, " ")[[1]][1]
# formatC(as.numeric(bootstrap.iter), width = 5, flag = "0"), ".lst")
GAMS.nonlinear.results<- readLines(target.files[bootstrap.iter] )


GAMS.nonlinear.results.params<- GAMS.nonlinear.results[grep("parameters to be estimated$", GAMS.nonlinear.results)]

GAMS.nonlinear.results.params <- GAMS.nonlinear.results.params[grep("VARIABLE", GAMS.nonlinear.results.params)]

GAMS.nonlinear.results.params.names <- gsub("[.]L", "", str_extract(GAMS.nonlinear.results.params, "[^ ]*[.]L") )


GAMS.nonlinear.results.params.numbers <- as.numeric(gsub("  parameters to be estimated", "",
  str_extract(GAMS.nonlinear.results.params, "[^ ]*  parameters to be estimated") ) )


GAMS.nonlinear.results.params.full <- GAMS.nonlinear.results.params.numbers
names(GAMS.nonlinear.results.params.full) <- GAMS.nonlinear.results.params.names

bootstrapped.all.params.ls[[bootstrap.iter]] <- GAMS.nonlinear.results.params.full

bootstrapped.thetas.ls[[bootstrap.iter]] <- 
  GAMS.nonlinear.results.params.full[grepl("theta", names(GAMS.nonlinear.results.params.full))]
  

}




#last.jackknife <- 475


#target.estimation <- "GMEnonlinearjackknifeMaiz"
#target.estimation <- "GMEnonlinearjackknifeHaba"
#target.estimation <- "GMEnonlinearjackknifeTrigo"

target.estimation <- c( "GMEnonlinearjackknifePapa", "GMEnonlinearjackknifeMaiz", "GMEnonlinearjackknifeCebada",  "GMEnonlinearjackknifeTrigo", "GMEnonlinearjackknifeHaba")[target.top.crop.number]

target.files <- listed.files[grepl( paste0(target.estimation, "[0-9]{5}[.]lst") , listed.files)]

# check to make sure that we have the 00000 estimation

target.files <- paste0(condor.gams.dir ,  target.files)



jackknifed.thetas.ls <- vector(mode="list", length=0) # last.bootstrap+1
jackknifed.all.params.ls <- vector(mode="list", length=0) # last.bootstrap+1


for ( bootstrap.iter in 1:length(target.files) ) {


# GAMS.nonlinear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytest.lst")

GAMS.nonlinear.results<- readLines(target.files[bootstrap.iter] )



GAMS.nonlinear.results.params<- GAMS.nonlinear.results[grep("parameters to be estimated$", GAMS.nonlinear.results)]

GAMS.nonlinear.results.params <- GAMS.nonlinear.results.params[grep("VARIABLE", GAMS.nonlinear.results.params)]

GAMS.nonlinear.results.params.names <- gsub("[.]L", "", str_extract(GAMS.nonlinear.results.params, "[^ ]*[.]L") )


GAMS.nonlinear.results.params.numbers <- as.numeric(gsub("  parameters to be estimated", "",
  str_extract(GAMS.nonlinear.results.params, "[^ ]*  parameters to be estimated") ) )


GAMS.nonlinear.results.params.full <- GAMS.nonlinear.results.params.numbers
names(GAMS.nonlinear.results.params.full) <- GAMS.nonlinear.results.params.names

jackknifed.all.params.ls[[bootstrap.iter]] <- GAMS.nonlinear.results.params.full

jackknifed.thetas.ls[[bootstrap.iter]] <- 
  GAMS.nonlinear.results.params.full[grepl("theta", names(GAMS.nonlinear.results.params.full))]
  

}



table(sapply(jackknifed.thetas.ls, FUN=length))

jackknifed.thetas.ls[sapply(jackknifed.thetas.ls, FUN=length)==10]

bootstrapped.thetas.ls <- lapply(bootstrapped.thetas.ls, FUN=function(x) {
x[!duplicated(names(x))]
})


bootstrapped.all.params.ls <- lapply(bootstrapped.all.params.ls, FUN=function(x) {
x[!duplicated(names(x))]
})

jackknifed.all.params.ls <- lapply(jackknifed.all.params.ls, FUN=function(x) {
x[!duplicated(names(x))]
})


jackknifed.thetas.df <- as.data.frame(do.call(rbind, jackknifed.thetas.ls))





theta.hats <- bootstrapped.thetas.ls[[1]]

bootstrapped.thetas.ls[[1]] <- NULL
# Here is where the 00000 ( i.e. the actual estimate) comes in 

#all.param.hats <- bootstrapped.all.params.ls[[1]]
# bootstrapped.all.params.ls[[1]] <- NULL
# Should I do this above?


table(sapply(bootstrapped.thetas.ls, FUN=length))

bootstrapped.thetas.ls[sapply(bootstrapped.thetas.ls, FUN=length)==10]
# Ok, some have duplicated thetas for unknown reasons, but they have the same values, so we should be OK.


bootstrapped.thetas.ls <- lapply(bootstrapped.thetas.ls, FUN=function(x) {
x[!duplicated(names(x))]
})


bootstrapped.thetas.df <- as.data.frame(do.call(rbind, bootstrapped.thetas.ls))

#par(mfcol=c(3,2))

#lapply(bootstrapped.thetas.df, FUN=hist, breaks=10)


library(lattice)



histogram( ~ theta01 + theta02 + theta03 + theta04 + theta05, data = bootstrapped.thetas.df,
    xlab = "Height (inches)", type = "density",
    panel = function(x) {
        panel.histogram(x, breaks=NULL, nint= 20)
#        panel.mathdensity(dmath = dnorm, col = "black",
#            args = list(mean=mean(x),sd=sd(x)))
    },
    )

# Thanks to http://stackoverflow.com/questions/7373238/how-to-change-the-scales-on-the-x-axis-in-the-histogram-made-using-lattice-packa

#scales = list(x = list(at = seq(60, 80, by = 2), rot = 45))



# install.packages("corrgram")
library("corrgram")

round(cor(bootstrapped.thetas.df), digits=4)

corrgram(bootstrapped.thetas.df, order=FALSE, lower.panel=panel.shade,
  upper.panel=panel.pie)


summary(bootstrapped.thetas.df)



#x <- bootstrapped.thetas.df[, 1]
#theta <- function(p,x) {sum(p*x)/sum(p)}
#results <- abcnon(x, theta)
# compute abc intervals for th

j <- 1

alpha <- .025

theta.ci.df <- data.frame( theta01=c(0,0), theta02=c(0,0), theta03=c(0,0), theta04=c(0,0), theta05=c(0,0))

# But really, we want to form intervals for the difference between, not just the values

for ( j in 1:length(theta.hats)) {

  last.bootstrap <- nrow(bootstrapped.thetas.df)

  bias.measure <- qnorm(sum(bootstrapped.thetas.df[, j] < theta.hats[j]) / last.bootstrap)
  # i.e. z_hat_0
  # 2nd answer here: http://stackoverflow.com/questions/19589191/the-reverse-inverse-of-the-normal-distribution-function-in-r
  
  mean.jackknife <- mean(jackknifed.thetas.df[, j])
  
  acceleration.measure <- sum( (mean.jackknife - jackknifed.thetas.df[, j])^3) /
    (6 * (sum( (mean.jackknife - jackknifed.thetas.df[, j])^2)^(3/2))  )
  # Eqn 14.15 in An Introduction to the Bootstrap
  # i.e. a_hat
  
  alpha.1 <- pnorm(
    bias.measure + (bias.measure + qnorm(alpha) ) /
      (1 - acceleration.measure * (bias.measure + qnorm(alpha) ))
  )
  # Eqn 14.10 in An Introduction to the Bootstrap
  
  alpha.2 <- pnorm(
    bias.measure + (bias.measure + qnorm(1-alpha) ) /
      (1 - acceleration.measure * (bias.measure + qnorm(1-alpha) ))
  )
  # Eqn 14.10 in An Introduction to the Bootstrap
  
  theta.ci.df[ 1,j ] <- quantile(bootstrapped.thetas.df[, j],  probs=alpha.1)
  theta.ci.df[ 2,j ] <- quantile(bootstrapped.thetas.df[, j],  probs=alpha.2)
  
  print(c(bias=bias.measure, acc=acceleration.measure ))


}

theta.ci.df

# compare to:

theta.false.ci.df <- data.frame( theta01=c(0,0), theta02=c(0,0), theta03=c(0,0), theta04=c(0,0), theta05=c(0,0))

for ( j in 1:length(theta.hats)) {
  theta.false.ci.df[ 1,j ] <- quantile(bootstrapped.thetas.df[, j],  probs=0.025)
  theta.false.ci.df[ 2,j ] <- quantile(bootstrapped.thetas.df[, j],  probs=0.975)
}

theta.ci.df
theta.false.ci.df



# Now expressed as a ratio

# BELOW IDS FOR RATIOS:

comparison.theta <- "theta01"

bootstrapped.thetas.df$theta06 <- 1
jackknifed.thetas.df$theta06 <- 1

if(length(theta.hats)<6) { theta.hats<- c(theta.hats, theta06=1) }


theta.dif.ci.ls <- list()

for ( targ.alpha in c(.05, .025, .005)) {


#alpha <- .05
alpha <- targ.alpha 
#alpha <- .005

theta.dif.ci.df <- data.frame( theta01=c(0,0), theta02=c(0,0), theta03=c(0,0), theta04=c(0,0), theta05=c(0,0), theta06=c(0,0))

theta.dif.ci.df <- theta.dif.ci.df[, colnames(theta.dif.ci.df)!=comparison.theta ]

# But really, we want to form intervals for the difference between, not just the values

theta.hats.comp <- theta.hats[names(theta.hats)!=comparison.theta]

target.theta.est <- theta.hats[names(theta.hats)==comparison.theta]




for ( j in names(theta.hats.comp)) {

#  bias.measure <- qnorm(sum(
#    (bootstrapped.thetas.df[, j] - bootstrapped.thetas.df[, comparison.theta])  < 
#      (theta.hats.comp[j] - target.theta.est)
#    ) / last.bootstrap)
  bias.measure <- qnorm(sum(
    (bootstrapped.thetas.df[, comparison.theta] / bootstrapped.thetas.df[, j])  < 
      ( target.theta.est / theta.hats.comp[j] )
    ) / last.bootstrap)  
    
  # i.e. z_hat_0
  # 2nd answer here: http://stackoverflow.com/questions/19589191/the-reverse-inverse-of-the-normal-distribution-function-in-r
  
  #mean.jackknife <- mean(jackknifed.thetas.df[, j] - jackknifed.thetas.df[, comparison.theta])
  mean.jackknife <- mean(jackknifed.thetas.df[, comparison.theta] / jackknifed.thetas.df[, j] )
  
  #jackknife.dif.vector <- jackknifed.thetas.df[, j] - jackknifed.thetas.df[, comparison.theta]
  jackknife.dif.vector <- jackknifed.thetas.df[, comparison.theta] / jackknifed.thetas.df[, j]
  
  acceleration.measure <- sum( (mean.jackknife - jackknife.dif.vector)^3) /
    (6 * (sum( (mean.jackknife - jackknife.dif.vector)^2)^(3/2))  )
  # Eqn 14.15 in An Introduction to the Bootstrap
  # i.e. a_hat
  
  alpha.1 <- pnorm(
    bias.measure + (bias.measure + qnorm(alpha) ) /
      (1 - acceleration.measure * (bias.measure + qnorm(alpha) ))
  )
  # Eqn 14.10 in An Introduction to the Bootstrap
  
  alpha.2 <- pnorm(
    bias.measure + (bias.measure + qnorm(1-alpha) ) /
      (1 - acceleration.measure * (bias.measure + qnorm(1-alpha) ))
  )
  # Eqn 14.10 in An Introduction to the Bootstrap
  
#  theta.dif.ci.df[ 1,j ] <- quantile(bootstrapped.thetas.df[, j] - 
#    bootstrapped.thetas.df[, comparison.theta] ,  probs=alpha.1)
#  theta.dif.ci.df[ 2,j ] <- quantile(bootstrapped.thetas.df[, j] - 
#    bootstrapped.thetas.df[, comparison.theta] ,  probs=alpha.2)
    
  theta.dif.ci.df[ 1,j ] <- quantile( bootstrapped.thetas.df[, comparison.theta]/ 
    bootstrapped.thetas.df[, j] ,  probs=alpha.1)
  theta.dif.ci.df[ 2,j ] <- quantile(bootstrapped.thetas.df[, comparison.theta]/ 
    bootstrapped.thetas.df[, j] ,  probs=alpha.2)
  
  print(c(bias=bias.measure, acc=acceleration.measure ))


}

theta.dif.ci.df <- as.data.frame(t(theta.dif.ci.df))

colnames(theta.dif.ci.df) <- c("Lower", "Upper")
theta.dif.ci.df$param <- rownames(theta.dif.ci.df)

theta.dif.ci.df$size <- targ.alpha

theta.dif.ci.ls[[as.character(targ.alpha)]]<- theta.dif.ci.df

}

theta.dif.ci.df <- do.call(rbind, theta.dif.ci.ls)

library("ggplot2")

# ADD TO SLIDIFY
ggplot(theta.dif.ci.df[nrow(theta.dif.ci.df):1,], aes(x = param)) +
#  geom_point(size = 4) +
  geom_errorbar(aes(ymax = Upper, ymin = Lower, size=size, width=0, colour=size)) +  # as.factor(size)
  geom_hline(yintercept=1, colour="red") +
  coord_trans(y="log2") +
  scale_y_continuous(breaks=c( 1,5,10,15)) 
  
# Hmm. Maybe just say "effectively zero":
# min(as.numeric(unlist(theta.dif.ci.df)), na.rm=TRUE)+.1  
# This problem with this of course is that log is not defined at zero:
#, limits=c(0, max(as.numeric(unlist(theta.dif.ci.df)), na.rm=TRUE)*1.2)




# BELOW IS FOR EXTRA COST CALCULATION

# I just need to do below to get the ln.E.string
saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/GAMS work/saved workspace.Rdata"
code.dir <- "/Users/travismcarthur/git/coursework/aae-637/paper/"
load(saved.workspace.path)
# Remember to change the above according to which crop we are doing
bootstrap.iter <- 1
bootstrap.selection.v <- TRUE
log.plus.one.cost <- FALSE
source(paste0(code.dir, "build-model-extract-parcels.r"))





double.beta.extra.first.part <- str_extract_all(ln.E.string, "[(]1/2[)] [*] [(][(][(] beta.*")[[1]]

double.beta.extra.first.part <- str_replace_all(double.beta.extra.first.part, " [+] [(][-][(] gamma.*", "") 

double.beta.extra.first.part <- str_replace_all(double.beta.extra.first.part, " [*] theta[0-9][0-9][)] [*]", ") *") 

double.beta.extra.first.part <- str_replace_all(double.beta.extra.first.part, "w[0-9][0-9] [*] ", "")

double.beta.extra.first.part <- str_replace_all(double.beta.extra.first.part, "[(]1/2[)] [*] ", "")


double.beta.extra.second.part <- str_extract_all(ln.E.string, "[(]1/2[)] [*] [(][(][(] beta.*")[[1]]

double.beta.extra.second.part <- str_replace_all(double.beta.extra.second.part, " [+] [(][-][(] gamma.*", "") 

double.beta.extra.second.part <- str_replace_all(double.beta.extra.second.part, "w[0-9][0-9] [*] ", "")

double.beta.extra.second.part <- str_replace_all(double.beta.extra.second.part, "w[0-9][0-9] [*] ", "")


gamma.extra.part <- str_extract_all(ln.E.string, "[(][-][(] gamma.*")[[1]]

gamma.extra.part <- str_replace_all(gamma.extra.part, "[+] zeta.*", "")

gamma.extra.part <- str_replace_all(gamma.extra.part, "w[0-9][0-9] [*] ", "")


single.beta.extra.part <- str_replace_all(ln.E.string, " [+] [(]1/2[)] [*] [(]alpha.*", "")

single.beta.extra.part <- str_replace_all(single.beta.extra.part, "beta0 [+] alpha01 [*] y01 [+] ", "")

single.beta.extra.part <- str_replace_all(single.beta.extra.part, "w[0-9][0-9] [*] ", "")



kappa.extra.part <- str_extract_all(ln.E.string, "[(][-][(]kappa.*")[[1]]

kappa.extra.part <- str_replace_all(kappa.extra.part, " [+] delta.*", "")

kappa.extra.part <- str_replace_all(kappa.extra.part, "w[0-9][0-9] [*] ", "")


extra.cost.string <- paste0(single.beta.extra.part, " + ", double.beta.extra.first.part,
  " + ", double.beta.extra.second.part, " + ", gamma.extra.part, " + ", kappa.extra.part)
  
# extra.cost.string <- paste0(single.beta.extra.part, " + ", double.beta.extra.first.part,  " + ", double.beta.extra.second.part, " + ", gamma.extra.part)

extra.cost.string <- str_replace_all(extra.cost.string, "[.]", "")



#comparison.theta <- "theta01"

#bootstrapped.thetas.df$theta06 <- 1
#jackknifed.thetas.df$theta06 <- 1

#if(length(theta.hats)<6) { theta.hats<- c(theta.hats, theta06=1) }

# extra.cost.ci.ls <- list()



#for ( targ.alpha in c(.05, .025, .005)) {


#alpha <- .05
# alpha <- targ.alpha 
#alpha <- .005

#theta.dif.ci.df <- data.frame( theta01=c(0,0), theta02=c(0,0), theta03=c(0,0), theta04=c(0,0), theta05=c(0,0), theta06=c(0,0))

#theta.dif.ci.df <- theta.dif.ci.df[, colnames(theta.dif.ci.df)!=comparison.theta ]

# But really, we want to form intervals for the difference between, not just the values

#theta.hats.comp <- theta.hats[names(theta.hats)!=comparison.theta]

#target.theta.est <- theta.hats[names(theta.hats)==comparison.theta]

















eval.bootstrapped.string <- function(string.to.eval) {


saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/GAMS work/saved workspace.Rdata"




load(saved.workspace.path)

#target.top.crop.number <- 4

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


bootstrap.iter <- 1
# NOTE: Bootstrap iter = 0 means actual estimate
bootstrap.selection.v <- TRUE
print(length(x01))
source(paste0(code.dir, "build-model-extract-parcels.r"), local=TRUE)
# Aha! This is the solution to my source()-within-a-function problem:
# http://stackoverflow.com/questions/6863817/source-ing-an-r-script-within-a-function-and-passing-a-variable-through-rodbc
# Above is a bit hacky
print(length(x01))
combined.df <- data.frame(mget(c("y01", paste0("x", lead.zero(1:N)), 
  paste0("w", lead.zero(1:N)),  paste0("q", lead.zero(1:J)) )))
  
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

set.seed(100)

#bootstrap.replications <- 1
#bootstrap.replications <- 1500
nrow(firm.df)


bootstrap.replications.v <- 0:(length(bootstrapped.all.params.ls)-1)
# 0:300 301:600 601:900 901:1200 1201:1500
# condor_R max-entropy-bootstrap.r bootmaiz1.log &

bootstrap.replications <- max(bootstrap.replications.v)


bootstrap.selection.mat<- matrix(sample( x=nrow(firm.df), size=nrow(firm.df)*bootstrap.replications, 
  replace=TRUE), nrow=nrow(firm.df))

time.counter <- c()

# 1:bootstrap.replications

bootstrapped.extra.cost.ls <- list()

use.orig.data.for.bootstrap <- FALSE

for ( bootstrap.iter in bootstrap.replications.v) {
#cat(bootstrap.iter , "\n")

if (length(bootstrapped.all.params.ls[[bootstrap.iter + 1]] ) ==0) {next}


if( bootstrap.iter==0 ) {
  bootstrap.selection.v <- TRUE
} else {
  bootstrap.selection.v <- bootstrap.selection.mat[, bootstrap.iter]
}


#for (target.top.crop.number in c(2,4,5)) {


#source(paste0(code.dir, "build-model-extract-parcels.r"))

if(!use.orig.data.for.bootstrap) { source(paste0(code.dir, "abbreviated-dataset-building.r"), local=TRUE) }


combined.df <- data.frame(mget(c("y01", paste0("x", lead.zero(1:N)), 
  paste0("w", lead.zero(1:N)),  paste0("q", lead.zero(1:J)) )))
  
region.matrix.df <-   as.data.frame(region.matrix)


colnames(region.matrix.df) <- iconv(colnames(region.matrix.df), to="ASCII//TRANSLIT")
colnames(region.matrix.df) <- gsub("'", "", colnames(region.matrix.df) )
colnames(region.matrix.df) <- gsub("[.]", "", colnames(region.matrix.df) )
  
combined.df <- cbind(combined.df, region.matrix.df)

log.plus.one.cost <- FALSE

log10_ceiling <- function(x) {
    10^(ceiling(log10(x)))
}
# Thanks to http://stackoverflow.com/questions/7906996/algorithm-to-round-to-the-next-order-of-magnitude-in-r


for ( i in 1:N) {

  if (scale.vars.on.orig.data) { 
    input.scaling  <- input.scaling.orig[i] 
  } else {
    input.scaling  <- log10_ceiling(
      sqrt(sum((c(combined.df[, paste0("x", lead.zero(i))], 
      combined.df[, paste0("w", lead.zero(i))])^2)/(nrow(combined.df)-1)))
    )
  # Got this idea from scale() function
  }
  
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

combined.df.w.boot.params <- c(as.list(combined.df ), as.list(bootstrapped.all.params.ls[[bootstrap.iter + 1]] ))
# + 1 since the first one is the zero "true" estimate

bootstrapped.extra.cost.ls[[bootstrap.iter + 1]] <- with(combined.df.w.boot.params, eval(parse(text=string.to.eval )))


}





#set.seed(100)
# Don't need to set seed, actually

#bootstrap.replications <- 1500
bootstrap.selection.mat<- matrix(rep(1:nrow(firm.df), nrow(firm.df)), 
  nrow=nrow(firm.df))

n <- nrow(firm.df)

bootstrap.selection.mat <- matrix(bootstrap.selection.mat[-seq(1,n^2,n+1)], n-1, n)

rm(n)

# bootstrap.replications.v <- 1:nrow(firm.df)
bootstrap.replications.v <- 1:length(jackknifed.all.params.ls)
# Just in case we don't have full jackknife, let's relax this a bit

jackknife.extra.cost.ls <- list()

for ( bootstrap.iter in bootstrap.replications.v) {
#cat(bootstrap.iter, nrow(firm.df), "\n")
if (length(jackknifed.all.params.ls[[bootstrap.iter]] ) ==0) {next}


if( bootstrap.iter==0 ) {
  bootstrap.selection.v <- TRUE
} else {
  bootstrap.selection.v <- bootstrap.selection.mat[, bootstrap.iter]
}


#for (target.top.crop.number in c(2,4,5)) {


#source(paste0(code.dir, "build-model-extract-parcels.r"))

if(!use.orig.data.for.bootstrap) { source(paste0(code.dir, "abbreviated-dataset-building.r"), local=TRUE) }


combined.df <- data.frame(mget(c("y01", paste0("x", lead.zero(1:N)), 
  paste0("w", lead.zero(1:N)),  paste0("q", lead.zero(1:J)) )))
  
region.matrix.df <-   as.data.frame(region.matrix)


colnames(region.matrix.df) <- iconv(colnames(region.matrix.df), to="ASCII//TRANSLIT")
colnames(region.matrix.df) <- gsub("'", "", colnames(region.matrix.df) )
colnames(region.matrix.df) <- gsub("[.]", "", colnames(region.matrix.df) )
  
#combined.df <- cbind(combined.df, region.matrix.df)
# actually don't need region matrix for this

log.plus.one.cost <- FALSE

log10_ceiling <- function(x) {
    10^(ceiling(log10(x)))
}
# Thanks to http://stackoverflow.com/questions/7906996/algorithm-to-round-to-the-next-order-of-magnitude-in-r


for ( i in 1:N) {

  if (scale.vars.on.orig.data) { 
    input.scaling  <- input.scaling.orig[i] 
  } else {
    input.scaling  <- log10_ceiling(
      sqrt(sum((c(combined.df[, paste0("x", lead.zero(i))], 
      combined.df[, paste0("w", lead.zero(i))])^2)/(nrow(combined.df)-1)))
    )
  # Got this idea from scale() function
  }
  
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

combined.df.w.boot.params <- c(as.list(combined.df ), as.list(jackknifed.all.params.ls[[bootstrap.iter]] ))


jackknife.extra.cost.ls[[bootstrap.iter]] <- with(combined.df.w.boot.params, eval(parse(text=string.to.eval )))


}


list(bootstrapped.extra.cost.ls, jackknife.extra.cost.ls)


}










# 


BCa.CIs <- function(param.hat, param.bootstraps, param.jackknifes, alpha)  {

num.bootstrap.replications <- length(param.bootstraps)


#for ( j in names(theta.hats.comp)) {

#  bias.measure <- qnorm(sum(
#    (bootstrapped.thetas.df[, j] - bootstrapped.thetas.df[, comparison.theta])  < 
#      (theta.hats.comp[j] - target.theta.est)
#    ) / last.bootstrap)
  bias.measure <- qnorm(sum(
    (param.bootstraps)  < 
      ( param.hat )
    ) / num.bootstrap.replications)  
    
  # i.e. z_hat_0
  # 2nd answer here: http://stackoverflow.com/questions/19589191/the-reverse-inverse-of-the-normal-distribution-function-in-r
  
  #mean.jackknife <- mean(jackknifed.thetas.df[, j] - jackknifed.thetas.df[, comparison.theta])
  mean.jackknife <- mean(param.jackknifes)
  
  #jackknife.dif.vector <- jackknifed.thetas.df[, j] - jackknifed.thetas.df[, comparison.theta]
  jackknife.dif.vector <- param.jackknifes
  
  acceleration.measure <- sum( (mean.jackknife - jackknife.dif.vector)^3) /
    (6 * (sum( (mean.jackknife - jackknife.dif.vector)^2)^(3/2))  )
  # Eqn 14.15 in An Introduction to the Bootstrap
  # i.e. a_hat
  
  alpha.1 <- pnorm(
    bias.measure + (bias.measure + qnorm(alpha) ) /
      (1 - acceleration.measure * (bias.measure + qnorm(alpha) ))
  )
  # Eqn 14.10 in An Introduction to the Bootstrap
  
  alpha.2 <- pnorm(
    bias.measure + (bias.measure + qnorm(1-alpha) ) /
      (1 - acceleration.measure * (bias.measure + qnorm(1-alpha) ))
  )
  # Eqn 14.10 in An Introduction to the Bootstrap
  
#  theta.dif.ci.df[ 1,j ] <- quantile(bootstrapped.thetas.df[, j] - 
#    bootstrapped.thetas.df[, comparison.theta] ,  probs=alpha.1)
#  theta.dif.ci.df[ 2,j ] <- quantile(bootstrapped.thetas.df[, j] - 
#    bootstrapped.thetas.df[, comparison.theta] ,  probs=alpha.2)
    

  
  print(c(bias=bias.measure, acc=acceleration.measure ))
  
 c(lower=quantile( param.bootstraps ,  probs=alpha.1),
  upper= quantile( param.bootstraps,  probs=alpha.2) )


}






extra.cost.stat.calc <- function(statistic.fn, hat.estimate, bootstraps, jackknifes, alpha, ...) {


init.param.bootstraps <- sapply(bootstraps, FUN=statistic.fn, ...)
init.param.bootstraps <- init.param.bootstraps[!is.na(init.param.bootstraps)]

init.param.jackknifes <- sapply(jackknifes, FUN=statistic.fn, ...)
init.param.jackknifes <- init.param.jackknifes[!is.na(init.param.jackknifes )]

BCa.CIs(param.hat = sapply(hat.estimate, FUN=statistic.fn, ...), 
  param.bootstraps = init.param.bootstraps, 
  param.jackknifes = init.param.jackknifes , 
  alpha=alpha)
}	







optimal.shares <- str_replace_all(share.numerators, " [*] theta[0-9][0-9]", "")
optimal.shares <- str_replace_all(optimal.shares, "[.]", "")


nonoptimal.predicted.shares <- str_replace_all(paste0(share.numerators, " / ", share.denominator), "[.]", "")



optimal.minus.nonoptimal.predicted.shares <- paste0(optimal.shares,  " - (", nonoptimal.predicted.shares, ")")

# so positive means we should be using more

nonoptimal.predicted.shares.evaled.ls <- eval.bootstrapped.string(nonoptimal.predicted.shares[1])

optimal.shares.evaled.ls <- eval.bootstrapped.string(optimal.shares[1])

optimal.minus.nonoptimal.predicted.shares.evaled.ls <- eval.bootstrapped.string(optimal.minus.nonoptimal.predicted.shares[1])

extra.cost.evaled.ls<- eval.bootstrapped.string(extra.cost.string)




conditional.optimal.minus.nonoptimal.predicted.shares.evaled.ls<- list()
length(conditional.optimal.minus.nonoptimal.predicted.shares.evaled.ls) <- 2

for ( i in 1:length(optimal.minus.nonoptimal.predicted.shares.evaled.ls[[1]])) {

  conditional.optimal.minus.nonoptimal.predicted.shares.evaled.ls[[1]][[i]] <- 
  optimal.minus.nonoptimal.predicted.shares.evaled.ls[[1]][[i]][
    nonoptimal.predicted.shares.evaled.ls[[1]][[i]] > 0 | 
      optimal.shares.evaled.ls[[1]][[i]] > 0
  ]

}

for ( i in 1:length(optimal.minus.nonoptimal.predicted.shares.evaled.ls[[2]])) {

  conditional.optimal.minus.nonoptimal.predicted.shares.evaled.ls[[2]][[i]] <- 
  optimal.minus.nonoptimal.predicted.shares.evaled.ls[[2]][[i]][
    nonoptimal.predicted.shares.evaled.ls[[2]][[i]] > 0 | 
      optimal.shares.evaled.ls[[2]][[i]] > 0
  ]

}

# Two loops since first one deals with bootstrap and the other deals with jackknife





for ( i in c("extra.cost.evaled.ls",
  "conditional.optimal.minus.nonoptimal.predicted.shares.evaled.ls") ) {

  extra.cost.list.to.input <-  get(i)


  hat.estimate <- extra.cost.list.to.input[[1]][1]
  bootstraps <- extra.cost.list.to.input[[1]][!sapply(extra.cost.list.to.input[[1]], FUN=is.null)]
  jackknifes = extra.cost.list.to.input[[2]][!sapply(extra.cost.list.to.input[[2]], FUN=is.null)]

  assign(paste0("median.CI.", i),  extra.cost.stat.calc(statistic.fn=median,
    hat.estimate = hat.estimate,
    bootstraps = bootstraps, 
    jackknifes = jackknifes,
    alpha=.05
  )
  )
}



theta.ci.ls[[strsplit(target.crop, " ")[[1]][1]]] <- 
  data.frame(theta.dif.ci.df, crop=strsplit(target.crop, " ")[[1]][1])

cost.ci.ls[[strsplit(target.crop, " ")[[1]][1]]] <- median.CI.extra.cost.evaled.ls

share.ci.ls[[strsplit(target.crop, " ")[[1]][1]]] <-
  median.CI.conditional.optimal.minus.nonoptimal.predicted.shares.evaled.ls


}
# END BIG LOOP














share.ci.df.final<- do.call(rbind, share.ci.ls)
colnames(share.ci.df.final) <- c("Lower 95% CI", "Upper 95% CI")


cost.ci.df.final<- do.call(rbind, cost.ci.ls)
colnames(cost.ci.df.final) <- c("Lower 95% CI", "Upper 95% CI")




theta.ci.df.final <- do.call(rbind, theta.ci.ls)

colnames(theta.ci.df.final)[colnames(theta.ci.df.final)=="size"] <- "alpha"

theta.ci.df.final$param <- c("Seed", "Organic Fert", "Plaguicidas", "Labor", "Tractor Hrs")
# NOTE: above is a bit fragile since it relies on the column to be in exactly the right order

names(theta.ci.df.final)[names(theta.ci.df.final)=="param"] <- "Input"



#bootstrap.selection.v <- TRUE

#source(paste0(code.dir, "abbreviated-dataset-building.r"), local=TRUE) 

#length(y01)





save( theta.ci.df.final, share.ci.df.final, cost.ci.df.final,
  file="/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/thetas cost and share.Rdata")










ggplot(theta.ci.df.final[nrow(theta.ci.df.final):1,], aes(x = Input)) +
#  geom_point(size = 4) +
  geom_errorbar(aes(ymax = Upper, ymin = Lower, size=alpha, width=0, colour=alpha)) +  # as.factor(size)
  geom_hline(yintercept=1, colour="red") +
  coord_trans(y="log2") +
  scale_y_continuous(breaks=c( 1,5,10,15))  +
  facet_grid(crop ~ . ) + # facet_grid(. ~ crop ) +
  ggtitle(bquote(paste("Bootstrap CI's of ", over(theta[fertilizer], theta[j])))) +
  theme(text = element_text(size = 20))
# Thanks to http://stackoverflow.com/questions/15297814/include-text-control-characters-in-plotmath-expressions
# Ah, I see: it's putting the inputs in alphabetical order






# Below can help show distribution of thetas, but this is just for the most recently processed crop:

histogram( ~ theta01 + theta02 + theta03 + theta04 + theta05, data = bootstrapped.thetas.df,
    xlab = "", type = "density",
    panel = function(x) {
        panel.histogram(x, breaks=NULL, nint= 20)
#        panel.mathdensity(dmath = dnorm, col = "black",
#            args = list(mean=mean(x),sd=sd(x)))
    },
    )












    
# install.packages("matrixcalc")
library("matrixcalc")

beta.adding.up.mat.sym <- beta.adding.up.mat


replacements <- data.frame(greek=c("alpha", "beta", "zeta", "gamma"), N=c(M,N,J,N), M=c(M,N,J,M))
replacements <- replacements[1:3, ]
# so do not actually need for gamma

k <- 2

# ok the below is trying to reverse it:
symm.mat<-matrix(paste0(replacements$greek[k], ".", apply(X=expand.grid(lead.zero(1:max(N,M)), lead.zero(1:max(N,M))), MARGIN=1, FUN=paste, collapse=".")), nrow=max(N,M), ncol=max(N,M))

symm.mat[upper.tri(symm.mat, diag = FALSE)] <- t(symm.mat)[upper.tri(symm.mat, diag = FALSE)]
symm.mat<-symm.mat[1:replacements$N[k], 1:replacements$M[k]]


expanded.greeks.grid <- expand.grid(lead.zero(1:max(N,M)), lead.zero(1:max(N,M)))
expanded.greeks.grid <- data.frame(expanded.greeks.grid$Var2, expanded.greeks.grid$Var1)

symm.mat.2<-matrix(paste0(replacements$greek[k], ".", apply(X=expanded.greeks.grid, MARGIN=1, FUN=paste, collapse=".")), nrow=max(N,M), ncol=max(N,M))

symm.mat.2[upper.tri(symm.mat.2, diag = FALSE)] <- t(symm.mat.2)[upper.tri(symm.mat.2, diag = FALSE)]
symm.mat.2<-symm.mat.2[1:replacements$N[k], 1:replacements$M[k]]


for ( i in 1:length(c(symm.mat.2))) {
  beta.adding.up.mat.sym <- str_replace_all(beta.adding.up.mat.sym, c(symm.mat)[i], c(symm.mat.2)[i])
}







beta.neg.def.test <- function(params) {

  evaled.beta.mat<-matrix(0, ncol=nrow(beta.adding.up.mat.sym), nrow=nrow(beta.adding.up.mat.sym))

  for ( i in 1:nrow(beta.adding.up.mat.sym)) {
    for ( j in 1:nrow(beta.adding.up.mat.sym)) {
      evaled.beta.mat[i,j] <- with(as.list(params), 
        eval(parse(text=gsub("[.]", "", beta.adding.up.mat.sym[i,j]))))
    }
  }

#  is.negative.semi.definite(evaled.beta.mat)
 evaled.beta.mat

}

eigen(beta.neg.def.test(bootstrapped.all.params.ls[[1]]))$values


eigen(beta.neg.def.test(bootstrapped.all.params.ls[[1]])[-1, -1])$values

eigen(beta.neg.def.test(bootstrapped.all.params.ls[[1]]))$values

is.negative.semi.definite(beta.neg.def.test(bootstrapped.all.params.ls[[1]]), tol=1e-7)


sapply( bootstrapped.all.params.ls[sapply(bootstrapped.all.params.ls, FUN=length)>0], FUN=beta.neg.def.test )




# list(bootstrapped.extra.cost.ls, jackknife.extra.cost.ls)

table(sapply( bootstrapped.all.params.ls[sapply(bootstrapped.all.params.ls, FUN=length)>0], FUN=beta.neg.def.test ))




is.symmetric.matrix(beta.neg.def.test(bootstrapped.all.params.ls[[1]]))

is.positive.semi.definite(beta.neg.def.test(bootstrapped.all.params.ls[[1]]))


eigen(beta.neg.def.test(bootstrapped.all.params.ls[[1]]))$values

is.singular.matrix(beta.neg.def.test(bootstrapped.all.params.ls[[1]]))

S.mat <- matrix(runif(100*100), ncol=100, nrow=100)
S.mat[upper.tri(S.mat)] <- 0

is.negative.semi.definite( - S.mat %*% t(S.mat))








theta.dif.ci.df.test <- rbind(data.frame(theta.dif.ci.df, crop="haba"), data.frame(theta.dif.ci.df, crop="maiz"))

# Facet info from http://www.cookbook-r.com/Graphs/Facets_(ggplot2)/


# ADD TO SLIDIFY
ggplot(theta.dif.ci.df.test[nrow(theta.dif.ci.df.test):1,], aes(x = param)) +
#  geom_point(size = 4) +
  geom_errorbar(aes(ymax = Upper, ymin = Lower, size=size, width=0, colour=size)) +  # as.factor(size)
  geom_hline(yintercept=1, colour="red") +
  coord_trans(y="log2") +
  scale_y_continuous(breaks=c( 1,5,10,15))  +
  facet_grid(. ~ crop )





# DELETE BELOW































# DELETE BELOW





lapply(extra.cost.evaled.ls[[1]], summary)

table(extra.cost.evaled.ls[[1]][[1]]<0)

    


extra.cost.stat.calc(statistic.fn=median,
  hat.estimate = optimal.shares.evaled.ls[[1]][1],
  bootstraps = optimal.shares.evaled.ls[[1]][!sapply(optimal.shares.evaled.ls[[1]], FUN=is.null)], 
  jackknifes = optimal.shares.evaled.ls[[2]][!sapply(optimal.shares.evaled.ls[[2]], FUN=is.null)],
  alpha=.05
)




extra.cost.stat.calc(statistic.fn=median,
  hat.estimate = nonoptimal.predicted.shares.evaled.ls[[1]][1],
  bootstraps = nonoptimal.predicted.shares.evaled.ls[[1]][!sapply(nonoptimal.predicted.shares.evaled.ls[[1]], FUN=is.null)], 
  jackknifes = nonoptimal.predicted.shares.evaled.ls[[2]][!sapply(nonoptimal.predicted.shares.evaled.ls[[2]], FUN=is.null)],
  alpha=.05
)






extra.cost.stat.calc(statistic.fn=median,
  hat.estimate = nonoptimal.predicted.shares.evaled.ls[[1]][1],
  bootstraps = nonoptimal.predicted.shares.evaled.ls[[1]][!sapply(nonoptimal.predicted.shares.evaled.ls[[1]], FUN=is.null)], 
  jackknifes = nonoptimal.predicted.shares.evaled.ls[[2]][!sapply(nonoptimal.predicted.shares.evaled.ls[[2]], FUN=is.null)],
  alpha=.05
)


































extra.cost.stat.calc(statistic.fn=median,
  hat.estimate = optimal.shares.evaled.ls[[1]][1],
  bootstraps = optimal.shares.evaled.ls[[1]][!sapply(optimal.shares.evaled.ls[[1]], FUN=is.null)], 
  jackknifes = optimal.shares.evaled.ls[[2]][!sapply(optimal.shares.evaled.ls[[2]], FUN=is.null)],
  alpha=.05
)







theta.dif.ci.df <- as.data.frame(t(theta.dif.ci.df))

colnames(theta.dif.ci.df) <- c("Lower", "Upper")
theta.dif.ci.df$param <- rownames(theta.dif.ci.df)

theta.dif.ci.df$size <- targ.alpha

theta.dif.ci.ls[[as.character(targ.alpha)]]<- theta.dif.ci.df

}

theta.dif.ci.df <- do.call(rbind, theta.dif.ci.ls)

ggplot(theta.dif.ci.df[nrow(theta.dif.ci.df):1,], aes(x = param)) +
#  geom_point(size = 4) +
  geom_errorbar(aes(ymax = Upper, ymin = Lower, size=size, width=0, colour=size)) + 
  geom_hline(yintercept=1, colour="red") +
  coord_trans(y="log2") +
  scale_y_continuous(breaks=c( 1,5,10,15)) 
  
# Hmm. Maybe just say "effectively zero":
# min(as.numeric(unlist(theta.dif.ci.df)), na.rm=TRUE)+.1  
# This problem with this of course is that log is not defined at zero:
#, limits=c(0, max(as.numeric(unlist(theta.dif.ci.df)), na.rm=TRUE)*1.2)
  











extra.cost.stat.calc(mean, alpha=.05)





> extra.cost.stat.calc(mean, alpha=.05)
       bias         acc 
-0.04874201 -0.05027917 
lower.2.871821% upper.92.34685% 
     -0.4538647       0.7855172 


extra.cost.stat.calc(quantile, alpha=.05, probs=.90)
extra.cost.stat.calc(quantile, alpha=.05, probs=.75)
extra.cost.stat.calc(quantile, alpha=.05, probs=.5)
extra.cost.stat.calc(quantile, alpha=.05, probs=.25)
extra.cost.stat.calc(quantile, alpha=.05, probs=.10)

quantile(bootstrapped.extra.cost.ls[[1]], probs=.5)
quantile(bootstrapped.extra.cost.ls[[1]], probs=.25)

statistic.fn <- quantile

sapply(bootstrapped.extra.cost.ls[1], FUN=statistic.fn, probs=.10)

summary(sapply(bootstrapped.extra.cost.ls[-1], FUN=statistic.fn, probs=.1))

hist(sapply(bootstrapped.extra.cost.ls[-1], FUN=statistic.fn, probs=.1))




































library(scales)     # Need the scales package
sp + scale_y_continuous(trans=log2_trans())

# log2 coordinate transformation (with visually-diminishing spacing)
sp + coord_trans(y="log2")






set.seed(0815)
df <- data.frame(x =1:10,
                 F =runif(10,1,2),
                 L =runif(10,0,1),
                 U =runif(10,2,3))

# install.packages("ggplot2")
require(ggplot2)
ggplot(df, aes(x = x, y = F)) +
  geom_point(size = 4) +
  geom_errorbar(aes(ymax = U, ymin = L))
  
theta.dif.ci.df <- as.data.frame(t(theta.dif.ci.df))

colnames(theta.dif.ci.df) <- c("Lower", "Upper")
theta.dif.ci.df$param <- rownames(theta.dif.ci.df)

theta.dif.ci.df.test <- theta.dif.ci.df
theta.dif.ci.df.test$Upper <- theta.dif.ci.df.test$Upper +.2
theta.dif.ci.df.test$Lower <- theta.dif.ci.df.test$Lower -.2

theta.dif.ci.df.test<-  rbind(theta.dif.ci.df, theta.dif.ci.df.test)
theta.dif.ci.df.test$size <- rep(c(.2,.5), each=4)

# , y = F
ggplot(theta.dif.ci.df.test, aes(x = param)) +
#  geom_point(size = 4) +
  geom_errorbar(aes(ymax = Upper, ymin = Lower, size=size, width=.1))



geom_pointrange





























# install.packages("aod")
library("aod")


#wald.test(Sigma, b, Terms = NULL, L = NULL, H0 = NULL,  
#            df = NULL, verbose = FALSE)


b <- c(5.5, 5)
sigma <- matrix(c(.5, 4, 4, .5), ncol=2)
L <- matrix(c(1, 0, 0, -1), ncol=2)

wald.test(Sigma=sigma, b=b, L = L)

sigma <- matrix(c(.5, -4, -4, .5), ncol=2)
wald.test(Sigma=sigma, b=b, L = L)







