
last.bootstrap <- 598


bootstrapped.thetas.ls <- vector(mode="list", length=0) # last.bootstrap+1



for ( bootstrap.iter in as.character(0:last.bootstrap) ) {


# GAMS.nonlinear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytest.lst")

GAMS.nonlinear.results<- readLines(paste0(GAMS.projdir, "GMEnonlinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(as.numeric(bootstrap.iter), width = 5, flag = "0"), ".lst"))


GAMS.nonlinear.results.params<- GAMS.nonlinear.results[grep("parameters to be estimated$", GAMS.nonlinear.results)]

GAMS.nonlinear.results.params <- GAMS.nonlinear.results.params[grep("VARIABLE", GAMS.nonlinear.results.params)]

GAMS.nonlinear.results.params.names <- gsub("[.]L", "", str_extract(GAMS.nonlinear.results.params, "[^ ]*[.]L") )


GAMS.nonlinear.results.params.numbers <- as.numeric(gsub("  parameters to be estimated", "",
  str_extract(GAMS.nonlinear.results.params, "[^ ]*  parameters to be estimated") ) )


GAMS.nonlinear.results.params.full <- GAMS.nonlinear.results.params.numbers
names(GAMS.nonlinear.results.params.full) <- GAMS.nonlinear.results.params.names

bootstrapped.thetas.ls[[bootstrap.iter]] <- 
  GAMS.nonlinear.results.params.full[grepl("theta", names(GAMS.nonlinear.results.params.full))]
  

}




last.jackknife <- 475


jackknifed.thetas.ls <- vector(mode="list", length=0) # last.bootstrap+1



for ( bootstrap.iter in as.character(1:last.jackknife) ) {


# GAMS.nonlinear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytest.lst")

GAMS.nonlinear.results<- readLines(paste0(GAMS.projdir, "GMEnonlinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(as.numeric(bootstrap.iter), width = 5, flag = "0"), ".lst"))


GAMS.nonlinear.results.params<- GAMS.nonlinear.results[grep("parameters to be estimated$", GAMS.nonlinear.results)]

GAMS.nonlinear.results.params <- GAMS.nonlinear.results.params[grep("VARIABLE", GAMS.nonlinear.results.params)]

GAMS.nonlinear.results.params.names <- gsub("[.]L", "", str_extract(GAMS.nonlinear.results.params, "[^ ]*[.]L") )


GAMS.nonlinear.results.params.numbers <- as.numeric(gsub("  parameters to be estimated", "",
  str_extract(GAMS.nonlinear.results.params, "[^ ]*  parameters to be estimated") ) )


GAMS.nonlinear.results.params.full <- GAMS.nonlinear.results.params.numbers
names(GAMS.nonlinear.results.params.full) <- GAMS.nonlinear.results.params.names

jackknifed.thetas.ls[[bootstrap.iter]] <- 
  GAMS.nonlinear.results.params.full[grepl("theta", names(GAMS.nonlinear.results.params.full))]
  

}


jackknifed.thetas.df <- as.data.frame(do.call(rbind, jackknifed.thetas.ls))




theta.hats <- bootstrapped.thetas.ls[[1]]

bootstrapped.thetas.ls[[1]] <- NULL

table(sapply(bootstrapped.thetas.ls, FUN=length))


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






# BELOW IDS FOR DIFFERENCES:

comparison.theta <- "theta01"


#alpha <- .05
alpha <- .025
#alpha <- .005

theta.dif.ci.df <- data.frame( theta01=c(0,0), theta02=c(0,0), theta03=c(0,0), theta04=c(0,0), theta05=c(0,0))

theta.dif.ci.df <- theta.dif.ci.df[, colnames(theta.dif.ci.df)!=comparison.theta ]

# But really, we want to form intervals for the difference between, not just the values

theta.hats.comp <- theta.hats[names(theta.hats)!=comparison.theta]

target.theta.est <- theta.hats[names(theta.hats)==comparison.theta]

for ( j in names(theta.hats.comp)) {

  bias.measure <- qnorm(sum(
    (bootstrapped.thetas.df[, j] - bootstrapped.thetas.df[, comparison.theta])  < 
      (theta.hats.comp[j] - target.theta.est)
    ) / last.bootstrap)
  # i.e. z_hat_0
  # 2nd answer here: http://stackoverflow.com/questions/19589191/the-reverse-inverse-of-the-normal-distribution-function-in-r
  
  mean.jackknife <- mean(jackknifed.thetas.df[, j] - jackknifed.thetas.df[, comparison.theta])
  
  jackknife.dif.vector <- jackknifed.thetas.df[, j] - jackknifed.thetas.df[, comparison.theta]
  
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
  
  theta.dif.ci.df[ 1,j ] <- quantile(bootstrapped.thetas.df[, j] - 
    bootstrapped.thetas.df[, comparison.theta] ,  probs=alpha.1)
  theta.dif.ci.df[ 2,j ] <- quantile(bootstrapped.thetas.df[, j] - 
    bootstrapped.thetas.df[, comparison.theta] ,  probs=alpha.2)
  
  print(c(bias=bias.measure, acc=acceleration.measure ))


}

theta.dif.ci.df


































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









