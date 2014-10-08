
library("stringr")

condor.gams.dir <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Condor/10-7-projdir/home/c/cschmidt/TravisImInYourInternets/gamsdir/projdir/"


listed.files <- list.files(condor.gams.dir )

#target.estimation <- "GMEnonlinearMaiz"
# target.estimation <- "GMEnonlinearHaba"
 target.estimation <- "GMEnonlinearTrigo"

target.files <- listed.files[grepl( paste0(target.estimation, "[0-9]{5}[.]lst") , listed.files)]

# check to make sure that we have the 00000 estimation

target.files <- paste0(condor.gams.dir ,  target.files)


bootstrapped.thetas.ls <- vector(mode="list", length=0) # last.bootstrap+1

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

bootstrapped.thetas.ls[[bootstrap.iter]] <- 
  GAMS.nonlinear.results.params.full[grepl("theta", names(GAMS.nonlinear.results.params.full))]
  

}




#last.jackknife <- 475


listed.files <- list.files(condor.gams.dir )

#target.estimation <- "GMEnonlinearjackknifeMaiz"
#target.estimation <- "GMEnonlinearjackknifeHaba"
target.estimation <- "GMEnonlinearjackknifeTrigo"

target.files <- listed.files[grepl( paste0(target.estimation, "[0-9]{5}[.]lst") , listed.files)]

# check to make sure that we have the 00000 estimation

target.files <- paste0(condor.gams.dir ,  target.files)



jackknifed.thetas.ls <- vector(mode="list", length=0) # last.bootstrap+1



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

jackknifed.thetas.ls[[bootstrap.iter]] <- 
  GAMS.nonlinear.results.params.full[grepl("theta", names(GAMS.nonlinear.results.params.full))]
  

}



table(sapply(jackknifed.thetas.ls, FUN=length))

jackknifed.thetas.ls[sapply(jackknifed.thetas.ls, FUN=length)==10]

bootstrapped.thetas.ls <- lapply(bootstrapped.thetas.ls, FUN=function(x) {
x[!duplicated(names(x))]
})


jackknifed.thetas.df <- as.data.frame(do.call(rbind, jackknifed.thetas.ls))





theta.hats <- bootstrapped.thetas.ls[[1]]

bootstrapped.thetas.ls[[1]] <- NULL
# Here is where the 00000 ( i.e. the actual estimate) comes in 

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

# BELOW IDS FOR DIFFERENCES:

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







