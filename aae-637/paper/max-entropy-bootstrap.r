
# 

## source("/Users/travismcarthur/git/coursework/aae-637/paper/initial-data-setup.r")


functional.form <- "SGM" # OR TRANSLOG


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

if (!synthetic.data) { 
  intended.seed <- 100 
  start.nonlin.from.ignorance <- TRUE
  global.max.seed <- 5
  do.SUR <- TRUE
  include.cost.fn <- TRUE
  only.cost.fn <- FALSE
  generate.synth.data.from.cost.fn <- FALSE
  start.at.true.xi <- FALSE
}



target.top.crop.number <- 1

#Including zero cost:
#Potatoes	4,058
#Maize	3,440
#Barley	2,048
#Wheat	1,647
#Fava Beans	1,484



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
for ( bootstrap.iter in 0) {

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


linear.GAMS.output <- TRUE
# linear.GAMS.output <- FALSE



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



# system(paste0("cd ", GAMS.projdir, "\n", "ls" ) )

if (functional.form =="TRANSLOG") {
run.linear.from.shell <-paste0("cd ", GAMS.projdir, "\n", 
   GAMS.exe.path, " ", 
   "GMElinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".gms", 
   " Ps=0 suppress=1")
}

if (functional.form =="SGM") {
run.linear.from.shell <-paste0("cd ", GAMS.projdir, "\n", 
   GAMS.exe.path, " ", 
   "sgmGMElinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".gms", 
   " Ps=0 suppress=1")
}

if (!start.nonlin.from.ignorance) {
  system(run.linear.from.shell)
}


# elapsed 0:08:19.548
# elapsed 0:08:26.802


theta.param.support <- qlnorm(seq(.1, .999, length.out=13), meanlog= 0, sdlog = 1.5)
theta.param.support <- theta.param.support/mean(theta.param.support)
xi.param.support <- theta.param.support
#xi.param.support <- c(-8, 1, 10)
# NOTE: Changing support dratically

# plot(c(0,13), c(0,13))
# rug(theta.param.support, col="red")

if (functional.form =="TRANSLOG") {
  source(paste0(code.dir, "GAMS-nonlinear-construction.r"))
}

if (functional.form =="TRANSLOG") {
run.nonlinear.from.shell <-paste0("cd ", GAMS.projdir, "\n", 
   GAMS.exe.path, " ", 
   "GMEnonlinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".gms", 
   " Ps=0 suppress=1")
}

if (functional.form =="SGM") {
  source(paste0(code.dir, "sgm-GAMS-nonlinear-construction.r"))
}

if (functional.form =="SGM") {
run.nonlinear.from.shell <-paste0("cd ", GAMS.projdir, "\n", 
   GAMS.exe.path, " ", 
   "sgmGMEnonlinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".gms", 
   " Ps=0 suppress=1")
}




system(run.nonlinear.from.shell)



#time.counter <- c(time.counter, Sys.time())
#save(time.counter, file=paste0(GAMS.projdir, strsplit(target.crop, " ")[[1]][1], 
#  "bootstrapcounter.Rdata"))


}


# }





# source("/Users/travismcarthur/git/coursework/aae-637/paper/max-entropy-postestimation.r")

# old: rvhess = 100
# old: rvstlm = 1.1

# Old-ish: lfmxns = 20000
# Old-ish: lfnicr = 10000
# Old-ish: lfstal = 10000



#load("/Users/travismcarthur/Desktop/gamsdir/projdir/Trigobootstrapcounter.Rdata")
#diff(time.counter)
#hist(diff(time.counter))




