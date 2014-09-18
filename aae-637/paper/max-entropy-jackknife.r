




saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/GAMS work/saved workspace.Rdata"

GAMS.projdir <-  "/Users/travismcarthur/Desktop/gamsdir/projdir/"

GAMS.exe.path <- "/Applications/GAMS/gams24.1_osx_x64_64_sfx/gams"

# GAMS.projdir.subdir <-  "/Users/travismcarthur/Desktop/gamsdir/projdir/bootstrap/"







load(saved.workspace.path)


target.top.crop.number <- 4


log.plus.one.cost <- FALSE

bootstrap.iter <- 1
# NOTE: Bootstrap iter = 0 means actual estimate
bootstrap.selection.v <- TRUE
source("/Users/travismcarthur/git/coursework/aae-637/paper/build-model-extract-parcels.r")
# Above is a bit hacky


set.seed(100)

#bootstrap.replications <- 1500
nrow(firm.df)


bootstrap.selection.mat<- matrix(rep(1:nrow(firm.df), nrow(firm.df)), 
  nrow=nrow(firm.df))
  
#bootstrap.selection.mat<- matrix(rep(1:13, 13), 
#  nrow=13)

n <- nrow(firm.df)

bootstrap.selection.mat <- matrix(bootstrap.selection.mat[-seq(1,n^2,n+1)], n-1, n)

rm(n)


# Thanks to http://stackoverflow.com/questions/18839090/removing-diagonal-elements-from-matrix-in-r


time.counter <- c()



# 1:bootstrap.replications
for ( bootstrap.iter in 19:ncol(bootstrap.selection.mat)) {

if( bootstrap.iter==0 ) {
  bootstrap.selection.v <- TRUE
} else {
  bootstrap.selection.v <- bootstrap.selection.mat[, bootstrap.iter]
}


#for (target.top.crop.number in c(2,4,5)) {


source("/Users/travismcarthur/git/coursework/aae-637/paper/build-model-extract-parcels.r")

# If want to make censoring plots:
# source("/Users/travismcarthur/git/coursework/aae-637/paper/analyze-summary-stats.r")


source("/Users/travismcarthur/git/coursework/aae-637/paper/GAMS-construction-functions.r")



cost.err.endpoint <- round(max(abs(resid(linear.sur.est.region)[grepl("cost", 
  names(resid(linear.sur.est.region)))])) * 1.4, digits=1)


share.err.endpoint <- round(max(abs(resid(linear.sur.est.region)[!grepl("cost", 
  names(resid(linear.sur.est.region)))])) * 1.4, digits=1)
  

cost.err.support <- seq(from = -cost.err.endpoint, to = cost.err.endpoint, length.out=3)

# -round( max(combined.df$cost) * 5 )

share.err.support <- seq(from = -share.err.endpoint, to = share.err.endpoint, length.out=3)

other.param.endpoint <- round( max(abs(coef(linear.sur.est.region))) * 3 , digits=1)

other.param.support <- seq(from = -other.param.endpoint, to = other.param.endpoint, length.out=5)


linear.GAMS.output <- FALSE


source("/Users/travismcarthur/git/coursework/aae-637/paper/GAMS-linear-construction.r")

# elapsed 0:08:19.548
# elapsed 0:08:26.802


theta.param.support <- qlnorm(seq(.1, .999, length.out=13), meanlog= 0, sdlog = 1.5)
theta.param.support <- theta.param.support/mean(theta.param.support)

# plot(c(0,13), c(0,13))
# rug(theta.param.support, col="red")


source("/Users/travismcarthur/git/coursework/aae-637/paper/jackknife-GAMS-nonlin-constr.r")
# /Users/travismcarthur/git/coursework/aae-637/paper/GAMS-nonlinear-construction.r

run.nonlinear.from.shell <-paste0("cd ", GAMS.projdir, "\n", 
   GAMS.exe.path, " ", 
   "GMEnonlinearjackknife", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".gms", 
   " Ps=0 suppress=1")

system(run.nonlinear.from.shell)

time.counter <- c(time.counter, Sys.time())
save(time.counter, file=paste0(GAMS.projdir, strsplit(target.crop, " ")[[1]][1], 
  "jackknifestrapcounter.Rdata"))


}


# }



load(paste0(GAMS.projdir, strsplit(target.crop, " ")[[1]][1], 
  "jackknifestrapcounter.Rdata"))
diff(time.counter)
hist(diff(time.counter))

  
  
