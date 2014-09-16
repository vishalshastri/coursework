
# 

## source("/Users/travismcarthur/git/coursework/aae-637/paper/initial-data-setup.r")



saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/GAMS work/saved workspace.Rdata"

GAMS.projdir <-  "/Users/travismcarthur/Desktop/gamsdir/projdir/"

GAMS.exe.path <- 





# load(saved.workspace.path)






target.top.crop.number <- 1

log.plus.one.cost <- FALSE



source("/Users/travismcarthur/git/coursework/aae-637/paper/build-model-extract-parcels.r")

# If want to make censoring plots:
# source("/Users/travismcarthur/git/coursework/aae-637/paper/analyze-summary-stats.r")


source("/Users/travismcarthur/git/coursework/aae-637/paper/GAMS-construction-functions.r")





other.param.support <- c(
-round( max(abs(coef(linear.sur.est.region))) * 3 ), 
0, 
round( max(abs(coef(linear.sur.est.region))) * 3 )
)


cost.err.endpoint <- round(max(abs(resid(linear.sur.est.region)[grepl("cost", 
  names(resid(linear.sur.est.region)))])) * 1.5, digits=1)


share.err.endpoint <- round(max(abs(resid(linear.sur.est.region)[!grepl("cost", 
  names(resid(linear.sur.est.region)))])) * 1.5, digits=1)
  
seq(from = -cost.err.endpoint, to = cost.err.endpoint, length.out=3)


cost.err.support <- seq(from = -cost.err.endpoint, to = cost.err.endpoint, length.out=3)

# -round( max(combined.df$cost) * 5 )

share.err.support <- seq(from = -share.err.endpoint, to = share.err.endpoint, length.out=3)


other.param.endpoint <- round( max(abs(coef(linear.sur.est.region))) * 3 , digits=1)

other.param.support <- seq(from = -other.param.endpoint, to = other.param.endpoint, length.out=5)




source("/Users/travismcarthur/git/coursework/aae-637/paper/GAMS-linear-construction.r")

# elapsed 0:08:19.548
# elapsed 0:08:26.802


theta.param.support <- qlnorm(seq(.1, .999, length.out=13), meanlog= 0, sdlog = 1.5)
theta.param.support <- theta.param.support/mean(theta.param.support)

# plot(c(0,13), c(0,13))
# rug(theta.param.support, col="red")



source("/Users/travismcarthur/git/coursework/aae-637/paper/GAMS-nonlinear-construction.r")

source("/Users/travismcarthur/git/coursework/aae-637/paper/max-entropy-postestimation.r")


