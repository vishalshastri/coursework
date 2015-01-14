
err.support.dem.eqns <- list()

max.abs.other.param <- 0

max.num.iterations <- 30000

eq.r.squared <- list()


for (targ.eqn in 1:length(demand.eqns) ) {



all.params.tobit <- unique(unlist(str_extract_all(demand.eqns[[targ.eqn]], 
"(s.[0-9][0-9].[0-9][0-9])|(b.y.[0-9][0-9])|(b.[0-9][0-9])|(b.y.y)|(d.[0-9][0-9].[0-9][0-9])|(c.[0-9][0-9] )|(c.[0-9][0-9].[0-9][0-9])"
  ))
)

all.params.tobit <- gsub(" ", "", all.params.tobit )


tobit.loglik <- function(param)  {
  sigma <- param[1]
  for ( i in 1:(length(param)-1)) {
    assign( all.params.tobit[i], param[i+1])
  }
  
  
  pred <- eval(parse(text=demand.eqns[[targ.eqn]]))
  if ( targ.eqn == (N + 1)) {
    y <- E.y01.data
  } else {
    y <- get(paste0("x", lead.zero(targ.eqn)))/y01
  }

  z <- 0
  result <-  ifelse(y > z, dnorm(y, pred, sigma, log = T),
                      pnorm((z - pred)/sigma, log = T))
#  return(ifelse(!is.finite(sum(result)), -10^30, sum(result)))
  sum(result)
}
# Thanks to http://tolstoy.newcastle.edu.au/R/e11/help/10/08/3925.html


predicted.nls <- FALSE

nls.sgm <- function(param)  {
  for ( i in 1:length(param)) {
    assign( all.params.tobit[i], param[i])
  }
  pred <- eval(parse(text=demand.eqns[[targ.eqn]]))
  if ( targ.eqn == (N + 1)) {
    y <- E.y01.data
  } else {
    y <- get(paste0("x", lead.zero(targ.eqn)))/y01
  }
  
  if (predicted.nls) {
    return(pred)
  } else {  
    return(sum(((y-pred))^2))
    # Important: only fitting on posi values if we use [y>0]
  }
}





regression.nls <- optim( par    = jitter(
                 ifelse(grepl("b[.]y[.][0-9][0-9]", all.params.tobit), 
                   ifelse(targ.eqn == (N + 1),  mean(E.y01.data), 
                     mean(get(paste0("x", lead.zero(targ.eqn)))/y01)),
                    0)),
			   fn      = nls.sgm ,
			   method  = "BFGS", #"Nelder-Mead", # "BFGS",
               control = list(fnscale = 1, trace=5, maxit=max.num.iterations))  
               

# 1789.741209

#regression.tobit <- optim( par    = c(sqrt(nls.sgm(regression.nls$par)/(length(y01)-length(regression.nls$par))),
#                  regression.nls$par),
#			   fn      = tobit.loglik ,
#			   method  = "BFGS", #"Nelder-Mead", # "BFGS", "BFGS", #
#               control = list(fnscale = -1, trace=5, maxit=max.num.iterations))  #maximize likelihood 


predicted.nls <- TRUE

#max.resid <- max( 
#  abs(get(paste0("x", lead.zero(targ.eqn)))/y01 - nls.sgm(regression.tobit$par[-1]))[
#    !(get(paste0("x", lead.zero(targ.eqn)))/y01==0 & nls.sgm(regression.tobit$par[-1])<0)
#  ]
#)


#max.resid <- max( 
#  abs(get(paste0("x", lead.zero(targ.eqn)))/y01 - nls.sgm(regression.nls$par))[
#    !(get(paste0("x", lead.zero(targ.eqn)))/y01==0 & nls.sgm(regression.nls$par)<0)
#  ]
#)

eq.r.squared[[targ.eqn]] <- 1 - regression.nls$value/(ifelse(targ.eqn == (N + 1),  var(E.y01.data), 
                     var(get(paste0("x", lead.zero(targ.eqn)))/y01)) * length(y01))

# since if the actual value is zero and the predicted value is neg, then we have made a
# correct prediction under the entropy max framework

#err.support.dem.eqns[[targ.eqn]] <- round( c(-max.resid, 0, max.resid) * 1.5, digits=4)
# times 2 since we want to be conservative with our error support bounds

# NOTE: Ok, we are going to go with just using the range:
max.resid <- (ifelse(targ.eqn == (N + 1),  max(E.y01.data), 
                     max(get(paste0("x", lead.zero(targ.eqn)))/y01)))
err.support.dem.eqns[[targ.eqn]] <- round( c(-max.resid, 0, max.resid) * 1.5, digits=4)

#max.abs.other.param <- max(c(max.abs.other.param, abs(regression.tobit$par[-1])))
# Also, the above MLE tobit does not equal the marginal effects, so taking the parameter
# values naively is no good
max.abs.other.param <- max(c(max.abs.other.param, abs(regression.nls$par)))

max.abs.other.param <- 30
# just set to 30 for now

}






#  abs(get(paste0("x", lead.zero(targ.eqn)))/y01 )[
#    !(get(paste0("x", lead.zero(targ.eqn)))/y01==0 & nls.sgm(regression.nls$par)<0)
#  ][3142]

#  nls.sgm(regression.nls$par)[
#    !(get(paste0("x", lead.zero(targ.eqn)))/y01==0 & nls.sgm(regression.nls$par)<0)
#  ][3142]



