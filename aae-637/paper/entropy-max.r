





library(Matrix)



# Let's do single-equation:

# 1) form weights
# 2) define constraints

# Maybe we should sort the data by whether it is censored


  <- function( eqns,
  param.supp,
  err.supp,
  data
  )

SO we will do the supports as lists, one element of the list for each param and observation.
For errors, we need for each obs times num of equations.
For now, we will pretnd that we only get the error support from one equation.
# "err.supp" corresponds to the v's
# Let's assume that param.supp is in the correct order
There are things that need to be done once before entering into the optiomization routine, 
and then there are things that need to be done while optimizing
# I don't need to duplicate data, but just need to note which vars are the dep vars in
# each equation. Let's assume that the user gives it to us.

param.supp <- list(a=1:5, b=10:14, c=1:5, d=10:14, e=1:5, f=10:14,  g=1:5, h=10:14)

censored.ls <- list()

data <- list(y=c(9,3,0,0,4,4,0, 8), x1=runif(8), x2=runif(8))

eqns <- list(eq1=as.formula(y ~ x1 * a + x2 * b), eq2=as.formula(y ~ x1 * c + x2 * a) )

for ( i in 1:length(eqns)) {
  censored.ls[[names(eqns)[i]]] <- with(data, eval(eqns[[i]][[2]])) == 0
# isTRUE(all.equal
}



Z.mat <- t( bdiag(lapply(param.supp, FUN=matrix)) ) # (eqn 2.6, Golan et al 1997)

#err.supp <- list(1:5, 10:14, 1:5, 10:14, 1:5, 10:14,  1:5, 10:14,
#1:5, 10:14, 1:5, 10:14, 1:5, 10:14,  1:5, 10:14)

err.supp <- rep(list(c(-100, 0, 100)), times=length(data[[1]])*length(eqns)) 
# Note: assuming all eqns have same # of observations
# y <- c(9,3,0,0,4,4,0, 8)

#y.censored <- y == 0

y.censored <- unlist(censored.ls)

w1.vec <- unlist(err.supp[!y.censored])

w2.vec <- unlist(err.supp[y.censored])

V1.mat <- t( bdiag(lapply(err.supp[!y.censored], FUN=matrix)) )

V2.mat <- t( bdiag(lapply(err.supp[ y.censored], FUN=matrix)) )



# param.supp <- list(1:4, 10:14, 1:3, 10:14, 1:9, 10:14,  1:5, 10:14)

# Ideally, we would like to pre-specify weights in the function arguments

param.weights <- rep(1/sapply(param.supp, FUN=length), sapply(param.supp, FUN=length)) 


params.point <- Z.mat %*% param.weights

params.point <- as.list(params.point)

names(params.point) <- names(param.supp)

# data <- as.data.frame(data)
# delete above

# uncensored.data <- as.list(data[!y.censored, ] )

params.point.and.data <- c(params.point, data )

#test.formula <- list(as.formula(y ~ x1 + x2), as.formula(y ~ x1 * x2) )
# params.point.and.data <- list(x1=1:100, x2=201:300)

test.formula <- eqns

eq.constraint <- function() {

  
#  dep.var.uncensored <- dep.var[dep.var > 0]
  
  ### USE WITH() !!!!!
  
  uncensored.evaled.RHS <- with( params.point.and.data, 
    lapply(test.formula, FUN=function(x) eval(x[[3]])[eval(x[[2]])>0] )
  )
  
  uncensored.evaled.LHS <- with( params.point.and.data, 
    lapply(test.formula, FUN=function(x) eval(x[[2]])[eval(x[[2]])>0] )
  )
  
  dep.var.uncensored.constraint <- 
    unlist(uncensored.evaled.RHS) + V1.mat %*% w1.vec - unlist(uncensored.evaled.LHS)
  # (eqn 4.4, Golan et al 1997)
    
  param.add.up.constraint <- 
    kronecker(diag(length(param.supp)), 
      matrix(1, ncol=length(param.supp[[1]]) )) %*% param.weights - 1
      # (eqn 4.6, Golan et al 1997)
 
  w1.constraint <- 
    kronecker(diag(sum(!y.censored)), 
      matrix(1, ncol=length(err.supp[[1]]) )) %*% w1.vec - 1 # (eqn 4.7, Golan et al 1997)
  
  w2.constraint <- 
    kronecker(diag(sum( y.censored)), 
      matrix(1, ncol=length(err.supp[[1]]) )) %*% w2.vec - 1 # (eqn 4.8, Golan et al 1997)

  # NOTE: the err.supp[[1]] code assumes that the number of support points for each parameter
  # and error weight is the same.

}

ineq.constraint <- function() {

  censored.evaled.RHS <- with( params.point.and.data, 
    lapply(test.formula, FUN=function(x) eval(x[[3]])[eval(x[[2]]) == 0] )
  )
  
  censored.evaled.LHS <- with( params.point.and.data, 
    lapply(test.formula, FUN=function(x) eval(x[[2]])[eval(x[[2]]) == 0] )
  )
  
  dep.var.censored.constraint <- 
    unlist(censored.evaled.RHS) + V2.mat %*% w2.vec - unlist(censored.evaled.LHS)
  # (eqn 4.4, Golan et al 1997)

}


objective.fn <- function(param.weights, err.weights) {
  return( - param.weights %*% log(param.weights) - err.weights %*% log(err.weights) )
  # The above is to be maximized
}

  

  

























res1 <- nloptr( x0=c(1.234,5.678),
eval_f=eval_f0,
lb = c(-Inf,0),
ub = c(Inf,Inf),
eval_g_ineq = eval_g0,
opts = list("algorithm"="NLOPT_LN_COBYLA"),
a = a,
b = b )





res1 <- nloptr( x0=c(1.234,5.678),
eval_f=eval_f0,
lb = c(-Inf,0),
ub = c(Inf,Inf),
eval_g_ineq = eval_g0,
opts = list("algorithm"="NLOPT_LN_COBYLA"),
a = a,
b = b )








