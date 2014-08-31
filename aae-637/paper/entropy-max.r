



#install.packages("nloptr")
library("nloptr")

library("Matrix")




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















eq.constraint <- function( x0, param.supp, err.supp, eqns, data, x0.names) {
  
  ### USE WITH() !!!!!
  
  names(x0) <- x0.names
  
  censored.ls <- list()
  
  for ( i in 1:length(eqns)) {
    censored.ls[[names(eqns)[i]]] <- with(data, eval(eqns[[i]][[2]])) == 0
  # isTRUE(all.equal
  }

  Z.mat <- t( bdiag(lapply(param.supp, FUN=matrix)) ) # (eqn 2.6, Golan et al 1997)

  y.censored <- unlist(censored.ls)
  
  w1.vec <- x0[grepl( "err.weights", names(x0))][!rep(y.censored, each=length(err.supp[[1]]))]
# w2.vec <- unlist(err.supp[y.censored])

  V1.mat <- t( bdiag(lapply(err.supp[!y.censored], FUN=matrix)) )

# param.weights <- rep(1/sapply(param.supp, FUN=length), sapply(param.supp, FUN=length)) 
  param.weights <- x0[!grepl( "err.weights", names(x0))]

  params.point <- Z.mat %*% param.weights

  params.point <- as.list(params.point)

  names(params.point) <- names(param.supp)

  params.point.and.data <- c(params.point, data)
  
  uncensored.evaled.RHS <- with( params.point.and.data, 
    lapply(eqns, FUN=function(x) eval(x[[3]])[eval(x[[2]])>0] )
  )
  
  uncensored.evaled.LHS <- with( params.point.and.data, 
    lapply(eqns, FUN=function(x) eval(x[[2]])[eval(x[[2]])>0] )
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
  
  w2.vec <- x0[grepl( "err.weights", names(x0))][rep(y.censored, each=length(err.supp[[1]]))]
  
  w2.constraint <- 
    kronecker(diag(sum( y.censored)), 
      matrix(1, ncol=length(err.supp[[1]]) )) %*% w2.vec - 1 # (eqn 4.8, Golan et al 1997)

  # NOTE: the err.supp[[1]] code assumes that the number of support points for each parameter
  # and error weight is the same.
  
  return(
    c(as.vector(dep.var.uncensored.constraint),
      as.vector(param.add.up.constraint),
      as.vector(w1.constraint),
      as.vector(w2.constraint)
    )
  )

}

ineq.constraint <- function( x0, param.supp, err.supp, eqns, data, x0.names) {

  names(x0) <- x0.names
  
  censored.ls <- list()

  for ( i in 1:length(eqns)) {
    censored.ls[[names(eqns)[i]]] <- with(data, eval(eqns[[i]][[2]])) == 0
  # isTRUE(all.equal
  }

  Z.mat <- t( bdiag(lapply(param.supp, FUN=matrix)) ) # (eqn 2.6, Golan et al 1997)

  y.censored <- unlist(censored.ls)
  
  w2.vec <- x0[grepl( "err.weights", names(x0))][rep(y.censored, each=length(err.supp[[1]]))]
# w2.vec <- unlist(err.supp[y.censored])

  V2.mat <- t( bdiag(lapply(err.supp[ y.censored], FUN=matrix)) )

# param.weights <- rep(1/sapply(param.supp, FUN=length), sapply(param.supp, FUN=length)) 
  param.weights <- x0[!grepl( "err.weights", names(x0))]

  params.point <- Z.mat %*% param.weights

  params.point <- as.list(params.point)

  names(params.point) <- names(param.supp)

  params.point.and.data <- c(params.point, data )

  censored.evaled.RHS <- with( params.point.and.data, 
    lapply(eqns, FUN=function(x) eval(x[[3]])[eval(x[[2]]) == 0] )
  )
  
  censored.evaled.LHS <- with( params.point.and.data, 
    lapply(eqns, FUN=function(x) eval(x[[2]])[eval(x[[2]]) == 0] )
  )
  
  dep.var.censored.constraint <- 
    unlist(censored.evaled.RHS) + V2.mat %*% w2.vec - unlist(censored.evaled.LHS)
  # (eqn 4.4, Golan et al 1997)
  
  return(  as.vector(dep.var.censored.constraint) )
  # ok, now we have switched the sign

}


objective.fn <- function(x0, param.supp, err.supp, eqns, data, x0.names) {
  # Need to put "param.supp, err.supp, eqns, data as arguments even
  # though we don't use them so the optimizer doesnt throw an error
  names(x0) <- x0.names
  param.weights <- x0[!grepl( "err.weights", names(x0))]
  err.weights <- x0[ grepl( "err.weights", names(x0))]
  return( - as.vector(- param.weights %*% log(param.weights) - err.weights %*% log(err.weights)) )
  # The above is to be maximized
}
# Note that we are using a minimizer, so I have made this the neg


# names should be param


#param.supp <- list(a=(-10):10, b=(-10):10, c=(-10):10)
# param.supp <- list(a= (-10):10, b=1:5, c=1:5, d=10:14, e=1:5, f=10:14,  g=1:5, h=10:14)
# param.supp <- list(a=c(-10, -5, 0, 5, 10), b=c(-10, -5, 0, 5, 10))
param.supp <- list(a=c(-50,  0,  50), b=c(-50,  0,  50))

observations.test <- 100


data <- list(y=c(), x1=runif(observations.test) - .5, x2=runif(observations.test) - .5)

data$y <- data$x1 * 5 + data$x2 * -2 + runif(observations.test)/5 - .5

data$y[data$y<0] <- 0

eqns <- list(eq1=as.formula(y ~ x1 * a + x2 * b))

#eqns <- list(eq1=as.formula(y ~ x1 * a + x2 * b), eq2=as.formula(y ~ x1 * c + x2 * a) )

err.supp <- rep(list(c(-20, 0, 20)), times=length(data[[1]])*length(eqns)) 

param.weights <- jitter( rep(1/sapply(param.supp, FUN=length), sapply(param.supp, FUN=length)) )

err.weights <- rep(1/sapply(err.supp, FUN=length), sapply(err.supp, FUN=length)) 

names(err.weights) <- rep("err.weights", length(err.weights))

x0 <- c(param.weights, err.weights)
x0.names <- names(x0)



ineq.constraint( x0=x0, param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names)
eq.constraint( x0=x0, param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names)
objective.fn( x0=x0, param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names)


ineq.constraint( x0=x0-runif(length(x0), min=-.1, max=.1), param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names)


optim.test <- nloptr( x0 = x0,
  eval_f = objective.fn,
  eval_g_ineq = ineq.constraint,
  eval_g_eq = eq.constraint,
  opts = list("algorithm"="NLOPT_GN_ISRES", maxeval=5000, print_level=1),
  ub=rep(1, length(x0)),
  lb=rep(0, length(x0)),
  param.supp = param.supp, 
  err.supp = err.supp, 
  eqns = eqns, 
  data = data,
  x0.names = x0.names
)

-34.0347

-6.4377516497364 

objective.fn( x0=optim.test$solution, param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names)

objective.fn( x0=unname(x0), param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names)


ineq.constraint( x0=optim.test$solution, param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names)
eq.constraint( x0=optim.test$solution, param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names)


best: -36.11968

let zero always take 0.1

error.weight.start.vals.fn <- function(x0, param.supp, err.supp, eqns, data, x0.names) {

  names(x0) <- x0.names
  
  Z.mat <- t( bdiag(lapply(param.supp, FUN=matrix)) ) # (eqn 2.6, Golan et al 1997)

# param.weights <- rep(1/sapply(param.supp, FUN=length), sapply(param.supp, FUN=length)) 
  param.weights <- x0[!grepl( "err.weights", names(x0))]

  params.point <- Z.mat %*% param.weights

  params.point <- as.list(params.point)

  names(params.point) <- names(param.supp)

  params.point.and.data <- c(params.point, data)

  evaled.RHS <- with( params.point.and.data, 
    lapply(eqns, FUN=function(x) eval(x[[3]]) )
  )
  
  evaled.LHS <- with( params.point.and.data, 
    lapply(eqns, FUN=function(x) eval(x[[2]]) )
  )
  
  err.remainder <- unlist(evaled.RHS) - unlist(evaled.LHS)
  
  err.guess <- err.supp
    
  for ( i in 1:length(err.supp) ) {
    err.guess[[i]][1] <- ( 0.9 * err.supp[[i]][1] - err.remainder[i]) /
      (err.supp[[i]][1] - err.supp[[i]][3])
      
    err.guess[[i]][3] <- 0.9 - err.guess[[i]][1]
    
    err.guess[[i]][2] <- .1
  }
 #   err.remainder 
  err.guess
#  err.remainder
    
}
  
error.weight.start.vals <- error.weight.start.vals.fn( x0=unname(x0), param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names)
    
error.weight.start.vals <- unlist(error.weight.start.vals)
names(error.weight.start.vals) <- rep("err.weights", length(error.weight.start.vals))
x0 <- c(param.weights, error.weight.start.vals)
x0.names <- names(x0)
  

ineq.constraint( x0=x0, param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names)
eq.constraint( x0=x0, param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names)

as.data.frame(data)[2, "y"]
as.data.frame(data)[2, "x1"] * params.point$a + as.data.frame(data)[2, "x2"] * params.point$b

error.weight.start.vals[4:6] %*% c(-20, 0, 20)

params.point <- Z.mat %*% param.weights

as.data.frame(data)[1, "x1"] * params.point$a + as.data.frame(data)[1, "x2"] * params.point$b +
  error.weight.start.vals[1:3] %*% c(-20, 0, 20) - as.data.frame(data)[1, "y"]



as.data.frame(data)[2, "x1"] * params.point$a + as.data.frame(data)[2, "x2"] * params.point$b +
  error.weight.start.vals[4:6] %*% c(-20, 0, 20) - as.data.frame(data)[2, "y"]






NLOPT_LN_SBPLX
NLOPT_LN_COBYLA
NLOPT_LN_BOBYQA


x0.slanted <- x0
x0.slanted[1:10] <- c(1,.01,.01,.01,.01, .01,.01,.01,.01,1)

x0.jittered <- x0 + runif(length(x0), min=-.2, max=.2)



# x0 <- c(param.weights, err.weights)
error.weight.start.vals <- unlist(error.weight.start.vals)
names(error.weight.start.vals) <- rep("err.weights", length(error.weight.start.vals))
x0 <- c(param.weights, error.weight.start.vals)
x0.names <- names(x0)

#x0.jittered

# NLOPT_LN_COBYLA
local_opts <- list( "algorithm" = "NLOPT_LN_SBPLX",
  "xtol_rel" = 1.0e-7 )
opts <- list( "algorithm" = "NLOPT_LN_AUGLAG",
  "xtol_rel" = 1.0e-7,
  "maxeval" = 5000,
  "local_opts" = local_opts,
  print_level=1 )
test.auglag <- nloptr( 
  x0 = x0,
  eval_f = objective.fn,
  eval_g_ineq = ineq.constraint,
  eval_g_eq = eq.constraint,
  opts = opts,
  ub=rep(1, length(x0)),
  lb=rep(0, length(x0)),
  param.supp = param.supp, 
  err.supp = err.supp, 
  eqns = eqns, 
  data = data,
  x0.names = x0.names
)

objective.fn( x0=test.auglag$solution, param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names)
objective.fn( x0=unname(x0), param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names)


get.point.est(x0=test.auglag$solution, param.supp=param.supp, x0.names=x0.names)
get.point.est(x0=x0, param.supp=param.supp, x0.names=x0.names)

# data$y <- data$x1 * 5 + data$x2 * -2 + runif(observations.test)/5 - .5


cov.estimator(x0=test.auglag$solution, 
  eqns=eqns, data=data, param.supp=param.supp, err.supp=err.supp, x0.names=x0.names )



cov.estimator(x0=x0, 
  eqns=eqns, data=data, param.supp=param.supp, err.supp=err.supp, x0.names=x0.names )



# NLOPT_GN_ISRES
optim.test <- nloptr.no.check( x0 = x0,
  eval_f = objective.fn,
  eval_g_ineq = ineq.constraint,
  eval_g_eq = eq.constraint,
  opts = list("algorithm"="NLOPT_LN_COBYLA", maxeval=5000, print_level=1),
  ub=rep(1, length(x0)),
  lb=rep(0, length(x0)),
  param.supp = param.supp, 
  err.supp = err.supp, 
  eqns = eqns, 
  data = data,
  x0.names = x0.names
)









# + runif(length(x0 ), min= -.2, max=.2)

get.point.est<- function(x0, param.supp, x0.names) {
  names(x0) <- x0.names
  Z.mat <- t( bdiag(lapply(param.supp, FUN=matrix)) ) # (eqn 2.6, Golan et al 1997)
# param.weights <- rep(1/sapply(param.supp, FUN=length), sapply(param.supp, FUN=length)) 
  param.weights <- x0[!grepl( "err.weights", names(x0))]  
  params.point <- Z.mat %*% param.weights
  params.point <- as.vector(params.point)
  names(params.point) <- names(param.supp)
  params.point
}

get.point.est(x0=test.auglag$solution, param.supp=param.supp, x0.names=x0.names)
get.point.est(x0=x0, param.supp=param.supp, x0.names=x0.names)

get.point.est(x0=solnp.test$pars, param.supp=param.supp, x0.names=x0.names)




ineq.constraint( x0=test.auglag$solution, param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names)
eq.constraint( x0=test.auglag$solution, param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names)




-7.04136930737249 


param.weights

get.cov.est<- function(x0, param.supp, x0.names) {
  names(x0) <- x0.names
  x0 <- x0[!grepl( "err.weights", names(x0))]
  Z.mat <- t( bdiag(lapply(param.supp, FUN=matrix)) ) # (eqn 2.6, Golan et al 1997)
# param.weights <- rep(1/sapply(param.supp, FUN=length), sapply(param.supp, FUN=length)) 
  sigma.p<- cov( matrix(x0, nrow = length(param.supp[[1]])) ) 
  t(Z.mat) %*% sigma.p %*% Z.mat
}

get.cov.est(x0=test.auglag$solution, param.supp=param.supp, x0.names=x0.names)
get.cov.est(x0=x0, param.supp=param.supp, x0.names=x0.names)


get.st.err.est<- function(x0, param.supp, x0.names) {
  names(x0) <- x0.names
  x0.params <- x0[!grepl( "err.weights", names(x0))]
  Z.mat <- t( bdiag(lapply(param.supp, FUN=matrix)) ) # (eqn 2.6, Golan et al 1997)
# param.weights <- rep(1/sapply(param.supp, FUN=length), sapply(param.supp, FUN=length)) 
#  var.p <- apply( matrix(x0, nrow=length(param.supp[[1]])), MARGIN=2 , FUN=var)
  x0.mat <- matrix(x0.params, nrow=length(param.supp[[1]]))
  p.var.v<- c()
  for ( i in 1:length(param.supp)) {
    p.var.v[i] <- sum(x0.mat[, i] * param.supp[[i]]^2) - sum(x0.mat[, i] * param.supp[[i]])^2
  }
  # Eq. 3.16a
#  var.p <- rowSums(Z.mat^2)
#  sqrt( rowSums(Z.mat^2) * var.p )
  # By eq. 3.19a
  sqrt( rowSums(Z.mat^2) * p.var.v ) 
}

get.st.err.est(x0=test.auglag$solution, param.supp=param.supp, x0.names=x0.names)
get.st.err.est(x0=x0, param.supp=param.supp, x0.names=x0.names)


#install.packages("numDeriv")
library(numDeriv)

eqn.fn.for.jacobian<- function(x, eqns, data) {

  params.point <- as.list(x)

  params.point.and.data <- c(params.point, data )

  ret <- with( params.point.and.data, 
    lapply(eqns, FUN=function(x) eval(x[[3]]) )
  )
  
  unlist(ret )
}


eqn.fn.for.jacobian(x0, eqns=eqns, data=data)

cov.estimator <- function(x0, eqns, data, param.supp, err.supp, x0.names ) {

  obs.num <- nrow(as.data.frame(data))
  
  names(x0) <- x0.names
  x0.err.weights <- x0[grepl( "err.weights", names(x0))]

  eqn.jacobian<- jacobian(func=eqn.fn.for.jacobian, 
    x = get.point.est(x0=x0, param.supp=param.supp, x0.names=x0.names),
    eqns=eqns, data=data
  )

  err.vec <- colSums(matrix(unlist(err.supp), nrow=length(err.supp[[1]])) * 
    matrix(x0.err.weights, nrow=length(err.supp[[1]])) )
    
  err.mat <- matrix( err.vec, byrow=TRUE, nrow=length(eqns))
  
  est.cov <- cov( t(err.mat)) / obs.num
  
  interior.cov <- kronecker(solve(est.cov), diag(obs.num))
  
  solve( t(eqn.jacobian) %*% interior.cov %*% eqn.jacobian ) / obs.num
  
}


cov.estimator(x0=test.auglag$solution, 
  eqns=eqns, data=data, param.supp=param.supp, err.supp=err.supp, x0.names=x0.names )

cov.estimator(x0=solnp.test$pars, 
  eqns=eqns, data=data, param.supp=param.supp, err.supp=err.supp, x0.names=x0.names )



# test.auglag$solution + runif(length(test.auglag$solution))/5

  names(x0) <- x0.names
  x0.err.weights <- x0[grepl( "err.weights", names(x0))]




# install.packages("Rsolnp")
# install.packages("Rsolnp",repos = "http://cran.us.r-project.org/")
# install.packages("/Users/travismcarthur/Downloads/Rsolnp_1.14.tgz", repos = NULL, type="source")
# ACtually, I misstyped the package name, but this is useful for the future anyway:
# http://altons.github.io/r/2013/06/23/when-package-xyz-is-not-available-for-a-specific-version-of-r/
library("Rsolnp")



  pars= = x0 + runif(length(x0 ), min= -.2, max=.2),
  fun = objective.fn,
  eval_g_ineq = ineq.constraint,
  eqfun = eq.constraint,
  opts = opts,
  ub=rep(1, length(x0)),
  lb=rep(0, length(x0)),
  param.supp = param.supp, 
  err.supp = err.supp, 
  eqns = eqns, 
  data = data,
  x0.names = x0.names
)

ineq.constraint.length <- length(ineq.constraint( x0=x0, param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names))

solnp.test <- solnp(pars=x0, fun=objective.fn, 
  eqfun = eq.constraint, eqB = NULL, ineqfun = ineq.constraint, 
  ineqLB = rep(-Inf, ineq.constraint.length),
  ineqUB = rep(0, ineq.constraint.length),   
  UB=rep(1, length(x0)),
  LB=rep(0, length(x0)),
  control = list(trace=2),
  param.supp = param.supp, 
  err.supp = err.supp, 
  eqns = eqns, 
  data = data,
  x0.names = x0.names
)

solnp.test$pars

solnp(pars=, fun, eqfun = NULL, eqB = NULL, ineqfun = NULL, ineqLB = NULL,
ineqUB = NULL, LB = NULL, UB = NULL, control = list(), ...)


# install.packages("compiler")
library("compiler")

enableJIT(3)

gosolnp.test <- gosolnp(pars=x0, fun=objective.fn, 
  eqfun = eq.constraint, eqB = NULL, ineqfun = ineq.constraint, 
  ineqLB = rep(-Inf, ineq.constraint.length),
  ineqUB = rep(0, ineq.constraint.length),   
  UB=rep(1, length(x0)),
  LB=rep(0, length(x0)),
  control = list(eval.type=2, trace=2),
  param.supp = param.supp, 
  err.supp = err.supp, 
  eqns = eqns, 
  data = data,
  x0.names = x0.names
)



get.point.est(x0=gosolnp.test$pars, param.supp=param.supp, x0.names=x0.names)




x0.test <- test.auglag$solution
names(x0.test) <- x0.names
x0.test <- x0.test[!grepl( "err.weights", names(x0))]
sigma.p<-  cov( matrix(x0.test, nrow=length(param.supp[[1]])) )

Z.mat <- t( bdiag(lapply(param.supp, FUN=matrix)) )
t(Z.mat) %*% sigma.p %*% t(Z.mat)

rowSums(Z.mat^2)



length(param.supp[[1]])




ineq.constraint( x0=test.auglag$solution, param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names)
eq.constraint( x0=test.auglag$solution, param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names)


install.packages("censReg")
library(censReg)

test.tobit <- censReg(y ~ x1 + x2, data=as.data.frame(data))
summary(test.tobit)
test.lm <- lm(y ~ x1 + x2, data=as.data.frame(data))
summary(test.lm)





test.auglag <- auglag(x0 = x0,
  fn = objective.fn, 
  gr = NULL,
  lower=rep(0, length(x0)),
  upper=rep(1, length(x0)), 
  hin = ineq.constraint,
  heq = eq.constraint,
  localsolver="lbfgs",
  param.supp = param.supp, 
  err.supp = err.supp, 
  eqns = eqns, 
  data = data,
  x0.names = x0.names
)



x0, fn, gr = NULL, lower = NULL, upper = NULL,
hin = NULL, hinjac = NULL, heq = NULL, heqjac = NULL,
localsolver = c("COBYLA"), localtol = 1e-6, ineq2local = FALSE,
nl.info = FALSE, control = list(), ...)




x0.1 <- unname(x0)
names(x0.1) <- x0.names



# Only some of the NLopt algorithms (AUGLAG, COBYLA, and ISRES) currently support nonlinear equality constraints.
# All of the global-optimization algorithms currently require you to specify bound constraints on all the optimization parameters. Of these algorithms, only ISRES and ORIG_DIRECT support nonlinear inequality constraints, and only ISRES supports nonlinear equality constraints. (However, any of them can be applied to nonlinearly constrained problems by combining them with the augmented Lagrangian method below.)

NLOPT_GN_ISRES is genetic

probbaly need bounds: NLOPT_GN_ISRES

NLOPT_LN_COBYLA
(The underlying COBYLA code only supports inequality constraints. Equality constraints are automatically transformed into pairs of inequality constraints, which in the case of this algorithm seems not to cause problems.)

NLOPT_LN_AUGLAG
 The algorithm NLOPT_LN_AUGLAG needs a local optimizer; specify an algorithm and termination condition in local_opts



nloptr( x0,
eval_f,
eval_grad_f = NULL,
lb = NULL,
ub = NULL,
eval_g_ineq = NULL,
eval_jac_g_ineq = NULL,
eval_g_eq = NULL,
eval_jac_g_eq = NULL,
opts = list(),
... )







Rprof("profile1.out") #, line.profiling=TRUE)

for ( i in 1:1000) {
ineq.constraint( x0=x0, param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names)
}

Rprof(NULL)
summaryRprof("profile1.out") #, lines = "show")

eq.constraint( x0=x0, param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names)
objective.fn( x0=x0, param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names)










#install.packages("alabama")
library("alabama")


test.alabama <- auglag(par=x0, fn=objective.fn, 
gr=objective.fn.gradient,
hin=ineq.constraint.posi, 
heq=eq.constraint, 
control.outer=list(trace=TRUE), 
control.optim=list(trace=6),
  param.supp = param.supp, 
  err.supp = err.supp, 
  eqns = eqns, 
  data = data,
  x0.names = x0.names
)

# 23 outer iterations




test.auglag.solution.fixed <- test.auglag$solution

test.auglag.solution.fixed[1:3] <- test.auglag.solution.fixed[1:3]/ sum(test.auglag.solution.fixed[1:3])

test.auglag.solution.fixed[4:6] <- test.auglag.solution.fixed[1:3]/ sum(test.auglag.solution.fixed[4:6])

test.alabama <- auglag(par=test.auglag.solution.fixed, fn=objective.fn, 
gr=objective.fn.gradient,
hin=ineq.constraint.posi, 
heq=eq.constraint, 
control.outer=list(trace=TRUE), 
control.optim=list(trace=3),
  param.supp = param.supp, 
  err.supp = err.supp, 
  eqns = eqns, 
  data = data,
  x0.names = x0.names
)







test.auglag <- nloptr( 
  x0 = x0,
  eval_f = objective.fn,
  eval_g_ineq = ineq.constraint,
  eval_g_eq = eq.constraint,
  opts = opts,
  ub=rep(1, length(x0)),
  lb=rep(0, length(x0)),
  param.supp = param.supp, 
  err.supp = err.supp, 
  eqns = eqns, 
  data = data,
  x0.names = x0.names
)





test.alabama$par



get.point.est(x0=test.alabama$par, param.supp=param.supp, x0.names=x0.names)
get.point.est(x0=x0, param.supp=param.supp, x0.names=x0.names)

# data$y <- data$x1 * 5 + data$x2 * -2 + runif(observations.test)/5 - .5

cov.estimator(x0=test.alabama$par, 
  eqns=eqns, data=data, param.supp=param.supp, err.supp=err.supp, x0.names=x0.names )


ineq.constraint( x0=test.alabama$par, param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names)
eq.constraint( x0=test.alabama$par, param.supp=param.supp, err.supp=err.supp, eqns=eqns, data=data, x0.names=x0.names)


colSums(matrix(test.alabama$par[names(test.alabama$par)=="err.weights"], nrow=3))








ineq.constraint.posi <- function( x0, param.supp, err.supp, eqns, data, x0.names) {

  names(x0) <- x0.names
  
  censored.ls <- list()

  for ( i in 1:length(eqns)) {
    censored.ls[[names(eqns)[i]]] <- with(data, eval(eqns[[i]][[2]])) == 0
  # isTRUE(all.equal
  }

  Z.mat <- t( bdiag(lapply(param.supp, FUN=matrix)) ) # (eqn 2.6, Golan et al 1997)

  y.censored <- unlist(censored.ls)
  
  w2.vec <- x0[grepl( "err.weights", names(x0))][rep(y.censored, each=length(err.supp[[1]]))]
# w2.vec <- unlist(err.supp[y.censored])

  V2.mat <- t( bdiag(lapply(err.supp[ y.censored], FUN=matrix)) )

# param.weights <- rep(1/sapply(param.supp, FUN=length), sapply(param.supp, FUN=length)) 
  param.weights <- x0[!grepl( "err.weights", names(x0))]

  params.point <- Z.mat %*% param.weights

  params.point <- as.list(params.point)

  names(params.point) <- names(param.supp)

  params.point.and.data <- c(params.point, data )

  censored.evaled.RHS <- with( params.point.and.data, 
    lapply(eqns, FUN=function(x) eval(x[[3]])[eval(x[[2]]) == 0] )
  )
  
  censored.evaled.LHS <- with( params.point.and.data, 
    lapply(eqns, FUN=function(x) eval(x[[2]])[eval(x[[2]]) == 0] )
  )
  
  dep.var.censored.constraint <- 
    unlist(censored.evaled.RHS) + V2.mat %*% w2.vec - unlist(censored.evaled.LHS)
  # (eqn 4.4, Golan et al 1997)
  
  return( - as.vector(dep.var.censored.constraint) )
  # ok, now we have switched the sign

}



objective.fn.gradient  <- function(x0, param.supp, err.supp, eqns, data, x0.names) {
  # Need to put "param.supp, err.supp, eqns, data as arguments even
  # though we don't use them so the optimizer doesnt throw an error
  names(x0) <- x0.names
  param.weights <- x0[!grepl( "err.weights", names(x0))]
  err.weights <- x0[ grepl( "err.weights", names(x0))]
  return( -  (- sum(log(param.weights) - 1) - sum(log(err.weights) - 1)) )
  # The above is to be maximized
}
# Note that we are using a minimizer, so I have made this the neg



nloptr.no.check <- function (x0, eval_f, eval_grad_f = NULL, lb = NULL, ub = NULL, 
    eval_g_ineq = NULL, eval_jac_g_ineq = NULL, eval_g_eq = NULL, 
    eval_jac_g_eq = NULL, opts = list(), ...) 
{
    .checkfunargs = function(fun, arglist, funname) {
        if (!is.function(fun)) 
            stop(paste(funname, " must be a function\n", sep = ""))
        flist = formals(fun)
        if (length(flist) > 1) {
            fnms = names(flist)[2:length(flist)]
            rnms = names(arglist)
            m1 = match(fnms, rnms)
            if (any(is.na(m1))) {
                mx1 = which(is.na(m1))
                for (i in 1:length(mx1)) {
                  stop(paste(funname, " requires argument '", 
                    fnms[mx1[i]], "' but this has not been passed to the 'nloptr' function.\n", 
                    sep = ""))
                }
            }
            m2 = match(rnms, fnms)
            if (any(is.na(m2))) {
                mx2 = which(is.na(m2))
                for (i in 1:length(mx2)) {
                  stop(paste(rnms[mx2[i]], "' passed to (...) in 'nloptr' but this is not required in the ", 
                    funname, " function.\n", sep = ""))
                }
            }
        }
        return(0)
    }
    arglist = list(...)
    .checkfunargs(eval_f, arglist, "eval_f")
    if (!is.null(eval_grad_f)) {
        .checkfunargs(eval_grad_f, arglist, "eval_grad_f")
    }
    if (!is.null(eval_g_ineq)) {
        .checkfunargs(eval_g_ineq, arglist, "eval_g_ineq")
    }
    if (!is.null(eval_jac_g_ineq)) {
        .checkfunargs(eval_jac_g_ineq, arglist, "eval_jac_g_ineq")
    }
    if (!is.null(eval_g_eq)) {
        .checkfunargs(eval_g_eq, arglist, "eval_g_eq")
    }
    if (!is.null(eval_jac_g_eq)) {
        .checkfunargs(eval_jac_g_eq, arglist, "eval_jac_g_eq")
    }
    if (is.null(lb)) {
        lb <- rep(-Inf, length(x0))
    }
    if (is.null(ub)) {
        ub <- rep(Inf, length(x0))
    }
    if (is.list(eval_f(x0, ...)) | is.null(eval_grad_f)) {
        eval_f_wrapper <- function(x) {
            eval_f(x, ...)
        }
    }
    else {
        eval_f_wrapper <- function(x) {
            return(list(objective = eval_f(x, ...), gradient = eval_grad_f(x, 
                ...)))
        }
    }
    num_constraints_ineq <- 0
    if (!is.null(eval_g_ineq)) {
        if (is.list(eval_g_ineq(x0, ...)) | is.null(eval_jac_g_ineq)) {
            eval_g_ineq_wrapper <- function(x) {
                eval_g_ineq(x, ...)
            }
        }
        else {
            eval_g_ineq_wrapper <- function(x) {
                return(list(constraints = eval_g_ineq(x, ...), 
                  jacobian = eval_jac_g_ineq(x, ...)))
            }
        }
        tmp_constraints <- eval_g_ineq_wrapper(x0)
        if (is.list(tmp_constraints)) {
            num_constraints_ineq <- length(tmp_constraints$constraints)
        }
        else {
            num_constraints_ineq <- length(tmp_constraints)
        }
    }
    else {
        eval_g_ineq_wrapper <- NULL
    }
    num_constraints_eq <- 0
    if (!is.null(eval_g_eq)) {
        if (is.list(eval_g_eq(x0, ...)) | is.null(eval_jac_g_eq)) {
            eval_g_eq_wrapper <- function(x) {
                eval_g_eq(x, ...)
            }
        }
        else {
            eval_g_eq_wrapper <- function(x) {
                return(list(constraints = eval_g_eq(x, ...), 
                  jacobian = eval_jac_g_eq(x, ...)))
            }
        }
        tmp_constraints <- eval_g_eq_wrapper(x0)
        if (is.list(tmp_constraints)) {
            num_constraints_eq <- length(tmp_constraints$constraints)
        }
        else {
            num_constraints_eq <- length(tmp_constraints)
        }
    }
    else {
        eval_g_eq_wrapper <- NULL
    }
    if ("local_opts" %in% names(opts)) {
        res.opts.add <- nloptr.add.default.options(opts.user = opts$local_opts, 
            x0 = x0, num_constraints_ineq = num_constraints_ineq, 
            num_constraints_eq = num_constraints_eq)
        local_opts <- res.opts.add$opts.user
        opts$local_opts <- NULL
    }
    else {
        local_opts <- NULL
    }
    res.opts.add <- nloptr.add.default.options(opts.user = opts, 
        x0 = x0, num_constraints_ineq = num_constraints_ineq, 
        num_constraints_eq = num_constraints_eq)
    opts <- res.opts.add$opts.user
    termination_conditions <- res.opts.add$termination_conditions
    if (opts$print_options_doc) {
        nloptr.print.options(opts.user = opts)
    }
    list_algorithms <- unlist(strsplit(nloptr.default.options[nloptr.default.options$name == 
        "algorithm", "possible_values"], ", "))
    if (opts$check_derivatives) {
        if (opts$algorithm %in% list_algorithms[grep("NLOPT_[G,L]N", 
            list_algorithms)]) {
            warning(paste("Skipping derivative checker because algorithm '", 
                opts$algorithm, "' does not use gradients.", 
                sep = ""))
        }
        else {
            cat("Checking gradients of objective function.\n")
            check.derivatives(.x = x0, func = function(x) {
                eval_f_wrapper(x)$objective
            }, func_grad = function(x) {
                eval_f_wrapper(x)$gradient
            }, check_derivatives_tol = opts$check_derivatives_tol, 
                check_derivatives_print = opts$check_derivatives_print, 
                func_grad_name = "eval_grad_f")
            if (num_constraints_ineq > 0) {
                cat("Checking gradients of inequality constraints.\n")
                check.derivatives(.x = x0, func = function(x) {
                  eval_g_ineq_wrapper(x)$constraints
                }, func_grad = function(x) {
                  eval_g_ineq_wrapper(x)$jacobian
                }, check_derivatives_tol = opts$check_derivatives_tol, 
                  check_derivatives_print = opts$check_derivatives_print, 
                  func_grad_name = "eval_jac_g_ineq")
            }
            if (num_constraints_eq > 0) {
                cat("Checking gradients of equality constraints.\n")
                check.derivatives(.x = x0, func = function(x) {
                  eval_g_eq_wrapper(x)$constraints
                }, func_grad = function(x) {
                  eval_g_eq_wrapper(x)$jacobian
                }, check_derivatives_tol = opts$check_derivatives_tol, 
                  check_derivatives_print = opts$check_derivatives_print, 
                  func_grad_name = "eval_jac_g_eq")
            }
        }
    }
    ret <- list(x0 = x0, eval_f = eval_f_wrapper, lower_bounds = lb, 
        upper_bounds = ub, num_constraints_ineq = num_constraints_ineq, 
        eval_g_ineq = eval_g_ineq_wrapper, num_constraints_eq = num_constraints_eq, 
        eval_g_eq = eval_g_eq_wrapper, options = opts, local_options = local_opts, 
        nloptr_environment = new.env())
    attr(ret, "class") <- "nloptr"
    ret$call <- match.call()
    ret$termination_conditions <- termination_conditions
 #   is.nloptr(ret)
    solution <- .Call(NLoptR_Optimize, ret)
    ret$environment <- NULL
    ret$status <- solution$status
    ret$message <- solution$message
    ret$iterations <- solution$iterations
    ret$objective <- solution$objective
    ret$solution <- solution$solution
    ret$version <- paste(c(solution$version_major, solution$version_minor, 
        solution$version_bugfix), collapse = ".")
    return(ret)
}


#environment(nloptr.no.check) <- as.environment("package:nloptr")
environment(nloptr.no.check)<- asNamespace('nloptr')
# How to have modified function access underlying functions in a package:
# http://stackoverflow.com/questions/3094232/add-objects-to-package-namespace

