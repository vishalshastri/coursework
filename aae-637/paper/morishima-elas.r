# Morishima elasticity of substitution
# Must first run part of bootstrap code to get data, then the postestimation to get param estimates

library("numDeriv")


morishima.e.s <- function(i, j, data, params, cost.fn.string, shadow=FALSE) {

  # All this is from eqn 8 of http://www.jstor.org/stable/pdf/1827940.pdf
  
  which.price <- ifelse(shadow, "xi", "w")
  
  if (shadow) {
    w_i <- params[ paste0(which.price, lead.zero(i)) ]
    w_j <- params[ paste0(which.price, lead.zero(j)) ]
    names(w_i) <- paste0(which.price, lead.zero(i)) 
    names(w_j) <- paste0(which.price, lead.zero(j)) 
    
  } else {
    w_i <- data[, paste0(which.price, lead.zero(i)) ]
    w_j <- data[, paste0(which.price, lead.zero(j)) ]
    names(w_i) <- paste0(which.price, lead.zero(i)) 
    names(w_j) <- paste0(which.price, lead.zero(j)) 
  }
  
#  cat(names(w_i), "\n") 
  
  assign(paste0("xi", lead.zero(N)), 1)
  assign(paste0("theta", lead.zero(N)), 1)

  temp.deriv.fn <- function(x, data) { 
    x <- c(as.list(x), as.list(data))
    with(x, eval(parse(text=gsub("[.]", "", paste0("y01 * (", cost.fn.string, ")")
    # Must multiply by y01 since estimated cost function divided by y01
    ) )) )
  }

  C_i <- grad(
    temp.deriv.fn, 
    x=w_i, method="complex", 
    data=c(params[names(params)!= paste0(which.price, lead.zero(i))], 
      data[names(data)!= paste0(which.price, lead.zero(i))] ))
      
#  print(C_i)
#  print(names(w_i))
    
  C_j <- grad(
    temp.deriv.fn, 
    x=w_j, method="complex", 
    data=c(params[names(params)!= paste0(which.price, lead.zero(j))], 
      data[names(data)!= paste0(which.price, lead.zero(j))] ))  
    
  C_hess <- hessian(
    temp.deriv.fn, 
    x=c(w_i, w_j),
    data=c(params[! names(params) %in% c(paste0(which.price, lead.zero(i)), paste0(which.price, lead.zero(j)))], 
      data[! names(data) %in% c(paste0(which.price, lead.zero(i)), paste0(which.price, lead.zero(j)))] ))
    
  C_ij <- C_hess[1, 2]
  C_ii <- C_hess[1, 1]
  print(C_hess)
  
  w_i * C_ij / C_j - w_i * C_ii / C_i
  
}

# TODO: Am I properly taking the shadow prices into account? See Kumbhakar 1992
# Seems Ok, once I added it. I.e. it is exactly the same.

morishima.e.s(1, 2, as.data.frame(t(colMeans(combined.df))), 
  GAMS.nonlinear.results.params.full, demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]])


morishima.mat <- matrix(NA, ncol=N, nrow=N)

for ( i in 1:N) {
  for (j in 1:N) {
    morishima.mat[i, j] <- morishima.e.s(i, j, as.data.frame(t(colMeans(combined.df))), 
      GAMS.nonlinear.results.params.full, demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]])
  }
}

morishima.mat
# i is row
# j is column

rowMeans(morishima.mat)
# Row means are a good measure, I think

mean(morishima.mat)


# trying median
morishima.mat <- matrix(NA, ncol=N, nrow=N)

for ( i in 1:N) {
  for (j in 1:N) {
    morishima.mat[i, j] <- morishima.e.s(i, j, 
      as.data.frame(t(apply(combined.df, 2, median, na.rm=TRUE))), 
       GAMS.nonlinear.results.params.full, demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]])
  }
}

morishima.mat
# i is row
# j is column

mean(morishima.mat)



morishima.mat <- matrix(NA, ncol=N-1, nrow=N-1)
# Shadow price

for ( i in 1:(N-1)) {
  for (j in 1:(N-1)) {
    morishima.mat[i, j] <- morishima.e.s(i, j, as.data.frame(t(colMeans(combined.df))), 
      GAMS.nonlinear.results.params.full, demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]],
      shadow=TRUE)
  }
}

morishima.mat
# i is row
# j is column


# Ok, shadow price and market price elasticity are the same

  

mean( (GAMS.nonlinear.results.params.full[grepl("xi", names( GAMS.nonlinear.results.params.full))] - 1)^2)
# Mean square distance from 1





# GAMS.nonlinear.results.params.full.save <- GAMS.nonlinear.results.params.full

set.seed(1000)
# Seed of 10 gives substitution

concavity.scaling.factor <- 1

all.params <- unique(unlist(str_extract_all(unlist(demand.eqns), 
"(s.[0-9][0-9].[0-9][0-9])|(b.y.[0-9][0-9])|(b.[0-9][0-9])|(b.y.y)|(d.[0-9][0-9].[0-9][0-9])|(c.[0-9][0-9] )|(c.[0-9][0-9].[0-9][0-9])"
  ))
)

all.params  <- gsub("([.])|( )", "", all.params )

synthetic.params <- rnorm(length(c(all.params, paste0("xi", lead.zero(1:N))))) 
synthetic.params <- as.numeric(synthetic.params)
names(synthetic.params) <- c(all.params, paste0("xi", lead.zero(1:N)))


synthetic.S.mat <- matrix(0, N-1, N-1)
synthetic.T.mat <- matrix(0, N-1, N-1)

# synthetic.S.mat[lower.tri(synthetic.S.mat, diag=TRUE)] <-  names(  synthetic.params[1:(N*(N-1)/2)] )
# A-ok

synthetic.T.mat[lower.tri(synthetic.T.mat, diag=TRUE)] <- rnorm((N*(N-1)/2)) * concavity.scaling.factor

synthetic.S.mat <-  - synthetic.T.mat %*% t(synthetic.T.mat)

eigen(synthetic.S.mat[-(1:2), -(1:2)])

GAMS.nonlinear.results.params.full[1:(N*(N-1)/2)] <- synthetic.S.mat[lower.tri(synthetic.S.mat, diag=TRUE)] 

GAMS.nonlinear.results.params.full <- GAMS.nonlinear.results.params.full.save

synthetic.S.mat[lower.tri(synthetic.S.mat, diag=TRUE)] <- 
  GAMS.nonlinear.results.params.full[1:(N*(N-1)/2)]

synthetic.S.mat
synthetic.S.mat <- t(synthetic.S.mat)

synthetic.S.mat[lower.tri(synthetic.S.mat, diag=TRUE)] <- 
  GAMS.nonlinear.results.params.full[1:(N*(N-1)/2)]

synthetic.S.mat
det(synthetic.S.mat)

#GAMS.nonlinear.results.params.full[1:(N*(N-1)/2)] <- 0


morishima.mat <- matrix(NA, ncol=N, nrow=N)

for ( i in 1:N) {
  for (j in 1:N) {
    morishima.mat[i, j] <- morishima.e.s(i, j, as.data.frame(t(colMeans(combined.df))), 
      GAMS.nonlinear.results.params.full, demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]])
  }
}

morishima.mat
# i is row
# j is column

rowMeans(morishima.mat)
# Row means are a good measure, I think

mean(morishima.mat)





allen.uzawa.e.s <- function(i, j, data, params, cost.fn.string, shadow=FALSE) {

  # All this is from eqn 1 of http://www.jstor.org/stable/pdf/1827940.pdf
  
  which.price <- ifelse(shadow, "xi", "w")
  
  if (shadow) {
    w_i <- params[ paste0(which.price, lead.zero(i)) ]
    w_j <- params[ paste0(which.price, lead.zero(j)) ]
    names(w_i) <- paste0(which.price, lead.zero(i)) 
    names(w_j) <- paste0(which.price, lead.zero(j)) 
    
  } else {
    w_i <- data[, paste0(which.price, lead.zero(i)) ]
    w_j <- data[, paste0(which.price, lead.zero(j)) ]
    names(w_i) <- paste0(which.price, lead.zero(i)) 
    names(w_j) <- paste0(which.price, lead.zero(j)) 
  }
  
#  cat(names(w_i), "\n") 
  
  assign(paste0("xi", lead.zero(N)), 1)
  assign(paste0("theta", lead.zero(N)), 1)

  temp.deriv.fn <- function(x, data) { 
    x <- c(as.list(x), as.list(data))
    with(x, eval(parse(text=gsub("[.]", "", paste0("y01 * (", cost.fn.string, ")")
    # Must multiply by y01 since estimated cost function divided by y01
    ) )) )
  }

  C_i <- grad(
    temp.deriv.fn, 
    x=w_i, method="complex", 
    data=c(params[names(params)!= paste0(which.price, lead.zero(i))], 
      data[names(data)!= paste0(which.price, lead.zero(i))] ))
      
#  print(C_i)
#  print(names(w_i))
    
  C_j <- grad(
    temp.deriv.fn, 
    x=w_j, method="complex", 
    data=c(params[names(params)!= paste0(which.price, lead.zero(j))], 
      data[names(data)!= paste0(which.price, lead.zero(j))] ))  
    
  C_hess <- hessian(
    temp.deriv.fn, 
    x=c(w_i, w_j),
    data=c(params[! names(params) %in% c(paste0(which.price, lead.zero(i)), paste0(which.price, lead.zero(j)))], 
      data[! names(data) %in% c(paste0(which.price, lead.zero(i)), paste0(which.price, lead.zero(j)))] ))
    
  C_ij <- C_hess[1, 2]
  
  Cost.fn.value <- 
    with(c(as.list(params), as.list(data)), 
      eval(parse(text=gsub("[.]", "", paste0("y01 * (", cost.fn.string, ")")))))
  
  Cost.fn.value * C_ij / (C_i * C_j) 
  
}


allen.uzawa.mat <- matrix(NA, ncol=N, nrow=N)

for ( i in 1:N) {
  for (j in 1:N) {
    allen.uzawa.mat[i, j] <- allen.uzawa.e.s(i, j, as.data.frame(t(colMeans(combined.df))), 
      GAMS.nonlinear.results.params.full, demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]])
  }
}

diag(allen.uzawa.mat) <- 0

allen.uzawa.mat
# i is row
# j is column


rowMeans(allen.uzawa.mat)
# Row means are a good measure, I think

mean(allen.uzawa.mat)









# Try to make standard errors

morishima.e.s.delta <- function(params) {
  morishima.e.s(i=i, j=j, data=as.data.frame(t(colMeans(combined.df))), 
    params=params, cost.fn.string=demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]], shadow=FALSE)
}

morishima.se.mat <- matrix(NA, ncol=N, nrow=N)

for ( i in 1:N) {
  for (j in 1:N) {
    morishima.grad <- grad(morishima.e.s.delta, GAMS.nonlinear.results.params.full)
    morishima.se.mat[i, j] <- morishima.grad %*% param.covar.mat %*% morishima.grad
  }
}

morishima.se.mat
# i is row
# j is column

rowMeans(morishima.se.mat)
# Row means are a good measure, I think

mean(morishima.se.mat)



















# BELOW IS A TEST OF ANALYTICAL DERIVATIVES. IT WORKS!





#test.exp <- as.expression(demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]])
test.exp <- parse(text=gsub("[.]", "", paste0("y01 * (", demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]], ")") ))

dx2x <- deriv(test.exp , "w01")

with(c(as.list(GAMS.nonlinear.results.params.full), as.list(as.data.frame(t(colMeans(combined.df))))), eval(dx2x))






  
  params <- GAMS.nonlinear.results.params.full
  which.price <- "w"
    i <- 1
  data <- as.data.frame(t(colMeans(combined.df)))
  cost.fn.string <- demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]]
  
    w_i <- data[, paste0(which.price, lead.zero(i)) ]
      names(w_i) <- paste0(which.price, lead.zero(i)) 

  temp.deriv.fn <- function(x, data) { 
    x <- c(as.list(x), as.list(data))
    with(x, eval(parse(text=gsub("[.]", "", paste0("y01 * (", cost.fn.string, ")")
    # Must multiply by y01 since estimated cost function divided by y01
    ) )) )
  }
  


  C_i <- grad(
    temp.deriv.fn, 
    x=w_i, method="complex", 
    data=c(params[names(params)!= paste0(which.price, lead.zero(i))], 
      data[names(data)!= paste0(which.price, lead.zero(i))] ))
 C_i     
#rm( params )


temp.deriv.fn( w_i , c(params[names(params)!= paste0(which.price, lead.zero(i))], 
      data[names(data)!= paste0(which.price, lead.zero(i))] ))



morishima.e.s(i, j,data=as.data.frame(t(colMeans(combined.df))), 
    params=GAMS.nonlinear.results.params.full, cost.fn.string=demand.eqns.nonlinear[[length(demand.eqns.nonlinear)]], shadow=FALSE)






