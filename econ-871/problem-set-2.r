
profits <- c(.11,.09)
wages=c(1,1)
mu=1
rho=0.8
alpha=0.4
x_underbar=1
gamma=6.5
labor=c(1,1)
kappa.mat=matrix(c(0.1, 0.2, 0.2, 0.1), ncol=2)
tau.mat=matrix(c(1, 1.15, 1.15, 1), ncol=2)

ppareto <- function(q, xm, alpha) ifelse(q > xm , 1 - (xm/q)^alpha, 0 )
# Thanks to http://stats.stackexchange.com/questions/78168/how-to-know-if-my-data-fits-pareto-distribution



chaney.eqm.fn <- function(profits,
         solve=TRUE,
         wages=c(1,1), 
         mu=1, 
         rho=0.8, 
         alpha=0.4, 
         x_underbar=1, 
         gamma=6.5, 
         labor=c(1,1), 
         kappa.mat=matrix(c(0.1, 0.2, 0.2, 0.1), ncol=2),
         tau.mat=matrix(c(1, 1.15, 1.15, 1), ncol=2)
         ) {
  
  sigma <- (1-rho)^-1
  
  wages.dup.mat <- matrix(rep(wages, length(wages)), ncol=length(wages))
  
  theta_i_k <- mu * (wages.dup.mat * tau.mat)^(-gamma) * 
    kappa.mat^(1-gamma/(sigma-1))
  # kappa.mat^((1-gamma)/(sigma-1))
  
  big.theta.gamma_k <- rowSums(theta_i_k)
  
  big.theta.gamma_k.mat <- matrix(rep(big.theta.gamma_k, length(wages)), ncol=length(wages), byrow=TRUE)
  
  Y_j <- (wages * labor + profits)
  
  estimated.profits <- (alpha*(sigma-1)/(sigma*gamma)) * 
    rowSums(
      matrix(rep(Y_j , length(wages)), byrow=TRUE, ncol=length(wages)) * 
        (theta_i_k/big.theta.gamma_k.mat)
    )
  
  if (solve) { return(sum(abs(profits -  estimated.profits)))}
  
  lambda_2 <- (sigma^(-1) * alpha )^(1/gamma-1/(sigma-1)) * 
    (sigma/(sigma-1)) * ((gamma*x_underbar^gamma)/(gamma-(sigma-1)))^(-1/gamma)
  
  price.index_j <- lambda_2 * Y_j^(1/gamma-1/(sigma-1)) * 
    big.theta.gamma_k^(-1/gamma)  
  
  price.index_j.mat <- matrix(rep(price.index_j, length(wages)), ncol=length(wages), byrow=TRUE)
  
  phi.hat_i_j <- (sigma/(sigma-1))*(wages.dup.mat*tau.mat/price.index_j.mat) *
    (sigma^(-1) * alpha * Y_j )^(-1/(sigma-1)) * kappa.mat^(1/(sigma-1))
  
  mass.consumed <- mu * (1-ppareto(phi.hat_i_j, x_underbar, gamma))
  
  fob.imports.and.domestic <- alpha * Y_j * theta_i_k / big.theta.gamma_k.mat
  
  list(i  = phi.hat_i_j, 
       ii = mass.consumed,
       iv = price.index_j,
       v  = fob.imports.and.domestic)
  
  
}

rho=0.8
alpha=0.4
gamma=6.5
sigma <- (1-rho)^-1

starting.profits <- alpha*(sigma-1) / ( sigma*gamma - alpha*(sigma-1) )

starting.profits <- rep(starting.profits, 2)
# starting.profits <- alpha*(sigma-1) / ( sigma*gamma - alpha*(sigma-1) ) + runif(2)

chaney.eqm.fn(starting.profits)

# QUESTION B

optim.profits <- optim(starting.profits, chaney.eqm.fn, control=list(trace=2))$par

chaney.B.ls <- chaney.eqm.fn(optim.profits, solve=FALSE)

# QUESTION C

optim.profits <- optim(starting.profits, chaney.eqm.fn, control=list(trace=2),
                       tau.mat=matrix(c(1, 1, 1, 1), ncol=2))$par

chaney.C.ls <- chaney.eqm.fn(optim.profits, solve=FALSE, tau.mat=matrix(c(1, 1, 1, 1), ncol=2))


# QUESTION D

epsilon <- 10^-8

optim.profits <- optim(starting.profits, chaney.eqm.fn, control=list(trace=2),
                       tau.mat=matrix(c(1, 1.15 - epsilon, 1.15 - epsilon, 1), ncol=2))$par

chaney.D.ls  <- chaney.eqm.fn(optim.profits, solve=FALSE, tau.mat=matrix(c(1, 1.15 - epsilon, 1.15 - epsilon, 1), ncol=2))

X_L_rel <- chaney.D.ls$v[1, 2]/chaney.D.ls$v[1, 1]
X_H_rel <- chaney.B.ls$v[1, 2]/chaney.B.ls$v[1, 1]

elast.tar.wrt.im <- (X_L_rel/X_H_rel - 1) /
  ((1.15 - epsilon)/1.15 - 1) + 1

elast.tar.wrt.im

elast.tar.wrt.im + 5.5

chaney.D.2.L.ls  <- chaney.eqm.fn(optim.profits, solve=FALSE, tau.mat=matrix(c(1, 1.05 - epsilon, 1.05 - epsilon, 1), ncol=2))
chaney.D.2.H.ls  <- chaney.eqm.fn(optim.profits, solve=FALSE, tau.mat=matrix(c(1, 1.05, 1.05, 1), ncol=2))


X_L_rel <- chaney.D.2.L.ls$v[1, 2]/chaney.D.2.L.ls$v[1, 1]
X_H_rel <- chaney.D.2.H.ls$v[1, 2]/chaney.D.2.H.ls$v[1, 1]

elast.tar.wrt.im.2 <- (X_L_rel/X_H_rel - 1) /
  ((1.05 - epsilon)/1.05 -1) + 1

elast.tar.wrt.im.2

chaney.D.3.L.ls  <- chaney.eqm.fn(optim.profits, solve=FALSE, tau.mat=matrix(c(1, 1 - epsilon, 1 - epsilon, 1), ncol=2))
chaney.D.3.H.ls  <- chaney.eqm.fn(optim.profits, solve=FALSE, tau.mat=matrix(c(1, 1.00, 1.00, 1), ncol=2))


X_L_rel <- chaney.D.3.L.ls$v[1, 2]/chaney.D.3.L.ls$v[1, 1]
X_H_rel <- chaney.D.3.H.ls$v[1, 2]/chaney.D.3.H.ls$v[1, 1]

elast.tar.wrt.im.3 <- (X_L_rel/X_H_rel - 1) /
  ((1 - epsilon)/1 -1) + 1

elast.tar.wrt.im.3

plot(x=c(1, 1.05, 1.15), y=c(elast.tar.wrt.im, elast.tar.wrt.im.2 , elast.tar.wrt.im.3), type="b")

c(elast.tar.wrt.im, elast.tar.wrt.im.2 , elast.tar.wrt.im.3) + 5.5
  