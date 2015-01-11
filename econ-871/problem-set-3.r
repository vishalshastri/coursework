

# QUESTION 1(a)

# install.packages("evd")
library("evd")
library("Matrix")
library("stargazer")

simplified.EK.fn <- function(wages, tau_n_i, N.countries=3, K.goods=10000, T_i = rep(1.5, N.countries) , distn_theta=4, L_n=rep(1,3), sigma=2, ret.share.mat=FALSE, ret.welfare=FALSE) {
  
  set.seed(100)
  
  p_n_i_k.args.ls <- lapply(as.list(T_i), FUN=function(T_i) {
    list(z_i_k=rfrechet(n=K.goods, loc = 0, scale = T_i^(1/distn_theta), shape=distn_theta))
  }
  )
  
  for ( i in 1:length(p_n_i_k.args.ls)) {
    p_n_i_k.args.ls[[i]]$w_i <- wages[i]
  }
  
  for ( i in 1:length(p_n_i_k.args.ls)) {
    p_n_i_k.args.ls[[i]]$tau_n_i <- tau_n_i
  }
   
  p_n_i_k.ls <- list()
  for ( i in 1:length(p_n_i_k.args.ls)) {
    x <- p_n_i_k.args.ls[[i]]
    
    p_n_i_k.ls[[i]] <-
      (x$w_i / x$z_i_k) * rep(1 + x$tau_n_i[, i], each=length(x$z_i_k))
    
  }
  
  p_n_i_k.mat<- do.call(cbind, p_n_i_k.ls)
  
  p_n_k.which.v <- apply(p_n_i_k.mat, 1, which.min) 
  
  p_n_k.which.mat <- matrix(p_n_k.which.v, ncol=N.countries)
  
  p_n_k.v <- apply(p_n_i_k.mat, 1, min) 
  
  p_n_k.mat <- matrix(p_n_k.v, ncol=N.countries)

  min.price.mat<-p_n_k.mat
  
  C_n.fn <- function(price.vec, targ.price, sigma, y) {
    (price.vec[targ.price]^-sigma * y) /
      sum(price.vec^(1-sigma))
  }
  
  exp.share.ls <- list()
  
  for ( i in 1:ncol(min.price.mat)) {
    exp.share.ls[[i]]<- 
      C_n.fn(price.vec=min.price.mat[, i], targ.price=1:nrow(min.price.mat), 
        sigma=sigma, y=L_n[i] * wages[i])   
    
    exp.share.ls[[i]] <- exp.share.ls[[i]] * min.price.mat[, i]
    # ok, this is the actual amount we are spending on the good, not just the quantity
    
  }
  
  trade.share.ls <- list()
  
  for ( i in 1:N.countries) {
    
    left.matrix <- sparseMatrix(i = p_n_k.which.mat[, i], j=1:nrow(p_n_k.which.mat)  ) 
    # NOTE: this will produce a bug if a country never has the lowest price for any good
    stopifnot(nrow(left.matrix) == N.countries )
    
    right.matrix <- matrix(exp.share.ls[[i]], ncol=1)
    
    trade.share.ls[[i]] <- as.matrix(left.matrix %*% right.matrix)
    
  }
  
  trade.share.mat <- do.call(cbind, trade.share.ls)
  # Ok, so here the rows are trading partners, so a sum of a column adds to
  # a country's total expenditure
  
  if (ret.share.mat) {
    return(trade.share.mat / matrix(rep(wages, each=N.countries), ncol=N.countries) )
    # Note this is an element-by element division
  }
  
  
  if (ret.welfare) {
    
    CES.price.agg <- apply(min.price.mat, 2, function(x) sum(x^(-sigma-1))^(-1/(sigma-1))  )
    
    return(wages/CES.price.agg)
    
  }
  
  
  
  sum( ( colSums(trade.share.mat) - rowSums(trade.share.mat) )^2 )
  
}



far.away.tau.mat <- matrix(c(  0, .05, .3,
                             .05,   0, .3,
                              .3,   .3,  0), nrow=3, ncol=3, byrow=TRUE)



far.away.wages<- optim(par=rep(1,3), fn=simplified.EK.fn, tau_n_i = far.away.tau.mat, K.goods=100000,  control=list(trace=5))$par

far.away.wages/far.away.wages[1]


far.away.share.mat <- simplified.EK.fn(far.away.wages, tau_n_i = far.away.tau.mat, K.goods=100000, ret.share.mat=TRUE)

far.away.welfare <- simplified.EK.fn(far.away.wages, tau_n_i = far.away.tau.mat, K.goods=100000, ret.welfare=TRUE)


rownames(far.away.share.mat ) <- c("Portlandia", "Levittown", "Potemkin")
colnames(far.away.share.mat ) <- c("Portlandia", "Levittown", "Potemkin")


stargazer(far.away.share.mat, summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Econ 871 Trade/Problem sets/PS3/table1.tex", 
  rownames=TRUE, title="Bilateral trade share matrix, $\\tau_{12} = \\tau_{21} = 1.05, \\tau_{13} = \\tau_{31} = \\tau_{32} = \\tau_{23} = 1.3$.")

far.away.welfare.disp.mat <- matrix(c(far.away.wages/far.away.wages[1], far.away.welfare), nrow=2, byrow=TRUE)

colnames(far.away.welfare.disp.mat) <- c("Portlandia", "Levittown", "Potemkin")
rownames(far.away.welfare.disp.mat) <- c("Wages", "Welfare")

stargazer(far.away.welfare.disp.mat, summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Econ 871 Trade/Problem sets/PS3/table2.tex", 
  rownames=TRUE, title="Wage and welfare measure, $\\tau_{12} = \\tau_{21} = 1.05, \\tau_{13} = \\tau_{31} = \\tau_{32} = \\tau_{23} = 1.3$.")






# QUESTION 1(b)

higher.prod.T_i <- c(1.5, 3, 1.5)

higher.prod.wages<- optim(par=far.away.wages, fn=simplified.EK.fn, tau_n_i = far.away.tau.mat, K.goods=100000, T_i = higher.prod.T_i, control=list(trace=5))$par

higher.prod.wages

higher.prod.wages/higher.prod.wages[1]

higher.prod.share.mat <- simplified.EK.fn(higher.prod.wages, tau_n_i = far.away.tau.mat, K.goods=100000, T_i = higher.prod.T_i, ret.share.mat=TRUE)

higher.prod.welfare <- simplified.EK.fn(higher.prod.wages, tau_n_i = far.away.tau.mat, K.goods=100000, T_i = higher.prod.T_i, ret.welfare=TRUE)



rownames(higher.prod.share.mat ) <- c("Portlandia", "Levittown", "Potemkin")
colnames(higher.prod.share.mat) <- c("Portlandia", "Levittown", "Potemkin")


stargazer(higher.prod.share.mat, summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Econ 871 Trade/Problem sets/PS3/table3.tex", 
  rownames=TRUE, title="Bilateral trade share matrix, $T_{2} = 3, \\tau_{12} = \\tau_{21} = 1.05, \\tau_{13} = \\tau_{31} = \\tau_{32} = \\tau_{23} = 1.3$.")

higher.prod.welfare.disp.mat <- matrix(c(higher.prod.wages/higher.prod.wages[1], higher.prod.welfare), nrow=2, byrow=TRUE)

colnames(higher.prod.welfare.disp.mat) <- c("Portlandia", "Levittown", "Potemkin")
rownames(higher.prod.welfare.disp.mat) <- c("Wages", "Welfare")

stargazer(higher.prod.welfare.disp.mat, summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Econ 871 Trade/Problem sets/PS3/table4.tex", 
  rownames=TRUE, title="Wage and welfare measure, $T_{2} = 3, \\tau_{12} = \\tau_{21} = 1.05, \\tau_{13} = \\tau_{31} = \\tau_{32} = \\tau_{23} = 1.3$.")




# QUESTION 1(c)

high.theta.wages<- optim(par=rep(1,3), fn=simplified.EK.fn, tau_n_i = far.away.tau.mat, distn_theta=8, K.goods=100000,  control=list(trace=5))$par

high.theta.wages/high.theta.wages[1]


high.theta.share.mat <- simplified.EK.fn(high.theta.wages, tau_n_i = far.away.tau.mat, distn_theta=8, ret.share.mat=TRUE)

high.theta.welfare <- simplified.EK.fn(high.theta.wages, tau_n_i = far.away.tau.mat, distn_theta=8, ret.welfare=TRUE)


rownames(high.theta.share.mat ) <- c("Portlandia", "Levittown", "Potemkin")
colnames(high.theta.share.mat) <- c("Portlandia", "Levittown", "Potemkin")


stargazer(high.theta.share.mat, summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Econ 871 Trade/Problem sets/PS3/table5.tex", 
  rownames=TRUE, title="Bilateral trade share matrix, $\\theta = 8, \\tau_{12} = \\tau_{21} = 1.05, \\tau_{13} = \\tau_{31} = \\tau_{32} = \\tau_{23} = 1.3$.")

high.theta.welfare.disp.mat <- matrix(c(high.theta.wages/high.theta.wages[1], high.theta.welfare), nrow=2, byrow=TRUE)

colnames(high.theta.welfare.disp.mat) <- c("Portlandia", "Levittown", "Potemkin")
rownames(high.theta.welfare.disp.mat) <- c("Wages", "Welfare")

stargazer(high.theta.welfare.disp.mat, summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Econ 871 Trade/Problem sets/PS3/table6.tex", 
  rownames=TRUE, title="Wage and welfare measure, $\\theta = 8, \\tau_{12} = \\tau_{21} = 1.05, \\tau_{13} = \\tau_{31} = \\tau_{32} = \\tau_{23} = 1.3$.")


# QUESTION 2(b)



risk.share.system <- function(params, alphas, y.s, n.agents) {
  consumption.FOCs <- params[1:n.agents]^-alphas - params[n.agents + 1]
  lagrangean.FOC <- sum(y.s) - sum(params[1:n.agents])
  
  sum(c(consumption.FOCs, lagrangean.FOC)^2)  
}


first.state.result <- optim(par=c(1,1,1,1), fn=risk.share.system, alphas=c(2,5,10), y.s=c(2,5,2), n.agents=3, control=list(trace=5))$par

first.state.mat <- matrix(c(first.state.result[-4], c(2,5,2)), byrow=TRUE, nrow=2)

first.state.mat <- rbind(first.state.mat, first.state.mat[2, ] - first.state.mat[1, ])

rownames(first.state.mat) <- c("Consumption", "Endowment", "Trade balance")
colnames(first.state.mat) <- c("Portlandia", "Levittown", "Potemkin")


stargazer(first.state.mat, summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Econ 871 Trade/Problem sets/PS3/table7.tex", 
  rownames=TRUE, title=paste0("Risk sharing profile for $y=\\{2,5,2\\}$; $p(s) =", round(first.state.result[4], 3), "$."))

# QUESTION 2(c)

second.state.result <- optim(par=c(1,1,1,1), fn=risk.share.system, alphas=c(2,5,10), y.s=c(1,2.5,1), n.agents=3, control=list(trace=5))$par


second.state.mat <- matrix(c(second.state.result[-4], c(1,2.5,1)), byrow=TRUE, nrow=2)

second.state.mat <- rbind(second.state.mat, second.state.mat[2, ] - second.state.mat[1, ])

rownames(second.state.mat) <- c("Consumption", "Endowment", "Trade balance")
colnames(second.state.mat) <- c("Portlandia", "Levittown", "Potemkin")


stargazer(second.state.mat, summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Econ 871 Trade/Problem sets/PS3/table8.tex", 
  rownames=TRUE, title=paste0("Risk sharing profile for $y=\\{1,2.5,1\\}$; $p(s) =", round(second.state.result[4], 3), "$."))


# QUESTION 2(d)

#log(second.state.mat[-3, ]) - log(first.state.mat[-3, ])
second.state.mat[-3, ] / first.state.mat[-3, ] - 1


stargazer((second.state.mat[-3, ] / first.state.mat[-3, ] - 1)*100, 
  summary=FALSE, out.header = FALSE, out="/Users/travismcarthur/Desktop/Econ 871 Trade/Problem sets/PS3/table9.tex", 
  rownames=TRUE, title="Percent change in consumption and endowment, first state to second state.")


pow.util <- function(x, alpha) {x^(1-alpha)/(1-alpha)}

plot(x=seq(1, 6, by=.01), y=pow.util(seq(1, 6, by=.01), 10), type="l", ylim=c(-1, 0))

lines(x=seq(1, 6, by=.01), y=pow.util(seq(1, 6, by=.01), 5), col="blue")
lines(x=seq(1, 6, by=.01), y=pow.util(seq(1, 6, by=.01), 2), col="red")






