
# USED TO HAVE M, N, J defined here

eliminate.irrigation.colinear.sgm <- TRUE


lead.zero <- function(x) {formatC(x, width = 2, flag = "0")}


J.2.dim <- expand.grid(paste0(".", lead.zero(1:J)), paste0(".", lead.zero(1:J)))

ln.sh.q.grid <- expand.grid( paste0("q", lead.zero(1:J),  " * "),
  paste0("q", lead.zero(1:J),  "/y01^2") )
  
ln.sh.q.grid.2 <- do.call(paste0, ln.sh.q.grid)


N.2.dim <- expand.grid(paste0(".", lead.zero(1:N)), paste0(".", lead.zero(1:N)))

ln.sh.w.grid <- expand.grid( paste0("w", lead.zero(1:N), " * "),
  paste0("w", lead.zero(1:N)) )
  
ln.sh.w.grid.2 <- do.call(paste0, ln.sh.w.grid)
  
ln.sh.s.grid <- expand.grid( paste0("s.", lead.zero(1:N)),
  paste0(".", lead.zero(1:N)) )
  
ln.sh.s.grid.2 <- do.call(paste0, ln.sh.s.grid)



N.J.dim <- expand.grid(paste0(".", lead.zero(1:N)), paste0(".", lead.zero(1:J)))

wn.qj <- expand.grid(paste0("w", lead.zero(1:N)), paste0("q", lead.zero(1:J), "/y01"))




# LET "N" be the thing that we replace to get each 1,...,N equation

first.term <- "b.y.N"
second.term <- "b.N/y01"
third.term <- "beta.N * b.y.y * y01"
fourth.term <- paste0("d.N.", lead.zero(1:J), " * q", lead.zero(1:J), "/y01", collapse=" + ")
fifth.term <- paste0("delta.N * (", 
  paste0("c.", lead.zero(1:J), " * q", lead.zero(1:J), collapse=" + "),
  ")")
sixth.term <- paste0("(eta.N/2) * (",
  paste0("c", do.call(paste0, J.2.dim ), " * ", ln.sh.q.grid.2, collapse=" + "),
  ")")
  
psi.w.summation <- paste0("(", paste0("psi.", lead.zero(1:N), " * w", lead.zero(1:N), collapse=" + "), ")")

first.g.term.numerator <- paste0("(", paste0("s.N.", lead.zero(1:N), " * w", lead.zero(1:N), collapse=" + "), ")")

first.g.term <- paste0(first.g.term.numerator, " / ", psi.w.summation)

second.g.term.numerator <- paste0("(", paste0(ln.sh.s.grid.2, " * ", ln.sh.w.grid.2 , collapse=" + "), ")")

second.g.term <- paste0("(psi.N/2) * ", second.g.term.numerator, "/", psi.w.summation, "^2")

demand.eqns.raw <- paste(first.g.term, " - ", second.g.term, " + ", first.term, " + ",
 second.term, " + ", third.term, " + ", fourth.term, " + ", fifth.term, " + ", sixth.term)
  
demand.eqns <- list()

for ( i in 1:N) {
  demand.eqns[[i]] <- gsub("N", lead.zero(i), demand.eqns.raw)
}


# Now make full cost function


cost.fn.g.term <- paste0("(1/2) * ", second.g.term.numerator, "/", psi.w.summation)


cost.fn.first.term <- paste0("b.y.", lead.zero(1:N), " * w", lead.zero(1:N), collapse=" + ")
cost.fn.second.term <- paste0("b.", lead.zero(1:N), " * w", lead.zero(1:N), "/y01", collapse= " + ")

cost.fn.third.term <- paste0( "(",
  paste0("beta.", lead.zero(1:N), " * w", lead.zero(1:N), collapse=" + "), ")",
  " * b.y.y * y01" )

cost.fn.fourth.term <- paste0("d", do.call(paste0, N.J.dim), " * ", wn.qj[[1]], " * ", wn.qj[[2]], collapse=" + " )

cost.fn.fifth.term <- paste0("(", 
  paste0("c.", lead.zero(1:J), " * q", lead.zero(1:J), collapse=" + "),
  ")",
  " * (",
  paste0("delta.", lead.zero(1:N), " * w", lead.zero(1:N), collapse=" + "), ")")
  
  
cost.fn.sixth.term <- paste0("(1/2) * (",
  paste0("c", do.call(paste0, J.2.dim ), " * ", ln.sh.q.grid.2, collapse=" + "),
  ")",
  " * (",
  paste0("eta.", lead.zero(1:N), " * w", lead.zero(1:N), collapse=" + "), ")")


demand.eqns[[length(demand.eqns) + 1]] <- 
  paste(cost.fn.g.term, cost.fn.first.term, cost.fn.second.term, cost.fn.third.term,
    cost.fn.fourth.term, cost.fn.fifth.term, cost.fn.sixth.term, sep=" + ")


# Ok, so the cost fn is going to be treated like any demand function







# S matrix N-1 rank restriction:

demand.eqns <- lapply(demand.eqns, FUN=function(x) {
  for ( i in 1:N) {
  x <- gsub(paste0("s.01.", lead.zero(i)), 
    paste0("(-(", paste0("s.", lead.zero(2:N), ".", lead.zero(i), collapse=" + "), " ))" ),
    x)

  x <- gsub(paste0("s.", lead.zero(i), ".01"), 
    paste0("(-(", paste0("s.", lead.zero(i), ".", lead.zero(2:N), collapse=" + "), " ))" ),
    x)
  }
  x
}
)


# Imposing symmetry:

library(stringr)

demand.eqns <- lapply(demand.eqns, FUN=function(x) {

replacements <- data.frame(greek=c("s", "c"), N=c(N,J), M=c(N,J))
# s corresponds to beta and c corresponds to zeta in the translog

for ( k in 1:nrow(replacements) ) {

if (replacements$greek[k]=="c" & J==1) {next}

# ok the below is trying to reverse it:
#symm.mat<-matrix(paste0(replacements$greek[k], ".", apply(X=expand.grid(lead.zero(1:max(N,M)), lead.zero(1:max(N,M))), MARGIN=1, FUN=paste, collapse=".")), nrow=max(N,M), ncol=max(N,M))

symm.mat<-matrix(paste0(replacements$greek[k], ".", apply(X=expand.grid(lead.zero(1:N), lead.zero(1:N)), MARGIN=1, FUN=paste, collapse=".")), nrow=N, ncol=N)

symm.mat[upper.tri(symm.mat, diag = FALSE)] <- t(symm.mat)[upper.tri(symm.mat, diag = FALSE)]
symm.mat<-symm.mat[1:replacements$N[k], 1:replacements$M[k]]


#expanded.greeks.grid <- expand.grid(lead.zero(1:max(N,M)), lead.zero(1:max(N,M)))
expanded.greeks.grid <- expand.grid(lead.zero(1:N), lead.zero(1:N))
expanded.greeks.grid <- data.frame(expanded.greeks.grid$Var2, expanded.greeks.grid$Var1)

#symm.mat.2<-matrix(paste0(replacements$greek[k], ".", apply(X=expanded.greeks.grid, MARGIN=1, FUN=paste, collapse=".")), nrow=max(N,M), ncol=max(N,M))
symm.mat.2<-matrix(paste0(replacements$greek[k], ".", apply(X=expanded.greeks.grid, MARGIN=1, FUN=paste, collapse=".")), nrow=N, ncol=N)

symm.mat.2[upper.tri(symm.mat.2, diag = FALSE)] <- t(symm.mat.2)[upper.tri(symm.mat.2, diag = FALSE)]
symm.mat.2<-symm.mat.2[1:replacements$N[k], 1:replacements$M[k]]

if (replacements$greek[k]=="s") {
for ( i in 1:length(c(symm.mat.2))) {
#  x <- str_replace_all(x, c(symm.mat.2)[i], c(symm.mat)[i])
  x <- str_replace_all(x, c(symm.mat)[i], c(symm.mat.2)[i])
}
}

if (replacements$greek[k]=="c") {
for ( i in 1:length(c(symm.mat.2))) {
  x <- str_replace_all(x, c(symm.mat)[i], c(symm.mat.2)[i])
}
}


}
x
}
)

if (eliminate.irrigation.colinear.sgm) {

  demand.eqns <- lapply(demand.eqns, FUN=function(x) {
    gsub("c.02 * q02 + ", "c.02.02 * q02 + ", x, fixed=TRUE)
  }
  )
  # Ok, this is a better way of doing it. It will mean that the c.02.02 will appear twice, 
  # and may deal with some of the issues with convexity in fixed inputs seeming to be too stringent
  # of a requirement

}















demand.eqns <- lapply(demand.eqns, FUN=function(x) {
  gsub("(psi)|(beta)|(delta)|(eta)", "inputmean", x)
}
)

#demand.err.support <- c(-30, 0, 30)

#other.param.support <-  seq(-30, 30, length.out=5)



#s.06.01
#psi.01
#b.y.06
#b.06
#beta.06
#b.y.y
#d.06.01
#delta.06
#eta.06
#c.01.01



demand.eqns.nonlinear <- lapply(demand.eqns, FUN=function(x) {
  for ( i in 1:N) {
    x <- gsub(paste0("w", lead.zero(i)), 
 #     paste0("w", lead.zero(i), " * exp(xi.", lead.zero(i), ")"),
     paste0("w", lead.zero(i), " * xi.", lead.zero(i)),
      x
    )
  }
  x
}
)

if ( !include.cost.fn) {
  demand.eqns <- demand.eqns[-length(demand.eqns)]
  demand.eqns.nonlinear <- demand.eqns.nonlinear[-length(demand.eqns.nonlinear)]
}














