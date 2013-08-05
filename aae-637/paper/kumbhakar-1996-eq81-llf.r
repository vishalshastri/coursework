

M <- 3
J <- 2

betas.mat<-matrix(paste0("beta", apply(X=expand.grid(1:M, 1:M), MARGIN=1, FUN=paste, collapse="")), nrow=M, ncol=M)
betas.mat[upper.tri(betas.mat, diag = FALSE)] <- t(betas.mat)[upper.tri(betas.mat, diag = FALSE)]

gammas.mat<-matrix(paste0("gamma", apply(X=expand.grid(1:max(J,M), 1:max(J,M)), MARGIN=1, FUN=paste, collapse="")), nrow=max(J,M), ncol=max(J,M))
gammas.mat[upper.tri(gammas.mat, diag = FALSE)] <- t(gammas.mat)[upper.tri(gammas.mat, diag = FALSE)]
gammas.mat<-gammas.mat[1:J, 1:M]

alphas.mat<-matrix(paste0("alpha", apply(X=expand.grid(1:J, 1:J), MARGIN=1, FUN=paste, collapse="")), nrow=J, ncol=J)
alphas.mat[upper.tri(alphas.mat, diag = FALSE)] <- t(alphas.mat)[upper.tri(alphas.mat, diag = FALSE)]




kappas = paste0("kappa", 1:M)
thetas = paste0("theta", 1:J)
data.p = paste0("p", 1:M)
data.y = paste0("y", 1:M)
data.w = paste0("w", 1:J)
data.x = paste0("x", 1:J)


R0m.v<-c()

for (m in 1:M) {

  R0m.v <- c(R0m.v, 
    paste0( 
    paste0("beta", m), " + ",
    "sum(c(", paste0( betas.mat[m, ], " * ", paste0("log(", kappas, " * ", data.p, "/", data.p[1], ")"), collapse=", "), "))", " + ",
# TODO: Not sure that the betas and kappas line up
    "sum(c(", paste0( gammas.mat[, m], " * ", paste0("log(", thetas, " * ", data.w, "/", data.w[1], ")"), collapse=", " ), "))",
    collapse=""
  )
  )
  
}

Q0j.v<-c()

for (j in 1:J) {

  Q0j.v <- c(Q0j.v, 
    paste0( 
    paste0("beta", m), " + ",
    "sum(c(", paste0( alphas.mat[j, ], " * ", paste0("log(", thetas, " * ", data.w, "/", data.p[1], ")"), collapse=", "), "))", " + ",
# TODO: Not sure that the betas and kappas line up
    "sum(c(", paste0( gammas.mat[j, ], " * ", paste0("log(", kappas, " * ", data.p, "/", data.p[1], ")"), collapse=", " ), "))",
    collapse=""
  )
  )
  
}
# TODO: Double check the thetas and Kappas here and data.p, etc.

H0 <- paste0( "(",
  "sum(c(", paste0( "(1/kappa", 1:M, " - 1) * (", R0m.v, ")", collapse=", "), ")) + ",
  "sum(c(", paste0( "(1/theta", 1:J, " - 1) * (", Q0j.v, ")", collapse=", "), 
  ")) )")
# TODO: Fix kappa 1:m to kappas
  
psi <- paste0( "(",
  "sum(c(", paste0( "(1/kappa", 1:M, " - 1) * sum(c(", apply(gammas.mat, 2, FUN=paste0, collapse=", "), "))", collapse=", "), ")) + ",
  "sum(c(", paste0( "(1/theta", 1:J, " - 1) * sum(c(", apply(alphas.mat, 1, FUN=paste0, collapse=", "), "))", collapse=", "), 
  ")) )")
  
Ram <- paste0("(", data.p, " * ", data.y, "/profit)")

u.R <- paste0("(", Ram, " * kappa", 1:M, " * ", H0, " - ", R0m.v, ")/(",
  "sum(c(",apply(gammas.mat, 1, FUN=paste0, collapse=", "), ")) - kappa", 1:M, " * ", psi, " * ", Ram, ")")
  
Qaj <- paste0("(", data.w, " * ", data.x, "/profit)")

u.Q <- paste0("- (", Qaj, " * theta", 1:J, " * ", H0, " + ", Q0j.v, ")/(",
  "sum(c(", apply(alphas.mat, 1, FUN=paste0, collapse=", "), ")) + theta", 1:M, " * ", psi, " * ", Qaj, ")")




u.R[1]

R0m.v is syntatically valid
Q0j.v is syntatically valid
psi   is syntatically valid
H0    is syntatically valid
u.R   is syntatically valid
u.Q   is syntatically valid

TODO: need to discard first element of u.R since it is m = 2,...,M



big.sigma0.mat <-matrix(paste0("sigma.mat", apply(X=expand.grid(1:(M+J-1), 1:(M+J-1)), MARGIN=1, FUN=paste, collapse="")), nrow=(M+J-1), ncol=(M+J-1))
big.sigma0.mat[upper.tri(big.sigma0.mat, diag = FALSE)] <- t(big.sigma0.mat)[upper.tri(big.sigma0.mat, diag = FALSE)]

mat.counter <- paste0(rep(1, M+J), collapse=",")

big.sigma0.mat.char <- paste("as.matrix(c(", paste0(big.sigma0.mat, collapse=","), "),", "ncol=length(c(", mat.counter,  "))")

sigma0.sq <- paste0( "(1/sigma.u.sq + rep(1, M+J) %*% ", big.sigma0.mat.char, " %*% rep(1, M+J))^-1)")

Ok, so this must be first line of function or something:


a0 <- paste0("(xi %*% (", big.sigma0.mat.char, ")^-1 %*% xi - ", sigma0.sq, " * (xi %*% (", big.sigma0.mat.char, ")^-1 %*% rep(1, M+J))^2 )")

Di


# TODO: Must define N, J, M


# TODO: Di not defined yet






gamma0 <- paste0( "sum(c(",apply(gammas.mat, 1, FUN=paste0, collapse=", "), "))")
alpha0 <- paste0( "sum(c(",apply(alphas.mat, 1, FUN=paste0, collapse=", "), "))")

#(x*k*h-r)/(g-k*p*x)
#deriv:
#(k(g*h-p*r))/(g-k*p*x)^2

#-(x*t*h+q)/(a+t*p*x)
#deriv:
#(t(p*q-a*h))/(a+p*t*x)^2

u.R.deriv <- paste0(
  "(,", kappas, " * (", gamma0, " * ", H0, " - ", psi, " * ", R0m.v, "))/(", gamma0, " - ", kappas, " * ", psi, " * ", Ram, ")^2"
)

u.Q.deriv <- paste0(
  "(", thetas, " * (", psi, " * ", Q0j.v, " - ", alpha0, " * ", H0, "))/(", alpha0, " + ", psi, " * ", thetas, " * ", Qaj, ")^2"
)

Di <- paste(u.R.deriv, u.Q.deriv, sep=" * ", collapse=" * ")


# Maybe I dont need at those extra c( in sum()


# START FUNCTION
arguments <- paste(unique(c(betas.mat)), unique(c(gammas.mat)), 
  unique(c(alphas.mat)), kappas, thetas, "sigma.u.sq", unique(c(big.sigma0.mat)), 
  data.p, data.y, data.w, data.x, sep=", ", collapse=", ")

first.line <- paste0( "function(", arguments, ") {")
second.line <- paste0("  xi <- c(", paste( u.R, u.Q, sep=",", collapse=","), ")")
third.line <- "  N <- length(y1)"
fourth.line <-paste0(
  "  N * log(2) - (N(", J, " + ", M, " - 1)/2) * log(2*pi) - (N/2) * log(det(", big.sigma0.mat.char,
  ")) + N * log(", sigma0.sq, "^.5) + sum(log(pnorm(xi %*% (", big.sigma0.mat.char, 
  ")^-1 %*% rep(1, ", M+J, ") * ",
  sigma0.sq, "^.5))) - N * log(sigma.u.sq^.5) - .5 * sum(", a0, ") + sum(log(prod(", Di, ")))"
)
fifth.line <- "}"

# May want to change the DI to sum of logs for numerical accuracy

ret <-paste(first.line, second.line, third.line, fourth.line, fifth.line, sep="\n\n")

#TODO: Not sure if it is M+J-1 or M+J


# END






> ML1 <- function(prob1,prob2,prob3,theta,x) {
prob <- c(prob1,prob2,prob3)[as.numeric(x$dilution)]
size <- x$n
-sum(dbetabinom(x$y,prob,size,theta,log=TRUE))
}

(m1 <- mle2(ML1,start=list(prob1=0.5,prob2=0.5,prob3=0.5,theta=1),
data=list(x=orob1)))



# not needed: xi <- paste0("c(", u.R, "," u.Q, ")", collapse=",") 
pnorm









  
  
paste(matrix(1:9, nrow=3))

apply(matrix(1:9, nrow=3), 1, paste, collapse="")


  
  paste0( "(p", m, " * ", "y", m, ")/profit", " * ", "kappa", m, " * ", H0
  

data.p


diag(x) <- paste(1:m, 1:m, sep="")

x[upper.tri(x, diag = FALSE)] <- combn(1:m, 2, FUN=paste, collapse="")
t(x)



for 

paste(1:m, 1:m, sep="")



Q0j


H0= R0m * sum(1/kappa.m - 1) + Q0j * sum(1/theta.j - 1)
psi=


