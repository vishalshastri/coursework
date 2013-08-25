llf.creator.fn <- function(M, J) {





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
    "(", paste0( betas.mat[m, ], " * ", paste0("log(", kappas[-1], " * ", data.p[-1], "/", data.p[1], ")"), collapse=" + "), ")", " + ",
# TODO: Not sure that the betas and kappas line up
    "(", paste0( gammas.mat[, m], " * ", paste0("log(", thetas, " * ", data.w, "/", data.w[1], ")"), collapse=" + " ), ")",
    collapse=""
  )
  )
  
}

Q0j.v<-c()

for (j in 1:J) {

  Q0j.v <- c(Q0j.v, 
    paste0( 
    paste0("alpha", j), " + ",
    "(", paste0( alphas.mat[j, ], " * ", paste0("log(", thetas, " * ", data.w, "/", data.p[1], ")"), collapse=" + "), ")", " + ",
# TODO: Not sure that the betas and kappas line up
    "(", paste0( gammas.mat[j, -1, drop=FALSE], " * ", paste0("log(", kappas[-1], " * ", data.p[-1], "/", data.p[1], ")"), collapse=" + " ), ")",
    collapse=""
  )
  )
  
}
# TODO: Double check the thetas and Kappas here and data.p, etc.

H0 <- paste0( "(",
  "(", paste0( "(1/kappa", 2:M, " - 1) * (", R0m.v[-1], ")", collapse=" + "), ") + ",
  "(", paste0( "(1/theta", 1:J, " - 1) * (", Q0j.v, ")", collapse=" + "), 
  ") )")
# TODO: Fix kappa 1:m to kappas
  
psi <- paste0( "(",
  "(", paste0( "(1/kappa", 2:M, " - 1) * (", apply(gammas.mat[,-1, drop=FALSE], 2, FUN=paste0, collapse=" + "), ")", collapse=" + "), ") + ",
  "(", paste0( "(1/theta", 1:J, " - 1) * (", apply(alphas.mat, 1, FUN=paste0, collapse=" + "), ")", collapse=" + "), 
  ") )")
  
Ram <- paste0("(", data.p, " * ", data.y, "/profit)")

u.R <- paste0("(", Ram, " * kappa", 1:M, " * ", H0, " - ", R0m.v, ")/(",
  "(",apply(gammas.mat, 1, FUN=paste0, collapse=" + "), ") - kappa", 1:M, " * ", psi, " * ", Ram, ")")
  
u.R <- u.R[-1]
# TODO: not 100% sure about cutting of first one
  
Qaj <- paste0("(", data.w, " * ", data.x, "/profit)")

u.Q <- paste0("- (", Qaj, " * theta", 1:J, " * ", H0, " + ", Q0j.v, ")/(",
  "(", apply(alphas.mat, 1, FUN=paste0, collapse=" + "), ") + theta", 1:J, " * ", psi, " * ", Qaj, ")")


#R0m.v is syntatically valid
#Q0j.v is syntatically valid
#psi   is syntatically valid
#H0    is syntatically valid
#u.R   is syntatically valid
#u.Q   is syntatically valid

#TODO: need to discard first element of u.R since it is m = 2,...,M



big.sigma0.mat <-matrix(paste0("sigma.mat", apply(X=expand.grid(1:(M+J-1), 1:(M+J-1)), MARGIN=1, FUN=paste, collapse="")), nrow=(M+J-1), ncol=(M+J-1))
big.sigma0.mat[upper.tri(big.sigma0.mat, diag = FALSE)] <- t(big.sigma0.mat)[upper.tri(big.sigma0.mat, diag = FALSE)]

mat.counter <- paste0(rep(1, M+J-1), collapse=",")

big.sigma0.mat.char <- paste("matrix(c(", paste0(big.sigma0.mat, collapse=","), "),", "ncol=length(c(", mat.counter,  ")))")

# as.matrix(nearPD(   )$mat)

sigma0.sq <- paste0( "((1/sigma.u.sq + rep(1, ", M+J-1, ") %*% solve(", big.sigma0.mat.char, ") %*% rep(1, ", M+J-1, "))^-1)")

a0 <- paste0("(rowSums(xi %*% solve(", big.sigma0.mat.char, ") * xi) - c(", sigma0.sq, ") * rowSums(xi %*% solve(", big.sigma0.mat.char, ") )^2)")
# %*% rep(1, ", M+J-1, ") %*% t( xi %*% (", big.sigma0.mat.char, ")^-1 %*% rep(1, ", M+J-1, ") )) 

gamma0 <- paste0( "(",apply(gammas.mat, 1, FUN=paste0, collapse=" + "), ")")
alpha0 <- paste0( "(",apply(alphas.mat, 1, FUN=paste0, collapse=" + "), ")")
cat("M:",M, "J:", J)

#(x*k*h-r)/(g-k*p*x)
#deriv:
#(k(g*h-p*r))/(g-k*p*x)^2

#-(x*t*h+q)/(a+t*p*x)
#deriv:
#(t(p*q-a*h))/(a+p*t*x)^2

u.R.deriv <- paste0(
  "(", kappas, " * (", gamma0, " * ", H0, " - ", psi, " * ", R0m.v, "))/(", gamma0, " - ", kappas, " * ", psi, " * ", Ram, ")^2"
)

u.Q.deriv <- paste0(
  "(", thetas, " * (", psi, " * ", Q0j.v, " - ", alpha0, " * ", H0, "))/(", alpha0, " + ", psi, " * ", thetas, " * ", Qaj, ")^2"
)

# Di <- paste(u.R.deriv[-1], u.Q.deriv, sep=" *\n\n ", collapse=" *\n\n ")

Di <-  paste0("apply(matrix(c(", 
  paste(paste(u.R.deriv[-1], collapse=" ,\n\n "), paste(u.Q.deriv, collapse=" ,\n\n "), sep=" ,\n\n "), 
  "), ncol=", M+J-1, "), MARGIN=1, FUN=prod)")

# Maybe I dont need at those extra c( in sum()



# START FUNCTION
arguments <- paste(c(paste0("beta", 2:M), paste0("alpha", 1:J), unique(c(betas.mat[betas.mat!="beta11"])), unique(c(gammas.mat)), 
  unique(c(alphas.mat)), kappas[-1], thetas, "sigma.u.sq", unique(c(big.sigma0.mat)), 
  data.p, data.y, data.w, data.x, "profit"), sep=", ", collapse=", ")

first.line <- paste0( "eff.llf <- function(", arguments, ") {")
second.line <- paste0("  xi <- matrix(c(\n", paste0(u.R, collapse="\n,\n"), "\n,\n", 
  paste0(u.Q, collapse="\n,\n"), "), ncol=", M+J-1, ")\n")
third.line <- paste0("  N <- length(y1)")
# - ", sigma0.sq
# paste0("(xi %*% (", big.sigma0.mat.char, ")^-1 %*% t(xi) ", ")"), ")"
# ; cat((", big.sigma0.mat.char, "))
 

fourth.line <-paste0(
  " ret <- - (N * log(2) - (N * (", J, " + ", M, " - 1)/2) * log(2*pi) - (N/2) * log(det(", big.sigma0.mat.char,
  ")) + N * log(", sigma0.sq, "^.5) + sum(log(pnorm(xi %*% solve(", big.sigma0.mat.char, 
  ") %*% rep(1, ", M+J-1, ") %*% ",
  sigma0.sq, "^.5))) - N * log(sigma.u.sq^.5) - .5 * sum(", a0, ") + \nsum(log(", Di, ")))"
)
fifth.line <- paste0(" if (is.finite(ret)) {return(ret)} else {return(1e+308)}\n}")
# cat((", a0, "));

# M+J is bare?

# May want to change the DI to sum of logs for numerical accuracy

ret <- paste(first.line, second.line, third.line, fourth.line, fifth.line, sep="\n\n")

#TODO: Not sure if it is M+J-1 or M+J

return(ret)

# END

}


function.text <- llf.creator.fn(12,5)
eval(parse(text=function.text))


eff.llf(beta1=1, beta2=1, alpha1=1, alpha2=1,
  beta11=1, beta21=1, beta22=1, gamma11=1, gamma21=1, gamma22=1, alpha11=1, alpha21=1, alpha22=1, kappa1=1, kappa2=1, theta1=1, theta2=1, sigma.u.sq=10, sigma.mat11=100, sigma.mat21=10, sigma.mat31=10, sigma.mat22=100, sigma.mat32=10, sigma.mat33=100, 
  p1=eff.prod.df$price.MAIZ, p2=eff.prod.df$price.PAPA, y1=eff.prod.df$total.value.MAIZ/eff.prod.df$price.MAIZ, y2=eff.prod.df$total.value.PAPA/eff.prod.df$price.PAPA, w1=eff.prod.df$labor.price, w2=eff.prod.df$fertilizer.price, x1=eff.prod.df$labor.input, x2=eff.prod.df$fertilizer.input,
  profit=eff.prod.df$total.value.MAIZ+eff.prod.df$total.value.PAPA - 
  (eff.prod.df$labor.price*eff.prod.df$labor.input + eff.prod.df$fertilizer.price*eff.prod.df$fertilizer.input)
  )







m0 <- mle2(eff.llf,
  start=list(beta1=1, beta2=1, alpha1=1, alpha2=1,
  beta11=1, beta21=1, beta22=1, gamma11=1, gamma21=1, gamma22=1, alpha11=1, alpha21=1, alpha22=1, kappa1=1, kappa2=1, theta1=1, theta2=1, sigma.u.sq=10, sigma.mat11=100, sigma.mat21=10, sigma.mat31=10, sigma.mat22=100, sigma.mat32=10, sigma.mat33=100), 
  data=list(p1=eff.prod.df$price.MAIZ, p2=eff.prod.df$price.PAPA, y1=eff.prod.df$total.value.MAIZ/eff.prod.df$price.MAIZ, y2=eff.prod.df$total.value.PAPA/eff.prod.df$price.PAPA, w1=eff.prod.df$labor.price, w2=eff.prod.df$fertilizer.price, x1=eff.prod.df$labor.input, x2=eff.prod.df$fertilizer.input,
  profit=eff.prod.df$total.value.MAIZ+eff.prod.df$total.value.PAPA - 
  (eff.prod.df$labor.price*eff.prod.df$labor.input + eff.prod.df$fertilizer.price*eff.prod.df$fertilizer.input)),  method= "Nelder-Mead",   control=list(trace=5, REPORT=1, maxit=10000))
  
test <-eff.DE.3$optim$bestmem
names(test) <- names(formals(eff.llf))[1:311]

m0.2 <- mle2(eff.llf,
  start=as.list(test),  
  data=list(w1=w1, w2=w2, w3=w3, w4=w4, w5=w5, x1=x1, x2=x2, x3=x3, x4=x4, x5=x5, 
p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6, p7=p7, p8=p8, p9=p9, p10=p10, 
p11=p11, p12=p12, y1=y1, y2=y2, y3=y3, y4=y4, y5=y5, y6=y6, y7=y7, y8=y8,
y9=y9, y10=y10, y11=y11, y12=y12, profit=profit),
  method= "Nelder-Mead",   control=list(trace=5, REPORT=1, maxit=10000))




m0.2 <- mle2(eff.llf,
  start=as.list(test),  
  data=list(w1=w1, w2=w2, w3=w3, w4=w4, w5=w5, x1=x1, x2=x2, x3=x3, x4=x4, x5=x5, 
p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6, p7=p7, p8=p8, p9=p9, p10=p10, 
p11=p11, p12=p12, y1=y1, y2=y2, y3=y3, y4=y4, y5=y5, y6=y6, y7=y7, y8=y8,
y9=y9, y10=y10, y11=y11, y12=y12, profit=profit),
  method= "BFGS",   control=list(trace=5, REPORT=1, maxit=10000))
  


# , 
#  data=list(p1=eff.prod.df$price.MAIZ, p2=eff.prod.df$price.PAPA, y1=eff.prod.df$total.value.MAIZ/eff.prod.df$price.MAIZ, y2=eff.prod.df$total.value.PAPA/eff.prod.df$price.PAPA, w1=eff.prod.df$labor.price, w2=eff.prod.df$fertilizer.price, x1=eff.prod.df$labor.input, x2=eff.prod.df$fertilizer.input,
#  profit=eff.prod.df$total.value.MAIZ+eff.prod.df$total.value.PAPA - 
  (eff.prod.df$labor.price*eff.prod.df$labor.input + eff.prod.df$fertilizer.price*eff.prod.df$fertilizer.input))

  
test <-eff.DE$optim$bestmem
names(test) <- NULL

m0.1 <- mle2(eff.llf.vec.arg,
  start=list(x=test),  method= "Nelder-Mead",   control=list(trace=5, REPORT=1, maxit=10000))
  

summary(m0.1)
  
  
  lower=c(rep(-Inf, 13), rep(0, 11)),
  c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN",
                 "Brent")
  $coef
  
  "Nelder-Mead"

NaNs:
log(kappa1 * p1/p1)

debug(eff.llf)


t(t(names(formals(eff.llf))))
      [,1]         
 [1,] "beta11"     
 [2,] "beta21"     
 [3,] "beta22"     
 [4,] "gamma11"    
 [5,] "gamma21"    
 [6,] "gamma31"    
 [7,] "gamma22"    
 [8,] "gamma32"    
 [9,] "alpha11"    
[10,] "alpha21"    
[11,] "alpha31"    
[12,] "alpha22"    
[13,] "alpha32"    
[14,] "alpha33"    
[15,] "kappa1"     
[16,] "kappa2"     
[17,] "theta1"     
[18,] "theta2"     
[19,] "theta3"     
[20,] "sigma.u.sq" 
[21,] "sigma.mat11"
[22,] "sigma.mat21"
[23,] "sigma.mat31"
[24,] "sigma.mat41"
[25,] "sigma.mat22"
[26,] "sigma.mat32"
[27,] "sigma.mat42"
[28,] "sigma.mat33"
[29,] "sigma.mat43"
[30,] "sigma.mat44"

[31,] "p1"         
[32,] "p2"         
[33,] "y1"         
[34,] "y2"         
[35,] "w1"         
[36,] "w2"         
[37,] "w3"         
[38,] "x1"         
[39,] "x2"         
[40,] "x3"  








(m0 <- mle2(mtmp,start=list(prob=0.2,theta=9),data=list(size=50)))



det(
matrix(c("sigma.mat11", "sigma.mat21", "sigma.mat31", "sigma.mat21", 
       "sigma.mat22", "sigma.mat32", "sigma.mat31", "sigma.mat32", "sigma.mat33"), 
       ncol = length(c(1, 1, 1)))
       ) 





> ML1 <- function(prob1,prob2,prob3,theta,x) {
prob <- c(prob1,prob2,prob3)[as.numeric(x$dilution)]
size <- x$n
-sum(dbetabinom(x$y,prob,size,theta,log=TRUE))
}

(m1 <- mle2(ML1,start=list(prob1=0.5,prob2=0.5,prob3=0.5,theta=1),
data=list(x=orob1)))



# not needed: xi <- paste0("c(", u.R, "," u.Q, ")", collapse=",") 
pnorm





det(as.matrix(c(sigma.mat11, sigma.mat21, sigma.mat31, sigma.mat21, 
       sigma.mat22, sigma.mat32, sigma.mat31, sigma.mat32, sigma.mat33), 
       ncol = length(c(1, 1, 1, 1)))) at <text>#15





  
  
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




















################
################
################

llf.creator.fn.2 <- function(M, J) {




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
    "(", paste0( betas.mat[m, ], " * ", paste0("log(", kappas[-1], " * ", data.p[-1], "/", data.p[1], ")"), collapse=" + "), ")", " + ",
# TODO: Not sure that the betas and kappas line up
    "(", paste0( gammas.mat[, m], " * ", paste0("log(", thetas, " * ", data.w, "/", data.w[1], ")"), collapse=" + " ), ")",
    collapse=""
  )
  )
  
}

Q0j.v<-c()

for (j in 1:J) {

  Q0j.v <- c(Q0j.v, 
    paste0( 
    paste0("alpha", j), " + ",
    "(", paste0( alphas.mat[j, ], " * ", paste0("log(", thetas, " * ", data.w, "/", data.p[1], ")"), collapse=" + "), ")", " + ",
# TODO: Not sure that the betas and kappas line up
    "(", paste0( gammas.mat[j, -1, drop=FALSE], " * ", paste0("log(", kappas[-1], " * ", data.p[-1], "/", data.p[1], ")"), collapse=" + " ), ")",
    collapse=""
  )
  )
  
}
# TODO: Double check the thetas and Kappas here and data.p, etc.

H0 <- paste0( "(",
  "(", paste0( "(1/kappa", 2:M, " - 1) * (", R0m.v[-1], ")", collapse=" + "), ") + ",
  "(", paste0( "(1/theta", 1:J, " - 1) * (", Q0j.v, ")", collapse=" + "), 
  ") )")
# TODO: Fix kappa 1:m to kappas
  
psi <- paste0( "(",
  "(", paste0( "(1/kappa", 2:M, " - 1) * (", apply(gammas.mat[,-1, drop=FALSE], 2, FUN=paste0, collapse=" + "), ")", collapse=" + "), ") + ",
  "(", paste0( "(1/theta", 1:J, " - 1) * (", apply(alphas.mat, 1, FUN=paste0, collapse=" + "), ")", collapse=" + "), 
  ") )")
  
Ram <- paste0("(", data.p, " * ", data.y, "/profit)")

u.R <- paste0("(", Ram, " * kappa", 1:M, " * ", H0, " - ", R0m.v, ")/(",
  "(",apply(gammas.mat, 1, FUN=paste0, collapse=" + "), ") - kappa", 1:M, " * ", psi, " * ", Ram, ")")
  
u.R <- u.R[-1]
# TODO: not 100% sure about cutting of first one
  
Qaj <- paste0("(", data.w, " * ", data.x, "/profit)")

u.Q <- paste0("- (", Qaj, " * theta", 1:J, " * ", H0, " + ", Q0j.v, ")/(",
  "(", apply(alphas.mat, 1, FUN=paste0, collapse=" + "), ") + theta", 1:J, " * ", psi, " * ", Qaj, ")")


#R0m.v is syntatically valid
#Q0j.v is syntatically valid
#psi   is syntatically valid
#H0    is syntatically valid
#u.R   is syntatically valid
#u.Q   is syntatically valid

#TODO: need to discard first element of u.R since it is m = 2,...,M



big.sigma0.mat <-matrix(paste0("sigma.mat", apply(X=expand.grid(1:(M+J-1), 1:(M+J-1)), MARGIN=1, FUN=paste, collapse="")), nrow=(M+J-1), ncol=(M+J-1))
big.sigma0.mat[upper.tri(big.sigma0.mat, diag = FALSE)] <- t(big.sigma0.mat)[upper.tri(big.sigma0.mat, diag = FALSE)]

mat.counter <- paste0(rep(1, M+J-1), collapse=",")

big.sigma0.mat.char <- paste("matrix(c(", paste0(big.sigma0.mat, collapse=","), "),", "ncol=length(c(", mat.counter,  ")))")

# as.matrix(nearPD(   )$mat)

sigma0.sq <- paste0( "((1/sigma.u.sq + rep(1, ", M+J-1, ") %*% solve(", big.sigma0.mat.char, ") %*% rep(1, ", M+J-1, "))^-1)")

a0 <- paste0("(rowSums(xi %*% solve(", big.sigma0.mat.char, ") * xi) - c(", sigma0.sq, ") * rowSums(xi %*% solve(", big.sigma0.mat.char, ") )^2)")
# %*% rep(1, ", M+J-1, ") %*% t( xi %*% (", big.sigma0.mat.char, ")^-1 %*% rep(1, ", M+J-1, ") )) 

gamma0 <- paste0( "(",apply(gammas.mat, 1, FUN=paste0, collapse=" + "), ")")
alpha0 <- paste0( "(",apply(alphas.mat, 1, FUN=paste0, collapse=" + "), ")")
cat("M:",M, "J:", J)

#(x*k*h-r)/(g-k*p*x)
#deriv:
#(k(g*h-p*r))/(g-k*p*x)^2

#-(x*t*h+q)/(a+t*p*x)
#deriv:
#(t(p*q-a*h))/(a+p*t*x)^2

u.R.deriv <- paste0(
  "(", kappas, " * (", gamma0, " * ", H0, " - ", psi, " * ", R0m.v, "))/(", gamma0, " - ", kappas, " * ", psi, " * ", Ram, ")^2"
)

u.Q.deriv <- paste0(
  "(", thetas, " * (", psi, " * ", Q0j.v, " - ", alpha0, " * ", H0, "))/(", alpha0, " + ", psi, " * ", thetas, " * ", Qaj, ")^2"
)

# Di <- paste(u.R.deriv[-1], u.Q.deriv, sep=" *\n\n ", collapse=" *\n\n ")

Di <-  paste0("apply(matrix(c(", 
  paste(paste(u.R.deriv[-1], collapse=" ,\n\n "), paste(u.Q.deriv, collapse=" ,\n\n "), sep=" ,\n\n "), 
  "), ncol=", M+J-1, "), MARGIN=1, FUN=prod)")

# Maybe I dont need at those extra c( in sum()


# START FUNCTION
arguments <- paste(c(paste0( "beta", 2:M), paste0("alpha", 1:J), unique(c(betas.mat[betas.mat!="beta11"])), unique(c(gammas.mat)), 
  unique(c(alphas.mat)), kappas[-1], thetas, "sigma.u.sq", unique(c(big.sigma0.mat))),  sep="\", \"", collapse="\", \"")
cat(arguments)


first.line <- paste0( "eff.llf.vec.arg <- function(x) { \nargs <- c(\"", arguments, "\")\nfor ( i in 1:length(args)) { assign(args[i], x[i])}")
second.line <- paste0("  xi <- matrix(c(\n", paste0(u.R, collapse="\n,\n"), "\n,\n", 
  paste0(u.Q, collapse="\n,\n"), "), ncol=", M+J-1, ")\n")
third.line <- paste0("  N <- length(y1)")
# - ", sigma0.sq
# paste0("(xi %*% (", big.sigma0.mat.char, ")^-1 %*% t(xi) ", ")"), ")"
# ; cat((", big.sigma0.mat.char, "))
#; print(cov(xi))
 

fourth.line <-paste0(
  " ret <- - (N * log(2) - (N * (", J, " + ", M, " - 1)/2) * log(2*pi) - (N/2) * log(det(", big.sigma0.mat.char,
  ")) + N * log(", sigma0.sq, "^.5) + sum(log(pnorm(xi %*% solve(", big.sigma0.mat.char, 
  ") %*% rep(1, ", M+J-1, ") %*% ",
  sigma0.sq, "^.5))) - N * log(sigma.u.sq^.5) - .5 * sum(", a0, ") + \nsum(log(", Di, ")))"
)
fifth.line <- paste0("cat(log(det(", big.sigma0.mat.char, ")), sum(log(pnorm(xi %*% solve(", big.sigma0.mat.char, 
  ") %*% rep(1, ", M+J-1, ") %*% ",
  sigma0.sq, "^.5)))   ,  table(", Di , "<0), names(table(", Di , "<0)), ret, sum(log(", Di, ")), \" WWWWWWWWWWWWWW \"); if (is.finite(ret)) {return(ret)} else {return(1e+308)}\n}")
#fifth.line <- paste0(" if (is.finite(ret)) {return(ret)} else {return(1e+308)}\n}")
# cat((", a0, "));

# M+J is bare?

# May want to change the DI to sum of logs for numerical accuracy

ret <- paste(first.line, second.line, third.line, fourth.line, fifth.line, sep="\n\n")

#TODO: Not sure if it is M+J-1 or M+J


return(ret)

# END


}



function.text <- llf.creator.fn.2(12,5)
eval(parse(text=function.text))




sigma.lower<-rep(-2e+04, 136)
sigma.lower[c(1, 17, 32, 46, 59, 71, 82, 92, 101, 109, 116, 122, 127, 131, 134, 136)] <-0



eff.DE.3 <- DEoptim(eff.llf.vec.arg,lower=c(rep(-10, 158), rep(0, 16), 0, sigma.lower),
  upper=c(rep(10, 158), rep(10, 16), 2e+04, rep(2e+04, 136)),  
  control=list(trace=TRUE, NP=100, itermax=3))









xi %*% solve(", big.sigma0.mat.char, 
  ") %*% rep(1, ", M+J-1, ") %*% ",
  sigma0.sq, "^.5 


# table(", Di , "<0), names(table(", Di , "<0)), ret, sum(log(", Di, "))


%*% rep(1, ", M+J-1, ") %*% ",
  sigma0.sq, "^.5)



"imputed.ag.wage"
"land.area"               
[11] "seed.price"               "abono.price"             
[13] "fert.price.quintal"       "fert.price.liter"        
[15] "plaguicida.price.quintal" "plaguicida.price.liter"  
[17] "labor.hours"              "fert.quintals"           
[19] "plaguicida.liters"        "seed.quintals"           
[21] "abono.quintals"           "harvest.r.ARROZ"         
[23] "price.ARROZ"              "harvest.r.MAIZ"          
[25] "price.MAIZ"               "harvest.r.PLATANO"       
[27] "price.PLATANO"            "harvest.r.YUCA"          
[29] "price.YUCA"               "harvest.r.ARVEJA"        
[31] "price.ARVEJA"             "harvest.r.CEBADA"        
[33] "price.CEBADA"             "harvest.r.CEBOLLA"       
[35] "price.CEBOLLA"            "harvest.r.HABA"          
[37] "price.HABA"               "harvest.r.OCA"           
[39] "price.OCA"                "harvest.r.PAPA"          
[41] "price.PAPA"               "harvest.r.QUINUA"        
[43] "price.QUINUA"             "harvest.r.TRIGO"         
[45] "price.TRIGO"  



firm.df<-firm.df[firm.df$harvest.r.ARROZ!=0 | firm.df$harvest.r.MAIZ!=0 |  firm.df$harvest.r.PLATANO!=0 |  firm.df$harvest.r.YUCA!=0 |   firm.df$harvest.r.ARVEJA!=0 |   firm.df$harvest.r.CEBADA!=0 |   firm.df$harvest.r.CEBOLLA!=0 |   firm.df$harvest.r.HABA!=0 |   firm.df$harvest.r.OCA!=0 |   firm.df$harvest.r.PAPA!=0 |   firm.df$harvest.r.QUINUA!=0 |   firm.df$harvest.r.TRIGO!=0,  ]

# TODO: need to fix

firm.df<-firm.df[-which.max(profit),]

firm.df<-firm.df[profit!=0,]

w1=w1, w2=w2, w3=w3, w4=w4, w5=w5, x1=x1, x2=x2, x3=x3, x4=x4, x5=x5, 
p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6, p7=p7, p8=p8, p9=p9, p10=p10, 
p11=p11, p12=p12, y1=y1, y2=y2, y3=y3, y4=y4, y5=y5, y6=y6, y7=y7, y8=y8,
y9=y9, y10=y10, y11=y11, y12=y12, profit=profit

w1 = firm.df$fert.price.quintal
w2 = firm.df$seed.price
w3 = firm.df$abono.price
w4 = firm.df$plaguicida.price.liter
w5 = firm.df$imputed.ag.wage

x1 = firm.df$fert.quintals
x2 = firm.df$seed.quintals
x3 = firm.df$abono.quintals
x4 = firm.df$plaguicida.liters
x5 = firm.df$labor.hours

p1 = firm.df$price.ARROZ
p2 = firm.df$price.MAIZ
p3 = firm.df$price.PLATANO
p4 = firm.df$price.YUCA
p5 = firm.df$price.ARVEJA
p6 = firm.df$price.CEBADA
p7 = firm.df$price.CEBOLLA
p8 = firm.df$price.HABA
p9 = firm.df$price.OCA
p10 = firm.df$price.PAPA
p11 = firm.df$price.QUINUA
p12 = firm.df$price.TRIGO

y1 = firm.df$harvest.r.ARROZ
y2 = firm.df$harvest.r.MAIZ
y3 = firm.df$harvest.r.PLATANO
y4 = firm.df$harvest.r.YUCA
y5 = firm.df$harvest.r.ARVEJA
y6 = firm.df$harvest.r.CEBADA
y7 = firm.df$harvest.r.CEBOLLA
y8 = firm.df$harvest.r.HABA
y9 = firm.df$harvest.r.OCA
y10 = firm.df$harvest.r.PAPA
y11 = firm.df$harvest.r.QUINUA
y12 = firm.df$harvest.r.TRIGO

profit= p1*y1 + p2*y2 + p3*y3 + p4*y4 + p5*y5 + p6*y6 + p7*y7 + p8*y8 + p9*y9 + 
  p10*y10 + p11*y11 + p12*y12 - ( w1*x1 + w2*x2 + w3*x3 + w4*x4 + w5*x5 )


summary(profit)


w1[w1==0] <- mean(w1[w1!=0]) + mean(w1[w1!=0])* rnorm(length(w1[w1==0]), mean = 0, sd = .1)
w2[w2==0] <- mean(w2[w2!=0]) + mean(w2[w2!=0])* rnorm(length(w2[w2==0]), mean = 0, sd = .1)
w3[w3==0] <- mean(w3[w3!=0]) + mean(w3[w3!=0])* rnorm(length(w3[w3==0]), mean = 0, sd = .1)
w4[w4==0] <- mean(w4[w4!=0]) + mean(w4[w4!=0])* rnorm(length(w4[w4==0]), mean = 0, sd = .1)
w5[w5==0] <- mean(w5[w5!=0]) + mean(w5[w5!=0])* rnorm(length(w5[w5==0]), mean = 0, sd = .1)

p1[p1==0] <- mean(p1[p1!=0]) + mean(p1[p1!=0])* rnorm(length(p1[p1==0]), mean = 0, sd = .1)
p2[p2==0] <- mean(p2[p2!=0]) + mean(p2[p2!=0])* rnorm(length(p2[p2==0]), mean = 0, sd = .1)
p3[p3==0] <- mean(p3[p3!=0]) + mean(p3[p3!=0])* rnorm(length(p3[p3==0]), mean = 0, sd = .1)
p4[p4==0] <- mean(p4[p4!=0]) + mean(p4[p4!=0])* rnorm(length(p4[p4==0]), mean = 0, sd = .1)
p5[p5==0] <- mean(p5[p5!=0]) + mean(p5[p5!=0])* rnorm(length(p5[p5==0]), mean = 0, sd = .1)
p6[p6==0] <- mean(p6[p6!=0]) + mean(p6[p6!=0])* rnorm(length(p6[p6==0]), mean = 0, sd = .1)
p7[p7==0] <- mean(p7[p7!=0]) + mean(p7[p7!=0])* rnorm(length(p7[p7==0]), mean = 0, sd = .1)
p8[p8==0] <- mean(p8[p8!=0]) + mean(p8[p8!=0])* rnorm(length(p8[p8==0]), mean = 0, sd = .1)
p9[p9==0] <- mean(p9[p9!=0]) + mean(p9[p9!=0])* rnorm(length(p9[p9==0]), mean = 0, sd = .1)
p10[p10==0] <- mean(p10[p10!=0]) + mean(p10[p10!=0])* rnorm(length(p10[p10==0]), mean = 0, sd = .1)
p11[p11==0] <- mean(p11[p11!=0]) + mean(p11[p11!=0])* rnorm(length(p11[p11==0]), mean = 0, sd = .1)
p12[p12==0] <- mean(p12[p12!=0]) + mean(p12[p12!=0])* rnorm(length(p12[p12==0]), mean = 0, sd = .1)


summary(w1)
summary(w2)
summary(w3)
summary(w4)
summary(w5)
summary(p1)
summary(p2)
summary(p3)
summary(p4)
summary(p5)
summary(p6)
summary(p7)
summary(p8)
summary(p9)
summary(p10)
summary(p11)
summary(p12)
summary()
summary()
summary()
summary()
summary()
summary()
summary()
summary()



sigma.lower<-rep(-10e+01, 136)
sigma.lower[c(1, 17, 32, 46, 59, 71, 82, 92, 101, 109, 116, 122, 127, 131, 134, 136)] <-0



eff.DE.3 <- DEoptim(eff.llf.vec.arg,lower=c(rep(-10, 158), rep(0, 16), 0, sigma.lower),
  upper=c(rep(10, 158), rep(10, 16), 10e+01, rep(10e+01, 136)),  
  control=list(trace=TRUE, NP=10, itermax=3))


eff.llf.vec.arg(eff.DE.3$optim$bestmem)


eff.DE.3 <- DEoptim(eff.llf.vec.arg,lower=c(rep(-100, 158), rep(0, 16), 0, sigma.lower),
  upper=c(rep(100, 158), rep(10, 16), 10e+04, rep(10e+04, 136)),  
  control=list(trace=TRUE, NP=5000, itermax=50))
  
  
  


158
16
1
136














eff.llf.vec.arg(c(rep(1, 13), rep(1, 4), 1, .11, .23, .36, .47, .58, .6))


p1=eff.prod.df$price.MAIZ
p2=eff.prod.df$price.PAPA
y1=eff.prod.df$total.value.MAIZ/eff.prod.df$price.MAIZ
y2=eff.prod.df$total.value.PAPA/eff.prod.df$price.PAPA
w1=eff.prod.df$labor.price
w2=eff.prod.df$fertilizer.price
x1=eff.prod.df$labor.input
x2=eff.prod.df$fertilizer.input
profit=eff.prod.df$total.value.MAIZ+eff.prod.df$total.value.PAPA - 
  (eff.prod.df$labor.price*eff.prod.df$labor.input + eff.prod.df$fertilizer.price*eff.prod.df$fertilizer.input)

eff.DE.3 <- DEoptim(eff.llf.vec.arg,lower=c(rep(-100, 11), rep(0, 3), 0, 0, -10e+09, -10e+09, 0, -10e+09, 0),
  upper=c(rep(100, 11), rep(10, 3), rep(10e+09, 7)),  
  control=list(trace=TRUE, NP=5000, itermax=50))
  
  

summary(eff.DE.3)


eff.llf.vec.arg(eff.DE.3$optim$bestmem)

plot(eff.DE.2, plot.type = "bestmemit")
plot(eff.DE.2, plot.type = "bestvalit")

# TODO: That is actually only one row of xi with a0
# Heuristics to
help ensure that the global minimum is found include re-running the problem with a larger
population size (value of NP), and increasing the maximum allowed number of generations.

eff.llf.vec.arg

#install.packages("DEoptim")
library("DEoptim")

inner.mat <- matrix(1:4, ncol=2)

outer.mat <- matrix(10:15, ncol=2)

outer.mat[1, ] %*% inner.mat


TODO:
1. Group crops into X number of crops, and maybe livestock. Maybe by agronomic characteristics or production function similarity. Must create prices by weighted average.
2. DONE: Group inputs into labor, land, and other expenses and get their prices
3. Soil characteristics as fixed input
4. DONE: Deflate prices
5. modify likelihood function to take into account fixed inputs




sort(unique(prod01.df$crop))


table(miembros01.df$S617)
"usted trabaja como" 

                                                 obrero(a) 
                                                      1192 
                                                  empleado 
                                                      1851 
                           trabajador(a) por cuenta propia 
                                                      4438 
patr\xf3n, socio o empleador que si recibe remuneraci\xf3n 
                                                        40 
patr\xf3n, socio o empleador que no recibe remuneraci\xf3n 
                                                       195 
                           cooperativista de producci\xf3n 
                                                        64 
     trabajador(a) familiar o aprendiz sin remuneraci\xf3n 
                                                      4428 
                                     empleada(o) del hogar 
                                                       276 
                                                    

HH labor hours:
hogar labor plus empresa labor

test.df<-as.data.frame.table(table(miembros01.df$S503A,miembros01.df$S503B,miembros01.df$S503C,miembros01.df$S503D,miembros01.df$S503E,miembros01.df$S503F,miembros01.df$S503G))

test.df[order(test.df$Freq),]

#empresa labor:




  quote is below the var

  
  
                                                                               YAPF 
                                                           "Ingreso a. p. Imputado" 
                                                                               YASF 
                                                            "Ingreso a.s. Imputado" 
                                                                              YAPAJ 
                                                            "Ingreso a.p. Ajustado" 
                                                                              YASAJ 
                                                           "Ingreso a. s. Ajustado" 
                                                                              YAPTF 
                                                        "Ingreso a. p. consolidado" 
                                                                              YASTF 
                                                        "Ingreso a. s. consolidado" 


                                                                                YAP 
                                                      "Ingreso a. p. Liquido total" 
                                                                               YAPT 
                                                        "Ingreso a. p. Consolidado" 
                                                        
                                                                             YAPSAL 
                                                          "Ingreso L\xedquido a.p." 
                                                          
                                                                             S626A 
                                                     "cuanto es su salario liquido" 
                                                                              S626B 
                                          "frecuencia cuanto es su salario liquido" 
                                                                               W625 
                                 "encuestador: categoria ocupacional de la persona" 
                                                                              W626A 
                                                     "cuanto es su salario liquido" 
                                                                              W626B 
                                          "frecuencia cuanto es su salario liquido"

lapply( miembros01.df[, c("YAPF", "YASF", "YAPAJ", "YASAJ", "YAPTF", "YASTF")],    FUN=summary)

lapply( miembros01.df[, c("S626A", "S626B", "YAPSAL" )],    FUN=summary)

monthly

with(miembros01.df, {
  matrix(c( YAPTF / (S624A * (S624HRS + S624MIN/60) * !self.emp.prim * (Z613ACTI %in% c("111", "113", "0111", "0112",  "0113", "130", "0130")) * 4.345),
  YASTF / (S638A * (S638HRS + S638MIN/60) * !self.emp.sec * (Z634ACTI %in% c("111", "113", "0111", "0112",  "0113", "130", "0130")) * 4.345)), ncol=2 )
  }
)[7930:7940, ]



(miembros01.df$Z613ACTI %in% c("111", "113", "0111", "0112",  "0113", "130", "0130"))[7930:7940]


test <-miembros01.df$YAPTF / (miembros01.df$S624A * (miembros01.df$S624HRS + miembros01.df$S624MIN/60) * (!self.emp.prim) * (miembros01.df$Z613ACTI %in% c("111", "113", "0111", "0112",  "0113", "130", "0130")) * 4.345)

# about 6 months in growing season, and 4.345 weeks in a month according to http://www.convertunits.com/from/weeks/to/months


we have:
semillas
abono
fert 
plagacidas (Insecticidas, fungicidas y herbicidas.)

unlist( lapply(hogar01.df[, grepl("S815.2", colnames(hogar01.df))], FUN=median, na.rm=TRUE) )

sort( unlist( lapply(hogar01.df[, grepl("S815.2", colnames(hogar01.df))], FUN=function(x) sum(x, na.rm=TRUE)/length(x)) ) )




                                                        S815A1 
                                            "GASTO EN PEONES?" 
                                                        S815A2 
                                     "CUANTO GASTO EN PEONES?" 
                                                        S815B1 
                                          "GASTO EN SEMILLAS?" 
                                                        S815B2 
                                   "CUANTO GASTO EN SEMILLAS?" 
                                                        S815C1 
                                    "GASTO EN ABONO ORGANICO?" 
                                                        S815C2 
                             "CUANTO GASTO EN ABONO ORGANICO?" 
                                                        S815D1 
                                     "GASTO EN FERTILIZANTES?" 
                                                        S815D2 
                              "CUENTO GASTO EN FERTILIZANTES?" 
                                                        S815E1 
                                        "GASTO EN TRANSPORTE?" 
                                                        S815E2 
                                 "CUANTO GASTO EN TRANSPORTE?" 
                                                        S815F1 
                                        "GASTO EN PESTICIDAS?" 
                                                        S815F2 
                                 "CUANTO GASTO EN PESTICIDAS?" 
                                                        S815G1 
                                "GASTO EN ASISTENCIA TECNICA?" 
                                                        S815G2 
                         "CUANTO GASTO EN ASISTENCIA TECNICA?" 
                                                        S815H1 
                          "GASTO EN ALQUILER DE MAQ.AGRICOLA?" 
                                                        S815H2 
                   "CUANTO GASTO EN ALQUILER DE MAQ.AGRICOLA?" 
                                                        S815I1 
                       "GASTO EN ALQUILER DE TRACCION ANIMAL?" 
                                                        S815I2 
                "CUANTO GASTO EN ALQUILER DE TRACCION ANIMAL?" 
                                                        S815J1 
                                "GASTO EN ALQUIER DE TERRENO?" 
                                                        S815J2 
                         "CUANTO GASTO EN ALQUIER DE TERRENO?" 
                                                        S815K1 
                            "GASTO EN ALIMENTO PARA ANIMALES?" 
                                                        S815K2 
                     "CUANTO GASTO EN ALIMENTO PARA ANIMALES?" 
                                                        S815L1 
                           "GASTO EN SERVICIO DE VETERINARIA?" 
                                                        S815L2 
                    "CUANTO GASTO EN SERVICIO DE VETERINARIA?" 
                                                        S815M1 
                              "GASTO EN INSUMOS VETERINARIOS?" 
                                                        S815M2 
                       "CUANTO GASTO EN INSUMOS VETERINARIOS?" 
                                                        S815N1 
                                      "GASTO EN OTROS COSTOS?" 
                                                        S815N2 
                               "CUANTO GASTO EN OTROS COSTOS?" 













prod.geog.df<-miembros01.df[, c("FOLIO", "ID01", "ID02", "ID03", "ID04", "ID05")]
names(prod.geog.df)<-c("FOLIO", "department", "province", "seccion", "canton", "village")
prod.geog.df$village<-do.call(paste0, prod.geog.df[, c("department", "province", "seccion", "canton", "village")])
prod.geog.df$canton<-do.call(paste0, prod.geog.df[, c("department", "province", "seccion", "canton")])
prod.geog.df$seccion<-do.call(paste0, prod.geog.df[, c("department", "province", "seccion")])
prod.geog.df$province<-do.call(paste0, prod.geog.df[, c("department",  "province")])
prod.geog.df$nation<-1
prod.geog.df<-unique(prod.geog.df)




table(miembros01.df$S617)
"usted trabaja como" 

                                                 obrero(a) 
                                                      1192 
                                                  empleado 
                                                      1851 
                           trabajador(a) por cuenta propia 
                                                      4438 
patr\xf3n, socio o empleador que si recibe remuneraci\xf3n 
                                                        40 
patr\xf3n, socio o empleador que no recibe remuneraci\xf3n 
                                                       195 
                           cooperativista de producci\xf3n 
                                                        64 
     trabajador(a) familiar o aprendiz sin remuneraci\xf3n 
                                                      4428 
                                     empleada(o) del hogar 
                                                       276 
                                                    

HH labor hours:
hogar labor plus empresa labor

test.df<-as.data.frame.table(table(miembros01.df$S503A,miembros01.df$S503B,miembros01.df$S503C,miembros01.df$S503D,miembros01.df$S503E,miembros01.df$S503F,miembros01.df$S503G))

test.df[order(test.df$Freq),]











