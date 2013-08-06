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
  
u.R <- u.R[-1]
# TODO: not 100% sure about cutting of first one
  
Qaj <- paste0("(", data.w, " * ", data.x, "/profit)")

u.Q <- paste0("- (", Qaj, " * theta", 1:J, " * ", H0, " + ", Q0j.v, ")/(",
  "sum(c(", apply(alphas.mat, 1, FUN=paste0, collapse=", "), ")) + theta", 1:M, " * ", psi, " * ", Qaj, ")")


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

sigma0.sq <- paste0( "((1/sigma.u.sq + rep(1, ", M+J-1, ") %*% ", big.sigma0.mat.char, " %*% rep(1, ", M+J-1, "))^-1)")

a0 <- paste0("(xi %*% (", big.sigma0.mat.char, ")^-1 %*% xi - ", sigma0.sq, " * (xi %*% (", big.sigma0.mat.char, ")^-1 %*% rep(1, ", M+J-1, "))^2 )")


gamma0 <- paste0( "sum(c(",apply(gammas.mat, 1, FUN=paste0, collapse=", "), "))")
alpha0 <- paste0( "sum(c(",apply(alphas.mat, 1, FUN=paste0, collapse=", "), "))")

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

Di <- paste(u.R.deriv, u.Q.deriv, sep=" *\n\n ", collapse=" *\n\n ")


# Maybe I dont need at those extra c( in sum()


# START FUNCTION
arguments <- paste(c(paste0("beta", 1:M), paste0("alpha", 1:J), unique(c(betas.mat)), unique(c(gammas.mat)), 
  unique(c(alphas.mat)), kappas, thetas, "sigma.u.sq", unique(c(big.sigma0.mat)), 
  data.p, data.y, data.w, data.x, "profit"), sep=", ", collapse=", ")

first.line <- paste0( "eff.llf <- function(", arguments, ") {")
second.line <- paste0("  xi <- c(\n", paste0(u.R, collapse="\n,\n"), "\n,\n", paste0(u.Q, collapse="\n,\n"), "\n)")
third.line <- "  N <- length(y1); ; cat(length(xi)) #cat(xi)"
fourth.line <-paste0(
  " -  N * log(2) - (N * (", J, " + ", M, " - 1)/2) * log(2*pi) - (N/2) * log(det(", big.sigma0.mat.char,
  ")) + N * log(", sigma0.sq, "^.5) + sum(log(pnorm(xi %*% (", big.sigma0.mat.char, 
  ")^-1 %*% rep(1, ", M+J-1, ") * ",
  sigma0.sq, "^.5))) - N * log(sigma.u.sq^.5) - .5 * sum(", a0, ") + \nsum(log(", Di, "))"
)
fifth.line <- "}"

# M+J is bare?

# May want to change the DI to sum of logs for numerical accuracy

ret <- paste(first.line, second.line, third.line, fourth.line, fifth.line, sep="\n\n")

#TODO: Not sure if it is M+J-1 or M+J

return(ret)

# END

}


function.text <- llf.creator.fn(2,2)
eval(parse(text=function.text))




(m0 <- mle2(eff.llf,
  start=list(beta1=1, beta2=1, alpha1=1, alpha2=1,
  beta11=1, beta21=1, beta22=1, gamma11=1, gamma21=1, gamma22=1, alpha11=1, alpha21=1, alpha22=1, kappa1=1, kappa2=1, theta1=1, theta2=1, sigma.u.sq=1, sigma.mat11=1, sigma.mat21=.1, sigma.mat31=.1, sigma.mat22=1, sigma.mat32=.1, sigma.mat33=1), 
  data=list(p1=eff.prod.df$price.MAIZ, p2=eff.prod.df$price.PAPA, y1=eff.prod.df$total.value.MAIZ/eff.prod.df$price.MAIZ, y2=eff.prod.df$total.value.PAPA/eff.prod.df$price.PAPA, w1=eff.prod.df$labor.price, w2=eff.prod.df$fertilizer.price, x1=eff.prod.df$labor.input, x2=eff.prod.df$fertilizer.input,
  profit=eff.prod.df$total.value.MAIZ+eff.prod.df$total.value.PAPA - 
  (eff.prod.df$labor.price*eff.prod.df$labor.input + eff.prod.df$fertilizer.price*eff.prod.df$fertilizer.input)), trace=TRUE))



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
psi=


