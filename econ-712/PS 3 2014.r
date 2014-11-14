# QUESTION 4.a

beta = 0.95
delta = 0.1
z = 1
N = 1
utility.fn <- function(con) con^(1-gamma)/(1-gamma)
gamma = 2
prod.fn <- function(K) z * K^0.35 * N^0.65
# Ok, for simplicity, we are just defining in terms of K and letting it bring in other args

kss <- (.35 * beta )^(1/(1-.35))

increment <- .001

K.seq <- seq(from=0.01, to=1, by= increment)
s.seq <- seq(from=0, to=0.99, by= increment)

K.grid <- expand.grid(s = s.seq, K=K.seq)
# this may seem backwards, but I want the K.prime to "cycle fastest"
# s is savings rate. I'll make it a  proportion

K.grid$K.prime <-  K.grid$K * (1-delta) + K.grid$s * prod.fn(K.grid$K)

K.grid$K.dif <- abs(K.grid$K - K.grid$K.prime)

#summary(K.grid$dif)
#hist(K.grid$dif)

K.grid$K.util <- utility.fn(prod.fn(K.grid$K) * (1 - K.grid$s))
K.grid$K.prime.util <- beta * utility.fn(prod.fn(K.grid$K.prime) * (1 - K.grid$s))


util.mat <- matrix(K.grid$K.util, nrow=length(K.seq), byrow=TRUE)


v <- rep(1, ncol(util.mat))
Tv <- rep(0, ncol(util.mat))

currTol <- 1
Tol <- .0001

s.decision <- rep(0, nrow(util.mat))


while (currTol > Tol) {
  for ( i in 1:nrow(util.mat)) {
    w <- util.mat[i, ] + beta * v
    Tv[i] <- max(w)  
    s.decision[i] <- s.seq[which.max(w) ]
  }
  currTol <- max(abs(v-Tv))
  v <- Tv
}


pdf(file="/Users/travismcarthur/Desktop/Macro 2014/Problem sets/PS 3/plot1.pdf", width=5, height=5)




plot(y=s.decision, x=K.seq, main="Fig. 1: Optimal policy function", type="l", 
  ylab="Saving rate", xlab="Initial Capital")

dev.off()


which.initial.K <- 491
# This means initial K is 0.5


K.path <- rep(K.seq[which.initial.K], 1001)
util.path <- rep(utility.fn(prod.fn(K.seq[which.initial.K])), 1001)
C.path <- rep(prod.fn(K.seq[which.initial.K]) * (1-s.decision[which.initial.K]), 1001)

for ( k in 1:1000) {
  K.path[k+1] <-  K.path[k] * (1-delta) + s.decision[which.initial.K] * prod.fn(K.path[k])  
  util.path[k+1] <-  utility.fn(prod.fn(K.path[k]))
  C.path[k+1] <-  prod.fn(K.path[k]) * (1-s.decision[which.initial.K])
}
K.path <- K.path[-1]
C.path <- C.path[-1]
util.path <- util.path[-1]

#no.ch.K.fn <- function(s) (delta/(s*N^.65))^(-1/.65)

no.ch.K.fn <- function(c, s) c*s/((1-s)*delta)

C.path.seq <- seq(min(C.path)-10, max(C.path)+10, by=.1)
no.ch.K.val <- no.ch.K.fn(C.path.seq , s.decision[which.initial.K])

no.ch.C.fn <- function(K, s) {
  alpha <- K * (1-delta) * N^(.65/.35) * (1-s)^(1/.35) 
  beta <- (s/(1-s)) * N^(.65/.35) * (1-s)^(1/.35) 
  alpha/(1-beta)
}

#no.ch.C.meta.fn <- function(K) {

#  no.ch.C.fn <- function(C, K, s=s.decision[which.initial.K]) {
#    sum( (C - (K*(1-delta ) + C*s/(1-s))^.35 * N^.65 * (1-s))^2 )
#  }

#  optim(1.2, no.ch.C.fn, K=K)$par
#}


#no.ch.C.val <- no.ch.C.meta.fn(1.2)



no.ch.C.val <- ((1/beta + delta - 1) /
    (z * .35 * N^(.65)) )^(-1/.65)




K.path.seq <- seq(.01, max(K.path)+10, by=.1)
for ( i in 1:length(K.path.seq)) {
 no.ch.C.val[i] <- no.ch.C.meta.fn(K.path.seq[i])
}

no.ch.K.val <- max(K.path)

pdf(file="/Users/travismcarthur/Desktop/Macro 2014/Problem sets/PS 3/plot2.pdf", width=5, height=5)

plot(x=K.path, y=C.path, ylim=c(0, max(C.path)), xlim=c(0, max(K.path)), 
  main="Fig. 2: Phase diagram")

lines( K.path.seq, no.ch.C.val, col="red")
abline(v=no.ch.K.val, col="blue", lty=2)
# lines( K.path.seq, no.ch.C.val, col="blue", lty=2)
legend("topleft", legend = c(expression(DeltaK==0), expression(DeltaC==0)), 
  col=c("red", "blue"), lty=c(1,2)) 


dev.off()

pdf(file="/Users/travismcarthur/Desktop/Macro 2014/Problem sets/PS 3/plot3.pdf", width=5, height=5)

plot(x=K.path, y=C.path, ylim=c(0, max(C.path)), xlim=c(0, max(K.path)),
  main="Fig. 3: Change in preferences")

#lines( no.ch.K.val, C.path.seq, col="red")
#lines( K.path.seq, no.ch.C.val, col="blue")
#abline(v=no.ch.C.val, col="blue", lty=2)

lines( K.path.seq, no.ch.C.val, col="red")
abline(v=no.ch.K.val, col="blue", lty=2)


K.ss <- max(K.path)

K.path.orig <- K.path
C.path.orig <- C.path 
no.ch.K.val.orig <- no.ch.K.val
no.ch.C.val.orig <- no.ch.C.val
C.path.seq.orig <- C.path.seq
K.path.seq.orig <- K.path.seq

###########################
# Just copy-paste the lines again:
###########################

# QUESTION 4.b



beta = 0.95
delta = 0.1
z = 1
N = 1
utility.fn <- function(con) con^(1-gamma)/(1-gamma)
gamma = 1.01
prod.fn <- function(K) z * K^0.35 * N^0.65
# Ok, for simplicity, we are just defining in terms of K and letting it bring in other args

kss <- (.35 * beta )^(1/(1-.35))

increment <- .001

K.seq <- seq(from=0.01, to=1, by= increment)
s.seq <- seq(from=0, to=0.99, by= increment)

K.grid <- expand.grid(s = s.seq, K=K.seq)
# this may seem backwards, but I want the K.prime to "cycle fastest"
# s is savings rate. I'll make it a  proportion

K.grid$K.prime <-  K.grid$K * (1-delta) + K.grid$s * prod.fn(K.grid$K)

K.grid$K.dif <- abs(K.grid$K - K.grid$K.prime)

#summary(K.grid$dif)
#hist(K.grid$dif)

K.grid$K.util <- utility.fn(prod.fn(K.grid$K) * (1 - K.grid$s))
K.grid$K.prime.util <- beta * utility.fn(prod.fn(K.grid$K.prime) * (1 - K.grid$s))


util.mat <- matrix(K.grid$K.util, nrow=length(K.seq), byrow=TRUE)


v <- rep(1, ncol(util.mat))
Tv <- rep(0, ncol(util.mat))

currTol <- 1
Tol <- .0001

s.decision <- rep(0, nrow(util.mat))


while (currTol > Tol) {
  for ( i in 1:nrow(util.mat)) {
    w <- util.mat[i, ] + beta * v
    Tv[i] <- max(w)  
    s.decision[i] <- s.seq[which.max(w) ]
  }
  currTol <- max(abs(v-Tv))
  v <- Tv
}


# plot(y=s.decision, x=K.seq)






which.initial.K <- 491
# This means initial K is 0.5


K.path <- rep(K.seq[which.initial.K], 1001)
util.path <- rep(utility.fn(prod.fn(K.seq[which.initial.K])), 1001)
C.path <- rep(prod.fn(K.seq[which.initial.K]) * (1-s.decision[which.initial.K]), 1001)

for ( k in 1:1000) {
  K.path[k+1] <-  K.path[k] * (1-delta) + s.decision[which.initial.K] * prod.fn(K.path[k])  
  util.path[k+1] <-  utility.fn(prod.fn(K.path[k]))
  C.path[k+1] <-  prod.fn(K.path[k]) * (1-s.decision[which.initial.K])
}
K.path <- K.path[-1]
C.path <- C.path[-1]
util.path <- util.path[-1]

#no.ch.K.fn <- function(s) (delta/(s*N^.65))^(-1/.65)

no.ch.K.fn <- function(c, s) c*s/((1-s)*delta)

C.path.seq <- seq(min(C.path)-10, max(C.path)+10, by=.1)
no.ch.K.val <- no.ch.K.fn(C.path.seq , s.decision[which.initial.K])

no.ch.C.fn <- function(K, s) {
  alpha <- K * (1-delta) * N^(.65/.35) * (1-s)^(1/.35) 
  beta <- (s/(1-s)) * N^(.65/.35) * (1-s)^(1/.35) 
  alpha/(1-beta)
}

no.ch.C.meta.fn <- function(K) {

  no.ch.C.fn <- function(C, K, s=s.decision[which.initial.K]) {
    sum( (C - (K*(1-delta ) + C*s/(1-s))^.35 * N^.65 * (1-s))^2 )
  }

  optim(1.2, no.ch.C.fn, K=K)$par
}


no.ch.C.val <- no.ch.C.meta.fn(1.2)



K.path.seq <- seq(.01, max(K.path)+10, by=.1)
for ( i in 1:length(K.path.seq)) {
 no.ch.C.val[i] <- no.ch.C.meta.fn(K.path.seq[i])
}

no.ch.K.val <- max(K.path)

#par(new=TRUE)

#plot(x=K.path, y=C.path, ylim=c(0, max(C.path)), xlim=c(0, max(K.path)))

points(x=K.path, y=C.path, col="darkgreen", pch=3)

#lines( no.ch.K.val, C.path.seq, col="purple")
#lines( K.path.seq, no.ch.C.val, col="brown")
#abline(v=no.ch.C.val, col="brown", lty=2)

lines( K.path.seq, no.ch.C.val, col="purple")
abline(v=no.ch.K.val, col="brown", lty=2)


legend("topleft", legend = c(expression(gamma==2), expression(gamma==1.01)), 
  col=c("black", "darkgreen"), pch=c(1,3)) 

dev.off()

###########################
# Just copy-paste the lines again:
###########################

# QUESTION 4.c



beta = 0.95
delta = 0.1
z = 1.2
N = 1
utility.fn <- function(con) con^(1-gamma)/(1-gamma)
gamma = 2
prod.fn <- function(K) z * K^0.35 * N^0.65
# Ok, for simplicity, we are just defining in terms of K and letting it bring in other args

kss <- (.35 * beta )^(1/(1-.35))

increment <- .001

#K.seq <- seq(from=0.01, to=1, by= increment)
# K.seq <- K.ss - 4 # From above
K.seq <- seq(from= K.ss - .5, to= K.ss , by=increment)
#K.seq <- jitter(rep(K.ss - 0.5, 100)) # This is a test
s.seq <- seq(from=0, to=0.99, by= increment)

K.grid <- expand.grid(s = s.seq, K=K.seq)
# this may seem backwards, but I want the K.prime to "cycle fastest"
# s is savings rate. I'll make it a  proportion

K.grid$K.prime <-  K.grid$K * (1-delta) + K.grid$s * prod.fn(K.grid$K)

K.grid$K.dif <- abs(K.grid$K - K.grid$K.prime)

#summary(K.grid$dif)
#hist(K.grid$dif)

K.grid$K.util <- utility.fn(prod.fn(K.grid$K) * (1 - K.grid$s))
K.grid$K.prime.util <- beta * utility.fn(prod.fn(K.grid$K.prime) * (1 - K.grid$s))


util.mat <- matrix(K.grid$K.util, nrow=length(K.seq), byrow=TRUE)

v <- rep(1, ncol(util.mat))
Tv <- rep(0, ncol(util.mat))

currTol <- 1
Tol <- .0001

s.decision <- rep(0, nrow(util.mat))


while (currTol > Tol) {
  for ( i in 1:nrow(util.mat)) {
    w <- util.mat[i, ] + beta * v
    Tv[i] <- max(w)  
    s.decision[i] <- s.seq[which.max(w) ]
  }
  currTol <- max(abs(v-Tv))
  v <- Tv
  cat("test\n")
}


# plot(y=s.decision, x=K.seq)




which.initial.K <- length(K.seq)
# We are taking the last one, i.e. the steady state




K.path <- rep(K.seq[which.initial.K], 1001)
util.path <- rep(utility.fn(prod.fn(K.seq[which.initial.K])), 1001)
C.path <- rep(prod.fn(K.seq[which.initial.K]) * (1-s.decision[which.initial.K]), 1001)

for ( k in 1:1000) {
  K.path[k+1] <-  K.path[k] * (1-delta) + s.decision[which.initial.K] * prod.fn(K.path[k])  
  util.path[k+1] <-  utility.fn(prod.fn(K.path[k]))
  C.path[k+1] <-  prod.fn(K.path[k]) * (1-s.decision[which.initial.K])
}
K.path <- K.path[-1]
C.path <- C.path[-1]
util.path <- util.path[-1]

#no.ch.K.fn <- function(s) (delta/(s*N^.65))^(-1/.65)

no.ch.K.fn <- function(c, s) c*s/((1-s)*delta)

C.path.seq <- seq(min(C.path)-10, max(C.path)+10, by=.1)
no.ch.K.val <- no.ch.K.fn(C.path.seq , s.decision[which.initial.K])

no.ch.C.fn <- function(K, s) {
  alpha <- K * (1-delta) * N^(.65/.35) * (1-s)^(1/.35) 
  beta <- (s/(1-s)) * N^(.65/.35) * (1-s)^(1/.35) 
  alpha/(1-beta)
}

no.ch.C.meta.fn <- function(K) {

  no.ch.C.fn <- function(C, K, s=s.decision[which.initial.K]) {
    sum(  (C - z *(K*(1-delta ) + C*s/(1-s))^.35 * N^.65 * (1-s))^2 )
  }

  optim(1.2, no.ch.C.fn, K=K)$par
}


no.ch.C.val <- no.ch.C.meta.fn(1.2)



K.path.seq <- seq(.01, max(K.path)+10, by=.1)
for ( i in 1:length(K.path.seq)) {
 no.ch.C.val[i] <- no.ch.C.meta.fn(K.path.seq[i])
}

no.ch.K.val <- max(K.path)

#par(new=TRUE)

pdf(file="/Users/travismcarthur/Desktop/Macro 2014/Problem sets/PS 3/plot4.pdf", width=5, height=5)

plot(x=K.path, y=C.path, ylim=c(0, max(C.path)), xlim=c(0, max(K.path)), col="darkgreen", pch=3,
  main="Fig. 4: Change in technology")



#lines( no.ch.K.val, C.path.seq, col="purple")
#lines( K.path.seq, no.ch.C.val, col="brown")
#abline(v=no.ch.C.val, col="brown", lty=2)

lines( K.path.seq, no.ch.C.val, col="purple")
abline(v=no.ch.K.val, col="brown", lty=2)


points(x=K.path.orig, y=C.path.orig, col="black", pch=1)

#lines( no.ch.K.val.orig, C.path.seq.orig, col="red")
#lines( K.path.seq.orig, no.ch.C.val.orig, col="blue")
#abline(v=no.ch.C.val, col="blue", lty=2)

lines( K.path.seq.orig, no.ch.C.val.orig, col="red")
abline(v=no.ch.K.val.orig, col="blue", lty=2)



legend("topleft", legend = c(expression(z==1), expression(z==1.2)), 
  col=c("black", "darkgreen"), pch=c(1,3)) 



dev.off()






















