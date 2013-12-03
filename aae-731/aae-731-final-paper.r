


grid.size <- 50

grid.mat<-expand.grid( sigma=seq(1, 10, length.out=grid.size),
          alpha=seq(0, 0.5, length.out=grid.size),
          L=10000,
          rho=0.95,
          a=seq(1, 1000, length.out=grid.size)
        )


condition.test <- function(x) {
  x["sigma"]/(1-x["alpha"]) - 1 < x["L"]/(x["rho"]*x["a"]) & 
    x["L"]/(x["rho"]*x["a"]) < (x["sigma"] - x["alpha"])/(1-x["alpha"])
}

condition.test <- function(x) {
  x["sigma"]/(1-x["alpha"]) - 1 < x["L"]/(x["rho"]*x["a"]) & 
    x["L"]/(x["rho"]*x["a"]) < x["sigma"] / x["alpha"] -1
}
#will give us general result of two loci at n<1 and n>1
# TODO: big issue


condition.result<-apply(grid.mat, 1, condition.test)

#plot3d(grid.mat[condition.result, c("sigma", "alpha", "a")])

#surface3d(grid.mat[condition.result, c("sigma", "alpha", "a")])

library("alphashape3d")
#library("geometry")
library("rgl")
#install.packages(c("R.basic"), contriburl="http://www.braju.com/R/repos/")
#library("R.basic")



grid.shape.mat<-grid.mat[condition.result, c("sigma", "alpha", "a")]

grid.list<-list()

for (i in c("sigma", "alpha", "a")) {

  for ( j in c("sigma", "alpha", "a")) {
   if (i==j) {next}
   for (targ.row in unique(grid.shape.mat[, j])) {
     grid.list[[length(grid.list)+1]]<-grid.shape.mat[grid.shape.mat[, j]==targ.row,][
     grid.shape.mat[grid.shape.mat[, j]==targ.row, i] %in% 
     range(grid.shape.mat[grid.shape.mat[, j]==targ.row, i]),
     ]
    }
  }

}




grid.f <- do.call(rbind, grid.list)
grid.f <-grid.shape.mat

x.max<-max(grid.f[, 1])
y.max<-max(grid.f[, 2])
z.max<-max(grid.f[, 3])

grid.f <- grid.f[order(grid.f[,1], grid.f[,2], grid.f[,3]), ]
for (i in 1:3) {
  grid.f[, i] <- grid.f[, i]/max(grid.f[, i])
}


grid.plot<-ashape3d(as.matrix(grid.f), alpha=.1, pert = TRUE, eps = 1e-09)

#open3d()
plot(grid.plot, col="blue")


axis3d("x", at = seq(0, 1, length.out=10), labels = signif(seq(0, x.max, length.out=10), 2 ))
axis3d("y", at = seq(0, 1, length.out=10), labels = signif(seq(0, y.max, length.out=10), 2 ))
axis3d("z", at = seq(0, 1, length.out=10), labels = signif(seq(0, z.max, length.out=10), 2))


# plot NN:

NN.line.fn<-function(a, L, n, sigma, alpha) {
  if (n<=1) {A<-alpha} else {A<-1-alpha}
  (a/L)*(1-A/sigma)
}

VV.line.fn<-function(rho, sigma, n, alpha) {
  if (n<=1) {A<-alpha} else {A<-1-alpha}
  A/(rho*sigma*n)
}

grid.pick <- grid.shape.mat[sample(1:nrow(grid.shape.mat), 1), ]
# grid.pick<-grid.mat[!condition.result, c("sigma", "alpha", "a")][sample(1:sum(!condition.result), 1), ]


n.max<-6
n.points <-30

NN.line<-rep(0, (n.max*n.points))

for (i in 1:(n.max*n.points)) {
  NN.line[i] <-NN.line.fn(a=grid.pick$a, L=10000, n=seq(0, n.max, length.out=n.max*n.points)[i], sigma=grid.pick$sigma, alpha=grid.pick$alpha)
}



VV.line<-rep(0, (n.max*n.points))

for (i in 1:(n.max*n.points)) {
  VV.line[i] <-VV.line.fn(rho=0.95, sigma=grid.pick$sigma, n=seq(0, n.max, length.out=n.max*n.points)[i], alpha=grid.pick$alpha)
}


plot(x=seq(0, n.max, length.out=n.max*n.points), y=NN.line, ylim=c( 0, max(NN.line)*2), col="red", type="l", xlab="", ylab="", xaxt='n')

lines(x=seq(0, n.max, length.out=n.max*n.points), y=VV.line, col="blue")


NN.VV.converge<-apply( matrix(c(NN.line, VV.line), ncol=2), 1, weighted.mean, w=c(.9, .1)) 

#converge.breakpoint <- which(diff(NN.VV.converge)>0)
converge.breakpoint1 <- which(NN.line>VV.line)[1]
converge.breakpoint2 <- converge.breakpoint1 + which(NN.line[-(1:converge.breakpoint1)]<VV.line[-(1:converge.breakpoint1)])[1]

converge.breakpoint3 <- converge.breakpoint2 + which(NN.line[-(1:converge.breakpoint2)]>VV.line[-(1:converge.breakpoint2)])[1]

lines(seq(0, n.max, length.out=n.max*n.points)[-1][1:converge.breakpoint1], 
  NN.VV.converge[1:converge.breakpoint1], 
 col="darkgreen")
 
lines(seq(0, n.max, length.out=n.max*n.points)[-1][(converge.breakpoint2+1):converge.breakpoint3], 
  NN.VV.converge[(converge.breakpoint2+1):converge.breakpoint3], 
 col="darkgreen")
 
 
arrowLine(seq(0, n.max, length.out=n.max*n.points)[-1][1:converge.breakpoint1][NN.VV.converge[1:converge.breakpoint1]<max(NN.line)*2], 
  NN.VV.converge[1:converge.breakpoint1][NN.VV.converge[1:converge.breakpoint1]<.1], 
  N=10, col="darkgreen")
  
arrowLine(seq(0, n.max, length.out=n.max*n.points)[-1][(converge.breakpoint2+1):converge.breakpoint3], 
  NN.VV.converge[(converge.breakpoint2+1):converge.breakpoint3],
  N=10, col="darkgreen")
  



n.star.1 <- converge.breakpoint1

n.star.2 <- converge.breakpoint3

lines(x=c(seq(0, n.max, length.out=n.max*n.points)[n.star.1], seq(0, n.max, length.out=n.max*n.points)[n.star.1]), 
  y=c(NN.VV.converge[n.star.1], -2), lty=2, col="black")

axis(1, at=seq(0, n.max, length.out=n.max*n.points)[n.star.1],labels=expression(paste(n[alpha], "*")), las=1)

lines(x=c(seq(0, n.max, length.out=n.max*n.points)[n.star.2], seq(0, n.max, length.out=n.max*n.points)[n.star.2]), 
  y=c(NN.VV.converge[n.star.2], -2), lty=2, col="black")

axis(1, at=seq(0, n.max, length.out=n.max*n.points)[n.star.2],labels=expression(paste(n[1-alpha], "*")), las=1)

axis(1, at=n.max,labels=expression(n[t]), las=1, tick=FALSE)

axis(2, at=max(NN.line)*2,labels=expression(V[t]), las=1, tick=FALSE)












H.intensive.ss.fn <- function(L, a, rho, sigma, alpha) {
  (L/(a*rho*sigma)) * (1/alpha-1/sigma)^-1
}


X.intensive.ss.fn <- function(L, a, rho, sigma, alpha) {
  (L/(a*rho*sigma)) * (1/(1-alpha)-1/sigma)^-1
}


H.intensive.n <- H.intensive.ss.fn(L=10000, a=grid.pick$a, rho=0.95, sigma=grid.pick$sigma, alpha=grid.pick$alpha)

X.intensive.n <- X.intensive.ss.fn(L=10000, a=grid.pick$a, rho=0.95, sigma=grid.pick$sigma, alpha=grid.pick$alpha)


supply.ratio.fn<-function(H.n, X.n, sigma, alpha) {
  (  ( (X.n^(1/(1-sigma)))^(-(1-alpha)/alpha) ) /
     ( (H.n^(1/(1-sigma)))^(alpha/(alpha-1)) ) ) *
     (1/alpha-1)^(  (2*alpha-1)^2 / ((1-alpha)*alpha)  )
}

supply.ratio.fn(H.n=H.intensive.n, X.n=X.intensive.n, sigma=grid.pick$sigma, alpha=grid.pick$alpha)

# ok, now let us compute all of these:

C.values.v<-rep(0, nrow(grid.shape.mat) )

for ( i in 1:nrow(grid.shape.mat) ) {

  grid.pick <- grid.shape.mat[i, ]

  H.intensive.n <- H.intensive.ss.fn(L=10000, a=grid.pick$a, rho=0.95, sigma=grid.pick$sigma, alpha=grid.pick$alpha)

  X.intensive.n <- X.intensive.ss.fn(L=10000, a=grid.pick$a, rho=0.95, sigma=grid.pick$sigma, alpha=grid.pick$alpha)

  C.values.v[i]<-supply.ratio.fn(H.n=H.intensive.n, X.n=X.intensive.n, sigma=grid.pick$sigma, alpha=grid.pick$alpha)

}

# TODO: figure out labor vs. intermediates expenditure from the Bol data?


#double log works well
C.values.v<-C.values.v[is.finite(C.values.v)]
C.values.col.v<-log(log(C.values.v+1)+1)+min(log(log(C.values.v+1)+1), na.rm=TRUE)
C.values.col.v<-C.values.col.v/max(C.values.col.v, na.rm=TRUE)

plot3d(grid.shape.mat[is.finite(C.values.v), ], cex=10,
  col=rgb(red=C.values.col.v, green=0, blue=0, maxColorValue = 1))


easier.subset<-seq(1, sum(is.finite(C.values.v)), length.out=sum(is.finite(C.values.v))/20)

plot3d(grid.shape.mat[is.finite(C.values.v), ][easier.subset, ], type="s", size=.7,
  col=rgb(red=C.values.col.v[easier.subset], green=0, blue=0, maxColorValue = 1))






unique.VV<- function(x) { 1+-exp(x-2)/(exp(x-2)+1)}
unique.NN<- function(x) { (1+-exp(x-2)/(exp(x-2)+1))/2+.2}

plot(seq(0, 5, length.out=200), unique.VV(seq(0, 5, length.out=200)), type="l", col="blue", xaxt='n', yaxt='n', ann=FALSE)

lines(seq(0, 5, length.out=200), unique.NN(seq(0, 5, length.out=200)), col="red")

NN.VV.converge<-rowMeans( matrix(c(unique.VV(seq(0, 5, length.out=200)), unique.NN(seq(0, 5, length.out=200))), ncol=2))

lines(seq(0, 5, length.out=200)[NN.VV.converge>unique.NN(seq(0, 5, length.out=200))], 
  NN.VV.converge[NN.VV.converge>unique.NN(seq(0, 5, length.out=200))], 
 col="darkgreen")

# Thanks to http://stackoverflow.com/questions/6893959/r-how-do-i-draw-a-line-with-multiple-arrows-in-it for the below

arrowLine <- function(x, y, N=10, ...){
  lengths <- c(0, sqrt(diff(x)^2 + diff(y)^2))
  l <- cumsum(lengths)
  tl <- l[length(l)]
  el <- seq(0, to=tl, length=N+1)[-1]
  
#  par(new=TRUE)

 # plot(x, y, t="l",   ...)

  for(ii in el){

    int <- findInterval(ii, l)
    xx <- x[int:(int+1)]
    yy <- y[int:(int+1)]

    ## points(xx,yy, col="grey", cex=0.5)

    dx <- diff(xx)
    dy <- diff(yy)
    new.length <- ii - l[int]
    segment.length <- lengths[int+1]

    ratio <- new.length / segment.length

    xend <- x[int] + ratio * dx
    yend <- y[int] + ratio * dy
#    points(xend,yend, col="white", pch=19)
    arrows(x[int], y[int], xend, yend, length=0.1, ...)

}

}

arrowLine(seq(0, 5, length.out=200)[NN.VV.converge>unique.NN(seq(0, 5, length.out=200))], 
  NN.VV.converge[NN.VV.converge>unique.NN(seq(0, 5, length.out=200))], 
  N=20, col="darkgreen")
  
n.star <- sum(NN.VV.converge>unique.NN(seq(0, 5, length.out=200)))

lines(x=c(seq(0, 5, length.out=200)[n.star], seq(0, 5, length.out=200)[n.star]), 
  y=c(NN.VV.converge[n.star], -2), lty=2, col="black")
  
axis(1, at=seq(0, 5, length.out=200)[n.star],labels="n*", las=1)

axis(1, at=5,labels=expression(n[t]), las=1, tick=FALSE)

axis(2, at=max(unique.VV(seq(0, 5, length.out=200))),labels=expression(V[t]), las=1, tick=FALSE)







#convhulln(grid.mat[condition.result, c("sigma", "alpha", "a")])


#ps <- matrix(rnorm(3000), ncol=3) # generate points on a sphere
ps <- grid.mat[condition.result, c("sigma", "alpha", "a")]
ts.surf <- t(convhulln(ps)) # see the qhull documentations for the options
## Not run:
plot3d(grid.mat[condition.result, c("sigma", "alpha", "a")], col="transparent")
rgl.triangles(ps[ts.surf,1],ps[ts.surf,2],ps[ts.surf,3],col="blue",alpha=.2)
for(i in 1:(8*360)) rgl.viewpoint(i/8)



d = c(-1,1)
pc = as.matrix(rbind(expand.grid(d,d,d),0))
tc = delaunayn(pc)
# example tetramesh
library(rgl)
clr = rep(1,3) %o% (1:nrow(tc)+1)
tetramesh(tc,pc,alpha=0.7,col=clr)



polygon3d(matmax(t(as.matrix(grid.mat[condition.result, c("sigma", "alpha", "a")]))))








#for ( i in 1:3) {
#  grid.f<-grid.f[!duplicated(grid.f[, combn(c("sigma", "alpha", "a"), 2)[, i]]), #]
#}

  
#plot3d(as.matrix(grid.f))

polygon3d(as.matrix(grid.f), fill=F)

polygon3d(as.matrix(grid.f), fill=F)





plot3d()
open3d()
polygon3d(do.call(rbind, test))


 x <- c(1:10, 10:1)
    y <- rev(c(rep(c(0,2), 5), rep(c(1.5,-0.5),5)))
    plot(x, y, type="n")
    polygon(x, y)
    open3d()
    shade3d( extrude3d(x, y), col = "red" )











plot(ashape3d(as.matrix(grid.mat[condition.result, c("sigma", "alpha", "a")]), alpha=.25, pert = TRUE, eps = 1e-09))