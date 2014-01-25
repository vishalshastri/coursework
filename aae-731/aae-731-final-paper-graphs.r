

work.dir <- "/Users/travismcarthur/Desktop/Development (731)/Term paper/"


unique.VV<- function(x) { 1+-exp(x-2)/(exp(x-2)+1)}
unique.NN<- function(x) { (1+-exp(x-2)/(exp(x-2)+1))/3+.3}

#png(filename = paste0(work.dir, "unique SS.png"), width = 360, height = 360, type="quartz", res=72*4)

# creating nice plots in R

png(filename = paste0(work.dir, "unique SS.png"), units="in", width=2.5, height=2.5,  pointsize=8, type="quartz", res=72*2)



# svg(filename=paste0(work.dir, "test.svg"))

par(mar=c(2.1,2.1,1,2.1), cex=1.5)


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

dev.off()

par(mar=c(5.1,4.1,4.1,2.1), cex=1)



#######################################################



grid.size <- 50

grid.mat<-expand.grid( sigma=seq(1, 10, length.out=grid.size),
          alpha=seq(0, 0.5, length.out=grid.size),
          L=10000,
          rho=0.95,
          a=seq(1, 1000, length.out=grid.size)
        )


condition.test <- function(x) {
  x["sigma"]/(1-x["alpha"]) - 1 < x["L"]/(x["rho"]*x["a"]) & 
    x["L"]/(x["rho"]*x["a"]) < x["sigma"] / x["alpha"] -1
}

condition.result<-apply(grid.mat, 1, condition.test)


NN.line.fn<-function(a, L, n, sigma, alpha) {
  if (n<=1) {A<-alpha} else {A<-1-alpha}
  (a/L)*(1-A/sigma)
}

VV.line.fn<-function(rho, sigma, n, alpha) {
  if (n<=1) {A<-alpha} else {A<-1-alpha}
  A/(rho*sigma*n)
}

set.seed(77)

#grid.pick <- grid.shape.mat[sample(1:nrow(grid.shape.mat), 1), ]
 grid.pick<-grid.mat[!condition.result, c("sigma", "alpha", "a")][sample(1:sum(!condition.result), 1), ]


n.max<-12
n.points <-30

NN.line<-rep(0, (n.max*n.points))

for (i in 1:(n.max*n.points)) {
  NN.line[i] <-NN.line.fn(a=grid.pick$a, L=10000, n=seq(0, n.max, length.out=n.max*n.points)[i], sigma=grid.pick$sigma, alpha=grid.pick$alpha)
}



VV.line<-rep(0, (n.max*n.points))

for (i in 1:(n.max*n.points)) {
  VV.line[i] <-VV.line.fn(rho=0.95, sigma=grid.pick$sigma, n=seq(0, n.max, length.out=n.max*n.points)[i], alpha=grid.pick$alpha)
}

par(mar=c(5.1,4.1,0,2.1), cex=1.5)

plot(x=seq(0, n.max, length.out=n.max*n.points), y=NN.line, ylim=c( 0, max(NN.line)*3), xlim=c(0,n.max), col="red", type="l", ylab=expression(V[t]), xlab=expression(n[t]), main="") 
# bquote(sigma == .(signif(grid.pick$sigma, 2)) & alpha == .(signif(grid.pick$alpha, 2)) )

cat("sigma =",  signif(grid.pick$sigma, 2), ", alpha =",  signif(grid.pick$alpha, 2))
    


lines(x=seq(0, n.max, length.out=n.max*n.points), y=VV.line, col="blue")

par(mar=c(5.1,4.1,4.1,2.1), cex=1)





#######################################################




set.seed(90)
# 200, 50

grid.pick <- grid.mat[condition.result, c("sigma", "alpha", "a")][sample(1:sum(condition.result), 1), ]
# grid.pick<-grid.mat[!condition.result, c("sigma", "alpha", "a")][sample(1:sum(!condition.result), 1), ]

cat("sigma =",  signif(grid.pick$sigma, 2), ", alpha =",  signif(grid.pick$alpha, 2), ", L/a=", signif(10000/grid.pick$a, 2) )

n.max<-3
n.points <-60

NN.line<-rep(0, (n.max*n.points))

for (i in 1:(n.max*n.points)) {
  NN.line[i] <-NN.line.fn(a=grid.pick$a, L=10000, n=seq(0, n.max, length.out=n.max*n.points)[i], sigma=grid.pick$sigma, alpha=grid.pick$alpha)
}



VV.line<-rep(0, (n.max*n.points))

for (i in 1:(n.max*n.points)) {
  VV.line[i] <-VV.line.fn(rho=0.95, sigma=grid.pick$sigma, n=seq(0, n.max, length.out=n.max*n.points)[i], alpha=grid.pick$alpha)
}

par(mar=c(5.1,4.1,0,2.1), cex=1.5)

plot(x=seq(0, n.max, length.out=n.max*n.points), y=NN.line, ylim=c( 0, max(NN.line)*2), col="red", type="l", xlab="", ylab="", xaxt='n', yaxt='n')

lines(x=seq(0, n.max, length.out=n.max*n.points), y=VV.line, col="blue")




######

plot(x=seq(0, n.max, length.out=n.max*n.points), y=NN.line, ylim=c( 0, max(NN.line)*2), col="red", type="l", xlab="", ylab="")

lines(x=seq(0, n.max, length.out=n.max*n.points), y=VV.line, col="blue")




rho <- 0.95

L <- 10000
an <- grid.pick$a
sigma <- grid.pick$sigma

A.fn <-function(x) {ifelse(x<1, grid.pick$alpha, 1-grid.pick$alpha)}

V<-.5
n<-.2

expand.grid(V=seq(0.06, 1, length.out=1000), n=)

i <- 1.809034

an<-90000
# seq(0.00, 20, length.out=1000)
for ( i in seq(1.809016 , 1.809036, length.out=100)) {
V<-i
n<-.2

for ( t in 1:100 ) {
  v.dot <-rho*V[t]- A.fn(n[t])/(rho*n[t])

  n.dot <- max(c(0, L/an - ( 1 - A.fn(n[t])/sigma)/V[t] ))
  
  V[t+1]<-V[t]+v.dot
  n[t+1]<-n[t]+n.dot

}
  cat(V[1], "\n")
  print(data.frame(V, n)[100,] )
}



plot(x=seq(0, n.max, length.out=n.max*n.points), y=NN.line, ylim=c( 0, max(NN.line)*2), col="red", type="l", xlab="", ylab="")


plot(data.frame(n, V)[1:26,], xlim=c(0,2), ylim=c(0,20))

lines(x=seq(0, n.max, length.out=n.max*n.points), y=NN.line)

lines(x=seq(0, n.max, length.out=n.max*n.points), y=VV.line, col="blue")


 


plot( type="l")












NN.VV.converge<-apply( matrix(c(NN.line, VV.line), ncol=2), 1, weighted.mean, w=c(.8, .2)) 

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
  NN.VV.converge[1:converge.breakpoint1][NN.VV.converge[1:converge.breakpoint1]<max(NN.line)*2], 
  N=7, col="darkgreen")
  
arrowLine(seq(0, n.max, length.out=n.max*n.points)[-1][(converge.breakpoint2+1):converge.breakpoint3], 
  NN.VV.converge[(converge.breakpoint2+1):converge.breakpoint3],
  N=5, col="darkgreen")
  



n.star.1 <- converge.breakpoint1

n.star.2 <- converge.breakpoint3



############

n <- seq(0, n.max, length.out=n.max*n.points)[n.star.1]

V <- VV.line[n.star.1]


i <- 1.809034

an<-90000
# seq(0.00, 20, length.out=1000)
#for ( i in seq(1.809016 , 1.809036, length.out=100)) {
#V<-i
#n<-.2


an <- grid.pick$a
n <- seq(0, n.max, length.out=n.max*n.points)[n.star.1-1]
V <- VV.line[n.star.1-1]

for ( t in 1:100 ) {
  v.dot <- -(rho*V[t]- A.fn(n[t])/(rho*n[t]))

  n.dot <- - (max(c(0, L/an - ( 1 - A.fn(n[t])/sigma)/V[t] )))
  
  V[t+1]<-V[t]+v.dot
  n[t+1]<-n[t]+n.dot

}

lines(data.frame(n, V), col="violet")


#  cat(V[1], "\n")
  print(data.frame(V, n)[100,] )
#}




##############




lines(x=c(seq(0, n.max, length.out=n.max*n.points)[n.star.1], seq(0, n.max, length.out=n.max*n.points)[n.star.1]), 
  y=c(NN.VV.converge[n.star.1], -2), lty=2, col="black")

axis(1, at=seq(0, n.max, length.out=n.max*n.points)[n.star.1],labels=expression(paste(n[alpha], "*")), las=1)

lines(x=c(seq(0, n.max, length.out=n.max*n.points)[n.star.2], seq(0, n.max, length.out=n.max*n.points)[n.star.2]), 
  y=c(NN.VV.converge[n.star.2], -2), lty=2, col="black")

axis(1, at=seq(0, n.max, length.out=n.max*n.points)[n.star.2],labels=expression(paste(n[1-alpha], "*")), las=1)

axis(1, at=n.max,labels=expression(n[t]), las=1, tick=FALSE)

axis(2, at=max(NN.line)*2,labels=expression(V[t]), las=1, tick=FALSE)



    


par(mar=c(5.1,4.1,4.1,2.1), cex=1)





#######################################################

## Isosurfaces of a brain With WebGL
## Splitting function courtesy of Duncan Murdoch
## Open in Safari - make sure Developer -> Enable WebGL clicked
## Should also open in Chrome (Firefox has initialization problem)
## Can comment out datadir/outdir 
## Clear the workspace - rm(list=ls())


### require(oro.nifti) # for reading
### require(rgl) # for rendering
### require(knitr)
### require(misc3d) # for contours
#################
## may need updated knitr for webgl hook
## use code below:
## require(devtools)
## install_github("knitr", username="yihui")
#################

hook_split_webgl <- function(before, options, envir) {
##   library(rgl)
  ## after a chunk has been evaluated
  if (before || rgl.cur() == 0) return()  # no active device
  name = tempfile('rgl', '.', '.html'); on.exit(unlink(name))
  par3d(windowRect = 100 + options$dpi * c(0, 0, options$fig.width, options$fig.height))
  Sys.sleep(.05) # need time to respond to window size change

  writeLines(c('%WebGL%', '<script>webGLStart();</script>'), tpl <- tempfile())
  warning("writing webGL to knitted doc")
  writeWebGL_split(dir = dirname(name), filename = name, template = tpl)
  res = readLines(name)
  res = res[!grepl('^\\s*$', res)] # remove blank lines
  paste(gsub('^\\s*<', '<', res), collapse = '\n') # no spaces before HTML tags
}

### setting hooks and options - can change options obviously 
knit_hooks$set(webgl = hook_split_webgl)
opts_chunk$set(echo=FALSE, prompt=FALSE, messages=FALSE, warning=FALSE)


### writeWebGL_split - has add on for splitting triangles to maximum number of vertices
### use writeIt = FALSE for knitr - just splits up triangles
### source('writeWebGL_split.R', chdir = FALSE)


#######################################################




library("alphashape3d")
library("rgl")

grid.shape.mat<-grid.mat[condition.result, c("sigma", "alpha", "a")]

grid.f <-grid.shape.mat

x.max<-max(grid.f[, 1])
y.max<-max(grid.f[, 2])
z.max<-max(grid.f[, 3])

grid.f <- grid.f[order(grid.f[,1], grid.f[,2], grid.f[,3]), ]
for (i in 1:3) {
  grid.f[, i] <- grid.f[, i]/max(grid.f[, i])
}


grid.plot<-ashape3d(as.matrix(grid.f)[seq(1, nrow(grid.f), by=2), ], alpha=.1, pert = TRUE, eps = 1e-09)

open3d()
plot(grid.plot, col="blue")
rgl.viewpoint(270,20)


axis3d("x",  at = seq(0, 1, length.out=10), labels = signif(seq(0, x.max, length.out=10), 2 ), color="black")
axis3d("y",  at = seq(0, 1, length.out=10), labels = signif(seq(0, y.max, length.out=10), 2 ), color="black")
axis3d("z",  at = seq(0, 1, length.out=10), labels = signif(seq(0, z.max, length.out=10), 2), color="black")

title3d(xlab ="sigma", ylab = "alpha", zlab = "a", color="black") 


source('/Users/travismcarthur/Downloads/RGL_Export-master-2/writeWebGL_split.R', chdir = FALSE)

writeWebGL_split(dir=getwd(), filename ="knitted_webGL_first.html", template = "/my_template.html", width=500)
# /Users/travismcarthur/Downloads/RGL_Export-master-2/
#rgl.quit()


#######################################################


### Template from MNI152 from FSL
#### template <- readNIfTI("MNI152_T1_2mm_brain.nii.gz", reorient=FALSE)

knit_hooks$set(webgl = hook_webgl)
#hook_split_webgl
opts_chunk$set(echo=FALSE, prompt=FALSE, messages=FALSE, warning=FALSE)



#grid.size <- 50

#grid.mat<-expand.grid( sigma=seq(1, 10, length.out=grid.size),
#          alpha=seq(0, 0.5, length.out=grid.size),
#          L=10000,
#          rho=0.95,
#          a=seq(1, 1000, length.out=grid.size)
#        )


#condition.test <- function(x) {
#  x["sigma"]/(1-x["alpha"]) < x["L"]/(x["rho"]*x["a"]) & 
#    x["L"]/(x["rho"]*x["a"]) < x["sigma"]/x["alpha"] - 1
#}

#condition.result<-apply(grid.mat, 1, condition.test)

#plot3d(grid.mat[condition.result, c("sigma", "alpha", "a")])

#surface3d(grid.mat[condition.result, c("sigma", "alpha", "a")])


#library("geometry")
library("rgl")
##### install.packages(c("R.basic"), contriburl="http://www.braju.com/R/repos/")
#library("R.basic")

grid.shape.mat<-grid.mat[condition.result, c("sigma", "alpha", "a")]

grid.f <-grid.shape.mat

x.max<-max(grid.f[, 1])
y.max<-max(grid.f[, 2])
z.max<-max(grid.f[, 3])

grid.f <- grid.f[order(grid.f[,1], grid.f[,2], grid.f[,3]), ]
for (i in 1:3) {
  grid.f[, i] <- grid.f[, i]/max(grid.f[, i])
}




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

#supply.ratio.fn(H.n=H.intensive.n, X.n=X.intensive.n, sigma=grid.pick$sigma, alpha=grid.pick$alpha)

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





#double log works well
C.values.v<-C.values.v[is.finite(C.values.v)]
C.values.col.v<-log(log(C.values.v+1)+1)+min(log(log(C.values.v+1)+1), na.rm=TRUE)
C.values.col.v<-C.values.col.v/max(C.values.col.v, na.rm=TRUE)

#plot3d(grid.shape.mat[is.finite(C.values.v), ], cex=10,
#  col=rgb(red=C.values.col.v, green=0, blue=0, maxColorValue = 1))


easier.subset<-seq(1, sum(is.finite(C.values.v)), length.out=sum(is.finite(C.values.v))/20)



#######################################################

### threshold for brain image - just the overall brain
### make lower for smoother surface - 4500 has good mix of features
### 1000 is smooth
###cut <- 4500

### dtemp <- dim(template)
#### contour3d(template, x=1:dtemp[1], y=1:dtemp[2], z=1:dtemp[3], level = cut, alpha = 0.1, draw = TRUE)

### this would be the ``activation'' or surface you want to render 
### here just taking the upper WM from the template image
### contour3d(template, level = c(8200, 8250), alpha = c(0.5, 0.8), add = TRUE, color=c("yellow", "red"))

### add text
#### text3d(x=dtemp[1]/2, y=dtemp[2]/2, z = dtemp[3]*0.98, text="Top")
#### text3d(x=dtemp[1]*0.98, y=dtemp[2]/2, z = dtemp[3]/2, text="Right")
########
##### If you're data is small  - use command below 
###### writeWebGL_split(writeIt=FALSE, verb=FALSE) 
#### and remove rgl.quit() in the code below
##### (and use onload="webGLStart();" in body) - see README
####### RStudio failed whhen the file was too large so just
##### write the html and embed it

rgl.bg( color=c("white","white"))
rgl.viewpoint(270,20)

plot3d(grid.shape.mat[is.finite(C.values.v), ][easier.subset, ], type="s", size=.7,
  col=rgb(red=C.values.col.v[easier.subset], green=0, blue=0, maxColorValue = 1))

source('/Users/travismcarthur/Downloads/RGL_Export-master-2/writeWebGL_split.R', chdir = FALSE)

writeWebGL_split(dir=getwd(), filename ="knitted_webGL.html", template = "my_template.html", width=500)
# /Users/travismcarthur/Downloads/RGL_Export-master-2/
rgl.quit()



#######################################################




condition.test <- function(x) {
  x["sigma"]/(1-x["alpha"]) - 1 < x["L"]/(x["rho"]*x["a"]) & 
    x["L"]/(x["rho"]*x["a"]) < (x["sigma"] - x["alpha"])/(1-x["alpha"])
}
# no amount of positive expectations will get us over the hurdle. Must have govt intervention



condition.result<-apply(grid.mat, 1, condition.test)


NN.line.fn<-function(a, L, n, sigma, alpha) {
  if (n<=1) {A<-alpha} else {A<-1-alpha}
  (a/L)*(1-A/sigma)
}

VV.line.fn<-function(rho, sigma, n, alpha) {
  if (n<=1) {A<-alpha} else {A<-1-alpha}
  A/(rho*sigma*n)
}

#set.seed(10)
# also 
#          sigma     alpha    a
#123394 8.897959 0.1734694 1000

grid.pick <- grid.shape.mat["123394", ] # based on a number of experiments
# grid.pick<-grid.mat[condition.result, c("sigma", "alpha", "a")][sample(1:sum(condition.result), 1), ]


n.max<-2
n.points <-200

NN.line<-rep(0, (n.max*n.points))

for (i in 1:(n.max*n.points)) {
  NN.line[i] <-NN.line.fn(a=grid.pick$a, L=10000, n=seq(0, n.max, length.out=n.max*n.points)[i], sigma=grid.pick$sigma, alpha=grid.pick$alpha)
}



VV.line<-rep(0, (n.max*n.points))

for (i in 1:(n.max*n.points)) {
  VV.line[i] <-VV.line.fn(rho=0.95, sigma=grid.pick$sigma, n=seq(0, n.max, length.out=n.max*n.points)[i], alpha=grid.pick$alpha)
}

par(mar=c(5.1,4.1,0,2.1), cex=1.5)

plot(x=seq(0, n.max, length.out=n.max*n.points), y=NN.line, ylim=c( 0, max(NN.line)*2), xlim=c(0,n.max), col="red", type="l", ylab=expression(V[t]), xlab=expression(n[t]), main="") 
# bquote(sigma == .(signif(grid.pick$sigma, 2)) & alpha == .(signif(grid.pick$alpha, 2)) )

cat("sigma =",  signif(grid.pick$sigma, 2), ", alpha =",  signif(grid.pick$alpha, 2), ", L/a=", signif(10000/grid.pick$a, 2) )
    


lines(x=seq(0, n.max, length.out=n.max*n.points), y=VV.line, col="blue")

par(mar=c(5.1,4.1,4.1,2.1), cex=1)

#######################################################



#require(ggplot2)
#qplot(wt, mpg, data = mtcars)


grid.shape.mat<-grid.mat[condition.result, c("sigma", "alpha", "a")]

grid.f <-grid.shape.mat

x.max<-max(grid.f[, 1])
y.max<-max(grid.f[, 2])
z.max<-max(grid.f[, 3])

grid.f <- grid.f[order(grid.f[,1], grid.f[,2], grid.f[,3]), ]
for (i in 1:3) {
  grid.f[, i] <- grid.f[, i]/max(grid.f[, i])
}


grid.plot<-ashape3d(as.matrix(grid.f)[seq(1, nrow(grid.f), by=2), ], alpha=.1, pert = TRUE, eps = 1e-09)

open3d()
plot(grid.plot, col="blue")
rgl.viewpoint(270,20)


axis3d("x",  at = seq(0, 1, length.out=10), labels = signif(seq(0, x.max, length.out=10), 2 ), color="black")
axis3d("y",  at = seq(0, 1, length.out=10), labels = signif(seq(0, y.max, length.out=10), 2 ), color="black")
axis3d("z",  at = seq(0, 1, length.out=10), labels = signif(seq(0, z.max, length.out=10), 2), color="black")

title3d(xlab ="sigma", ylab = "alpha", zlab = "a", color="black") 


source('/Users/travismcarthur/Downloads/RGL_Export-master-2/writeWebGL_split.R', chdir = FALSE)

writeWebGL_split(dir=getwd(), filename ="knitted_webGL_third.html", template = "/my_template.html", width=500)
# /Users/travismcarthur/Downloads/RGL_Export-master-2/
#rgl.quit()

# http://www.stattler.com/article/three-dimensional-plots-using-rgl-package
# https://stackoverflow.com/questions/8702393/rgl-postscript-file-saved-without-varying-text-sizes
# http://r.789695.n4.nabble.com/Exporting-an-rgl-graph-td1872712.html







