

library(broman)

# triplot()
#    x <- cbind(c(0.9, 0.05, 0.05), c(0.8, 0.1, 0.1), c(0.1, 0.9, 0), c(0, 0.9, 0.1))
#    tripoints(x, lwd=2, col=c("black","blue","red","green"), pch=16)
#    trilines(x, lwd=2, col="orange")
#    y <- cbind(c(0.05, 0.05, 0.9), c(0.25, 0.25, 0.5))
#    triarrow(y, col="blue", lwd=2, len=0.1)



c("a", "c", "d")
x <- rbind(
c( 4/7, 0, 3/7),
c( 0, 2/3, 1/3),

c( NA, 0, 5/7),
c( 0, 1, 0),

c( 0,NA ,1/2 ),
c( 1/2, 0, NA),

c( 0, 1/2, NA),
c( NA,1/2 , 0),

c( 2/3, 0, NA),
c( 0, 1, 0),

c( 2/5, NA, 0),
c( 0,0 ,1 )

)

for ( i in 1:ncol(x)) {
  x[is.na(x[, i]), i] <- 1 - rowSums(x, na.rm=TRUE)[is.na(x[, i])]
}

x<-x[, c(3, 1, 2)]

triplot(c("a", "c", "d")[c(3, 1, 2)])

gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}

for ( i in 1:(nrow(x)/2)) {

  trilines(x[c(i*2-1, i*2 ),], lwd=2, lty=i, col=gg_color_hue(nrow(x)/2)[i]) #    col=rainbow(nrow(x)/2)[i])
  
}


# trilines(x[c(i*2-1, i*2 ),], lwd=2, col=c("black","blue","red","green")[i])

# tripoints(x, lwd=2, col=c("black","blue","red","green"), pch=16)



