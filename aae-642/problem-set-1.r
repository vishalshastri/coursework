
library("foreign")

work.dir <- "/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 1/"

lsms.df <- read.dta(paste0(work.dir, "pset1.dta"))

# lsms.df <- lsms.df[!duplicated(lsms.df$codhh), ]

# summary(lsms.df$f_geogra)

lsms.df$real.per.cap.income <- lsms.df$i_income/lsms.df$f_geogra

lsms.df$real.per.cap.consumption<- lsms.df$constot / lsms.df$hhsize

summary(lsms.df$hhwght)


fgt.poverty <- function(y, weight, z, alpha) {
  # y is income vector, alpha is alpha, z is the poverty line
#  N <- length(y)
#  weight <- weight[y<=z]
#  
#  y <- y[y<=z] # only count the poor
#  (1/N) *  sum( ((z-y)/z)^alpha * weight) / sum(weight)
  
  sum( (y<=z) * (1-y/z)^alpha * weight ) /sum(weight)
  
}

fgt.poverty(y=lsms.df$real.per.cap.income, weight=lsms.df$hhwght, z=lsms.df$xpovline[1], alpha=0)

# fgt.poverty(y=lsms.df$real.per.cap.income, weight=rep(1, nrow(lsms.df)), z=lsms.df$xpovline[1], alpha=0)

table(lsms.df$xpovline)

#unique(lsms.df$departmt)
#unique(lsms.df$region)


# st error, plus dealing with clustering

Howes.Lanjouw.clustered.se <- function(y, cluster.var, weight, z, alpha) {
  
  n_h <- length(unique(cluster.var))
  
#  pi_k <- (y<=z) * (1-y/z)^alpha 
  pi_k <-  (y<=z) * (1-y/z)^alpha * weight 
  
  t_hc <- aggregate(pi_k, by=list(cluster.var), FUN=sum)[, 2]
  
  t_hc <- t_hc / aggregate(weight, by=list(cluster.var), FUN=sum)[, 2]

#  t_hc <- t_hc / aggregate(pi_k, by=list(cluster.var), FUN=length)[, 2]
  # just weight clusters by how large they are in the dataset
  
  t_h <- (1/n_h) * sum(t_hc)
  
#  if (n_h>1 ) {
    var_t_h <- (1/ (n_h*(n_h-1))) * sum((t_hc-t_h)^2)
#  } else {
#    var_t_h <- sum((t_hc-t_h)^2)
#  }
  
  sqrt(var_t_h)

}


# Howes.Lanjouw.clustered.se(y=lsms.df$real.per.cap.income, cluster.var=lsms.df$departmt,  z=lsms.df$xpovline[1], alpha=0)



poverty.options <- expand.grid(alpha=c(0,2), 
  urban=1:0, 
  measure=c("real.per.cap.consumption", "real.per.cap.income"), 
  line=c(lsms.df$povline[1], lsms.df$xpovline[1]),
  stringsAsFactors = FALSE
)

poverty.options$poverty <- 0
poverty.options$poverty.se <- 0
poverty.options$N <- 0


lsms.df$constant <- factor("a")

# TODO: The clustered standard errors probably aren't correct


for (i in 1:nrow(poverty.options)) {
  
  lsms.df.subset <- lsms.df[lsms.df$urban==poverty.options$urban[i], ]
  
  poverty.options$N[i] <- nrow(lsms.df.subset)
  
  poverty.options$poverty[i] <- fgt.poverty(
      y=lsms.df.subset[, poverty.options$measure[i]], 
      weight=lsms.df.subset$hhwght,
      z=poverty.options$line[i], 
      alpha=poverty.options$alpha[i])
  
  poverty.options$poverty.se[i] <- Howes.Lanjouw.clustered.se(
      y=lsms.df.subset[, poverty.options$measure[i]], 
      cluster.var=lsms.df.subset$departmt,   # constant
      weight=lsms.df.subset$hhwght,
      z=poverty.options$line[i], 
      alpha=poverty.options$alpha[i])
   
}

print(poverty.options, digits=1)

#y=lsms.df$real.per.cap.income
#cluster.var=lsms.df$departmt
#z=lsms.df$xpovline[1]
#alpha=0
#weight=lsms.df$hhwght

# So subset by taking just hh's first

# TODO: Must remember to weight for this one too



function(x, theta=1, alpha=1) {
  by(x, INDICES=codhh, FUN=function(x, theta=theta, alpha=alpha) {
    x$total.adults <- x$adultm + x$adultf
    
    adult.equivs <- (x$total.adults) 
    
  }
    
    ))
}




lsms.df$family.type <- "Misc"
lsms.df$family.type[ (lsms.df$adultm + lsms.df$adultf + lsms.df$old65) >= 4] <- "Intergenerational"
lsms.df$family.type[ lsms.df$adultf == 1 & lsms.df$adultm == 0 & lsms.df$kid0_14 > 0] <- "Single mother"
lsms.df$family.type[ lsms.df$adultf == 1 & lsms.df$adultm == 1 & lsms.df$kid0_14 == 0] <- "Man & woman"

library(ggplot2)
library(reshape2)
 




# target.family <- "Single mother"

# table(lsms.df$hhsize)

equiv.mat.ls <- list()

for ( target.family in unique(lsms.df$family.type)) {
  
  x <- lsms.df
  
  for (location in 0:1) {
    
    equiv.mat <- matrix(0, nrow=length(seq(.25, 1, by=.05)), 
      ncol=length(seq(.5, 1, by=.05)))
    
    colnames(equiv.mat) <- as.character(seq(.5, 1, by=.05))
    rownames(equiv.mat) <- as.character(seq(.25, 1, by=.05))
    
    equiv.grid <- expand.grid(alpha.equiv=seq(.25, 1, by=.05), theta=seq(.5, 1, by=.05) )
    
    for ( j in 1:nrow(equiv.grid)) {
    alpha.equiv <- equiv.grid$alpha.equiv[j]
    theta <- equiv.grid$theta[j]
    
    x.subset <- x[x$urban==location & x$family.type==target.family, ]
    x.subset$total.adults <- x.subset$adultm + x.subset$adultf + x.subset$old65
    x.subset$adult.equivs <- (x.subset$total.adults + alpha.equiv * x.subset$kid0_14)^theta  
    x.subset$real.equiv.consumption<- x.subset$constot / x.subset$adult.equivs
    
    pov.data.container <- vector(mode = "numeric", length = 3)
    names(pov.data.container) <- c("N", "poverty", "poverty.se")
  
    pov.data.container[1] <- nrow(x.subset)
  
    pov.data.container[2] <- fgt.poverty(
      y=x.subset$real.equiv.consumption, 
      weight=x.subset$hhwght,
      z=x.subset$povline[1], 
      alpha=2)
  
    pov.data.container[3] <- Howes.Lanjouw.clustered.se(
      y=x.subset$real.equiv.consumption, 
      cluster.var=x.subset$departmt,   # constant
      weight=x.subset$hhwght,
      z=x.subset$povline[1], 
      alpha=2)
    
    equiv.mat[rownames(equiv.mat)==alpha.equiv, 
      colnames(equiv.mat)==theta]<- pov.data.container[2]
    #print(pov.data.container, digits=2)
    
    }
    
    equiv.mat.ls[[length(equiv.mat.ls )+1]] <- equiv.mat
    names(equiv.mat.ls)[length(equiv.mat.ls )] <- 
      paste0(target.family, ifelse(location==1, ", Urban", ", Rural"))
    
  }
}
  
max(unlist(equiv.mat.ls))
min(unlist(equiv.mat.ls))

library(corrgram)

corrgram(x


corrgram(equiv.mat, type="cor")

library(scales)

#ggplot likes the data 'melted' one value per row
p <- ggplot(data=melt(equiv.mat) , aes(x=Var1, y=Var2, fill=value)) + geom_tile() + scale_fill_gradient2(mid="beige", high=muted("red", c=200), limits=c(min(equiv.mat), max(equiv.mat)))
print(p )
  
  
poverty.options$poverty <- 0
poverty.options$poverty.se <- 0
poverty.options$N <- 0


lsms.df$constant <- factor("a")

# TODO: The clustered standard errors probably aren't correct


for (i in 1:nrow(poverty.options)) {
  
  lsms.df.subset <- lsms.df[lsms.df$urban==poverty.options$urban[i], ]
  
  poverty.options$N[i] <- nrow(lsms.df.subset)
  
  poverty.options$poverty[i] <- fgt.poverty(
      y=lsms.df.subset[, poverty.options$measure[i]], 
      weight=lsms.df.subset$hhwght,
      z=poverty.options$line[i], 
      alpha=poverty.options$alpha[i])
  
  poverty.options$poverty.se[i] <- Howes.Lanjouw.clustered.se(
      y=lsms.df.subset[, poverty.options$measure[i]], 
      cluster.var=lsms.df.subset$departmt,   # constant
      weight=lsms.df.subset$hhwght,
      z=poverty.options$line[i], 
      alpha=poverty.options$alpha[i])
   
}

print(poverty.options, digits=1)

  
}








lsms.df





poverty.options <- expand.grid(alpha=c(0,2), 
  urban=1:0, 
  measure=c("real.per.cap.consumption", "real.per.cap.income"), 
  line=c(lsms.df$povline[1], lsms.df$xpovline[1]),
  stringsAsFactors = FALSE
)

poverty.options$poverty <- 0
poverty.options$poverty.se <- 0
poverty.options$N <- 0


lsms.df$constant <- factor("a")

# TODO: The clustered standard errors probably aren't correct


for (i in 1:nrow(poverty.options)) {
  
  lsms.df.subset <- lsms.df[lsms.df$urban==poverty.options$urban[i], ]
  
  poverty.options$N[i] <- nrow(lsms.df.subset)
  
  poverty.options$poverty[i] <- fgt.poverty(
      y=lsms.df.subset[, poverty.options$measure[i]], 
      weight=lsms.df.subset$hhwght,
      z=poverty.options$line[i], 
      alpha=poverty.options$alpha[i])
  
  poverty.options$poverty.se[i] <- Howes.Lanjouw.clustered.se(
      y=lsms.df.subset[, poverty.options$measure[i]], 
      cluster.var=lsms.df.subset$departmt,   # constant
      weight=lsms.df.subset$hhwght,
      z=poverty.options$line[i], 
      alpha=poverty.options$alpha[i])
   
}

print(poverty.options, digits=1)



# Proposal:
# Intergenerational family (4 adults, any number of children) ( hard to say what age people are grandparents, so will not distiguish on age. Could be just "adult, unmarried" children in house). In general, for sure we are going to pick up "non-target" households.
# Single mother - may just be due to temporary labor migration.
# Two adults, no children
# Misc category







kid0_14  adultf


lsms.df <- lsms.df[!duplicated(lsms.df$codhh), ]



equiv.grid$values <- c(equiv.mat)

library(lattice)
panel.corrgram.2(equiv.grid$alpha.equiv, equiv.grid$theta, equiv.grid$equiv.mat)


library(ellipse)


panel.corrgram.2<-function(x,y,z,
subscripts,at=pretty(z),scale=0.8,...)
{
require('grid',quietly=TRUE)
x<-as.numeric(x)[subscripts]
y<-as.numeric(y)[subscripts]
z<-as.numeric(z)[subscripts]
zcol<-level.colors(z,at=at,...)
for(i in seq(along=z))
{
lims<-range(0,z[i])
tval<-2*base::pi*
seq(from=lims[1],to=lims[2],by=0.01)
grid.polygon(x=x[i]+.5*scale*c(0,sin(tval)),
y=y[i]+.5*scale*c(0,cos(tval)),
default.units='native',
gp=gpar(fill=zcol[i]))
grid.circle(x=x[i],y=y[i],r=.5*scale,
default.units='native')
}
}


levelplot(cor(longley),
at=do.breaks(c(-1.01,1.01),101),xlab=NULL,  # at=do.breaks(c(-1.01,1.01),101)
ylab=NULL,colorkey=list(space='top'),
scales=list(x=list(rot=90)),
panel=panel.corrgram.2,
col.regions=colorRampPalette(c('red','white','blue')))


equiv.mat[1:5, 1:5]*100

install.packages("corrplot")
library(corrplot)

corrplot(cor(longley), method = "pie")

corrplot(equiv.mat, method = "pie", is.corr = FALSE, cl.lim = c(0, max(unlist(equiv.mat.ls))))

pdf(paste0(work.dir, 'corrgram_pacman.pdf'),  width=8, height=10.5) #paper="letter") #

for ( j in (1:ceiling(length(equiv.mat.ls)/4)) -1) {

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))

for (i in 1:4+j*4) {
  
  corrplot(equiv.mat.ls[[i]][nrow(equiv.mat.ls[[i]]):1, ], 
    method = "pie", 
    is.corr = FALSE, 
    cl.lim = c(0, max(unlist(equiv.mat.ls))), 
    tl.col="black", 
    title=paste0("Figure 1.", letters[i], ": ", names(equiv.mat.ls)[i]), 
    mar = c(2, 2, 2, 2))
  # add=c(FALSE, TRUE, TRUE, TRUE)[i]

  mtext(expression(alpha), side = 2, las=1, cex=1.5, col="blue") # , padj = 5
  mtext(expression(theta), side = 3, padj =1.3, cex=1.5, col="blue")
}
}

dev.off()



screen(1)

par(mfg=c(1,1))


## allocate figure 2 the intersection of column 2 and row 2


layout(



# Cairo and cairoDevice packages is strongly recommended to produce high-quality PNG, JPEG, TIFF bitmap files, especially for that method circle, ellipse .

corrplot(equiv.mat.ls[[2]][nrow(equiv.mat.ls[[2]]):1, ], method = "pie", is.corr = FALSE, cl.lim = c(0, max(unlist(equiv.mat.ls))), add=TRUE, )

plot(1)

# .pardefault <- par(no.readonly = T)
par(.pardefault)

par(mar = c(3, 3, 3, 1))





title(main="My Title", col.main="red", 
    sub="My Sub-title", col.sub="blue", 
 	 xlab="My X label", ylab="My Y label",
  col.lab="green", cex.lab=0.75)

resetPar <- function() {
    dev.new()
    op <- par(no.readonly = TRUE)
    dev.off()
    op
}

par(resetPar())


test.mat <- equiv.mat.ls[[1]]

Vectorize
do.call()

colnames(test.mat) <- c(expression(as.list(paste0(beta, ": ", colnames(test.mat)))), expression(paste0(beta, ": ", colnames(test.mat))))

corrplot([nrow(equiv.mat.ls[[1]]):1, ], method = "pie", is.corr = FALSE, cl.lim = c(0, max(unlist(equiv.mat.ls))), tl.col="black"), xlab="My X label")

expression(paste("Sampled values, ", mu, "=5, ", sigma,
    "=1"))

