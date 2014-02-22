
library("foreign")


# .pardefault <- par(no.readonly = T)
# par(.pardefault)

# par(mar = c(3, 3, 3, 1))

#install.packages("corrplot")
library(corrplot)
library(lattice)

# TODO: With the HH weights, do we have to re-weight them when we look at individuals?

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

Ravallion.92.se <- function(y,  weight, z, alpha) {
  sqrt(
    (fgt.poverty(y=y, weight=weight, z=z, alpha=alpha*2) - 
    fgt.poverty(y=y, weight=weight, z=z, alpha=alpha)^2)/length(y)
  )
}


# From Ravallion 1992, "Poverty Comparisons: A Guide to Concepts and Methods", p. 50



poverty.options <- expand.grid(alpha=c(0,2), 
  urban=1:0, 
  measure=c("real.per.cap.consumption", "real.per.cap.income"), 
  line=c(lsms.df$povline[1], lsms.df$xpovline[1]),
  stringsAsFactors = FALSE
)

poverty.options  <- poverty.options [do.call(order, poverty.options ), ]

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
  
  poverty.options$poverty.se[i] <- Ravallion.92.se(
      y=lsms.df.subset[, poverty.options$measure[i]], 
      #cluster.var=lsms.df.subset$departmt,   # constant
      weight=lsms.df.subset$hhwght,
      z=poverty.options$line[i], 
      alpha=poverty.options$alpha[i])
   
}

print(poverty.options, digits=1)

#welch.t.test <- function(mu1, se1, n1, mu2, se2, n2) {
#  se1 <- se1 * sqrt(n1)
#  se2 <- se2 * sqrt(n2)
#  sd.combined <- sqrt(se1^2/n1 + se2^2/n2 )
#  (mu1 - mu2)/ sd.combined
#}
# From http://staff.ustc.edu.cn/~zwp/teach/Reg/Welch%20test.pdf. Recall that SE=s/sqrt(n)

poverty.h.test <- function(mu1, n1, mu2, n2) {
  H <- (n1*mu1 + n2*mu2)/(n1+n2)
  sd.combined <- sqrt(H*(1-H)* (1/n1 + 1/n2))
  (mu1 - mu2)/ sd.combined
}
# TODO: The above may only be for the headcount, for P_2, for example
# This is from Ravallion

pov.test <- t(combn(1:nrow(poverty.options), 2))
# pov.test <- pov.test[pov.test[, 1]!=pov.test[, 2], ]

pov.test <- as.data.frame(pov.test)

pov.test$p <- 100
rownames(poverty.options) <- 1:nrow(poverty.options)

for ( i in 1:nrow(pov.test)) {
  
  test.stat <- with(poverty.options, {
    poverty.h.test(poverty[pov.test[i, 1]],  N[pov.test[i, 1]], # poverty.se[pov.test[i, 1]],
      poverty[pov.test[i, 2]],  N[pov.test[i, 2]]) # poverty.se[pov.test[i, 2]],
  } )
  pov.test$p[i] <- 2*pnorm(-abs(test.stat))
}

hist(pov.test$p)
summary(pov.test$p)
pov.test$p[pov.test$p>0.01]

unsignif.tests<- pov.test[pov.test$p>0.01, ]

for ( i in 1:nrow(unsignif.tests)) { 
  print( poverty.options[unsignif.tests[i, 1], ])
  print( poverty.options[unsignif.tests[i, 2], ])
  cat("\n")
}
poverty.options

  
#y=lsms.df$real.per.cap.income
#cluster.var=lsms.df$departmt
#z=lsms.df$xpovline[1]
#alpha=0
#weight=lsms.df$hhwght

# So subset by taking just hh's first

# TODO: Must remember to weight for this one too



#function(x, theta=1, alpha=1) {
#  by(x, INDICES=codhh, FUN=function(x, theta=theta, alpha=alpha) {
#    x$total.adults <- x$adultm + x$adultf
#    adult.equivs <- (x$total.adults)   
#  }
#    ))
#}




lsms.df$family.type <- "Misc"
lsms.df$family.type[ (lsms.df$adultm + lsms.df$adultf + lsms.df$old65) >= 4] <- "Intergenerational"
lsms.df$family.type[ lsms.df$adultf == 1 & lsms.df$adultm == 0 & lsms.df$kid0_14 > 0] <- "Single mother"
lsms.df$family.type[ lsms.df$adultf == 1 & lsms.df$adultm == 1 & lsms.df$kid0_14 == 0] <- "Man & woman"

library(ggplot2)
library(reshape2)
 



# New code takes the poverty at an individual level

# target.family <- "Single mother"

# table(lsms.df$hhsize)

equiv.mat.ls <- list()
pov.data.container.ls <- list()

for ( target.family in unique(lsms.df$family.type)) {
  
#  x.df <- lsms.df[!duplicated(lsms.df$codhh), ]
  x.df <- lsms.df
  
  for (location in 0:1) {
    
    equiv.mat <- matrix(0, nrow=length(seq(.25, 1, by=.05)), 
      ncol=length(seq(.5, 1, by=.05)))
    
    colnames(equiv.mat) <- as.character(seq(.5, 1, by=.05))
    rownames(equiv.mat) <- as.character(seq(.25, 1, by=.05))
    
    equiv.grid <- expand.grid(alpha.equiv=seq(.25, 1, by=.05), theta=seq(.5, 1, by=.05) )
    
    for ( j in 1:nrow(equiv.grid)) {
    alpha.equiv <- equiv.grid$alpha.equiv[j]
    theta <- equiv.grid$theta[j]
    
    x.subset <- x.df[x.df$urban==location & x.df$family.type==target.family, ]
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
      
    pov.data.container[3] <- Ravallion.92.se(
      y=x.subset$real.equiv.consumption, 
     # cluster.var=x.subset$departmt,   # constant
      weight=x.subset$hhwght,
      z=x.subset$povline[1], 
      alpha=2)
  
#    pov.data.container[3] <- Howes.Lanjouw.clustered.se(
#      y=x.subset$real.equiv.consumption, 
#      cluster.var=x.subset$departmt,   # constant
#      weight=x.subset$hhwght,
#      z=x.subset$povline[1], 
#      alpha=2)
    if (alpha.equiv == 1 & theta == 1) {
      pov.data.container.ls[[length(pov.data.container.ls)+1]]<- pov.data.container
    }
    
    
    equiv.mat[rownames(equiv.mat)==alpha.equiv, 
      colnames(equiv.mat)==theta]<- pov.data.container[2]
    #print(pov.data.container, digits=2)
    
    }
    
    equiv.mat.ls[[length(equiv.mat.ls )+1]] <- equiv.mat
    names(equiv.mat.ls)[length(equiv.mat.ls )] <- 
      paste0(target.family, ifelse(location==1, ", Urban", ", Rural"))
    
  }
}

E.1.groups.df <- as.data.frame(do.call( rbind, pov.data.container.ls) )

E.1.groups.df$group <- names(equiv.mat.ls)




pov.test <- t(combn(1:nrow(E.1.groups.df), 2))
# pov.test <- pov.test[pov.test[, 1]!=pov.test[, 2], ]

pov.test <- as.data.frame(pov.test)

pov.test$p <- 100
rownames(E.1.groups.df) <- 1:nrow(E.1.groups.df)

for ( i in 1:nrow(pov.test)) {
  
  test.stat <- with(E.1.groups.df, {
    poverty.h.test(poverty[pov.test[i, 1]],  N[pov.test[i, 1]], # poverty.se[pov.test[i, 1]],
      poverty[pov.test[i, 2]],  N[pov.test[i, 2]]) # poverty.se[pov.test[i, 2]],
  } )
  pov.test$p[i] <- 2*pnorm(-abs(test.stat))
}

hist(pov.test$p)
summary(pov.test$p)
pov.test$p[pov.test$p>0.05]

unsignif.tests<- pov.test[pov.test$p>0.05, ]

for ( i in 1:nrow(unsignif.tests)) { 
  print( E.1.groups.df[unsignif.tests[i, 1], ])
  print( E.1.groups.df[unsignif.tests[i, 2], ])
  cat("\n")
}
E.1.groups.df

















#  crossover.check <- expand.grid(1:length(equiv.mat.ls), 1:length(equiv.mat.ls))

crossover.check <- t(combn(1:length(equiv.mat.ls), 2))

require("gplots")

pdf(paste0(work.dir, 'crossover plot.pdf'),  width=8, height=10.5) #paper="letter") #

#grid.newpage()
#pushViewport(viewport(layout = grid.layout(3,2)))

#layout(matrix(1:6, 3, 2, byrow = TRUE))

k <- 0

for ( i in 1:nrow(crossover.check) ) {
  mat.comparison <- equiv.mat.ls[[crossover.check[i, 1]]]<equiv.mat.ls[[crossover.check[i, 2]]]
  
  if (length(table(mat.comparison))==1 ) {next} 
  k <- 1+k
#  pushViewport(viewport(layout.pos.col=2, layout.pos.row=2))
#  if (k==7) { layout(matrix(1:6, 3, 2, byrow = TRUE)) }
  
  p <- levelplot(t(mat.comparison), border="black", border.lwd = 0.3, 
    colorkey=FALSE, col.regions=c("white", "red"), 
    ylab=list(label=expression(alpha), rot=0, cex=1.5 ), 
    xlab=list(label=expression(theta), cex=1.5),
    main=list(label=paste0("Figure 2.", letters[k], ": Red indicates \n",
      names(equiv.mat.ls)[crossover.check[i, 1]], " < ", 
      names(equiv.mat.ls)[crossover.check[i, 2]] ),
      cex=.8),
    scales=list(x=list(cex=.42), y=list(cex=.5))
    )   
  
  print(p, split=c(ifelse(k %in% c(1:3,7:9), 1, 2) ,
    ifelse(k %% 3 == 0, 3, k %% 3), 2,3), more= k!=6)
}

dev.off()



# test.mat <- equiv.mat.ls[[crossover.check[62, 1]]]>equiv.mat.ls[[crossover.check[62, 2]]]
# mode(test.mat) <- "numeric"
# 
# heatmap(test.mat,  Rowv=NA, Colv=NA, col=c("grey", "blue"), scale='none' )
# 
# (p <- ggplot(melt(test.mat) ) +
#     geom_tile(aes(fill = rescale), colour = "black") +
#     scale_fill_gradient(low = "white",high = "steelblue"))
# 


# install.packages("gplots")

#heatmap.2(test.mat, dendrogram="none",
#          sepwidth=c(0.025, 0.025),  # width of the borders
#          sepcolor='black',        # color of the separation lines
#          )



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


# settle.equiv.df <- lsms.df[lsms.df$parent %in% "head", ]
settle.equiv.df <- lsms.df

# x.subset <- x[x$urban==location & x$family.type==target.family, ]
settle.equiv.df$total.adults <- settle.equiv.df$adultm + settle.equiv.df$adultf + settle.equiv.df$old65
settle.equiv.df$adult.equivs <- (settle.equiv.df$total.adults + 0.75 * settle.equiv.df$kid0_14)^0.75
settle.equiv.df$real.equiv.consumption <- settle.equiv.df$constot / settle.equiv.df$adult.equivs


summary(settle.equiv.df$real.equiv.consumption)


gini.coef <- function(y, weights) {
  y <- rep(y, times=weights)
  n <- length(y)
  y <- sort(y)
  2*sum( 1:n * y) / (n * sum(y)) - (n+1)/n  
}
# From wikipedia


gini.coef.2 <- function(y, weights) {
  mu <- weighted.mean(y, weights)
  n <- sum(weights)
  double.grid <- expand.grid(1:length(y), 1:length(y))
  ret <- weights[double.grid[, 1]] * weights[double.grid[, 2]] * 
    abs(y[double.grid[, 1]]-y[double.grid[, 2]])
  sum(ret) / (2 * n^2 * mu)
}
# From lecture notes



kuznets.20.40 <- function(y, weights) {
  y <- rep(y, times=weights)
  poor.40th <-quantile(y, 0.4)
  rich.20th <-quantile(y, 0.8)
    cat(sum(y[y > rich.20th]))
  sum(y[y > rich.20th]) / sum(y[y<=poor.40th]) 

}

library(e1071) 

#unbiased.CV <- function(y) {
#  N <- length(y)
#  biased.est <- ( sqrt((1/(N-1)) * sum((y-mean(y))^2)  ) )/ mean(y)
#  gamma_1 <- skewness(y)
#  
#  bias.measure <- (biased.est^(3/2)/N ) * 
#      (3*biased.est^(1/2) - 2*gamma_1)
#    
#  sqrt( biased.est - bias.measure )   
#}
# Ok, this is for the squre of the CV, so we dont need it 
# From "An almost unbiased estimator of the coefficient of variation" : http://www.sciencedirect.com/science/article/pii/S0165176500003517

coef.of.variation <- function(y, weights) {
  wt <- weights/sum(weights)
  ym <- weighted.mean(y, wt)
  sqrt( sum(wt * (y - ym)^2) ) / ym
}

coef.of.variation.2 <- function(y, weights) {
  mu <- weighted.mean(y, weights)
  n <- sum(weights)
  ret <- sum( (weights/n) * (y - mu)^2 )
  sqrt(ret)/mu
}
# From lecture notes

kuznets.20.40(settle.equiv.df$real.equiv.consumption, settle.equiv.df$hhwght)
weighted.mean(settle.equiv.df$real.equiv.consumption, settle.equiv.df$hhwght)
# kuznets.20.40(settle.equiv.df$real.equiv.consumption, 1)

# y <- settle.equiv.df$real.equiv.consumption

#system.time(gini.coef(settle.equiv.df$real.equiv.consumption, settle.equiv.df$hhwght) ) /
#system.time(gini.coef.2(settle.equiv.df$real.equiv.consumption, settle.equiv.df$hhwght))

gini.coef(settle.equiv.df$real.equiv.consumption, settle.equiv.df$hhwght) 

gini.coef(settle.equiv.df$real.equiv.consumption[1:1000], settle.equiv.df$hhwght[1:1000]) 
gini.coef.2(settle.equiv.df$real.equiv.consumption[1:1000], settle.equiv.df$hhwght[1:1000])

#unbiased.CV(settle.equiv.df$real.equiv.consumption)
coef.of.variation(settle.equiv.df$real.equiv.consumption, settle.equiv.df$hhwght)
coef.of.variation.2(settle.equiv.df$real.equiv.consumption, settle.equiv.df$hhwght)
# cv(settle.equiv.df$real.equiv.consumption)

system.time(
coef.of.variation(settle.equiv.df$real.equiv.consumption, settle.equiv.df$hhwght)
) / 
system.time(
coef.of.variation.2(settle.equiv.df$real.equiv.consumption, settle.equiv.df$hhwght)
)

boot.ineq <- function(ineq.fn, y, weight, replications) {
  bootstraps <- apply(matrix(
    sample(1:length(y), size=replications*length(y), replace=TRUE),nrow=replications),
    1, FUN=function(x) {ineq.fn(y[x], weight[x])}
  )
  sqrt(var(bootstraps))  
}

num.of.straps <- 1000

ineq.measures.df <- with(settle.equiv.df, {
data.frame(gini = c(gini.coef(real.equiv.consumption[urban==0], hhwght[urban==0]), 
    gini.coef(real.equiv.consumption[urban==1], hhwght[urban==1]),
    gini.coef(real.equiv.consumption, hhwght)),
  gini.se = c( 
      boot.ineq( gini.coef, real.equiv.consumption[urban==0], settle.equiv.df$hhwght[urban==0], num.of.straps),
      boot.ineq( gini.coef, real.equiv.consumption[urban==1], settle.equiv.df$hhwght[urban==1], num.of.straps),
      boot.ineq( gini.coef, real.equiv.consumption, settle.equiv.df$hhwght, num.of.straps)
     ),
  kuznets = c(kuznets.20.40(real.equiv.consumption[urban==0], hhwght[urban==0]), 
    kuznets.20.40(real.equiv.consumption[urban==1], hhwght[urban==1]),
    kuznets.20.40(real.equiv.consumption, hhwght)),
  kuznets.se = c( 
      boot.ineq( kuznets.20.40, real.equiv.consumption[urban==0], settle.equiv.df$hhwght[urban==0], num.of.straps),
      boot.ineq( kuznets.20.40, real.equiv.consumption[urban==1], settle.equiv.df$hhwght[urban==1], num.of.straps),
      boot.ineq( kuznets.20.40, real.equiv.consumption, settle.equiv.df$hhwght, num.of.straps)
     ),
  CV = c(coef.of.variation(real.equiv.consumption[urban==0], hhwght[urban==0]), 
    coef.of.variation(real.equiv.consumption[urban==1], hhwght[urban==1]),
    coef.of.variation(real.equiv.consumption, hhwght)),
  CV.se = c( 
      boot.ineq( coef.of.variation, real.equiv.consumption[urban==0], settle.equiv.df$hhwght[urban==0], num.of.straps),
      boot.ineq( coef.of.variation, real.equiv.consumption[urban==1], settle.equiv.df$hhwght[urban==1], num.of.straps),
      boot.ineq( coef.of.variation, real.equiv.consumption, settle.equiv.df$hhwght, num.of.straps)
     )
  )
  }
)

ineq.measures.df[, 2]/ ineq.measures.df[, 1]
ineq.measures.df[, 4]/ ineq.measures.df[, 3]
ineq.measures.df[, 6]/ ineq.measures.df[, 5]


ineq.measures.df$area <- c("Rural", "Urban", "Combined") 


ineq.measures.df$N <- c(sum(settle.equiv.df$urban==0),
  sum(settle.equiv.df$urban==1),
  nrow(settle.equiv.df)
)

ineq.measures.df







rural.lorenz <- with(settle.equiv.df, { 
  cumsum(sort(rep(real.equiv.consumption[urban==0], times=hhwght[urban==0]))) / 
  sum(rep(real.equiv.consumption[urban==0], times=hhwght[urban==0])) 
  }
)

urban.lorenz <- with(settle.equiv.df, { 
  cumsum(sort(rep(real.equiv.consumption[urban==1], times=hhwght[urban==1]))) / 
  sum(rep(real.equiv.consumption[urban==1], times=hhwght[urban==1])) 
  }
)




plot(x=0:1, y=0:1, type="l", ylim=0:1, xlim=0:1, xaxs = "i", yaxs = "i")

lines( (1:length(rural.lorenz)) / length(rural.lorenz),  y = rural.lorenz)

lines( (1:length(urban.lorenz)) / length(urban.lorenz),  y = urban.lorenz, lty=3, col="red")
# abline(h=0)
# abline(v=1)
# Maybe no way to deal with not having dotted lines


table(quantile( rural.lorenz, probs=seq(0, 1, .001) ) < quantile( urban.lorenz, probs=seq(0, 1, .001) ))

lorenz.which <- which(quantile( rural.lorenz, probs=seq(0, 1, .001) ) < quantile( urban.lorenz, probs=seq(0, 1, .001) ))

1 - quantile( rural.lorenz, probs=seq(0, 1, .001) )[lorenz.which]
1 - quantile( urban.lorenz, probs=seq(0, 1, .001) )[lorenz.which]

require(quantreg) 


test.rq <- rq(settle.equiv.df$real.equiv.consumption[settle.equiv.df$urban==1] ~ 1, tau = c(.5) , weights=settle.equiv.df$hhwght[settle.equiv.df$urban==1]/sum(settle.equiv.df$hhwght[settle.equiv.df$urban==1]))

summary(test.rq, se="boot", R=10000)

  bootmed <- apply(matrix(sample(settle.equiv.df$real.equiv.consumption[settle.equiv.df$urban==1],rep=TRUE,10^4*length(settle.equiv.df$real.equiv.consumption[settle.equiv.df$urban==1])),nrow=10^4),1,median)
  quantile(bootmed, c(.05,0.95))


  bootmed <- apply(matrix(sample(rural.lorenz,size= 10^5*length(x), replace=TRUE),nrow=10^5),1,median)
  quantile(bootmed, c(.05,0.95))











# settle.equiv.df <- lsms.df[!duplicated(lsms.df$codhh), ]

table(lsms.df$parent)
length(unique(lsms.df$codhh))
# These two quantitles above are equal, so just take the subset wit hthe 


settle.equiv.df$is.poor <- as.numeric(settle.equiv.df$real.equiv.consumption <= settle.equiv.df$povline )

# educ hhsize runwater drtfloor electr bathroom departmt region male age act parent

table(settle.equiv.df$departmt, settle.equiv.df$region)


settle.equiv.head.df <- settle.equiv.df[settle.equiv.df$parent %in% "head", ]


set.seed(100)

sample.split <- rep(FALSE, nrow(settle.equiv.head.df))

sample.split[sample(1:nrow(settle.equiv.head.df), size=round(nrow(settle.equiv.head.df)/2))] <- TRUE

svyglm() in the survey

# install.packages("survey")
library(survey)

summary(
poverty.probit <- svyglm(is.poor ~ educ + I(educ^2) + hhsize + runwater + drtfloor + electr + bathroom + region + male + age + I(age^2) + act, 
  design=svydesign(id=~1, weights=~hhwght, data=settle.equiv.head.df, subset=sample.split),
  family= quasibinomial(link = "probit")
    )
) # + region  departmt

settle.equiv.cross.v.df <- settle.equiv.head.df[!sample.split, ]

settle.equiv.cross.v.df <- settle.equiv.cross.v.df[complete.cases( settle.equiv.cross.v.df[, colnames(settle.equiv.cross.v.df) %in% attr(terms(poverty.probit$formula),"term.labels")]), ]


poverty.predict <- predict(poverty.probit, 
  newdata = settle.equiv.cross.v.df,
  type="response")

hist(poverty.predict)

prop.table(table(poverty.predict > .5))
prop.table(table(settle.equiv.cross.v.df$is.poor))
prop.table(table(poverty.predict > .5, settle.equiv.cross.v.df$is.poor))
prop.table(table(poverty.predict > .5, settle.equiv.cross.v.df$is.poor), margin=2)

prop.table(table(settle.equiv.cross.v.df$is.poor))

cutoff.seq <- seq(0, 1, by=.005)

#cutoff.seq <- cutoff.seq[c(-1, -length(cutoff.seq))]

classification.error.mat <- matrix(0, ncol=2, nrow=length(cutoff.seq) )

for ( i in 1:length(cutoff.seq)) {

  error.rate.table <- prop.table(table(
    rep(poverty.predict, times=settle.equiv.cross.v.df[, "hhwght"]) > cutoff.seq[i], 
    rep(settle.equiv.cross.v.df[, "is.poor"], times=settle.equiv.cross.v.df[, "hhwght"])
    ), margin=2)
  
  classification.error.mat[i , 1] <- error.rate.table[ 1, 2]
  if (nrow(error.rate.table)==1 & rownames(error.rate.table)=="TRUE") {
    classification.error.mat[i , 2] <-1
    classification.error.mat[i , 1] <-0
    next}
  
    if (nrow(error.rate.table)==1 & rownames(error.rate.table)=="FALSE") {
    classification.error.mat[i , 2] <-0
    classification.error.mat[i , 1] <-1
    next}
  
  
  classification.error.mat[i , 2] <- error.rate.table[ 2, 1]

}


plot(matrix(c(0,1,0,1), nrow=2), col="white")


lines(x=cutoff.seq, y=rowSums(classification.error.mat), col="green", lwd=2)
lines(x=cutoff.seq, y=classification.error.mat[, 1], col="red", lwd=2)
lines(x=cutoff.seq, y=classification.error.mat[, 2], col="blue", lwd=2)
cutoff.green.zone <- cutoff.seq[classification.error.mat[, 1] <= 0.1]

cutoff.green.zone[length(cutoff.green.zone)]

abline(v=cutoff.green.zone[length(cutoff.green.zone)], lty=2)

# TODO: need to divide by the total amount

























# TRASH CODE:




boot.gini.u <- with(settle.equiv.head.df, {
  apply(matrix(sample(1:sum(urban==1),rep=TRUE,10^4*sum(urban==1)),nrow=10^4),1,FUN=function(x) {
    gini.coef(real.equiv.consumption[urban==1][x], hhwght[urban==1][x])
  }
  )
} )

sqrt(var(boot.gini.u))
mean(boot.gini.u)

boot.gini.r <- with(settle.equiv.head.df, {
  apply(matrix(sample(1:sum(urban==0),rep=TRUE,10^4*length(sum(urban==0))),nrow=10^4),1,FUN=function(x) {
    gini.coef(real.equiv.consumption[urban==0][x], hhwght[urban==0][x])
  }
  )
} )



sqrt(var(boot.gini.r))








hist(bootmed)
mean(bootmed)

range(bootmed)
      
  quantile(bootmed, c(.05,0.95))


  
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



corrplot(cor(longley), method = "pie")

corrplot(equiv.mat, method = "pie", is.corr = FALSE, cl.lim = c(0, max(unlist(equiv.mat.ls))))


screen(1)

par(mfg=c(1,1))


## allocate figure 2 the intersection of column 2 and row 2


#layout(



# Cairo and cairoDevice packages is strongly recommended to produce high-quality PNG, JPEG, TIFF bitmap files, especially for that method circle, ellipse .

corrplot(equiv.mat.ls[[2]][nrow(equiv.mat.ls[[2]]):1, ], method = "pie", is.corr = FALSE, cl.lim = c(0, max(unlist(equiv.mat.ls))), add=TRUE, )

plot(1)


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

