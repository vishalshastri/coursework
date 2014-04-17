

library("foreign")

library("stargazer")

# QUESTION 1.1

mex.df <- read.dta("/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/hw3data.dta")

mex.df$regionNW <- ifelse(mex.df$region==1, 1, 0)
mex.df$regionCN <- ifelse(mex.df$region==2, 1, 0)
mex.df$regionCS <- ifelse(mex.df$region==3, 1, 0)
mex.df$regionS  <- ifelse(mex.df$region==4, 1, 0)

mex.df$region.factor <- factor(mex.df$region , levels=1:4, labels=c("NW", "CN", "CS", "S"))


unprocessed.summary.table <- by(mex.df[, c("area", "pctpd0306", "dcarr", "avgelev", "avgslope", "ejido", "regionNW", "regionCN", "regionCS", "regionS", "psemidecid", "pselva")], INDICES=mex.df$treat1, 
  FUN=function(x) {
    cbind(
      sapply(x, FUN=mean),
      sapply(x, FUN=sd),
      sapply(x, FUN=min),
      sapply(x, FUN=max)
      )
  } 
)

# NOTE: Ok, in this version, there is no median

processed.summary.table <- cbind(
  unprocessed.summary.table[[1]][, 1],
  unprocessed.summary.table[[2]][, 1],
  unprocessed.summary.table[[1]][, 2],
  unprocessed.summary.table[[2]][, 2],
  unprocessed.summary.table[[1]][, 3],
  unprocessed.summary.table[[2]][, 3],
  unprocessed.summary.table[[1]][, 4],
  unprocessed.summary.table[[2]][, 4]
)

 processed.summary.table<- cbind(rownames(processed.summary.table), as.data.frame(processed.summary.table))

# TODO: Touch this up with column headers, rotate table, and maybe smaller font:
stargazer(processed.summary.table , summary=FALSE)


# QUESTION 1.2

t.stat.check <- sapply(mex.df[, c("area", "pctpd0306", "dcarr", "avgelev", "avgslope", "ejido", "regionNW", "regionCN", "regionCS", "regionS", "psemidecid", "pselva")],  FUN=function(x) {
  ret <- t.test(x[mex.df$treat1==0], x[mex.df$treat1==1])
  c(mean(x[mex.df$treat1==0]), mean(x[mex.df$treat1==1]), ret$statistic, ret$p.value)
  }
)

stargazer(cbind(colnames(t.stat.check), as.data.frame(t(t.stat.check ))) , summary=FALSE)


# QUESTION 1.3

pdf(file="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/kernelDensity1.pdf", width=4, height=4)

density.1 <- density(mex.df$avgelev[mex.df$treat1==0])
density.2 <- density(mex.df$avgelev[mex.df$treat1==1])

plot(density.1, main="Kernel density of average elevation", xlab="",
  xlim=c(min(c(density.1$x, density.2$x)), max(c(density.1$x, density.2$x))), 
  ylim=c(min(c(density.1$y, density.2$y)), max(c(density.1$y, density.2$y))),
  lwd=2
  )

lines(density.2, lty=2, lwd=2)

legend("topleft", legend=c("Control", "Treatment"), cex=.7, col=c("black"), lty=c(1, 2), lwd=2)

dev.off()




pdf(file="/Users/travismcarthur/Desktop/Dev - AAE 642/Problem sets/PS 3/kernelDensity2.pdf", width=4, height=4)

density.1 <- density(mex.df$avgslope[mex.df$treat1==0])
density.2 <- density(mex.df$avgslope[mex.df$treat1==1])

plot(density.1, main="Kernel density of average slope", xlab="",
  xlim=c(min(c(density.1$x, density.2$x)), max(c(density.1$x, density.2$x))), 
  ylim=c(min(c(density.1$y, density.2$y)), max(c(density.1$y, density.2$y))),
  lwd=2
  )

lines(density.2, lty=2, lwd=2)

legend("topright", legend=c("Control", "Treatment"), cex=.7, col=c("black"), lty=c(1, 2), lwd=2)

dev.off()


# QUESTION 2.1

library(censReg)

uncontrolled.tobit <- censReg(pctpd0306 ~ treat1, data=mex.df)

summary(margEff(uncontrolled.tobit))

controlled.tobit <- censReg(pctpd0306 ~ treat1 + area + dcarr + avgelev + ejido + 
    region.factor + psemidecid + pselva, data=mex.df)

summary(margEff(controlled.tobit))

# Ok, I'm not sure which marginal effects I get, so will have to figure this out later {reference http://www.stata.com/manuals13/rtobitpostestimation.pdf }


# QUESTION 2.3

region.het.tobit <- censReg(pctpd0306 ~ treat1*, data=mex.df)




