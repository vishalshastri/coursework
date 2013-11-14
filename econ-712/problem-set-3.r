
gdp.df <- read.csv(paste0(work.dir, "gdp data.csv"), stringsAsFactors=FALSE)

gdp.df$GDP.real.per.cap <- with( gdp.df, (GDP/Population)*(100/GDP.deflator)*1000000 )

gdp.df$K.real.per.cap <- with( gdp.df, (Capital.stock/Population)*(100/GDP.deflator)*1000000 )

gdp.df$I.real.per.cap <- with( gdp.df, (Investment/Population)*(100/GDP.deflator)*1000000 )

gdp.df$C.real.per.cap <- with( gdp.df, (Consumption/Population)*(100/GDP.deflator)*1000000 )

gdp.df<-gdp.df[gdp.df$Year>=1948, ]

pdf(file = paste0(work.dir, "gdp plots.pdf"))

par(mfrow=c(2,2))

# plot 1
plot(gdp.df$Year, gdp.df$GDP.real.per.cap, type="l", log="y", lwd=2, main="Fig. 1: Real per capita GDP (2009 dollars)", xlab="", ylab="", cex.main=.8)

# plot 2
plot(gdp.df$Year, gdp.df$Hours.per.worker, type="l", lwd=2, main="Fig. 2: Average hours per week", xlab="", ylab="", cex.main=.8)

# plot 3
plot(gdp.df$Year, gdp.df$K.real.per.cap, type="l", lwd=2, main="Fig. 3: Real capital stock per capita (2009 dollars)", xlab="", ylab="", cex.main=.8)

# plot 4
plot(gdp.df$Year, gdp.df$K.real.per.cap/gdp.df$GDP.real.per.cap, type="l", lwd=2, main="Fig. 4: Capital/output ratio", xlab="", ylab="", cex.main=.8)

par(mfrow=c(2,2))

# plot 5
plot(gdp.df$Year, gdp.df$C.real.per.cap/gdp.df$GDP.real.per.cap, type="l", ylim=c(0,1), lwd=2, main="Fig. 5: Expenditure shares of GDP", xlab="", ylab="", cex.main=.8)
lines(gdp.df$Year, gdp.df$I.real.per.cap/gdp.df$GDP.real.per.cap, type='l', col=2, lty=3, lwd=2)
legend("topright", legend = c("C/Y", "I/Y"), col=1:2, lty=c(1,3), lwd=2) 

# plot 6
plot(gdp.df$Year, gdp.df$Labor.income/gdp.df$GDP, type="l", ylim=c(0,1), lwd=2, main="Fig. 6: Income shares of GDP", xlab="", ylab="", cex.main=.8)
lines(gdp.df$Year, gdp.df$Capital.income/gdp.df$GDP, type='l', col=2, lty=3, lwd=2)
legend("topright", legend = c("labor", "capital"), col=1:2, lty=c(1,3), lwd=2) 

# plot 7
plot(gdp.df$Year[-1], diff(gdp.df$GDP.real.per.cap)/gdp.df$GDP.real.per.cap[-nrow(gdp.df)], type="l", lwd=2, main="Fig. 7: Growth rate of GDP", xlab="", ylab="", cex.main=.8)

# plot 8
plot(gdp.df$Year[-1], diff(gdp.df$GDP.real.per.cap)/gdp.df$GDP.real.per.cap[-nrow(gdp.df)], type="l", lwd=2, main="Fig. 8: Growth rate of GDP and components", xlab="", ylab="", cex.main=.8)
lines(gdp.df$Year[-1], diff(gdp.df$C.real.per.cap)/gdp.df$C.real.per.cap[-nrow(gdp.df)], type='l', col=2, lty=3, lwd=2)
legend("topright", legend = c("Y", "C"), col=1:2, lty=c(1,3), lwd=2) 

par(mfrow=c(2,1))

# plot 9
plot(gdp.df$Year[-1], diff(gdp.df$GDP.real.per.cap)/gdp.df$GDP.real.per.cap[-nrow(gdp.df)], type="l", lwd=2, main="Fig. 9: Growth rate of GDP and components", xlab="", ylab="", cex.main=.8)
lines(gdp.df$Year[-1], diff(gdp.df$C.real.per.cap)/gdp.df$C.real.per.cap[-nrow(gdp.df)], type='l', col=2, lty=3, lwd=2)
lines(gdp.df$Year[-1], diff(gdp.df$I.real.per.cap)/gdp.df$I.real.per.cap[-nrow(gdp.df)], col=3, lty=1, lwd=2, type = "b", pch = 1)
legend("topright", legend = c("Y", "C", "I"), col=1:3, lty=c(1,3,1), pch = c(NA, NA, 1), merge = TRUE, lwd=2, bg="white")

# plot 10
plot(gdp.df$Year[-1], diff(gdp.df$I.real.per.cap)/gdp.df$I.real.per.cap[-nrow(gdp.df)], col=3, lty=1, lwd=2, type = "b", pch = 1, main="Fig. 10: Growth rate of GDP and components", xlab="", ylab="", cex.main=.8)
lines(gdp.df$Year[-1], diff(gdp.df$C.real.per.cap)/gdp.df$C.real.per.cap[-nrow(gdp.df)], type='l', col=2, lty=3, lwd=2)
lines(gdp.df$Year[-1], diff(gdp.df$GDP.real.per.cap)/gdp.df$GDP.real.per.cap[-nrow(gdp.df)], type="l", lwd=2)
legend("topright", legend = c("Y", "C", "I"), col=1:3, lty=c(1,3,1), pch = c(NA, NA, 1), merge = TRUE, lwd=2, bg="white")

dev.off()



# NOW, GROWTH ACCOUNTING:

g.accounting.df<-data.frame(
  Year=gdp.df$Year[-1],
  gdp.growth=diff(gdp.df$GDP.real.per.cap)/gdp.df$GDP.real.per.cap[-nrow(gdp.df)],
  k.income.share=(gdp.df$Capital.income/gdp.df$GDP)[-nrow(gdp.df)],
  k.stock.growth=diff(gdp.df$K.real.per.cap)/gdp.df$K.real.per.cap[-nrow(gdp.df)],
  l.income.share=(gdp.df$Labor.income/gdp.df$GDP)[-nrow(gdp.df)],
  l.growth=diff(gdp.df$Hours.per.worker)/gdp.df$Hours.per.worker[-nrow(gdp.df)]
  )

g.accounting.df$residual<-with(g.accounting.df,
 ( gdp.growth - (k.income.share * k.stock.growth + l.income.share * l.growth) ) / (1-k.income.share)
)

g.accounting.df$residual.index<-1

for ( i in 2:nrow(g.accounting.df) ) {
  g.accounting.df$residual.index[i]<-g.accounting.df$residual.index[i-1] * (1+g.accounting.df$residual[i])
}

years<-length(g.accounting.df$residual.index[!is.na(g.accounting.df$residual.index)])
g.accounting.df$residual.index[years]

ave.resid.growth<-exp(log(g.accounting.df$residual.index[years])/years) - 1

alpha.param <- mean(g.accounting.df$k.income.share, na.rm=TRUE)

saving.rate<-mean(gdp.df$Investment/gdp.df$GDP)


#plot(g.accounting.df$Year, g.accounting.df$residual.index, type="l")

c.d.production <- function(t, Z, K_0, L_0, GDP_0, a, g, s, deprec) {

  K_1 <- (1-deprec) * K_0 + s * GDP_0
  
  GDP_1 <- Z * K_1^a * ((1+g)^t * L_0)^(1-a)
  
  data.frame( GDP_0=GDP_1, K_0=K_1, consump = (1-s)*GDP_1, invest = s*GDP_1, r = a*GDP_1/K_1, w = (1-a)*GDP_1/L_0)
  
}


depreciation.rate<- mean(
-(gdp.df$K.real.per.cap[-1] - saving.rate*gdp.df$GDP.real.per.cap[-nrow(gdp.df)])/gdp.df$K.real.per.cap[-nrow(gdp.df)] + 1, na.rm=TRUE)


Z_init <- gdp.df$GDP.real.per.cap[1] /
  c.d.production(t=1, Z=1, K_0=gdp.df$K.real.per.cap[1], L_0=gdp.df$Hours.per.worker[1], GDP_0=gdp.df$GDP.real.per.cap[1], a=alpha.param, g=ave.resid.growth, s=saving.rate, deprec=depreciation.rate)$GDP_0


K_0<-gdp.df$K.real.per.cap[1]
GDP_0<-gdp.df$GDP.real.per.cap[1]

gdp.sim<-list(data.frame(GDP_0=GDP_0, K_0=K_0, consump = 0, invest = 0, r = 0, w = 0))

for (i in 1:years) {

  prod.result <- c.d.production(t=i, Z=Z_init, K_0=K_0, L_0=gdp.df$Hours.per.worker[1], GDP_0=GDP_0, a=alpha.param, g=ave.resid.growth, s=saving.rate, deprec=depreciation.rate)

  K_0<-prod.result$K_0
  GDP_0<-prod.result$GDP_0
  gdp.sim[[i]] <- prod.result

}

gdp.sim.df<-do.call(rbind, gdp.sim) 
gdp.sim.df$Year<-gdp.df$Year[-c(1, nrow(gdp.df))]

pdf(file = paste0(work.dir, "gdp sim plots.pdf"))

plot(gdp.df$Year[1]:(length(gdp.df$GDP.real.per.cap)+gdp.df$Year[1]-1), gdp.df$GDP.real.per.cap, type="l", lwd=2, main="Fig. 11: Simulated GDP vs. actual GDP", xlab="", ylab="")
lines(gdp.df$Year[1]:(length(gdp.sim)+gdp.df$Year[1]-1), gdp.sim.df$GDP_0, type='l', col=2, lty=3, lwd=2)


par(mfrow=c(2,1))

plot(gdp.sim.df$Year[-1], diff(gdp.sim.df$GDP_0)/gdp.sim.df$GDP_0[-nrow(gdp.sim.df)], type="l", lwd=2, main="Fig. 12: Growth rate of GDP", xlab="", ylab="", cex.main=.8)

plot(gdp.sim.df$Year, gdp.sim.df$K_0/gdp.sim.df$GDP_0, type="l", lwd=2, main="Fig. 13: Capital/output ratio", xlab="", ylab="", cex.main=.8)


dev.off()

g.accounting.sim.df<-data.frame(
  Year=gdp.sim.df$Year[-1],
  gdp.growth=diff(gdp.sim.df$GDP_0)/gdp.sim.df$GDP_0[-nrow(gdp.sim.df)],
  k.income.share=alpha.param,
  k.stock.growth=diff(gdp.sim.df$K_0)/gdp.sim.df$K_0[-nrow(gdp.sim.df)],
  l.income.share=1-alpha.param,
  l.growth=0
  )


colMeans(g.accounting.df, na.rm=TRUE)
colMeans(g.accounting.sim.df, na.rm=TRUE)


