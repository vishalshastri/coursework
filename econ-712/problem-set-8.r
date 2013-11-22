
gdp.df <- read.csv(paste0(work.dir, "gdp data.csv"), stringsAsFactors=FALSE)

gdp.df$GDP.real.per.cap <- with( gdp.df, (GDP/Population)*(100/GDP.deflator)*1000000 )

gdp.df$K.real.per.cap <- with( gdp.df, (Capital.stock/Population)*(100/GDP.deflator)*1000000 )

gdp.df$I.real.per.cap <- with( gdp.df, (Investment/Population)*(100/GDP.deflator)*1000000 )

gdp.df$C.real.per.cap <- with( gdp.df, (Consumption/Population)*(100/GDP.deflator)*1000000 )

gdp.df<-gdp.df[gdp.df$Year>=1948, ]



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


#Z_init <- gdp.df$GDP.real.per.cap[1] /
#  c.d.production(t=1, Z=1, K_0=gdp.df$K.real.per.cap[1], L_0=gdp.df$Hours.per.worker[1], GDP_0=gdp.df$GDP.real.per.cap[1], a=alpha.param, g=ave.resid.growth, s=saving.rate, deprec=depreciation.rate)$GDP_0
  
  
midway <-floor(nrow(gdp.df)/2)
  


#K_0<-gdp.df$K.real.per.cap[floor(nrow(gdp.df)/2)]
#K_0<-gdp.df$K.real.per.cap[1]


#beta.param <- mean(1/(1+gdp.df$Capital.income/gdp.df$Capital.stock), na.rm=TRUE)
beta.param <- mean(1/(1+gdp.df$Capital.income/gdp.df$K.real.per.cap), na.rm=TRUE)

#K_0 = (alpha.param*beta.param/(1 - beta.param*(1 - depreciation.rate)))^(1/(1 - alpha.param));
#C_tilde = (K_tilde^alpha) - delta*K_tilde;


GDP_0<-gdp.df$GDP.real.per.cap[1]


# STOP

#gdp.sim<-list(data.frame(GDP_0=GDP_0, K_0=K_0, consump = 0, invest = 0, r = 0, w = 0))

#for (i in 1:years) {

#  prod.result <- c.d.production(t=i, Z=Z_init, K_0=K_0, L_0=gdp.df$Hours.per.worker[1], GDP_0=GDP_0, a=alpha.param, g=ave.resid.growth, s=saving.rate, deprec=depreciation.rate)

#  K_0<-prod.result$K_0
#  GDP_0<-prod.result$GDP_0
#  gdp.sim[[i]] <- prod.result

#}

#K_ss <- K_0


# Q2. We are at K steady state


#gdp.sim<-list(data.frame(GDP_0=GDP_0, K_0=K_0, consump = 0, invest = 0, r = 0, w = 0))

#for (i in 1:years) {

#  prod.result <- c.d.production(t=i, Z=Z_init, K_0=K_0, L_0=gdp.df$Hours.per.worker[1], GDP_0=GDP_0, a=alpha.param, g=ave.resid.growth, s=saving.rate, deprec=depreciation.rate)

#  K_0<-prod.result$K_0
#  GDP_0<-prod.result$GDP_0
#  gdp.sim[[i]] <- prod.result

#}

#gdp.sim.ss<-list(data.frame(Y = 0, K = 0, C = 0, X = 0, L = 0)[-1,])

# START

X_0 = K_0*(1+ave.resid.growth)-K_0*(1-depreciation.rate)
Y_0 = Z_init*K_0^alpha.param * gdp.df$Hours.per.worker[1]^(1-alpha.param)
C_0 = Y_0 - X_0
L_bar <- with(gdp.df,  mean((Hours.per.worker*Employment/Population) * 52, na.rm=TRUE))


Z_init <- gdp.df$GDP.real.per.cap[1] /
  (K_0^alpha.param * (L_bar)^(1-alpha.param))

K_0 <- L_bar*((Z_init*alpha.param*beta.param/(1 + ave.resid.growth - beta.param*(1 - depreciation.rate)))^(1/(1 - alpha.param)));



ss.growth.rate<-(1+ave.resid.growth)^(0:years)


data.frame(Y = Y_0*ss.growth.rate, 
  K = K_0*ss.growth.rate, 
  C = C_0*ss.growth.rate, 
  X = X_0*ss.growth.rate, 
  L = L_bar)

# TODO: compare with real US economy



# 3

# TODO: my Z could be wrong

#beta.param <- mean(1/(1+gdp.df$Capital.income/gdp.df$K.real.per.cap), na.rm=TRUE)






K_t <- K_0/10
C_init <-rep(0, 100000)
C_init[1] <- K_0^alpha.param / 2  #C_0/10
#switch.lever <-FALSE

# upper.c <- C_0*10
 upper.c <- C_0*500
lower.c <- 0 

for (j in 1:100000) {

K_t <- K_0/10
C_t <- C_init[j]

C.K.path.df <- data.frame(K=rep(1, 501), C=rep(1, 501))


for ( i in 1:500) {

#K_t_plus_1 <- (1 - depreciation.rate)* K_t +
#  Z_init*K_t^alpha.param * (  L_bar)^(1-alpha.param) - C_t

K_t_plus_1 <- ((1 - depreciation.rate)/(1 + ave.resid.growth))*K_t + (Z.init/(1 + ave.resid.growth))*...
            (K_t^alpha.param)*(L_bar^(1 - alpha.param)) - C_t/(1 + ave.resid.growth)

#        capital(j+1,1) = ((1 - delta)/(1 + g))*capital(j,1) + (Z/(1 + g))*...
#            (capital(j,1)^alpha)*(L_bar^(1 - alpha)) - consumption(j,1)/(1 + g)
  

C_t_plus_1 <- (beta.param/(1+ave.resid.growth)) * C_t * 
  ( 1 - depreciation.rate + 
    alpha.param * Z_init*K_t_plus_1^(alpha.param-1) * (  L_bar)^(1-alpha.param) )
    # (1+ave.resid.growth)^i *


  # (1+ave.resid.growth)^i *
  
C.K.path.df$C[i+1] <- C_t_plus_1
C.K.path.df$K[i+1] <- K_t_plus_1

C_t <- C_t_plus_1
K_t <- K_t_plus_1

}

 if (abs(C.K.path.df$K[501] - K_0) < 1000 & is.finite(C.K.path.df$K[501]) ) { break }

if (

  sum(diff(C.K.path.df$C[is.finite(C.K.path.df$C)])<0)>2
  
 ) {
   C_init[j+1] <- (C_init[j] + upper.c)/2 
   lower.c <- C_init[j]   
  # K_0^alpha.param
  # K_0
} else {
  C_init[j+1] <- (C_init[j] + lower.c)/2   #min(C_init[C_init!=0]) )/2
  upper.c <- C_init[j]
}


cat("\n", C_init[j])

if (j>200) break

#if (C.K.path.df$K[501]/C.K.path.df$C[501]< K_0/C_0 |
#  (C.K.path.df$C[501]==0 | !is.finite(C.K.path.df$C[501])) &
#  sum(diff(C.K.path.df$C[is.finite(C.K.path.df$C)])<0)>1
# ) {
#  C_init[j+1] <- (C_init[j] + min(C_init[C_init!=0]) )/2
#}


}

plot(C.K.path.df)
C.K.path.df






# C.K.path.df$K[501]/C.K.path.df$C[501]> K_0/C_0 |


# Must remembers 

# C.K.path.df$C[501]==0 & 
  min(C.K.path.df$K[C.K.path.df$K>0]) < min(C.K.path.df$C[C.K.path.df$C>0])
  
    C.K.path.df$C[501]==0 & 
  min(C.K.path.df$K[C.K.path.df$K!=0]) > min(C.K.path.df$C[C.K.path.df$C!=0])



  
C_t_plus_1
  


























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






alpha = 1 - mean(W/Y);
beta = 1/(1 + mean(R/(K/(P/100))));
g = mean((Y[2:56,1]/(P[2:56,1]*N[2:56,1]))/(Y[1:55,1]/(P[1:55,1]*N[1:55,1]))) - 1;
delta = mean(X/K) - g;
L_bar = mean((H*E)/N)*50;
Z = (Y[1,1]*100000000/(P[1,1]*N[1,1]))/(((K[1,1]*100000000/(P[1,1]*N[1,1]))^alpha)*
    (L_bar)^(1 - alpha));


%--------------------------------------------------------------------------
% b. Compute the balanced growth path
%--------------------------------------------------------------------------

%compute steady-state values of K-tilde and C-tilde
K_tilde = L_bar*((Z*alpha*beta/(1 + g - beta*(1 - delta)))^(1/(1 - alpha)));
C_tilde = Z*(K_tilde^alpha)*(L_bar^(1 - alpha)) - (g + delta)*K_tilde;












