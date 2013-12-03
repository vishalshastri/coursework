
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

  
midway <-floor(nrow(gdp.df)/2)
  


GDP_0<-gdp.df$GDP.real.per.cap[1]



ave.resid.growth <- mean( with(gdp.df, GDP.real.per.cap[-1]/GDP.real.per.cap[-length(GDP)]-1 ) )
  
beta.param <- mean(1/(1+gdp.df$Capital.income*1000000/(with( gdp.df, (Capital.stock)*(100/GDP.deflator)*1000000 ))), na.rm=TRUE)

depreciation.rate<- mean(gdp.df$I.real.per.cap/gdp.df$K.real.per.cap, na.rm=TRUE) - ave.resid.growth

L_bar <- with(gdp.df,  mean((Hours.per.worker*Employment/Population) * 52, na.rm=TRUE))

K_0 <- L_bar*((Z_init*alpha.param*beta.param/(1 + ave.resid.growth - beta.param*(1 - depreciation.rate)))^(1/(1 - alpha.param)));

# TODO: not sure if these formulas are right
X_0 = K_0*depreciation.rate
Y_0 = Z_init*K_0^alpha.param * L_bar^(1-alpha.param)
C_0 = Y_0 - X_0



#Z_init <- gdp.df$GDP.real.per.cap[1] /  (K_0^alpha.param * (L_bar)^(1-alpha.param))
  
Z_init <- gdp.df$GDP.real.per.cap[1] /
  (gdp.df$K.real.per.cap[1]^alpha.param * (L_bar)^(1-alpha.param))
  





ss.growth.rate<-(1+ave.resid.growth)^(0:years)


ss.growth.df<-data.frame(Y = Y_0*ss.growth.rate, 
  K = K_0*ss.growth.rate, 
  C = C_0*ss.growth.rate, 
  X = X_0*ss.growth.rate, 
  L = mean(gdp.df$Hours.per.worker)
)


pdf(file = paste0(work.dir, "K-C plots.pdf"))


par(mfcol=c(2,3))

var.names<-c("GDP.real.per.cap", "K.real.per.cap", "C.real.per.cap", "I.real.per.cap", "Hours.per.worker")

for ( i in 1:5) {

  plot(
gdp.df[, var.names][, i], type="l", main=paste("Figure", i, ":", var.names[i])
  )
  lines(ss.growth.df[,i], lty=2)
  
}


# 3


par(mfcol=c(1,1))


K_t <- K_0/10
C_init <-rep(0, 100000)
C_init[1] <- K_0^alpha.param / 2  #C_0/10
#switch.lever <-FALSE

# upper.c <- C_0*10
upper.c <- C_0*2
lower.c <- 0 

for (j in 1:100000) {

K_t <- K_0/10
C_t <- C_init[j]

C.K.path.df <- data.frame(K=rep(1, 151), C=rep(1, 151))


for ( i in 1:150) {



K_t_plus_1 <- ((1 - depreciation.rate)/(1 + ave.resid.growth))*K_t + (Z_init/(1 + ave.resid.growth))*
            (K_t^alpha.param)*(L_bar^(1 - alpha.param)) - C_t/(1 + ave.resid.growth)

C_t_plus_1 <- (beta.param/(1+ave.resid.growth)) * C_t * 
  ( 1 - depreciation.rate + 
    alpha.param * Z_init*K_t_plus_1^(alpha.param-1) * (  L_bar)^(1-alpha.param) )

  
C.K.path.df$C[i+1] <- C_t_plus_1
C.K.path.df$K[i+1] <- K_t_plus_1

C_t <- C_t_plus_1
K_t <- K_t_plus_1

}

 if (abs(C.K.path.df$K[151] - K_0) < 1 & is.finite(C.K.path.df$K[151]) ) { break }

if (

  sum(diff(C.K.path.df$C[is.finite(C.K.path.df$C)])<0)>2
  
 ) {
   C_init[j+1] <- (C_init[j] + upper.c)/2 
   lower.c <- C_init[j]   

} else {
  C_init[j+1] <- (C_init[j] + lower.c)/2   #min(C_init[C_init!=0]) )/2
  upper.c <- C_init[j]
}


cat("\n", C_init[j])

if (j>200) break

}

plot(C.K.path.df[-1,], type="b", main=expression(paste( plain("Figure 6: Capital and consumption, with "), K[0]==K^ss/10)))
C.K.path.df





# Q4

#tax<-c(rep(.4, 10), rep(.2, 500))
tax<-c( .4, .2)

tax.path.ls<-list()

for ( tax.i in 1:2) {


K_0 <- L_bar * (
  ((1-tax[tax.i])*Z_init*alpha.param*beta.param) /
    (1+ ave.resid.growth - beta.param * (1- depreciation.rate) )
   )^(1/(1-alpha.param))


K_t <- K_0/10
C_init <-rep(0, 100000)
C_init[1] <- K_0^alpha.param / 2  #C_0/10
#switch.lever <-FALSE

# upper.c <- C_0*10
upper.c <- C_0*2
lower.c <- 0 

for (j in 1:100000) {

K_t <- K_0/10
C_t <- C_init[j]

C.K.path.df <- data.frame(K=rep(1, 101), C=rep(1, 101))


for ( i in 1:100) {

K_t_plus_1 <- ((1 - depreciation.rate)/(1 + ave.resid.growth))*K_t + (Z_init/(1 + ave.resid.growth))*
            (K_t^alpha.param)*(L_bar^(1 - alpha.param)) - C_t/(1 + ave.resid.growth)

C_t_plus_1 <- (beta.param/(1+ave.resid.growth)) * C_t * 
  ( 1 - depreciation.rate + 
    (1 - tax[tax.i]) * alpha.param * Z_init*K_t_plus_1^(alpha.param-1) * (  L_bar)^(1-alpha.param) )
  
C.K.path.df$C[i+1] <- C_t_plus_1
C.K.path.df$K[i+1] <- K_t_plus_1

C_t <- C_t_plus_1
K_t <- K_t_plus_1

}

 if (abs(C.K.path.df$K[100] - K_0) < 1 & is.finite(C.K.path.df$K[100]) ) { break }

if ( sum(diff(C.K.path.df$C[is.finite(C.K.path.df$C)])<0)>2 ) {
   C_init[j+1] <- (C_init[j] + upper.c)/2 
   lower.c <- C_init[j]   
} else {
  C_init[j+1] <- (C_init[j] + lower.c)/2   #min(C_init[C_init!=0]) )/2
  upper.c <- C_init[j]
}


cat("\n", C_init[j])

if (j>200) break
}

plot(C.K.path.df[-1,])


tax.path.ls[[tax.i]] <- C.K.path.df

}

C.K.path.tax.not.anticipated.df <- rbind(tax.path.ls[[1]][2:10, ], tax.path.ls[[2]][11:nrow(tax.path.ls[[2]]), ])

#plot( C.K.path.tax.not.anticipated.df, type="b" )

plot(C.K.path.tax.not.anticipated.df, type="b", main=expression( paste(plain("Figure 8: K & C, with "), K[0]==K^ss/10,  plain(", "), tau[t]^k==0.4, plain(", "),  t<=9, plain(", "),  tau[t]^k==0.2, plain(", "),  t>=10)))



# Q5


tax<-c( .4, .2)


K_0 <- L_bar * (
  ((1-tax[2])*Z_init*alpha.param*beta.param) /
    (1+ ave.resid.growth - beta.param * (1- depreciation.rate) )
   )^(1/(1-alpha.param))


K_t <- K_0/10
C_init <-rep(0, 100000)
C_init[1] <- mean(c( 0,  Z_init*(K_t^alpha.param)*(L_bar^(1 - alpha.param)) - (ave.resid.growth + depreciation.rate)*K_t ) )
#switch.lever <-FALSE

# upper.c <- C_0*10
upper.c <- C_0*2
lower.c <- 0 

for (j in 1:100000) {

K_t <- K_0/10
C_t <- C_init[j]


anticipated.ls<-list()

C.K.path.df <- data.frame(K=rep(1, 11), C=rep(1, 11))

for ( i in 1:10) {

K_t_plus_1 <- ((1 - depreciation.rate)/(1 + ave.resid.growth))*K_t + (Z_init/(1 + ave.resid.growth))*
            (K_t^alpha.param)*(L_bar^(1 - alpha.param)) - C_t/(1 + ave.resid.growth)

C_t_plus_1 <- (beta.param/(1+ave.resid.growth)) * C_t * 
  ( 1 - depreciation.rate + 
    (1 - tax[1]) * alpha.param * Z_init*K_t_plus_1^(alpha.param-1) * (  L_bar)^(1-alpha.param) )
  
C.K.path.df$C[i+1] <- C_t_plus_1
C.K.path.df$K[i+1] <- K_t_plus_1

C_t <- C_t_plus_1
K_t <- K_t_plus_1

}


anticipated.ls[["high.tax.init"]] <- C.K.path.df


K_t <- anticipated.ls[["high.tax.init"]]$K[11]
C_t <- anticipated.ls[["high.tax.init"]]$C[11]

C.K.path.df <- data.frame(K=rep(1, 101), C=rep(1, 101))

for ( i in 1:100) {

K_t_plus_1 <- ((1 - depreciation.rate)/(1 + ave.resid.growth))*K_t + (Z_init/(1 + ave.resid.growth))*
            (K_t^alpha.param)*(L_bar^(1 - alpha.param)) - C_t/(1 + ave.resid.growth)

C_t_plus_1 <- (beta.param/(1+ave.resid.growth)) * C_t * 
  ( 1 - depreciation.rate + 
    (1 - tax[2]) * alpha.param * Z_init*K_t_plus_1^(alpha.param-1) * (  L_bar)^(1-alpha.param) )
  
C.K.path.df$C[i+1] <- C_t_plus_1
C.K.path.df$K[i+1] <- K_t_plus_1

C_t <- C_t_plus_1
K_t <- K_t_plus_1

}


anticipated.ls[["low.tax.init"]] <- C.K.path.df


 if (abs(C.K.path.df$K[100] - K_0) < 1 & is.finite(C.K.path.df$K[100]) ) { break }

if ( sum(diff(C.K.path.df$C[is.finite(C.K.path.df$C)])<0)>2 ) {
   C_init[j+1] <- (C_init[j] + upper.c)/2 
   lower.c <- C_init[j]   
} else {
  C_init[j+1] <- (C_init[j] + lower.c)/2   #min(C_init[C_init!=0]) )/2
  upper.c <- C_init[j]
}


cat("\n", C_init[j])

if (j>200) break
}


C.K.path.anticipated.df<-rbind(
anticipated.ls[["high.tax.init"]][-1,],
anticipated.ls[["low.tax.init"]][-1,] )


plot(C.K.path.anticipated.df, type="b", main=expression( paste(plain("Figure 9: K & C, with (anticipated) "), K[0]==K^ss/10,  plain(", "), tau[t]^k==0.4, plain(", "),  t<=9, plain(", "),  tau[t]^k==0.2, plain(", "),  t>=10)))


#plot(C.K.path.tax.not.anticipated.df, type="b")

#lines(C.K.path.anticipated.df, type="b", col="red", pch=as.character(1:nrow(C.K.path.anticipated.df)))

#plot(C.K.path.tax.not.anticipated.df$C)

#points(C.K.path.anticipated.df$C[1:100], col="red")

plot(C.K.path.anticipated.df$C[1:100]-C.K.path.tax.not.anticipated.df$C, main="Figure 10: Consumption in \"anticipated\" simulation less \nconsumption in unanticipated simulation", ylab="Consumption", xlab="Period")

dev.off()

sum(C.K.path.tax.not.anticipated.df$C[1:100])
sum(C.K.path.anticipated.df$C[1:100])


U_surprise = 0;
U_anticipated = 0;
for (i in 1:100) {
    U_surprise = U_surprise + (beta.param^(i - 1))*log(C.K.path.tax.not.anticipated.df$C[i]);
    U_anticipated = U_anticipated + (beta.param^(i - 1))*log(C.K.path.anticipated.df$C[i]);
}

U_surprise
U_anticipated 


