
# Travis McArthur
# 718 Problem Set 1


# QUESTION 1.a

# install.packages("stargazer")
library("stargazer")


summary( fert.big.lm <- lm(log(fert.exp +1) ~ indig.prop + drive.time.urban + T.CASO4 + received.credit, data=crop.wide.df))


stargazer(fert.big.lm, out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 1/table1.tex", no.space=TRUE, single.row=TRUE, keep.stat=c("n", "adj.rsq"))
  


# QUESTION 1.b

summary(partitioned.1.lm <- lm( log(fert.exp +1) ~ T.CASO4 + received.credit, data=crop.wide.df))

X.1 <- as.matrix(crop.wide.df[, c("indig.prop", "drive.time.urban")])

X.2 <- as.matrix(crop.wide.df[, c("T.CASO4",  "received.credit")])

X.2 <- cbind(1, X.2)


M.2 <- diag(nrow(X.2)) - X.2 %*% solve(t(X.2) %*% X.2) %*% t(X.2)


summary(partitioned.3.lm <- lm( resid(partitioned.1.lm) ~ I(M.2 %*% X.1) - 1 ))

stargazer(partitioned.3.lm, out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 1/table2.tex", no.space=TRUE, single.row=TRUE, keep.stat=c("n", "adj.rsq"))

# QUESTION 1.c

summary(partitioned.4.lm <- lm( log(fert.exp +1) ~ I(M.2 %*% X.1) - 1 , data=crop.wide.df))

stargazer(partitioned.4.lm, out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 1/table3.tex", no.space=TRUE, single.row=TRUE, keep.stat=c("n", "adj.rsq"))



# QUESTION 2.a


# install.packages("AER")
library("AER")

crop.wide.df$remittances <- crop.wide.df$REMPAIS + crop.wide.df$REMEXT


summary(fert.ivreg <- ivreg( log(fert.exp+1) ~ log(remittances+1) + log(total.area) | hhh.sex + log(total.area), data=crop.wide.df))


stargazer(fert.ivreg, out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 1/table4.tex", no.space=TRUE, single.row=TRUE, keep.stat=c("n", "adj.rsq"))


# QUESTION 2.b

summary( first.stage.lm <- lm(log(remittances+1) ~ hhh.sex + log(total.area), data=crop.wide.df))
  
summary( second.stage.lm <- lm( log(fert.exp+1) ~ I(predict(first.stage.lm)) + log(total.area), data=crop.wide.df))



stargazer(first.stage.lm, out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 1/table5.tex", no.space=TRUE, single.row=TRUE, keep.stat=c("n", "adj.rsq"))

stargazer(second.stage.lm, out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 1/table6.tex", no.space=TRUE, single.row=TRUE, keep.stat=c("n", "adj.rsq"))


# QUESTION 2.c
  

summary( red.form.1.lm <- lm(log(fert.exp+1) ~  hhh.sex + log(total.area), data=crop.wide.df))

summary( red.form.2.lm <- lm( log(remittances+1) ~ hhh.sex + log(total.area), data=crop.wide.df))

stargazer(red.form.1.lm, out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 1/table6b.tex", no.space=TRUE, single.row=TRUE, keep.stat=c("n", "adj.rsq"))

stargazer(red.form.2.lm, out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 1/table6c.tex", no.space=TRUE, single.row=TRUE, keep.stat=c("n", "adj.rsq"))


coef(red.form.1.lm)["hhh.sexmujer"] / coef(red.form.2.lm)["hhh.sexmujer"]

coef(fert.ivreg)



# Question 2.d
  
cov.Y.lm<- lm( log(fert.exp+1) ~  log(total.area), data=crop.wide.df)
  
cov.T.lm<- lm( log(remittances+1) ~  log(total.area) , data=crop.wide.df)

cov.Z.lm<- lm( as.numeric(hhh.sex) ~  log(total.area), data=crop.wide.df)

cov(resid(cov.Z.lm), resid(cov.Y.lm)) /
  cov(resid(cov.Z.lm), resid(cov.T.lm))



# QUESTION 3.a
  
num.obs <- 10000
  
set.seed(100)
  
T.i <- rgamma(num.obs, shape=3)

beta.0 <- -32
beta.1 <- 8.7
  
u.i <- rnorm(num.obs, sd=20)
  
Y.i <- beta.0 + beta.1 * T.i + u.i 
  
tau.1.i <- T.i + rnorm(num.obs, sd=5)
  
tau.2.i <- T.i + rnorm(num.obs, sd=10)

summary(Q.a.lm <-  lm(Y.i ~ T.i))

stargazer(Q.a.lm, out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 1/table7.tex", no.space=TRUE, single.row=TRUE, keep.stat=c("n", "adj.rsq"))


# QUESTION 3.b

summary(Q.b.lm <-  lm(Y.i ~ tau.1.i))
  
beta.1 * var(T.i) / (var(T.i) + 5^2)

beta.1 * 3 / (3 + 5^2)

stargazer(Q.b.lm, out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 1/table8.tex", no.space=TRUE, single.row=TRUE, keep.stat=c("n", "adj.rsq"))


# QUESTION 3.c 

summary( meas.err.ivreg  <- ivreg(Y.i ~ tau.1.i |  tau.2.i))

stargazer(meas.err.ivreg, out.header = FALSE, 
  out="/Users/travismcarthur/Desktop/Econ 718 Metrics/problem sets/PS 1/table9.tex", no.space=TRUE, single.row=TRUE, keep.stat=c("n"))



