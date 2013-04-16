
hprice.df<-read.table("http://www.ssc.wisc.edu/~bhansen/710/hprice1.dat", sep="")

colnames(hprice.df)<-c("price", "assessed.val", "no.bedrooms", "lot.size", "house.size", "colonial", "ln.price", "ln.assessed.val","ln.lot.size", "ln.sqrft")

B.times <- 10000

hprice.lm <- lm(price ~ no.bedrooms + lot.size + house.size + colonial, data=hprice.df)

Tn.boot.ls<-vector(mode="list", length=B.times)

for (i in 1:B.times ){
	
	hprice.df.boot <- hprice.df[sample(nrow(hprice.df), nrow(hprice.df) , replace=TRUE), ]
	
	hprice.lm.boot <- lm(price ~ no.bedrooms + lot.size + house.size + colonial, data=hprice.df.boot)
	
	Tn.boot.ls[[i]] <- abs( coef(hprice.lm.boot) - coef(hprice.lm)) / 
		sqrt(diag(vcov(hprice.lm.boot)))
	
}
	
Tn<-do.call(rbind, Tn.boot.ls)

qn <-apply(Tn, MARGIN=2, FUN=quantile, probs=.95)

hprice.sterr<-summary(hprice.lm)$coefficients[,"Std. Error"] 

boot.ci.lower <- coef(hprice.lm) - hprice.sterr*qn
boot.ci.upper <- coef(hprice.lm) + hprice.sterr*qn

format(confint(hprice.lm),digits=2, scientific=FALSE)
format(cbind(boot.ci.lower, boot.ci.upper),digits=2, scientific=FALSE)















confint.white(hprice.lm, hccm(hprice.lm))


confint.white <- function (object, vcov, parm, level = 0.95, ...)  {
	cf <- coef(object)
	pnames <- names(cf)
	if (missing(parm)) 
		parm <- pnames
	else if (is.numeric(parm)) 
		parm <- pnames[parm]
	a <- (1 - level)/2
	a <- c(a, 1 - a)
	pct <- a
	fac <- qnorm(a)
	ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm, 
																														 pct))
	ses <- sqrt(diag(vcov))[parm]
	ci[] <- cf[parm] + ses %o% fac
	ci
}






###### END










coef.boot.sterr <-apply(coef.boot.mat, MARGIN=2, FUN=sd) / sqrt(B.times)

repmat <- function(a,n,m) {kronecker(matrix(1,n,m),a)}

Tn=abs(coef.boot.mat-repmat(t(coef(hprice.lm)),B.times,1)) / 
	repmat(t(coef.boot.sterr),B.times,1)





hprice.sterr*1.96
	
summary(hprice.lm)$coefficients[,"Std. Error"]




1. price, in dollars 
2. assessed value, in dollars 
3. number of bedrooms 
4. size of lot, square feet 
5. size of house, square feet 
6. dummy =1 if home is colonial style 
7. log(price) 
8. log(assessed value) 
9. log(lotsize) 
10. log(sqrft)





