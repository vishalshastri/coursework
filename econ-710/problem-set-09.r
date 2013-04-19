card.df<-read.table("http://www.ssc.wisc.edu/~bhansen/710/card.dat", sep="")

colnames(card.df) <- c("id", "nearc2", "nearc4", "educ", "age", "fathereduc", "mothereduc", "weight", "momdad14", "sinmom14", "step14", "reg661", "reg662", "reg663", "reg664", "reg665", "reg666", "reg667", "reg668", "reg669", "south66", "black", "smsa", "south", "smsa66", "wage", "enroll", "married", "exper")

card.lm<- lm( I(log(wage)) ~ educ + exper + I(exper^2) + south + black , data=card.df)

summary(card.lm)

install.packages("systemfit")
library("systemfit")

card.2sls <- systemfit( I(log(wage)) ~ educ + exper + I(exper^2) + south + black,
					 method = "2SLS", 
												inst= ~ exper + I(exper^2) + south + black + nearc4, data=card.df)

summary(card.2sls )


card.2sls.extended <- systemfit( I(log(wage)) ~ educ + exper + I(exper^2) + south + black,
												method = "2SLS", 
												inst= ~ exper + I(exper^2) + south + black + nearc4 + nearc2 + fathereduc, data=card.df)

summary(card.2sls.extended )

inst.vars <- with(card.df, 
									data.frame(exper, exper^2, south, black, nearc4, nearc2, fathereduc)
)

weight.mat<- var(inst.vars - card.2sls$eq[[1]]$residuals)


install.packages("gmm")
library("gmm")

card.gmm <- gmm(I(log(wage)) ~ educ + exper + I(exper^2) + south + black, 
						~ exper + I(exper^2) + south + black + nearc4 + nearc2 + fathereduc, 
						data=card.df,
						type="twoStep", wmatrix="optimal")

specTest(card.gmm)





### DELETE BELOW




systemfit( ,
					 method = "2SLS", 
					 inst= ~ exper + I(exper^2) + south + black + nearc4 + nearc2 + fathereduc, data=card.df)



var()



install.packages("sem")
library("sem")

card.2sls <- tsls(I(log(wage)) ~ educ + exper + I(exper^2) + south + black,
									~ exper + I(exper^2) + south + black + nearc4, data=card.df)
card.df$nearc4

summary(card.2sls )

inv(1)
test <- t(as.matrix(card.df[, c("educ", "nearc4")])) %*% as.matrix(card.df[, c("educ", "nearc4")])

test^-1 %*% test

test^{-1} %*% test

ginv(test) %*% test


colnames(hprice.df)<-c("price", "assessed.val", "no.bedrooms", "lot.size", "house.size", "colonial", "ln.price", "ln.assessed.val","ln.lot.size", "ln.sqrft")




1. id person identifier
2. nearc2 =1 if near 2 yr college, 1966
3. nearc4 =1 if near 4 yr college, 1966
4. educ years of schooling, 1976
5. age in years
6. fatheduc father's schooling
7. motheduc mother's schooling
8. weight NLS sampling weight, 1976
9. momdad14 =1 if live with mom, dad at 14
10. sinmom14 =1 if with single mom at 14
11. step14 =1 if with step parent at 14
12. reg661 =1 for region 1, 1966
13. reg662 =1 for region 2, 1966
14. reg663 =1 for region 3, 1966
15. reg664 =1 for region 4, 1966
16. reg665 =1 for region 5, 1966
17. reg666 =1 for region 6, 1966
18. reg667 =1 for region 7, 1966
19. reg668 =1 for region 8, 1966
20. reg669 =1 for region 9, 1966
21. south66 =1 if in south in 1966
22. black =1 if black
23. smsa =1 in in SMSA, 1976
24. south =1 if in south, 1976
25. smsa66 =1 if in SMSA, 1966
26. wage hourly wage in cents, 1976
27. enroll =1 if enrolled in school, 1976
28. married =1 if married, 1976
29. exper age - educ - 6