


# QUESTION 2

work.dir <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/"

load(paste0(work.dir, "crop wide df2.Rdata"))

names(crop.wide.df) <- gsub("[.]+$", "", names(crop.wide.df))

crop.wide.df <- crop.wide.df[crop.wide.df$harvest.r.MAIZ>0 & crop.wide.df$area.r.MAIZ>0, ]

cor.test(crop.wide.df$harvest.r.MAIZ, crop.wide.df$area.r.MAIZ)


area.median <- median(crop.wide.df$area.r.MAIZ)

crop.wide.df$fake.treatment <- ifelse(crop.wide.df$area.r.MAIZ>area.median, TRUE, FALSE)



h1 <- 0.3
x <- crop.wide.df$area.r.MAIZ
y <- crop.wide.df$harvest.r.MAIZ
kern11u <- ifelse( x>area.median & x<(area.median+h1), 1, 0)
kern11l <- ifelse(x<area.median & x>(area.median-h1), 1, 0)


# Kernel Regression
summary(predict(lm( y ~ 1, weights=kern11u)) - predict(lm( y ~ 1, weights=kern11l)))

# Local linear regression
predict(lm( y ~ x, weights=kern11u), newdata=data.frame(x=area.median)) - 
    predict(lm( y ~ x, weights=kern11l), newdata=data.frame(x=area.median))


# Polynomials
gt5 <- ifelse(x > area.median , 1, 0)

lm( y ~ gt5 + poly(x, degree=3, raw=TRUE) + gt5:poly(x-area.median, degree=3, raw=TRUE))


# QUESTION 3

set.seed(100)
x <- runif(1000)
Tr <- ifelse(x > jitter(rep(0.5, 1000), 5), 1 ,0 )
#Tr <- ifelse(x > .5, 1 ,0 )
g=Tr+3*log(x+1)+sin(x*12)/3

y <- g+qnorm(runif(1000))/5

plot(x, y)
abline(v=.5)


library("AER")


#summary(fert.ivreg <- ivreg( log(fert.exp+1) ~ log(remittances+1) + log(total.area) | hhh.sex + log(total.area), data=crop.wide.df))

#summary(dummy.ivreg <- ivreg( y ~ x | Tr))

summary(dummy.ivreg <- ivreg( 
  y ~ Tr +  poly(x, degree=5, raw=TRUE) + Tr:poly(x-0.5, degree=5, raw=TRUE) | 
  I(x>.5) +  poly(x, degree=5, raw=TRUE) + I(x>.time.amanzanada5):poly(x-0.5, degree=5, raw=TRUE)
  ))





# QUESTION 4

load("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/double hurdle.Rdata")


library("AER")


chisq.test(crop.wide.df$received.credit, crop.wide.df$hhh.literacy)

summary(fert.ivreg <- ivreg( log(fert.exp+1) ~ 
  received.credit + indig.prop + drive.time.urban + mean.ann.rain.5yr + elevation + hhh.sex | 
  hhh.literacy + indig.prop + drive.time.urban + mean.ann.rain.5yr + elevation + hhh.sex, 
data=crop.wide.df))


# QUESTION 4.c

#target.vars <- c("received.credit", "indig.prop", "drive.time.urban", "mean.ann.rain.5yr", "elevation" "hhh.sex")

crop.wide.df$hhh.sex <- ifelse(crop.wide.df$hhh.sex=="hombre", 1, 0)

crop.wide.df$Z_i <- runif(nrow(crop.wide.df))
beta_1 <- 6
beta_2 <- 4
crop.wide.df$T_i <- with(crop.wide.df, 1 + beta_1 * Z_i + 
    beta_2*(indig.prop + drive.time.urban + mean.ann.rain.5yr + elevation + hhh.sex) + 
    rnorm(nrow(crop.wide.df), sd=200) )

gamma_1 <- 15
gamma_2 <- 20

crop.wide.df$Y_i <- with(crop.wide.df, 1 + gamma_1 * T_i + 
    gamma_2*(indig.prop + drive.time.urban + mean.ann.rain.5yr + elevation + hhh.sex) + 
    rnorm(nrow(crop.wide.df), sd=100) )

summary(fert.ivreg.perfect <- ivreg( Y_i ~ 
  T_i + indig.prop + drive.time.urban + mean.ann.rain.5yr + elevation + hhh.sex | 
  Z_i + indig.prop + drive.time.urban + mean.ann.rain.5yr + elevation + hhh.sex, 
data=crop.wide.df))









chisq.test(crop.wide.df$received.credit, crop.wide.df$hhh.sex)


cor.test(as.numeric(crop.wide.df$received.credit), crop.wide.df$drive.time.urban)
cor.test(as.numeric(crop.wide.df$received.credit), crop.wide.df$drive.time.amanzanada)
cor.test(as.numeric(crop.wide.df$received.credit), as.numeric(crop.wide.df$hhh.literacy))

summary(fert.ivreg <- ivreg( log(fert.exp+1) ~ log(remittances+1) + log(total.area) | hhh.sex + log(total.area), data=crop.wide.df))












summary(test.lm <- lm( y ~ Tr +  poly(x, degree=8, raw=TRUE) + Tr:poly(x-0.5, degree=8, raw=TRUE)))


summary(test.lm <- lm( y ~ Tr +  poly(x, degree=8, raw=TRUE) + Tr:poly(x-0.5, degree=8, raw=TRUE)))







summary(test.lm <- lm( y ~ Tr(x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) )))

plot(x=x, predict(test.lm ))

#lm( y ~ gt5*poly(x, degree=3, raw=TRUE))

# TODO: finish and figure out weird behavior









sort x
replace x=0.5 if _n==1001
replace g=. if _n==1001
replace y=. if _n==1001
gen g0=g if T==0
gen g1=g if T==1






