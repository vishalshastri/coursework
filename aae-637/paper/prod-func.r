# EXPLORATION OF PRODUCTION FUNCTION

## Production function specifications
# Need to: 

Files:
data-prep: Data prep should all be in one file, but maybe with sections or if statements so the file doesnt construct all the data at once if not needed

fert-tobit: Then there is the tobit reg for determinants of fertilizer use

not sure: Integration of markets - i.e. correlation of quantity and price

prod-func: Then estimation of production function - try to figure out good way to compare multiple crops & specifications.

Where does spatial autocorrelation come in?

load: Also, package installation and loading data files already prepared

Separately, need to have work dir and mapquest api key, maybe in a git.ignore file


options(encoding = "CP850")
prod01.df.test<-read.spss(paste0(work.dir, "bd18 (2001).zip Folder/agricola.sav"), to.data.frame = TRUE, reencode="CP850")
prod01.df.test[, 3]<-gsub("(^[[:space:]]+)|([[:space:]]+$)", "", prod01.df.test[, 3])
write.csv(rev(sort(table(prod01.df.test[, 3]))), file=paste0(work.dir, "unclean cropnames.csv"), fileEncoding="CP850")



prod01.agg<-aggregate(prod01.df[, c("total.value", "area.r"), drop=FALSE], by=list(FOLIO=prod01.df$FOLIO), FUN=sum, na.rm=TRUE)

plot(prod01.agg[, c("total.value", "area.r")])
cor(prod01.agg[, c("total.value", "area.r")])

prod01.agg$FOLIO[prod01.agg$area.r>4000]

prod01.df[prod01.df$FOLIO==142010411, ]
# some bad outliers

crop.df<-merge( crop.wide.df, prod01.agg)
# somehow we lost some observations when making it wide, maybe due to only picking up on the top 10 crops

fmla.prod<-as.formula("I(log(total.value+1)) ~   I(log(area.r+1)) + I(log(fert.exp+1)) + I(log(seed.exp+1)) + I(log(num.pers.agropecuaria+1)) + elevation + I(elevation^2) +  I(log(rain.grow.season.2001+1)) + AWC.CLASS + T.GRAVEL +  T.SILT + T.CLAY + T.BULK.DENSITY + T.OC + T.PH.H2O + T.CEC.CLAY + T.CEC.SOIL + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE + indig.prop + department + hhh.edu.measure + hhh.sex")

summary(prod.lm <- lm(fmla.prod,  data=crop.df[crop.df$total.value<1000000 & crop.df$area.r<1000, ]))


median((prod01.df$consumption.r/(prod01.df$harvest.r-prod01.df$lost.r))[prod01.df$crop.r=="PAPA"], na.rm=TRUE)
crop.r



hist(((prod01.df$sales.quant.r+prod01.df$bartered.r)/(prod01.df$harvest.r-prod01.df$lost.r))[prod01.df$crop.r=="MAIZ"], freq=F)


prod01.by.crop.agg<-aggregate(prod01.df[, c("total.value", "area.r", "harvest.r"), drop=FALSE], by=list(FOLIO=prod01.df$FOLIO, crop=prod01.df$crop.r), FUN=sum, na.rm=TRUE)

# rev(sort(table(prod01.by.crop.agg$crop)))[1:20]
#     PAPA      MAIZ      HABA    CEBADA     TRIGO     ARROZ      YUCA       OCA 
#     1539      1250       565       485       482       412       300       291 
#  PLATANO   CEBOLLA    ARVEJA    QUINUA     AVENA      MANI   NARANJA    TOMATE 
#      278       232       230       186       121       120       111       109 
# PAPALISA ZANAHORIA      COCA MANDARINA 
#       98        95        81        73 


# somehow we lost some observations when making it wide, maybe due to only picking up on the top 10 crops
# TODO: ABENA should be AVENA


#+ AWC.CLASS + T.GRAVEL +  T.SILT + T.CLAY + T.BULK.DENSITY + T.OC + T.PH.H2O + T.CEC.CLAY + T.CEC.SOIL + T.TEB + T.CACO3 + T.CASO4 + T.ESP + T.ECE
# SU.SYM90
# SU.SYM90:log(fert.exp+1) +

fmla.prod<-as.formula("log(harvest.r+1) ~  log(area.r+1) + log(fert.exp+1) + log(seed.exp+1) + log(hired.labor.exp+1) + log(manure.exp+1) + log(transport.exp+1) + log(pesticide.exp+1) + log(extension.exp+1) + log(machine.exp+1) + log(draft.anmial.exp+1) + log(other.exp+1) + log(num.pers.agropecuaria+1) + log(crop.labor+1) + log(crop.and.livestick.labor+1)  +  elevation + I(elevation^2) + log(rain.grow.season.2001+1)*log(riego.pct+1)  + SU.SYM90 +  indig.prop + department + department:log(riego.pct+1) + hhh.edu.measure + hhh.sex + hh.member.incident:hh.member.incident.income + I(hogar.incident==\"Desastres naturales\")")

fmla.prod<-as.formula("log(harvest.r+1) ~  log(area.r+1) + log(fert.exp+1) + log(seed.exp+1) + log(hired.labor.exp+1) + log(manure.exp+1) + log(transport.exp+1) + log(pesticide.exp+1) + log(extension.exp+1) + log(machine.exp+1) + log(draft.anmial.exp+1) + log(other.exp+1) + log(num.pers.agropecuaria+1) + log(crop.labor+1) + log(crop.and.livestick.labor+1)  +  elevation  + log(rain.grow.season.2001+1) + log(riego.pct+1)  + AWC.CLASS + T.GRAVEL + T.SILT + T.CLAY + T.OC + T.CEC.CLAY + T.CEC.SOIL +  indig.prop + I(hogar.incident==\"Desastres naturales\")")

maiz.prod.df<-merge( crop.wide.df, prod01.by.crop.agg[prod01.by.crop.agg$crop=="PAPA", ])
# maiz.prod.df<-merge( crop.wide.df, prod01.by.crop.agg[prod01.by.crop.agg$crop=="MAIZ" & prod01.by.crop.agg$FOLIO %in% names(table(prod01.by.crop.agg$FOLIO)[table(prod01.by.crop.agg$FOLIO)==1]), ])

#maiz.prod.df$SU.SYM90<-relevel(maiz.prod.df$SU.SYM90, ref="RGd")

summary(prod.lm <- lm(fmla.prod, data=maiz.prod.df))
# test.relimp<-calc.relimp(prod.lm, type="last")
test.boot.relimp<-boot.relimp(prod.lm, type=c("last", "first", "pratt", "genizi", "car"), rela=TRUE)
plot(booteval.relimp(test.boot.relimp))
#levels(crop.wide.df$department)
# names(maiz.prod.df)[sapply(maiz.prod.df, FUN=function(x) length(levels(x)))==1]

# TODO: positive coefficient on 

# TODO: for arroz there are negative values: maiz.prod.df$total.value
# TODO: hogar.incident has 154 NA's
# log(log(riego.pct/VIVIENDA + 1) + 1) can get us .10 p value in PAPA
# log(log(riego.pct + 1) + 1)  gives us a .01 p value on Maiz, but its sign is negative
# department:log(riego.pct+1) gives us a significantly better model with HABA


# This is TRUE: all(rownames(crop.wide.df)==1:nrow(crop.wide.df))
#test.d<-dnearneigh(as.matrix(maiz.prod.df[names(residuals(prod.lm)), c("X", "Y")]), -1, 500, row.names = NULL, longlat = TRUE)
test.d<-dnearneigh(as.matrix(maiz.prod.df[, c("X", "Y")]), -1, 500, row.names = NULL, longlat = TRUE)

#test.nb <- knn2nb(test.d, row.names = NULL, sym = FALSE)

#test.d<-knearneigh(as.matrix(maiz.prod.df[names(residuals(prod.lm)), c("X", "Y")]), k=100, longlat = TRUE)

test.listw<-nb2listw(test.d, zero.policy=TRUE)
#test.listw<-nb2listw(knn2nb(test.d), zero.policy=TRUE)

#lm.morantest(prod.lm, listw=test.listw, zero.policy=TRUE)
#lm.LMtests(prod.lm, listw=test.listw)

prod.lm.LMtests<- lm.LMtests(prod.lm, test.listw, zero.policy=TRUE, test="all", spChk=NULL, naSubset=TRUE)

prod.residuals.df<-data.frame(prod.residuals=residuals(prod.lm))
prod.residuals.spdf<-SpatialPointsDataFrame(crop.wide.df[names(residuals(prod.lm)), c("X", "Y")], prod.residuals.df)
plot(variogram(prod.residuals ~ 1, locations = coordinates(prod.residuals.spdf), data=prod.residuals.spdf,  cloud = F),  type = "b", main = "Variogram of prod.residuals") 



 + log(seed.exp+1) + log(hired.labor.exp+1) + log(manure.exp+1) + log(transport.exp+1) + log(pesticide.exp+1) + log(extension.exp+1) + log(machine.exp+1) + log(draft.anmial.exp+1) + log(other.exp+1) + log(num.pers.agropecuaria+1) + log(crop.labor+1) + log(crop.and.livestick.labor+1)  +  elevation + I(elevation^2) + log(rain.grow.season.2001+1)*log(riego.pct+1) + indig.prop

fmla.prod<-as.formula("log(harvest.r+1) ~  log(area.r+1)  + log(fert.exp+1)")



prod.lm <- lm(fmla.prod, data=maiz.prod.df)

maiz.prod.df.for.model<-maiz.prod.df[names(residuals(prod.lm)), ]

maiz.prod.df.for.model[] <- lapply(maiz.prod.df.for.model,function(x) if(is.factor(x)) factor(x) else x)

sapply(maiz.prod.df.for.model,function(x) if(is.factor(x)) levels(x) else 0)


prod.rev.lm <- lm(fmla.prod, data=maiz.prod.df.for.model)

calc.relimp(prod.rev.lm)

boot <- boot.relimp(prod.rev.lm, b = 1000, type = c("lmg"), rank = TRUE, 
  diff = TRUE, rela = TRUE)
boot <- boot.relimp(prod.lm, b = 9, type = c("lmg"), rela = TRUE)
# "Higher order terms supported for lmg only"
booteval.relimp(boot) # print result
plot(booteval.relimp(boot,sort=TRUE)) # plot result

levelplot(cor(model.matrix(fmla.prod, data=maiz.prod.df)[, -1]))

test.mat <- matrix(c(1:10, 10:1, (1:10)*5), nrow=10)
(t(test.mat) %*% test.mat) %*% svd( t(test.mat) %*% test.mat)$v


X = u w v’… and compute X.v





fmla.prod.caterpillar<-strsplit("log(harvest.r+1) ~  log(area.r+1) + log(fert.exp+1) + log(seed.exp+1) + log(hired.labor.exp+1) + log(manure.exp+1) + log(transport.exp+1) + log(pesticide.exp+1) + log(extension.exp+1) + log(machine.exp+1) + log(draft.anmial.exp+1) + log(other.exp+1) + log(num.pers.agropecuaria+1) + log(crop.labor+1) + log(crop.and.livestick.labor+1)  +  elevation + I(elevation^2) + log(rain.grow.season.2001+1) + log(riego.pct+1)  + AWC.CLASS + T.GRAVEL + T.SILT + T.CLAY + T.OC + T.CEC.CLAY + T.CEC.SOIL  +  indig.prop + department + hhh.sex + I(hogar.incident==\"Desastres naturales\")", " [+] ")[[1]]

for (i in 29:length(fmla.prod.caterpillar)) {
  fmla.prod.test<-as.formula(paste(fmla.prod.caterpillar[1:i], collapse="+"))
  prod.rev.lm <- lm(fmla.prod.test, data=maiz.prod.df.for.model)
  test<-calc.relimp(prod.rev.lm, rela = TRUE)
}

x<-model.matrix(fmla.prod.test, data=maiz.prod.df.for.model)

# http://stats.stackexchange.com/questions/56645/what-is-the-fastest-method-for-determining-collinearity-degree



# Variance Inflation Factor:
sqrt(vif(prod.lm)) > 2 # problem?
# This seems only to be a problem with the soil variables, which makes sense
# http://www.statmethods.net/stats/rdiagnostics.html



n<-100
p<-20
#non-ill conditioned part of the dataset.
x<-matrix(rnorm(n*p),nc=p)
x<-scale(x)
#introduce a variable that causes x to be 
#ill conditioned.
y<-x%*%c(rnorm(1),0, 0, 0, rnorm(1), 0, rnorm(1), rnorm(1), rep(0,p))[1:p]
#y<-scale(y)
#x<-cbind(x,y)
p<-ncol(x)

A<-svd(x,nu=0)
#x is ill-conditioned: this ratio is larger 
#than 10. (step 1)
2*log(A$d[1]/A$d[p])

#check what is causing it: (step 2)
round(A$v[,ncol(A$v)],2)
#you can write the last variable as (.23*x_1+.5*x_2-.45*x_3)/(-.7) [1]

min(A$d)
    #is 0. if min(A$d)>0 then this gives you how much there is noise 
#there is arround [1].

# http://stats.stackexchange.com/questions/56645/what-is-the-fastest-method-for-determining-collinearity-degree

isTRUE(
all.equal(x[, -ncol(x)] %*% A$v[1:(nrow(A$v)-1),ncol(A$v)] / A$v[nrow(A$v),ncol(A$v)], x[, ncol(x), drop=F])
)

cor(x[, -ncol(x)] %*% A$v[1:(nrow(A$v)-1),ncol(A$v)] / A$v[nrow(A$v),ncol(A$v)], x[, ncol(x), drop=F])



semillas: S815B2
abono: S815C2
Fert: S815D2
transp: S815E2
Pest: S815F2
tech assistance: S815G2
rent mach: S815H2
animal traction: S815I2
Other costs: S815N2



c("S815B2", "S815C2", "S815D2", "S815E2", "S815F2", "S815G2", "S815H2", "S815I2", "S815N2")


apply(hogar01.df[, c("S815B2", "S815C2", "S815D2", "S815E2", "S815F2", "S815G2", "S815H2", "S815I2", "S815N2")], MARGIN=2, FUN=mean, na.rm=TRUE)


    S815B2     S815C2     S815D2     S815E2     S815F2     S815G2     S815H2     S815I2     S815N2 
 346.87354  587.67200  404.21550  610.82857 1484.41940  581.28571  847.40214   93.25312  957.22277 

S815B2 S815C2 S815D2 S815E2 S815F2 S815G2 S815H2 S815I2 S815N2 
  80.0  100.0  125.0  100.0   85.0  100.0  200.0   50.0   27.5 


test.data<-data.frame(treatment=sample(1:2, 10000, replace=TRUE), value=runif(1000))

summary(lm(test.data$value ~ test.data$treatment))
mean(test.data$value[test.data$treatment==1])
sd(test.data$value[test.data$treatment==1])



make.add.to =
function(x) {
add.x.to = function(u) x + u
add.x.to
}




f<-function(x,y) eval(parse(text="x+y"))

create.fun <-function(x,y) {

 function(
 eval(parse(text="x+y"))
 
 optim(starting values, log-likelihood, data)
 
 
 mle(minuslogl, start = formals(minuslogl), method = "BFGS",
    fixed = list(), nobs, ...)
    
    nobs = NROW(y))
    
 x=rbinom(n=100, size=1, prob=.25)
    
 like.test <- function(param, n)  { -sum(log(param^x*(1-param)^(n-x))) }

 mle(like.test, start=list(param=.3, n=1))


like.test(param=.3, n=1)


function(m, j)

betas = 
combn(1:m, 2, FUN=paste, collapse="")
paste(1:m, 1:m, sep="")
























