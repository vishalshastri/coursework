
M <- 5
N <-5

lead.zero <- function(x) {formatC(x, width = 2, flag = "0")}

# llf.creator.fn <- function(M, N, Z) 

y.perm <- expand.grid(paste0("y", lead.zero(1:M)), paste0("y", lead.zero(1:M)))
M.2.dim <- gsub( "y", ".", do.call(paste0, y.perm))

N.2.dim <- expand.grid(paste0(".", lead.zero(1:N)), paste0(".", lead.zero(1:N)))

ln.sh.w.grid <- expand.grid( paste0("log(w", lead.zero(1:N), " * theta", lead.zero(1:N), ") * "),
  paste0("log(w", lead.zero(1:N), " * theta", lead.zero(1:N), ")") )

ln.sh.w.grid.2 <- do.call(paste0, ln.sh.w.grid)

M.N.dim <- expand.grid(paste0(".", lead.zero(1:M)), paste0(".", lead.zero(1:N)))

ym.wn <- expand.grid(paste0("y", lead.zero(1:M)), paste0("log(w", lead.zero(1:N), " * theta", lead.zero(1:N), ")"))

# data.frame(M.N.dim, ym.wn) # checks out

# ln.c part
# beta0 + 
ln.c.1 <- paste0("alpha", lead.zero(1:M), " * " , 
 "y", lead.zero(1:M), collapse=" + ")
# +
ln.c.2 <-  paste0("beta", lead.zero(1:N),  " * ",
 "log(w", lead.zero(1:N), " * ", "theta", lead.zero(1:N), ")", collapse=" + ")
# + (1/2) *
ln.c.3 <-  paste0("alpha", M.2.dim , " * ", y.perm[[1]], " * ", y.perm[[2]], collapse=" + ")
# +
ln.c.4 <-  paste0("beta", do.call(paste0, N.2.dim ), " * ", ln.sh.w.grid.2, collapse=" + ")
# +
ln.c.5 <- paste0("gamma", do.call(paste0, M.N.dim), " * ", ym.wn[[1]], " * ", ym.wn[[2]], collapse=" + " )

ln.c <- paste0("beta0 + ", ln.c.1, " + ", ln.c.2, " + (1/2) * ", ln.c.3, " + ", 
  ln.c.4, " + ", ln.c.5)



gamma.special<-c()
gamma.mat<-matrix(1:(M*N), nrow=M, ncol=N)
for ( i in 1:N) {
  gamma.special[i] <- paste0(
    paste0("gamma", do.call(paste0, M.N.dim), " * ", ym.wn[[1]])[gamma.mat[, i]],
    collapse=" + " )
}

beta.special<-c()
beta.mat<-matrix(1:(N*N), nrow=N, ncol=N)
for ( i in 1:N) {
  beta.special[i] <- paste0(
    paste0("beta", do.call(paste0, N.2.dim[2:1]) , " * ", gsub("[*] $", "", ln.sh.w.grid[[1]]))[beta.mat[, i]],
    collapse=" + " )
}


ln.E.2nd <- paste0( "log(" , 
  paste0(
  paste0("w", lead.zero(1:N), " / (w", lead.zero(1:N), " * theta", 
   lead.zero(1:N), ") * ", 
   "(beta", lead.zero(1:N), " + ",
   gamma.special, " + ",
   beta.special, ")"),
  collapse=" + " ), ")" )
   
# ln.E <- paste0("test.fn <- function(X) {", ln.c, " + ", ln.E.2nd, "}")
ln.E <- paste0("nls.formula.ln.E <- ln.E.data ~ ", ln.c, " + ", ln.E.2nd)

#function.text <- llf.creator.fn(12,5,1)
eval(parse(text=ln.E))



ln.E.data <- log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + w05*x05 )


#test.nls <- nls(nls.formula.ln.E, trace=TRUE)



ln.E.vars <- all.vars(nls.formula.ln.E)
ln.E.vars <- ln.E.vars[ !grepl("(w[0-9])|(y[0-9])|(ln.E.data)", ln.E.vars ) ]
ln.E.start.vals <- vector(mode="numeric", length=length(ln.E.vars))
ln.E.vars <- sort(ln.E.vars)
names(ln.E.start.vals) <- ln.E.vars
ln.E.start.vals[grepl("(beta)|(gamma)|(alpha)", names(ln.E.start.vals))] <- 5
ln.E.start.vals[grepl("theta", names(ln.E.start.vals))] <- 1
ln.E.start.vals

test.nls <- nls(nls.formula.ln.E, start=ln.E.start.vals, trace=TRUE, algorithm="plinear")

# install.packages("minpack.lm")
library(minpack.lm)
### Examples from 'nls' doc ###
fm1DNase1 <- nlsLM(nls.formula.ln.E, start=ln.E.start.vals, trace=TRUE )


library(DEoptim)

mod <- function(x) x[1] + x[2]*x[3]^ExponCycles
fun <- function(x) sum((ExponValues-mod(x))^2)

ss <- DEoptim(fun, lower=rep(0,3), upper=c(10e7, 10, 10),
              control=list(trace=FALSE))

pa <- ss$optim$bestmem


eval(parse(text=paste0("mod <- function() { ", ln.c, " + ", ln.E.2nd, "}")))
fun <- function() sum((ln.E.data - mod())^2)

first.line <- paste0( "args <- c(\"", paste(ln.E.vars, sep="\", \"", collapse="\", \""), "\")\nfor ( i in 1:length(args)) { assign(args[i], x[i])} ; ")

eval(parse(text=paste0("mod <- function(x) {", first.line, 
"  sum((ln.E.data - ", ln.c, " + ", ln.E.2nd, ")^2) }")))


#fun <- function() sum((ln.E.data - mod())^2)

ln.E.low.vals <- vector(mode="numeric", length=length(ln.E.vars))
names(ln.E.low.vals) <- ln.E.vars
ln.E.low.vals[grepl("(beta)|(gamma)|(alpha)", names(ln.E.low.vals))] <- 0
ln.E.low.vals[grepl("theta", names(ln.E.low.vals))] <- .1
ln.E.high.vals <- vector(mode="numeric", length=length(ln.E.vars))
names(ln.E.high.vals) <- ln.E.vars
ln.E.high.vals[grepl("(beta)|(gamma)|(alpha)", names(ln.E.high.vals))] <- 1
ln.E.high.vals[grepl("theta", names(ln.E.high.vals))] <- 7

eval(parse(text=paste0("test.fn <- function(x) {", first.line, 
"  ", ln.c, " + ", ln.E.2nd, " }")))
test.fn(ln.E.high.vals) # pa

mod(ln.E.high.vals)



ss <- DEoptim(mod, lower=ln.E.low.vals, upper=ln.E.high.vals,
              control=list(trace=TRUE, itermax=50))

pa <- ss$optim$bestmem

fm1DNase1 <- nlsLM(nls.formula.ln.E, start=pa, trace=TRUE )
# install.packages("nlmrt")
library(nlmrt)

fm1DNase1 <- nlfb(nls.formula.ln.E, data=.GlobalEnv, start=pa, trace=TRUE )

fm1DNase1 <- nlfb(start=pa, resfn=mod , trace=TRUE )

fm1DNase1 <- nlxb(nls.formula.ln.E, data=.GlobalEnv, start=pa, trace=TRUE )













work.dir <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/"

load("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/prod01.df imputed prices.Rdata")
load("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/crop wide df4.Rdata")

miembros01.df<-read.spss(paste0(work.dir, "bd18 (2001).zip Folder/mcv01.sav"), to.data.frame = TRUE)

hogar01.df<-read.spss(paste0(work.dir, "bd18 (2001).zip Folder/hogar.sav"), to.data.frame = TRUE)




collapse=" + ")






function.text <- llf.creator.fn(12,5,1)
eval(parse(text=function.text))









firm.df<-firm.df[apply(firm.df, 1, FUN=function(x) !any(is.na(x))), ]

firm.df<-firm.df[firm.df$harvest.r.ARROZ!=0 | firm.df$harvest.r.MAIZ!=0 |  firm.df$harvest.r.PLATANO!=0 |  firm.df$harvest.r.YUCA!=0 |   firm.df$harvest.r.ARVEJA!=0 |   firm.df$harvest.r.CEBADA!=0 |   firm.df$harvest.r.CEBOLLA!=0 |   firm.df$harvest.r.HABA!=0 |   firm.df$harvest.r.OCA!=0 |   firm.df$harvest.r.PAPA!=0 |   firm.df$harvest.r.QUINUA!=0 |   firm.df$harvest.r.TRIGO!=0,  ]

# WARNING: must do this after actual computation of these
firm.df<-firm.df[0!=rowSums(firm.df[, c("fert.quintals", "seed.quintals","abono.quintals", "plaguicida.liters", "labor.hours") ]), ]





# TODO: need to fix

#firm.df<-firm.df[-which.max(profit),]

#firm.df<-firm.df[!is.na(profit) & profit!=0 & firm.df$land.area>0,]

#w01=w01, w02=w02, w03=w03, w04=w04, w05=w05, x01=x01, x02=x02, x03=x03, x04=x04, x05=x05, 
#p01=p01, p02=p02, p03=p03, p04=p04, p05=p05, p06=p06, p07=p07, p08=p08, p09=p09, p010=p010, 
#p011=p011, p012=p012, y01=y01, y02=y02, y03=y03, y04=y04, y05=y05, y06=y06, y07=y07, y08=y08,
#y09=y09, y010=y010, y011=y011, y012=y012, profit=profit

w01 = firm.df$fert.price.quintal
w02 = firm.df$seed.price
w03 = firm.df$abono.price
w04 = firm.df$plaguicida.price.liter
w05 = firm.df$imputed.ag.wage

x01 = firm.df$fert.quintals
x02 = firm.df$seed.quintals
x03 = firm.df$abono.quintals
x04 = firm.df$plaguicida.liters
x05 = firm.df$labor.hours

p01 = firm.df$price.PAPA
p02 = firm.df$price.MAIZ
p03 = firm.df$price.PLATANO
p04 = firm.df$price.YUCA
p05 = firm.df$price.ARVEJA
p06 = firm.df$price.CEBADA
p07 = firm.df$price.CEBOLLA
p08 = firm.df$price.HABA
p09 = firm.df$price.OCA
p010 = firm.df$price.ARROZ
p011 = firm.df$price.QUINUA
p012 = firm.df$price.TRIGO

y01 = firm.df$harvest.r.PAPA 
y02 = firm.df$harvest.r.MAIZ
y03 = firm.df$harvest.r.PLATANO
y04 = firm.df$harvest.r.YUCA
y05 = firm.df$harvest.r.ARVEJA
y06 = firm.df$harvest.r.CEBADA
y07 = firm.df$harvest.r.CEBOLLA
y08 = firm.df$harvest.r.HABA
y09 = firm.df$harvest.r.OCA
y010 = firm.df$harvest.r.ARROZ
y011 = firm.df$harvest.r.QUINUA
y012 = firm.df$harvest.r.TRIGO

z1 = firm.df$land.area

profit= p01*y01 + p02*y02 + p03*y03 + p04*y04 + p05*y05 + p06*y06 + p07*y07 + p08*y08 + p09*y09 + 
  p010*y010 + p011*y011 + p012*y012 - ( w01*x01 + w02*x02 + w03*x03 + w04*x04 + w05*x05 )
  
profit.test= p01*y01 + p02*y02 + p03*y03 + p04*y04 + p05*y05 + p06*y06 + p07*y07 + p08*y08 + p09*y09 + 
  p010*y010 + p011*y011 + p012*y012 - ( w01*x01 + w02*x02 + w03*x03 + w04*x04 )

summary(profit)
summary(profit.test)


w01[w01==0] <- mean(w01[w01!=0]) + mean(w01[w01!=0])* rnorm(length(w01[w01==0]), mean = 0, sd = .1)
w02[w02==0] <- mean(w02[w02!=0]) + mean(w02[w02!=0])* rnorm(length(w02[w02==0]), mean = 0, sd = .1)
w03[w03==0] <- mean(w03[w03!=0]) + mean(w03[w03!=0])* rnorm(length(w03[w03==0]), mean = 0, sd = .1)
w04[w04==0] <- mean(w04[w04!=0]) + mean(w04[w04!=0])* rnorm(length(w04[w04==0]), mean = 0, sd = .1)
w05[w05==0] <- mean(w05[w05!=0]) + mean(w05[w05!=0])* rnorm(length(w05[w05==0]), mean = 0, sd = .1)

p01[p01==0] <- mean(p01[p01!=0]) + mean(p01[p01!=0])* rnorm(length(p01[p01==0]), mean = 0, sd = .1)
p02[p02==0] <- mean(p02[p02!=0]) + mean(p02[p02!=0])* rnorm(length(p02[p02==0]), mean = 0, sd = .1)
p03[p03==0] <- mean(p03[p03!=0]) + mean(p03[p03!=0])* rnorm(length(p03[p03==0]), mean = 0, sd = .1)
p04[p04==0] <- mean(p04[p04!=0]) + mean(p04[p04!=0])* rnorm(length(p04[p04==0]), mean = 0, sd = .1)
p05[p05==0] <- mean(p05[p05!=0]) + mean(p05[p05!=0])* rnorm(length(p05[p05==0]), mean = 0, sd = .1)
p06[p06==0] <- mean(p06[p06!=0]) + mean(p06[p06!=0])* rnorm(length(p06[p06==0]), mean = 0, sd = .1)
p07[p07==0] <- mean(p07[p07!=0]) + mean(p07[p07!=0])* rnorm(length(p07[p07==0]), mean = 0, sd = .1)
p08[p08==0] <- mean(p08[p08!=0]) + mean(p08[p08!=0])* rnorm(length(p08[p08==0]), mean = 0, sd = .1)
p09[p09==0] <- mean(p09[p09!=0]) + mean(p09[p09!=0])* rnorm(length(p09[p09==0]), mean = 0, sd = .1)
p010[p010==0] <- mean(p010[p010!=0]) + mean(p010[p010!=0])* rnorm(length(p010[p010==0]), mean = 0, sd = .1)
p011[p011==0] <- mean(p011[p011!=0]) + mean(p011[p011!=0])* rnorm(length(p011[p011==0]), mean = 0, sd = .1)
p012[p012==0] <- mean(p012[p012!=0]) + mean(p012[p012!=0])* rnorm(length(p012[p012==0]), mean = 0, sd = .1)














































m0.4 <- mle2(eff.llf,
  start=as.list(test),  
  data=list(w1=w1, w2=w2, w3=w3, w4=w4, w5=w5, x1=x1, x2=x2, x3=x3, x4=x4, x5=x5, 
p1=p1, p2=p2, p3=p3, p4=p4, p5=p5, p6=p6, p7=p7, p8=p8, p9=p9, p10=p10, 
p11=p11, p12=p12, y1=y1, y2=y2, y3=y3, y4=y4, y5=y5, y6=y6, y7=y7, y8=y8,
y9=y9, y10=y10, y11=y11, y12=y12, profit=profit),
  method= "BFGS",  skip.hessian=TRUE, control=list(trace=5, REPORT=1, maxit=200))
  
eff.llf.comp<-cmpfun(eff.llf)





library( systemfit )
data( ppine )

hg.formula <- hg ~ exp( h0 + h1*log(tht) + h2*tht^2 + h3*elev + h4*cr)
dg.formula <- dg ~ exp( d0 + d1*log(dbh) + d2*hg + d3*cr + d4*ba  )
labels <- list( "height.growth", "diameter.growth" )
inst <- ~ tht + dbh + elev + cr + ba
start.values <- c(h0=-0.5, h1=0.5, h2=-0.001, h3=0.0001, h4=0.08,
                  d0=-0.5, d1=0.009, d2=0.25, d3=0.005, d4=-0.02 )
model <- list( hg.formula, dg.formula )

model.ols <- nlsystemfit( "OLS", model, start.values, data=ppine, eqnlabels=labels )
print( model.ols )

model.sur <- nlsystemfit( "SUR", model, start.values, data=ppine, eqnlabels=labels )
print( model.sur )

model.2sls <- nlsystemfit( "2SLS", model, start.values, data=ppine,
   eqnlabels=labels, inst=inst )
print( model.2sls )

model.3sls <- nlsystemfit( "3SLS", model, start.values, data=ppine,
                                    eqnlabels=labels, inst=inst )
print( model.3sls )