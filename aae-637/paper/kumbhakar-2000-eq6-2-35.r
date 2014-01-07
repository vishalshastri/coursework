

# Our flow must be:
# 1. Construct nonlinear string
# 2. Impose symmetry restrictions on this string
# 3. Contruct linear string from this nonlinear string
# 4. Make the system of equations
# 5. Create crossreferene dataframe
# 6. Determine which vars are singular, and note them
# 7. Delete them from all the equations




M <- 12
N <-4

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
# + (1/2) *
ln.c.4 <-  paste0("beta", do.call(paste0, N.2.dim ), " * ", ln.sh.w.grid.2, collapse=" + ")
# +
ln.c.5 <- paste0("gamma", do.call(paste0, M.N.dim), " * ", ym.wn[[1]], " * ", ym.wn[[2]], collapse=" + " )

ln.c <- paste0("beta0 + ", ln.c.1, " + ", ln.c.2, " + (1/2) * (", ln.c.3, 
") + (1/2) * (", ln.c.4, ") + ", ln.c.5)



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


ln.E.data <- log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + w05*x05 )
# ln.E.data <- log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + 2  )


#test.nls <- nls(nls.formula.ln.E, trace=TRUE)

#ln.E.string <- paste(ln.c, " + ", ln.E.2nd)

ln.E.string <- ln.c


#### imposing symmetry restrictions


####################### IMPOSING ADDING-UP RESTRICTIONS
library(stringr)


ln.E.vars <- all.vars(as.formula(paste("ln.E.data ~", ln.E.string)))
ln.E.vars <- ln.E.vars[ !grepl("(w[0-9])|(y[0-9])|(ln.E.data)", ln.E.vars ) ]
ln.E.vars <- sort(ln.E.vars)


replacements <- data.frame(greek=c("alpha", "beta", "gamma"), N=c(M,N,N), M=c(M,N,M))
replacements <- replacements[1:2, ]
# so do not actually need for gamma

for ( k in 1:nrow(replacements) ) {

if (replacements$greek[k]=="alpha" & M==1) {next}

symm <- ln.E.vars[grepl(paste0(replacements$greek[k], "[.]"), ln.E.vars) ]

symm.mat<-matrix(paste0(replacements$greek[k], ".", apply(X=expand.grid(lead.zero(1:max(N,M)), lead.zero(1:max(N,M))), MARGIN=1, FUN=paste, collapse=".")), nrow=max(N,M), ncol=max(N,M))
symm.mat[upper.tri(symm.mat, diag = FALSE)] <- t(symm.mat)[upper.tri(symm.mat, diag = FALSE)]
symm.mat<-symm.mat[1:replacements$N[k], 1:replacements$M[k]]

# data.frame(symm, c(symm.mat))

for ( i in 1:length(symm)) {
  ln.E.string <- str_replace_all(ln.E.string, symm[i], c(symm.mat)[i])
}

}

all.vars(as.formula(paste("ln.E.data ~", ln.E.string)))

#######################

# TODO: double check the symmetry replacements

ln.E <- paste0("nls.formula.ln.E <- ln.E.data ~ ", ln.E.string)

#function.text <- llf.creator.fn(12,5,1)
eval(parse(text=ln.E))




# load(file=paste0(work.dir, "firm df.Rdata"))
#### Ok, what would an lm-compatible formula look like?
#test.df <- data.frame(x=runif(500), y=runif(500))
#summary(lm(y ~ x, data=test.df ))

# Let's try direct modification of 
#beta01 * log(w01 * theta01) 
#alpha01 *
#alpha.01.01 * y01 * y01
#beta.01.01 * log(w01 * theta01) * log(w01 * theta01)
#gamma.01.03 * y01 * log(w03 * theta03)

#restrict.matrix = "Investment_(Intercept) = Consumption_(Intercept)"

#We can only use the first part, which means the parameter restrictions will go away:
ln.c <- ln.E.string 
ln.c.linear <- ln.c

ln.c.linear <- gsub("beta0 [+]", "", ln.c.linear)
ln.c.linear <- gsub("beta[0-9]+ [*] ", "", ln.c.linear)
ln.c.linear <- gsub(" [*] theta[0-9]+", "", ln.c.linear)
ln.c.linear <- gsub("alpha[0-9]+ [*]", "", ln.c.linear)
ln.c.linear <- gsub("alpha.[0-9]+.[0-9]+ [*] ", "", ln.c.linear)
ln.c.linear <- gsub("beta.[0-9]+.[0-9]+ [*]", "", ln.c.linear)
ln.c.linear <- gsub("gamma.[0-9]+.[0-9]+ [*]", "", ln.c.linear)
ln.c.linear <- gsub("[(]1/2[)] [*]", "", ln.c.linear)
# we should just scale the parameters to get the 1/2 part down
ln.c.linear <- gsub("[*]", ":", ln.c.linear)
# convert to single interaction term
for (i in 1:max(c(N,M))) {
  temp.y <- paste0("y", lead.zero(i))
  ln.c.linear <- gsub(paste0(temp.y, " : ", temp.y), paste0("I(",temp.y, "^2)"), ln.c.linear)
  temp.w <- paste0("w", lead.zero(i))
  ln.c.linear <- gsub(paste0("log[(]", temp.w, "[)] : log[(]", temp.w, "[)]"), paste0("I(log(", temp.w, ")^2)"), ln.c.linear)
}

#test.lm <-lm(as.formula(paste0("ln.E.data ~", ln.c.linear)))

# So we can impose linear restrictions after all


S.n.H <- list()

for ( n in 1:N) {
 S.n.H[[n]] <- as.formula(
   paste0( "I( (x", lead.zero(n), " * ", "w", lead.zero(n), ")/ln.E.data) ~ ",
    paste0("log(w", lead.zero(1:N), ")", collapse=" + "), " + ", 
    paste0("y", lead.zero(1:M), collapse=" + ")
   )
  )
  names(S.n.H)[n] <- paste0("S.n.H.", lead.zero(n))
}
S.n.H[[1]] <- NULL

S.n.H[[length(S.n.H)+1]] <- as.formula(paste0("ln.E.data ~", ln.c.linear))
names(S.n.H)[length(S.n.H)] <- "cost.fn"


#test.lm <-lm(terms(S.n.H[[5]], keep.order=TRUE))
#names(coef( test.lm))
#names(coef( test.lm)[is.na(coef( test.lm))])
#kleinOls <- systemfit( S.n.H[1:4], "SUR", maxit = 1  )
#names(coef( kleinOls))

# We need a cross reference for the parameter names

# So we have it with:
# A. Linear fit (cross equation parameter restrictions)
#   1. Cost function
#   2. Cost share
# B. Nonlinear fit (Just replace appropriate parameters)
#   1. Cost function
#   2. Cost share



ln.E.vars <- all.vars(as.formula(paste("ln.E.data ~", ln.c)))
# ln.E.vars <- all.vars(as.formula(paste("ln.E.data ~", ln.E.string)))
ln.E.vars <- ln.E.vars[ !grepl("(theta[0-9])|(w[0-9])|(y[0-9])|(ln.E.data)", ln.E.vars ) ]
ln.E.vars
param.crossref.df <- data.frame(nlm=ln.E.vars, lm=names(coef(test.lm)), lm.share=NA)
# already has symmetry

for ( i in lead.zero(2:N)) {
  param.crossref.df[param.crossref.df$nlm==paste0("beta", i), "lm.share"] <- 
    paste0("S.n.H.", i, "_(Intercept)")
  for ( j in lead.zero(1:M)) {
    param.crossref.df[
      param.crossref.df$nlm==paste0("beta.", i, ".", j), "lm.share"] <-
    paste0("S.n.H.", i, "_", "log(w", j, ")")
    param.crossref.df[
      param.crossref.df$nlm==paste0("gamma.", j, ".", i), "lm.share"] <-
    paste0("S.n.H.", i, "_", "y", j)
  }
}

#singular.test.lm <-lm(terms(S.n.H[[length(S.n.H)]], keep.order=TRUE))
singular.test.lm <-lm(S.n.H[[length(S.n.H)]])
singular.terms <- names(coef(singular.test.lm )[is.na(coef(singular.test.lm ))])
# singular.terms <- gsub(":", " : ", singular.terms)
#singular.terms <- paste0(" [+] ", singular.terms)

S.n.H.cost.fn.string <- as.character(S.n.H[[length(S.n.H)]])[3]
S.n.H.cost.fn.string <- gsub("\n", "", S.n.H.cost.fn.string )
S.n.H.cost.fn.string <- gsub(" +", " ", S.n.H.cost.fn.string )
for ( i in singular.terms) {
  S.n.H.cost.fn.string <- gsub(paste0(" [+] ", i), "", S.n.H.cost.fn.string)
  rev.temp<-paste0(gsub("y[0-9]+:", "", i), ":", gsub(":y[0-9]+", "", i))
  S.n.H.cost.fn.string <- gsub(paste0(" [+] ", rev.temp), "", S.n.H.cost.fn.string)
}

S.n.H[[length(S.n.H)]] <- as.formula(paste0("ln.E.data ~ ", S.n.H.cost.fn.string))

kleinOls <- systemfit( S.n.H, "SUR", maxit = 1  )

singular.test.lm <-lm(terms(S.n.H[[length(S.n.H)]], keep.order=TRUE))

param.crossref.no.singular.df <- 
  param.crossref.df[param.crossref.df$lm %in% names(coef(singular.test.lm )), ]
lm.param.restrictions <- paste0("cost.fn_", param.crossref.no.singular.df$lm[!is.na(param.crossref.no.singular.df$lm.share)], " = ", 
  param.crossref.no.singular.df$lm.share[!is.na(param.crossref.no.singular.df$lm.share)]
)

linear.sur.est <- systemfit( S.n.H, "SUR", restrict.matrix = lm.param.restrictions,  maxit = 5000  )

# kleinOls.M6.N4 <- systemfit( S.n.H, "SUR", restrict.matrix = lm.param.restrictions,  maxit = 5000  )


# Next:
# maybe impose adding-up restrictions on linear SUR
# Need to find a way to link linear SUR parameter names to nonlinear SUR
# Need to zap singular variables in nonlinear string


names(linear.sur.est)


























grep(i, S.n.H.cost.fn.string)


data( "Kmenta" )
eqDemand <- consump ~ price + income
eqSupply <- consump ~ price + farmPrice + trend
system <- list( demand = eqDemand, supply = eqSupply )

restrict <- c( "demand_income - supply_trend = 0",
   "- demand_price + supply_price = 0.5", "goop = goopB" )
fitols2b <- systemfit( system, data = Kmenta, restrict.matrix = restrict )
print( fitols2b )

list( Consumption = eqConsump, Investment = eqInvest,
    PrivateWages = eqPrivWage )


# x*w/E
# TODO: pretty sure that this is cost share, but may want to double-check

-1 for no intercept

# ln.E.string <- ln.c

####################### IMPOSING ADDING-UP RESTRICTIONS
library(stringr)


ln.E.vars <- all.vars(as.formula(paste("ln.E.data ~", ln.E.string)))
ln.E.vars <- ln.E.vars[ !grepl("(w[0-9])|(y[0-9])|(ln.E.data)", ln.E.vars ) ]
ln.E.vars <- sort(ln.E.vars)

if (M>1) {

alphas.single <- sort(ln.E.vars[grepl("alpha[0-9][0-9]", ln.E.vars)])

ln.E.string <- str_replace_all(ln.E.string, "alpha01", paste0("(-(", paste0(alphas.single[-1], collapse=" + "), " - 1))" ) )
# from p. 4 of http://ageconsearch.umn.edu/bitstream/22027/1/sp03mo02.pdf

alpha.input.adding.up <- ln.E.vars[grepl("alpha[.][0-9][0-9]", ln.E.vars)]

alpha.adding.up.mat <-matrix(sort(alpha.input.adding.up ), ncol=M)

alpha.adding.up.mat[, 1] <-
  paste("(-(",
    apply(alpha.adding.up.mat[, -1], 1, paste, collapse=" + " ),
  "))" )
  
# data.frame(alpha.input.adding.up, c(alpha.adding.up.mat))

for ( i in 1:length(alpha.input.adding.up )) {
  ln.E.string <- str_replace_all(ln.E.string, alpha.input.adding.up[i], c(alpha.adding.up.mat)[i])
}

}

############




gsub("[*] theta[0-9]+", "", ln.E.string)

all.vars(as.formula(paste("ln.E.data ~", gsub("[*] theta[0-9]+", "", ln.E.string))))






gammas.mat<-matrix(paste0("gamma.", apply(X=expand.grid(lead.zero(1:max(N,M)), lead.zero(1:max(N,M))), MARGIN=1, FUN=paste, collapse=".")), nrow=max(N,M), ncol=max(N,M))
gammas.mat[upper.tri(gammas.mat, diag = FALSE)] <- t(gammas.mat)[upper.tri(gammas.mat, diag = FALSE)]
gammas.mat<-gammas.mat[1:J, 1:M]



first.line <- paste0( "args <- c(\"", paste(ln.E.vars, sep="\", \"", collapse="\", \""), "\")\nfor ( i in 1:length(args)) { assign(args[i], x[i])} ; ")

eval(parse(text=paste0("mod.predicted <- function(x) {", first.line, 
"  ret <- ", ln.E.string, "; ifelse(is.finite(ret), ret, 10^300) }")))

mod.predicted(ln.E.start.vals)


# ln.E.vars <- all.vars(nls.formula.ln.E)
ln.E.vars <- all.vars(as.formula(paste("ln.E.data ~", ln.E.string)))
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
fm1DNase1 <- nlsLM(nls.formula.ln.E, start=ln.E.start.vals, trace=TRUE, control=list(maxiter=5000, maxfev=2147483647) )


library(DEoptim)

mod <- function(x) x[1] + x[2]*x[3]^ExponCycles
fun <- function(x) sum((ExponValues-mod(x))^2)

ss <- DEoptim(fun, lower=rep(0,3), upper=c(10e7, 10, 10),
              control=list(trace=FALSE))

pa <- ss$optim$bestmem


eval(parse(text=paste0("mod <- function() { ", ln.c, " + ", ln.E.2nd, "}")))
fun <- function() sum((ln.E.data - mod())^2)









#### start here


first.line <- paste0( "args <- c(\"", paste(ln.E.vars, sep="\", \"", collapse="\", \""), "\")\nfor ( i in 1:length(args)) { assign(args[i], x[i])} ; ")

eval(parse(text=paste0("mod <- function(x) {", first.line, 
"  ret <- sum((ln.E.data - ", ln.E.string, ")^2); ifelse(is.finite(ret), ret, 10^300) }")))


#fun <- function() sum((ln.E.data - mod())^2)

ln.E.low.vals <- vector(mode="numeric", length=length(ln.E.vars))
names(ln.E.low.vals) <- ln.E.vars
ln.E.low.vals[grepl("(beta)|(gamma)|(alpha)", names(ln.E.low.vals))] <- .01
ln.E.low.vals[grepl("(alpha[.])|(gamma[.])", names(ln.E.low.vals))] <- -5
ln.E.low.vals[grepl("beta0", names(ln.E.low.vals))] <- - 10
ln.E.low.vals[grepl("theta", names(ln.E.low.vals))] <- .1
ln.E.high.vals <- vector(mode="numeric", length=length(ln.E.vars))
names(ln.E.high.vals) <- ln.E.vars
ln.E.high.vals[grepl("(beta)|(gamma)|(alpha)", names(ln.E.high.vals))] <- 5
ln.E.high.vals[grepl("theta", names(ln.E.high.vals))] <- 7



ln.E.low.vals <- vector(mode="numeric", length=length(ln.E.vars))
names(ln.E.low.vals) <- ln.E.vars
ln.E.low.vals[grepl("(beta)|(gamma)|(alpha)", names(ln.E.low.vals))] <- 10^-10
ln.E.low.vals[grepl("(alpha[.])|(gamma[.])", names(ln.E.low.vals))] <- -10^-10
ln.E.low.vals[grepl("beta0$", names(ln.E.low.vals))] <- - 10^3
ln.E.low.vals[grepl("theta", names(ln.E.low.vals))] <- 1
ln.E.high.vals <- vector(mode="numeric", length=length(ln.E.vars))
names(ln.E.high.vals) <- ln.E.vars
ln.E.high.vals[grepl("(beta)|(gamma)|(alpha)", names(ln.E.high.vals))] <- 2*10^-10
ln.E.high.vals[grepl("theta", names(ln.E.high.vals))] <- 1

ss <- DEoptim(mod, lower=ln.E.low.vals, upper=ln.E.high.vals,
              control=list(trace=TRUE, itermax=10))


x <- c(0.000168052,  2.85579e-05, 0.000177429,  9.5069e-05, -3.55729e-05,  4.36031e-05, -3.73951e-06,  0.00010519, -5.9171e-07,  0.000144837,   0.174041,  0.0345056,  0.000223896,  0.000190657,  0.000110123,  0.00013711,  0.000131213,  0.000225418,  0.00032942,  0.000159108,  0.000170218,  0.000109729,  3.25756e-05,  9.26746e-05, -0.000831097,  0.000130167,  0.000142167,  0.000316966,  0.000389458, -0.000116375,  0.000800904,  0.000136488,  6.74213e-05,  0.000108244,  4.35118e-05, -1.90977e-05,  0.000282828,   -303.722, -0.000629927,  0.000435498, -0.00198801,  0.00247938, -0.000963892,  0.00626949,  0.00681186,  0.00342678,  0.00898112, -0.000346027,  0.0306188,  0.0324538,  0.0414827,  0.0398277, -0.00381113,  0.00553564,  0.00579218,  0.00860441,  0.00561822, -0.00111591,    2.73775,    2.68718,    3.18559,    9.95825 ,  0.165479)

mod(x)
ln.E.low.vals

eval(parse(text=paste0("mod.predicted <- function(x) {", first.line, 
"  ret <- ", ln.E.string, "; ifelse(is.finite(ret), ret, 10^300) }")))

mod.predicted(ss$optim$bestmem)


#eval(parse(text=paste0("test.fn <- function(x) {", first.line, 
#"  ", ln.c, " + ", ln.E.2nd, " }")))
#test.fn(ln.E.high.vals) # pa

#mod(ln.E.high.vals)
# install.packages("corrgram")
library(corrgram)
corrgram( 
cor(data.frame(y01, y02, y03, y04, y05, x01, x02, x03, x04, x05))
, lower.panel=panel.shade, upper.panel=panel.pie)


pa <- ss$optim$bestmem

fm1DNase1 <- nlsLM(as.formula(paste0("ln.E.data ~ ", ln.E.string)), start=pa, trace=TRUE, lower=ifelse(grepl("theta", names(ln.E.low.vals)), 0, -Inf), control=list( maxiter=500) )
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

firm.df$revenue <- rowSums( 
  as.matrix(firm.df[, grepl("^price", colnames(firm.df))]) *
  as.matrix(firm.df[, grepl("^harvest", colnames(firm.df))])
  )
  
# trim.set <- quantile(firm.df$revenue, probs = c(.1, .9))
# do this once

firm.df<-firm.df[firm.df$revenue >= trim.set[1] & firm.df$revenue <= trim.set[2], ]

# firm.df <- firm.df[ firm.df$harvest.r.PAPA>0 & rowSums(firm.df[, grepl("harvest.r", colnames(firm.df))]) - firm.df$harvest.r.PAPA == 0, ]


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
p10 = firm.df$price.ARROZ
p11 = firm.df$price.QUINUA
p12 = firm.df$price.TRIGO

y01 = firm.df$harvest.r.PAPA 
y02 = firm.df$harvest.r.MAIZ
y03 = firm.df$harvest.r.PLATANO
y04 = firm.df$harvest.r.YUCA
y05 = firm.df$harvest.r.ARVEJA
y06 = firm.df$harvest.r.CEBADA
y07 = firm.df$harvest.r.CEBOLLA
y08 = firm.df$harvest.r.HABA
y09 = firm.df$harvest.r.OCA
y10 = firm.df$harvest.r.ARROZ
y11 = firm.df$harvest.r.QUINUA
y12 = firm.df$harvest.r.TRIGO

z1 = firm.df$land.area

profit= p01*y01 + p02*y02 + p03*y03 + p04*y04 + p05*y05 + p06*y06 + p07*y07 + p08*y08 + p09*y09 + 
  p10*y10 + p11*y11 + p12*y12 - ( w01*x01 + w02*x02 + w03*x03 + w04*x04 + w05*x05 )
  
profit.test= p01*y01 + p02*y02 + p03*y03 + p04*y04 + p05*y05 + p06*y06 + p07*y07 + p08*y08 + p09*y09 + 
  p10*y10 + p11*y11 + p12*y12 - ( w01*x01 + w02*x02 + w03*x03 + w04*x04 )

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
p10[p10==0] <- mean(p10[p10!=0]) + mean(p10[p10!=0])* rnorm(length(p10[p10==0]), mean = 0, sd = .1)
p11[p11==0] <- mean(p11[p11!=0]) + mean(p11[p11!=0])* rnorm(length(p11[p11==0]), mean = 0, sd = .1)
p12[p12==0] <- mean(p12[p12!=0]) + mean(p12[p12!=0])* rnorm(length(p12[p12==0]), mean = 0, sd = .1)














































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










data( "KleinI" )
eqConsump  <- consump ~ corpProf + corpProfLag + wages
eqInvest   <- invest ~ corpProf + corpProfLag + capitalLag
eqPrivWage <- privWage ~ gnp + gnpLag + trend
inst <- ~ govExp + taxes + govWage + trend + capitalLag + corpProfLag + gnpLag
system <- list( Consumption = eqConsump, Investment = eqInvest,
    PrivateWages = eqPrivWage )
# OLS estimation:
 kleinOls <- systemfit( system, data = KleinI, restrict.matrix = "Investment_(Intercept) = Consumption_(Intercept)" )
 round( coef( summary( kleinOls ) ), digits = 3 )
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
