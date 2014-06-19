
#TODO: Any adding up restrictions for fixed inputs?

library(systemfit)

# ln.E.data <- log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + w05*x05 )
# ln.E.data <- log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + 1  )
 
# ln.E.data <- log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + w05*x05 + 1 )

# M <- 5
# M <- 12

# Setting M relies on previous processing of firms.df to remove the extra columns according to which crops are actually in the subset

# M <- sum(grepl("harvest.r", colnames(firm.df))) 
# N <- 5
# J <- 2


M <- 1
N <- 6
J <- 3






lead.zero <- function(x) {formatC(x, width = 2, flag = "0")}

# llf.creator.fn <- function(M, N, Z) 

y.perm <- expand.grid(paste0("y", lead.zero(1:M)), paste0("y", lead.zero(1:M)))
M.2.dim <- gsub( "y", ".", do.call(paste0, y.perm))

N.2.dim <- expand.grid(paste0(".", lead.zero(1:N)), paste0(".", lead.zero(1:N)))

J.2.dim <- expand.grid(paste0(".", lead.zero(1:J)), paste0(".", lead.zero(1:J)))

ln.sh.w.grid <- expand.grid( paste0("log(w", lead.zero(1:N), " * theta", lead.zero(1:N), ") * "),
  paste0("log(w", lead.zero(1:N), " * theta", lead.zero(1:N), ")") )
  
ln.sh.q.grid <- expand.grid( paste0("log(q", lead.zero(1:J),  ") * "),
  paste0("log(q", lead.zero(1:J),  ")") )

ln.sh.w.grid.2 <- do.call(paste0, ln.sh.w.grid)

ln.sh.q.grid.2 <- do.call(paste0, ln.sh.q.grid)

M.N.dim <- expand.grid(paste0(".", lead.zero(1:M)), paste0(".", lead.zero(1:N)))

ym.wn <- expand.grid(paste0("y", lead.zero(1:M)), paste0("log(w", lead.zero(1:N), " * theta", lead.zero(1:N), ")"))

M.J.dim <- expand.grid(paste0(".", lead.zero(1:M)), paste0(".", lead.zero(1:J)))

ym.qj <- expand.grid(paste0("y", lead.zero(1:M)), paste0("log(q", lead.zero(1:J), ")"))

J.N.dim <- expand.grid(paste0(".", lead.zero(1:J)), paste0(".", lead.zero(1:N)))

qj.wn <- expand.grid(paste0("log(q", lead.zero(1:J), ")"), paste0("log(w", lead.zero(1:N), " * theta", lead.zero(1:N), ")"))

# data.frame(M.N.dim, ym.wn) # checks out  data.frame(M.J.dim, ym.qj)

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

ln.c.6 <-  paste0("zeta", lead.zero(1:J),  " * ", "log(q", lead.zero(1:J), ")", collapse=" + ")
 
ln.c.7 <-  paste0("zeta", do.call(paste0, J.2.dim ), " * ", ln.sh.q.grid.2, collapse=" + ")

ln.c.8 <-  paste0("kappa", do.call(paste0, J.N.dim), " * ", qj.wn[[1]], " * ", qj.wn[[2]], collapse=" + " )

ln.c.9 <-  paste0("delta", do.call(paste0, M.J.dim), " * ", ym.qj[[1]], " * ", ym.qj[[2]], collapse=" + " )

ln.c <- paste0("beta0 + ", ln.c.1, " + ", ln.c.2, " + (1/2) * (", ln.c.3, 
  ") + (1/2) * (", ln.c.4, ") + ", ln.c.5, " + ", ln.c.6, " + (1/2) * (", ln.c.7 ,
  ") + ", ln.c.8, " + ", ln.c.9)



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

kappa.special<-c()
kappa.mat<-matrix(1:(J*N), nrow=J, ncol=N)
for ( i in 1:N) {
  kappa.special[i] <- paste0(
    paste0("kappa", do.call(paste0, J.N.dim), " * ", qj.wn[[1]])[kappa.mat[, i]],
    collapse=" + " )
}


ln.E.2nd <- paste0( "log(" , 
  paste0(
  paste0("(w", lead.zero(1:N), " / (w", lead.zero(1:N), " * theta", 
   lead.zero(1:N), ")) * ", 
   "(beta", lead.zero(1:N), " + ",
   gamma.special, " + ",
   beta.special, " + ",
   kappa.special, ")"),
  collapse=" + " ), ")" )
   
# ln.E <- paste0("test.fn <- function(X) {", ln.c, " + ", ln.E.2nd, "}")





#test.nls <- nls(nls.formula.ln.E, trace=TRUE)

#ln.E.string <- paste(ln.c, " + ", ln.E.2nd)

ln.E.string <- ln.c


#### imposing symmetry restrictions

library(stringr)


ln.E.vars <- all.vars(as.formula(paste("ln.E.data ~", ln.E.string)))
ln.E.vars <- ln.E.vars[ !grepl("(w[0-9])|(q[0-9])|(y[0-9])|(ln.E.data)", ln.E.vars ) ]
ln.E.vars <- sort(ln.E.vars)


# replacements <- data.frame(greek=c("alpha", "beta", "gamma"), N=c(M,N,N), M=c(M,N,M))
# so do not actually need for gamma
replacements <- data.frame(greek=c("alpha", "beta", "zeta"), N=c(M,N,J), M=c(M,N,J))
if (J==1) {replacements <- replacements[1:2, ] }


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
ln.c.linear <- gsub("zeta[0-9]+ [*] ", "", ln.c.linear)
ln.c.linear <- gsub("zeta.[0-9]+.[0-9]+ [*]", "", ln.c.linear)
ln.c.linear <- gsub("kappa.[0-9]+.[0-9]+ [*]", "", ln.c.linear)
ln.c.linear <- gsub("delta.[0-9]+.[0-9]+ [*]", "", ln.c.linear)

ln.c.linear <- gsub("[(]1/2[)] [*]", "", ln.c.linear)
# we should just scale the parameters to get the 1/2 part down
ln.c.linear <- gsub("[*]", ":", ln.c.linear)
# convert to single interaction term
for (i in 1:max(c(N,M))) {
  temp.y <- paste0("y", lead.zero(i))
  ln.c.linear <- gsub(paste0(temp.y, " : ", temp.y), paste0("I(",temp.y, "^2)"), ln.c.linear)
  temp.w <- paste0("w", lead.zero(i))
  ln.c.linear <- gsub(paste0("log[(]", temp.w, "[)] : log[(]", temp.w, "[)]"), paste0("I(log(", temp.w, ")^2)"), ln.c.linear)
  temp.q <- paste0("q", lead.zero(i))
  ln.c.linear <- gsub(paste0("log[(]", temp.q, "[)] : log[(]", temp.q, "[)]"), paste0("I(log(", temp.q, ")^2)"), ln.c.linear)
}

#test.lm <-lm(as.formula(paste0("ln.E.data ~", ln.c.linear)))

# So we can impose linear restrictions after all


S.n.H <- list()

for ( n in 1:N) {
 S.n.H[[n]] <- as.formula(
   paste0( "I( (x", lead.zero(n), " * ", "w", lead.zero(n), ")/exp(ln.E.data)) ~ ",
    paste0("log(w", lead.zero(1:N), ")", collapse=" + "), " + ", 
    paste0("y", lead.zero(1:M), collapse=" + "), " + ", 
    paste0("log(q", lead.zero(1:J), ")", collapse=" + ")
   )
  )
  names(S.n.H)[n] <- paste0("S.n.H.", lead.zero(n))
}
S.n.H[[1]] <- NULL


S.n.H[[length(S.n.H)+1]] <- as.formula(paste0("ln.E.data ~", ln.c.linear))
names(S.n.H)[length(S.n.H)] <- "cost.fn"


test.lm <-lm(terms(S.n.H[[length(S.n.H)]], keep.order=TRUE))

length(attr(terms(S.n.H[[length(S.n.H)]], keep.order=TRUE),"term.labels"))


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
ln.E.vars <- ln.E.vars[ !grepl("(theta[0-9])|(w[0-9])|(q[0-9])|(y[0-9])|(ln.E.data)", ln.E.vars ) ]
ln.E.vars
param.crossref.df <- data.frame(nlm=ln.E.vars, lm=names(coef(test.lm)), lm.share=NA, stringsAsFactors=FALSE)
# already has symmetry

for ( i in grep("kappa", param.crossref.df$nlm)) {
  param.crossref.df$lm[i] <- paste0(gsub(".+:", "", param.crossref.df$lm[i]), ":", gsub(":.+", "", param.crossref.df$lm[i]) )
}



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
  for ( j in lead.zero(1:J)) {
      param.crossref.df[
      param.crossref.df$nlm==paste0("kappa.", j, ".", i), "lm.share"] <-
    paste0("S.n.H.", i, "_", "log(q", j, ")")
  }
}

#singular.test.lm <-lm(terms(S.n.H[[length(S.n.H)]], keep.order=TRUE))
orig.singular.model <- S.n.H[[length(S.n.H)]]
singular.test.lm <-lm(orig.singular.model)
singular.terms <- names(coef(singular.test.lm )[is.na(coef(singular.test.lm ))])
# singular.terms <- gsub(":", " : ", singular.terms)
#singular.terms <- paste0(" [+] ", singular.terms)

S.n.H.cost.fn.string <- as.character(S.n.H[[length(S.n.H)]])[3]
S.n.H.cost.fn.string <- gsub("\n", "", S.n.H.cost.fn.string )
S.n.H.cost.fn.string <- gsub(" +", " ", S.n.H.cost.fn.string )


# linear.terms.for.adding.up <-lm(terms(S.n.H[[length(S.n.H)]], keep.order=TRUE))

linear.terms.for.adding.up <-lm(terms(as.formula(paste0("ln.E.data ~ ", S.n.H.cost.fn.string)), keep.order=TRUE))

linear.terms.for.adding.up <- paste0(names(coef(linear.terms.for.adding.up)), collapse=" + ")

S.n.H.cost.fn.string <- gsub("[(]Intercept[)] .", "",  linear.terms.for.adding.up)


for ( i in 1:M) {
#  S.n.H.cost.fn.string <- gsub(paste0(":y", lead.zero(i)),  
#    paste0(":I(0.5*y", lead.zero(i), ")"), S.n.H.cost.fn.string)
# So the cross terms are supposed to appear twice, but don't, so we can choose not to divide by 0.5 and get the same effect. The squared terms are supposed to appear only once.
  S.n.H.cost.fn.string <- gsub(paste0("I[(]y", lead.zero(i), ".2[)]"),   
    paste0("I(0.5*y", lead.zero(i), "^2)"), S.n.H.cost.fn.string)  
    
}



for ( i in 1:N) {
#  S.n.H.cost.fn.string <- gsub(paste0("log[(]w", lead.zero(i), "[)]:"),  
#    paste0("I(0.5*log(w", lead.zero(i), ")):"), S.n.H.cost.fn.string)
  S.n.H.cost.fn.string <- gsub(paste0("I[(]log[(]w", lead.zero(i), "[)].2[)]"),   
    paste0("I(0.5*log(w", lead.zero(i), ")^2)"), S.n.H.cost.fn.string) 
}

for ( i in 1:J) {
#  S.n.H.cost.fn.string <- gsub(paste0("log[(]w", lead.zero(i), "[)]:"),  
#    paste0("I(0.5*log(w", lead.zero(i), ")):"), S.n.H.cost.fn.string)
  S.n.H.cost.fn.string <- gsub(paste0("I[(]log[(]q", lead.zero(i), "[)].2[)]"),   
    paste0("I(0.5*log(q", lead.zero(i), ")^2)"), S.n.H.cost.fn.string) 
}

linear.terms.for.adding.up <- S.n.H.cost.fn.string


#length(attr(terms(as.formula(paste0("ln.E.data ~ ", S.n.H.cost.fn.string)), keep.order=TRUE), "term.labels"))

#names(coef(linear.terms.for.adding.up))

# there are a bunch of duplicates or something

orig.singular.model <- as.formula(paste0("ln.E.data ~ ", S.n.H.cost.fn.string))
singular.test.lm <-lm(orig.singular.model)
singular.terms <- names(coef(singular.test.lm )[is.na(coef(singular.test.lm ))])


# gsub("(9)", "", "6v ghu676b(9)", fixed=TRUE)

for ( i in singular.terms) {
  S.n.H.cost.fn.string <- gsub(paste0(" + ", gsub(" ", "", i)), "", S.n.H.cost.fn.string, fixed=TRUE)
  #rev.temp<-paste0(gsub("y[0-9]+:", "", i), ":", gsub(":y[0-9]+", "", i))
  # I don't think I need the reversal anymore
  # S.n.H.cost.fn.string <- gsub(paste0(" [+] ", rev.temp), "", S.n.H.cost.fn.string, fixed=TRUE)
}


# S.n.H.cost.fn.string <-gsub(" : ", ":", S.n.H.cost.fn.string)



S.n.H[[length(S.n.H)]] <- as.formula(paste0("ln.E.data ~ ", S.n.H.cost.fn.string))

# kleinOls <- systemfit( S.n.H, "SUR", maxit = 1  )


singular.test.lm <-lm(terms(S.n.H[[length(S.n.H)]], keep.order=TRUE))

singular.test.lm.names <- names(coef(singular.test.lm ))

for ( i in grep("log[(]q[0-9][0-9][)]:log[(]w[0-9][0-9][)]", param.crossref.df$lm) ) {
  param.crossref.df$lm[i] <- paste0(gsub(".+:", "", param.crossref.df$lm[i]), ":", gsub(":.+", "", param.crossref.df$lm[i]) )
}

param.crossref.no.singular.df <- 
  param.crossref.df[param.crossref.df$lm %in% singular.test.lm.names, ]
  
lm.param.restrictions <- paste0("cost.fn_", param.crossref.no.singular.df$lm[!is.na(param.crossref.no.singular.df$lm.share)], " = ", 
  param.crossref.no.singular.df$lm.share[!is.na(param.crossref.no.singular.df$lm.share)]
)

lm.param.restrictions <- c(lm.param.restrictions, 
  paste0("cost.fn_I(0.5 * log(w", lead.zero(2:N), ")^2) = S.n.H.", lead.zero(2:N), "_log(w", lead.zero(2:N),")")
)

#if (J>1) {
#lm.param.restrictions <- c(lm.param.restrictions, 
#  paste0("cost.fn_I(0.5 * log(q", lead.zero(2:J), ")^2) = S.n.H.", lead.zero(2:J), "_log(w", #lead.zero(2:N),")")
#)
#}
# Above we never correct - we dont have that restriction since w00 does not appear in the  q00 cross terms

# REFERENCE: Kumbhaker & Lovell 2000, p. 223 for parameter restrictions and 178, footnote 10 of Reinhard and Thijssen

# ln.lm.E.vars <- names(coef(test.lm))
# ln.lm.E.vars <- names(coef(linear.terms.for.adding.up))
# ln.lm.E.vars <- names(coef(linear.terms.for.adding.up))
# add up across 2nd number (w) for kappa

ln.lm.E.vars <- attr(terms(as.formula(
  paste0("ln.E.data ~ ",  gsub("[(]Intercept[)] .", "",  linear.terms.for.adding.up))
  ), keep.order=TRUE), "term.labels")


#for ( i in grep("log[(]w[0-9][0-9][)]:log[(]q[0-9][0-9][)]", ln.lm.E.vars) ) {
#  ln.lm.E.vars[i] <- paste0(gsub(".+:", "", ln.lm.E.vars[i]), ":", gsub(":.+", "", ln.lm.E.vars[i]) )
#}


# ln.c.linear

# REFERENCE: http://www.jstor.org/stable/2328171
# Single betas sum to 1
# Double betas sum to 0 in 1st dimension
# gammas sum to zero in 2nd (w[xx]) direction




# if (M>1) {

betas.single <- sort(ln.lm.E.vars[grepl("^log[(]w[0-9][0-9][)]$", ln.lm.E.vars)])

 ln.E.string <- str_replace_all(ln.E.string, "beta01", paste0("(-(", paste0(betas.single[-1], collapse=" + "), " - 1))" ) )
# OK, this reference may be wrong, but the code above is right: p. 4 of http://ageconsearch.umn.edu/bitstream/22027/1/sp03mo02.pdf

single.beta.restriction <- paste0( paste0("cost.fn_", betas.single, collapse=" + "), " = 1" )



# beta.input.adding.up <- ln.lm.E.vars[grepl("(I[(]0.5 . log[(]w[0-9][0-9][)].2[)])|(log[(]w[0-9][0-9][)].I[(]0.5 . log[(]w[0-9][0-9][)][)])" , ln.lm.E.vars )]

beta.input.adding.up <- ln.lm.E.vars[grepl("(I[(]0.5 . log[(]w[0-9][0-9][)].2[)])|(log[(]w[0-9][0-9][)].log[(]w[0-9][0-9][)])" , ln.lm.E.vars )]

# "log(w02):I(0.5 * log(w01))"
# "I(0.5 * log(w02)^2)"


m2 <- matrix("",  ncol=N, nrow=N)
lower.tri(m2)
m2[lower.tri(m2, diag=TRUE)] <- beta.input.adding.up
ind <- upper.tri(m2) 
m2[ind] <- t(m2)[ind] 
# Thanks to http://r.789695.n4.nabble.com/Symmetric-matrix-td853754.html

# m2[m2 %in% names(coef(test.lm))[is.na(coef(test.lm))] ] <- NA
# m2[m2 %in% names(coef(singular.test.lm )[is.na(coef(singular.test.lm ))]) ] <- NA
m2[m2 %in% singular.terms ] <- NA



beta.adding.up.mat <- matrix(paste0("cost.fn_", m2), ncol=N, nrow=N)

# y02:I(0.5 * y04)
# I(0.5 * y02^2)

double.beta.restrictions <-
  paste("",
    apply(beta.adding.up.mat, 1, paste, collapse=" + " ),
  " = 0" )
  
double.beta.restrictions <- gsub("[+] cost.fn_NA", "", double.beta.restrictions)
double.beta.restrictions <- gsub("cost.fn_NA [+] ", "", double.beta.restrictions)



gamma.input.adding.up <- ln.lm.E.vars[grepl("y[0-9][0-9].log[(]w[0-9][0-9][)]" , ln.lm.E.vars )]

# "log(w02):I(0.5 * log(w01))"
# "I(0.5 * log(w02)^2)"

gamma.adding.up.mat <- matrix(paste0("cost.fn_", gamma.input.adding.up), nrow=M, ncol=N)

for ( i in singular.terms) {
  gamma.adding.up.mat <- gsub(i , "NA", gamma.adding.up.mat, fixed=TRUE)
}


# y02:I(0.5 * y04)
# I(0.5 * y02^2)

double.gamma.restrictions <-
  paste("",
    apply(gamma.adding.up.mat, 1, paste, collapse=" + " ),
  " = 0" )
  
double.gamma.restrictions <- gsub("[+] cost.fn_NA", "", double.gamma.restrictions)
double.gamma.restrictions <- gsub("cost.fn_NA [+] ", "", double.gamma.restrictions)
double.gamma.restrictions <- gsub("cost.fn_NA", "", double.gamma.restrictions)

double.gamma.restrictions <- double.gamma.restrictions[grepl("cost.fn", double.gamma.restrictions)]


kappa.input.adding.up <- ln.lm.E.vars[grepl("log[(]w[0-9][0-9][)].log[(]q[0-9][0-9][)]" , ln.lm.E.vars )]

kappa.adding.up.mat <- matrix(paste0("cost.fn_", kappa.input.adding.up), nrow=J, ncol=N)

for ( i in singular.terms) {
  kappa.adding.up.mat <- gsub(i , "NA", kappa.adding.up.mat, fixed=TRUE)
}

double.kappa.restrictions <-
  paste("",
    apply(kappa.adding.up.mat, 1, paste, collapse=" + " ),
  " = 0" )
  
double.kappa.restrictions <- gsub("[+] cost.fn_NA", "", double.kappa.restrictions)
double.kappa.restrictions <- gsub("cost.fn_NA [+] ", "", double.kappa.restrictions)
double.kappa.restrictions <- gsub("cost.fn_NA", "", double.kappa.restrictions)

double.kappa.restrictions <- double.kappa.restrictions[grepl("cost.fn", double.kappa.restrictions)]


# }

lm.param.restrictions <- c(lm.param.restrictions, single.beta.restriction , double.beta.restrictions, double.gamma.restrictions, double.kappa.restrictions    )


linear.sur.est <- systemfit( S.n.H, "SUR", restrict.matrix = lm.param.restrictions,  maxit = 5000 )


# TODO: say in paper that restrictions ensure that null hypothesis means that the shadow is same as orig


# lm.param.restrictions <- lm.param.restrictions[-c(31, 33)]


