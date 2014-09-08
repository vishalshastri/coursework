

############# MUST START HERE when constructing nonlinear string



ln.c <- paste0("beta0 + ", ln.c.1, " + ", ln.c.2, " + (1/2) * (", ln.c.3, 
") + (1/2) * (", ln.c.4, ") + ", ln.c.5)

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
  paste0(" (w", lead.zero(1:N), " / (w", lead.zero(1:N), " * theta", 
   lead.zero(1:N), ")) * ", 
   "(beta", lead.zero(1:N), " + ",
   gamma.special, " + ",
   beta.special, " + ",
   kappa.special, ")"),
  collapse=" + " ), ")" )
   
# ln.E <- paste0("test.fn <- function(X) {", ln.c, " + ", ln.E.2nd, "}")


#ln.E.data <- log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + w05*x05 )
# ln.E.data <- log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + 2  )

#test.nls <- nls(nls.formula.ln.E, trace=TRUE)

#ln.E.string <- ln.c

# ln.E.string <- ln.c
# TODO: Restore this to the below when it is go-time - DONE
 ln.E.string <- paste(ln.c, " + ", ln.E.2nd)

ln.E.string.before.symm <-  ln.E.string 
# ln.E.string.before.symm <-  paste(ln.c, " + ", ln.E.2nd)



library(stringr)

####################### IMPOSING SYMMETRY RESTRICTIONS



#replacements <- data.frame(greek=c("alpha", "beta", "zeta", "gamma"), N=c(M,N,J,N), M=c(M,N,J,M))
#replacements <- replacements[1:3, ]
# so do not actually need for gamma

#for ( k in 1:nrow(replacements) ) {

#if (replacements$greek[k]=="alpha" & M==1) {next}

#symm <- ln.E.vars[grepl(paste0(replacements$greek[k], "[.]"), ln.E.vars) ]

#symm.mat<-matrix(paste0(replacements$greek[k], ".", apply(X=expand.grid(lead.zero(1:max(N,M)), #lead.zero(1:max(N,M))), MARGIN=1, FUN=paste, collapse=".")), nrow=max(N,M), ncol=max(N,M))

# ok the below is trying to reverse it:
#symm.mat<-matrix(paste0(replacements$greek[k], ".", apply(X=expand.grid(lead.zero(1:max(N,M)), #lead.zero(1:max(N,M))), MARGIN=1, FUN=paste, collapse=".")), nrow=max(N,M), ncol=max(N,M))

#symm.mat[upper.tri(symm.mat, diag = FALSE)] <- t(symm.mat)[upper.tri(symm.mat, diag = FALSE)]
#symm.mat<-symm.mat[1:replacements$N[k], 1:replacements$M[k]]

# data.frame(symm, c(symm.mat))

#for ( i in 1:length(symm)) {
#  ln.E.string <- str_replace_all(ln.E.string, symm[i], c(symm.mat)[i])
#}

#}

# It seems symmetry has already been imposed









####################### IMPOSING SYMMETRY RESTRICTIONS Before adding-up



replacements <- data.frame(greek=c("alpha", "beta", "zeta", "gamma"), N=c(M,N,J,N), M=c(M,N,J,M))
replacements <- replacements[1:3, ]
# so do not actually need for gamma

for ( k in 1:nrow(replacements) ) {

if (replacements$greek[k]=="alpha" & M==1) {next}

if (replacements$greek[k]=="zeta" & J==1) {next}

# ok the below is trying to reverse it:
symm.mat<-matrix(paste0(replacements$greek[k], ".", apply(X=expand.grid(lead.zero(1:max(N,M)), lead.zero(1:max(N,M))), MARGIN=1, FUN=paste, collapse=".")), nrow=max(N,M), ncol=max(N,M))

symm.mat[upper.tri(symm.mat, diag = FALSE)] <- t(symm.mat)[upper.tri(symm.mat, diag = FALSE)]
symm.mat<-symm.mat[1:replacements$N[k], 1:replacements$M[k]]


expanded.greeks.grid <- expand.grid(lead.zero(1:max(N,M)), lead.zero(1:max(N,M)))
expanded.greeks.grid <- data.frame(expanded.greeks.grid$Var2, expanded.greeks.grid$Var1)

symm.mat.2<-matrix(paste0(replacements$greek[k], ".", apply(X=expanded.greeks.grid, MARGIN=1, FUN=paste, collapse=".")), nrow=max(N,M), ncol=max(N,M))

symm.mat.2[upper.tri(symm.mat.2, diag = FALSE)] <- t(symm.mat.2)[upper.tri(symm.mat.2, diag = FALSE)]
symm.mat.2<-symm.mat.2[1:replacements$N[k], 1:replacements$M[k]]



if (replacements$greek[k]=="alpha") {
for ( i in 1:length(c(symm.mat.2))) {
  ln.E.string <- str_replace_all(ln.E.string, c(symm.mat.2)[i], c(symm.mat)[i])
}
}

if (replacements$greek[k]=="beta") {
for ( i in 1:length(c(symm.mat.2))) {
  ln.E.string <- str_replace_all(ln.E.string, c(symm.mat)[i], c(symm.mat.2)[i])
}
}

if (replacements$greek[k]=="zeta") {
for ( i in 1:length(c(symm.mat.2))) {
  ln.E.string <- str_replace_all(ln.E.string, c(symm.mat)[i], c(symm.mat.2)[i])
}
}


}













####################### IMPOSING ADDING-UP RESTRICTIONS



ln.E.vars <- all.vars(as.formula(paste("ln.E.data ~", ln.E.string.before.symm)))
ln.E.vars <- ln.E.vars[ !grepl("(w[0-9])|(y[0-9])|(ln.E.data)", ln.E.vars ) ]
ln.E.vars <- sort(ln.E.vars)

# used to have "if (M>1) {" here, but we need to run this part below because they deal with the w's

betas.single <- sort(ln.E.vars[grepl("beta[0-9][0-9]", ln.E.vars)])

ln.E.string <- str_replace_all(ln.E.string, "beta01", paste0("(-(", paste0(betas.single[-1], collapse=" + "), " - 1))" ) )
# from p. 4 of http://ageconsearch.umn.edu/bitstream/22027/1/sp03mo02.pdf

beta.input.adding.up <- sort( ln.E.vars[grepl("beta[.][0-9][0-9]", ln.E.vars)] )

beta.adding.up.mat <-matrix(sort(beta.input.adding.up ), ncol=N)

beta.adding.up.mat[, 1] <-
  paste("(-(",
    apply(beta.adding.up.mat[, -1], 1, paste, collapse=" + " ),
  "))" )

symm.mat<-beta.adding.up.mat

k <- 2

#symm.mat<-matrix(paste0(replacements$greek[k], ".", apply(X=expand.grid(lead.zero(1:max(N,M)), lead.zero(1:max(N,M))), MARGIN=1, FUN=paste, collapse=".")), nrow=max(N,M), ncol=max(N,M))

# ok the below is trying to reverse it:
#symm.mat<-matrix(paste0(replacements$greek[k], ".", apply(X=expand.grid(lead.zero(1:max(N,M)), lead.zero(1:max(N,M))), MARGIN=1, FUN=paste, collapse=".")), nrow=max(N,M), ncol=max(N,M))

symm.mat[upper.tri(symm.mat, diag = FALSE)] <- t(symm.mat)[upper.tri(symm.mat, diag = FALSE)]
symm.mat<-symm.mat[1:replacements$N[k], 1:replacements$M[k]]  

symm.mat[1,1] <- paste0("(-( ", paste0(c(symm.mat[-1, -1]), collapse= " + "), " ))")
# used to do unique()

beta.adding.up.mat <- symm.mat
  
# data.frame(alpha.input.adding.up, c(alpha.adding.up.mat))

for ( i in 1:length(beta.input.adding.up )) {
  ln.E.string <- str_replace_all(ln.E.string, beta.input.adding.up[i], c(beta.adding.up.mat)[i])
}

# if (M>1) {

gamma.input.adding.up <- sort( ln.E.vars[grepl("gamma[.][0-9][0-9]", ln.E.vars)] )

gamma.adding.up.mat <-matrix(sort(gamma.input.adding.up ), ncol=M, byrow=FALSE)

gamma.adding.up.mat[1, ] <-
  paste("(-(",
    apply(gamma.adding.up.mat[-1, , drop=FALSE], 2, paste, collapse=" + " ),
  "))" )
  
# data.frame(alpha.input.adding.up, c(alpha.adding.up.mat))

for ( i in 1:length(gamma.input.adding.up)) {
  ln.E.string <- str_replace_all(ln.E.string, gamma.input.adding.up[i], c(gamma.adding.up.mat)[i])
}


# }



share.denominator <- str_extract(ln.E.string, "log[(] .*")

share.denominator <- sub("log[(] ", "", share.denominator )
share.denominator <- sub(")$", "", share.denominator )

share.numerators <- strsplit(share.denominator, "[(]w[0-9][0-9] / [(]w[0-9][0-9] [*] theta[0-9][0-9][)][)] [*]")[[1]][-1]

share.numerators <- gsub("([+] $)|([+]  $)", "", share.numerators)

share.numerators <- paste0(
paste0( "(w", lead.zero(1:N), " / (w", lead.zero(1:N), " * theta", lead.zero(1:N), ")) *"),
share.numerators
)

share.numerators <- paste0("(", share.numerators, ")")

share.denominator <- paste0("(", share.denominator, ")")



S.n <- list()

for ( n in 1:N) {

 S.n[[n]] <- 
   paste0( "I( (x", lead.zero(n), " * ", "w", lead.zero(n), ")/exp(ln.E.data)) ~ ",
    share.numerators[n], " / ",
    share.denominator
   )
  
  names(S.n)[n] <- paste0("S.n", lead.zero(n))
}


S.n[[1]] <- NULL



















######### NOW ADDING COST SHARE EQUATIONS
####################################
####################################



#gamma.special<-c()
#gamma.mat<-matrix(1:(M*N), nrow=M, ncol=N)
#for ( i in 1:N) {
#  gamma.special[i] <- paste0(
#    paste0("gamma", sort(do.call(paste0, M.N.dim[1:2])), " * ", ym.wn[[1]])[gamma.mat[, i]],
#    collapse=" + " )
#}

#beta.special<-c()
#beta.mat<-matrix(1:(N*N), nrow=N, ncol=N)
#for ( i in 1:N) {
#  beta.special[i] <- paste0(
#    paste0("beta", do.call(paste0, N.2.dim[1:2]) , " * ", gsub("[*] $", "", #ln.sh.w.grid[[1]]))[beta.mat[, i]],
#    collapse=" + " )
#}


#kappa.special<-c()
#kappa.mat<-matrix(1:(J*N), nrow=J, ncol=N)
#for ( i in 1:N) {
#  kappa.special[i] <- paste0(
#    paste0("kappa", do.call(paste0, J.N.dim), " * ", qj.wn[[1]])[kappa.mat[, i]],
#    collapse=" + " )
#}


#S.n.divisor <- paste0( "" , 
#  paste0(
#  paste0(" (w", lead.zero(1:N), " / (w", lead.zero(1:N), " * theta", 
#   lead.zero(1:N), ")) * ", 
#   "(beta", lead.zero(1:N), " + ",
#   gamma.special, " + ",
#   beta.special, " + ",
#   kappa.special, ")"),
#  collapse=" + " ), "" )
   
# ln.E <- paste0("test.fn <- function(X) {", ln.c, " + ", ln.E.2nd, "}")

#S.n.top <- paste0(" w", lead.zero(1:N), " / (w", lead.zero(1:N), " * theta", 
#   lead.zero(1:N), ")" )





#S.n <- list()

#for ( n in 1:N) {
#  betas.for.S.n <- paste0("beta.", lead.zero(n), ".", lead.zero(1:N) )
#  gammas.for.S.n <- paste0("gamma.", lead.zero(1:M), ".", lead.zero(n))
#  kappas.for.S.n <- paste0("kappa.", lead.zero(1:J), ".", lead.zero(n))
 
# S.n[[n]] <- 
#   paste0( "I( (x", lead.zero(n), " * ", "w", lead.zero(n), ")/exp(ln.E.data)) ~ (",
#    "beta", lead.zero(n), " + ",
#    paste0( betas.for.S.n, " * " , " log(w", lead.zero(1:N), " * ", "theta", lead.zero(1:N),  ")", collapse=" + "), " + ", 
#    paste0(gammas.for.S.n, " * ", "y", lead.zero(1:M), collapse=" + "), " + ", 
    # TODO: NOTE: Needed to switch this to lead.zero(1:M) from lead.zero(1:N). This was a definite error from before
#    paste0(kappas.for.S.n, " * ", "log(q", lead.zero(1:J), ")", collapse=" + "),
#    " ) * (",
#    S.n.top[n], ") / (", S.n.divisor, ")"
#   )
  
#  names(S.n)[n] <- paste0("S.n", lead.zero(n))
#}


#S.n[[1]] <- NULL

# grepl('gamma.01.05', as.character(S.n[[1]][3]) )




#for (j in 1:length(S.n) ) {

#targ.S.n <- S.n[[j]]


#targ.S.n.vars <- all.vars(as.formula(targ.S.n))
#targ.S.n.vars <- targ.S.n.vars[ !grepl("(x[0-9])|(q[0-9])|(w[0-9])|(y[0-9])|(ln.E.data)", #targ.S.n.vars ) ]
#targ.S.n.vars <- sort(targ.S.n.vars)

#if (M>1) {

#betas.single <- sort(targ.S.n.vars[grepl("beta[0-9][0-9]", targ.S.n.vars)])

#targ.S.n <- str_replace_all(targ.S.n, "beta01", paste0("(-(", paste0(betas.single[-1], collapse=" + "), " - 1))" ) )
# from p. 4 of http://ageconsearch.umn.edu/bitstream/22027/1/sp03mo02.pdf

# beta.input.adding.up <- sort( targ.S.n.vars[grepl("beta[.][0-9][0-9]", targ.S.n.vars)] )

#beta.adding.up.mat <-matrix(sort(beta.input.adding.up ), ncol=N)

#beta.adding.up.mat[, 1] <-
#  paste("(-(",
#    apply(beta.adding.up.mat[, -1], 1, paste, collapse=" + " ),
#  "))" )
  
# data.frame(alpha.input.adding.up, c(alpha.adding.up.mat))

#for ( i in 1:length(beta.input.adding.up )) {
#  targ.S.n <- str_replace_all(targ.S.n, beta.input.adding.up[i], c(beta.adding.up.mat)[i])
#}


# gamma.input.adding.up <- sort( targ.S.n.vars[grepl("gamma[.][0-9][0-9]", targ.S.n.vars)] )

# gamma.adding.up.mat <-matrix(sort(gamma.input.adding.up ), ncol=M, byrow=FALSE)

# gamma.adding.up.mat[1, ] <-
#   paste("(-(",
#     apply(gamma.adding.up.mat[-1, ], 2, paste, collapse=" + " ),
#   "))" )
  
# data.frame(alpha.input.adding.up, c(alpha.adding.up.mat))

#for ( i in 1:length(gamma.input.adding.up)) {
#  targ.S.n <- str_replace_all(targ.S.n, gamma.input.adding.up[i], c(gamma.adding.up.mat)[i])
#}


#}




####################### IMPOSING SYMMETRY RESTRICTIONS again after adding-up restrictions



#replacements <- data.frame(greek=c("alpha", "beta", "gamma"), N=c(M,N,N), M=c(M,N,M))
#replacements <- replacements[1:2, ]
# so do not actually need for gamma

#for ( k in 1:nrow(replacements) ) {

#if (replacements$greek[k]=="alpha" & M==1) {next}

# ok the below is trying to reverse it:
#symm.mat<-matrix(paste0(replacements$greek[k], ".", apply(X=expand.grid(lead.zero(1:max(N,M)), lead.zero(1:max(N,M))), MARGIN=1, FUN=paste, collapse=".")), nrow=max(N,M), ncol=max(N,M))

#symm.mat[upper.tri(symm.mat, diag = FALSE)] <- t(symm.mat)[upper.tri(symm.mat, diag = FALSE)]
#symm.mat<-symm.mat[1:replacements$N[k], 1:replacements$M[k]]


#expanded.greeks.grid <- expand.grid(lead.zero(1:max(N,M)), lead.zero(1:max(N,M)))
#expanded.greeks.grid <- data.frame(expanded.greeks.grid$Var2, expanded.greeks.grid$Var1)

#symm.mat.2<-matrix(paste0(replacements$greek[k], ".", apply(X=expanded.greeks.grid, MARGIN=1, FUN=paste, collapse=".")), nrow=max(N,M), ncol=max(N,M))

#symm.mat.2[upper.tri(symm.mat.2, diag = FALSE)] <- t(symm.mat.2)[upper.tri(symm.mat.2, diag = FALSE)]
#symm.mat.2<-symm.mat.2[1:replacements$N[k], 1:replacements$M[k]]



#if (replacements$greek[k]=="alpha") {
#for ( i in 1:length(c(symm.mat.2))) {
#  targ.S.n <- str_replace_all(targ.S.n, c(symm.mat.2)[i], c(symm.mat)[i])
#}
#}

#if (replacements$greek[k]=="beta") {
#for ( i in 1:length(c(symm.mat.2))) {
#  targ.S.n <- str_replace_all(targ.S.n, c(symm.mat)[i], c(symm.mat.2)[i])
#}
#}


#}


#S.n[[j]] <- targ.S.n

#}






####################################
####################################




singular.test.lm <-lm(orig.singular.model)
singular.terms <- names(coef(singular.test.lm )[is.na(coef(singular.test.lm ))])

print(c("SINGULAR TERMS", singular.terms))

# If we dont have <if> statement, then we will get something like this in ln.E.string:  * y01 * y0102.01 * y02 * y0103.01 * y03 * y0104.01 * y04 * y0105.01 * y05 * y0102.01 * y01 * y0202.02 * y02 * y0203.02 * y03 * y0204.02 * y04 * y0205.02 * y05 * y0203.01 
if (length(singular.terms)>0 && !any(grepl("q", singular.terms)) ) {

singular.terms <- gsub("(I[(]0.5 . )|([)])", "", singular.terms )



for ( i in singular.terms) {
  singular.terms<-c(singular.terms, 
    paste0(gsub("y[0-9]+:", "", i), ":", gsub(":y[0-9]+", "", i)) )
}

#for ( i in singular.terms) {
#  singular.terms<-c(singular.terms, 
#    paste0(gsub("q[0-9]+:", "", i), ":", gsub(":q[0-9]+", "", i)) )
#}

# TODO: for now, we will just set the irrigation so it is not singular

singular.terms <- gsub(":", " . ", singular.terms )

# Ok, this will give an error if one of the crops never uses a particular input

#for ( i in singular.terms) {
#   ln.E.string <- 
#     gsub(paste0("alpha[.][0-9]{2}[.][0-9]{2} . ", i, " . "), "", ln.E.string)
#}

 singular.terms <- paste0("alpha.", gsub("( )|(y)", "", singular.terms) )

singular.terms <- c(singular.terms, 
  paste0("alpha", gsub("^alpha.[0-9][0-9]", "", singular.terms), 
    gsub("(alpha)|(.[0-9][0-9]$)", "", singular.terms)) )

for ( i in singular.terms) {
   ln.E.string <- 
     gsub(paste0("alpha[.][0-9]{2}[.][0-9]{2} . ", i, " . "), "", ln.E.string)
}

singular.terms <- paste0( gsub("( )|(y)", "", singular.terms) )

for ( i in singular.terms) {
   ln.E.string <- gsub(paste0(i, " . " ), "", ln.E.string)
   ln.E.string <- gsub(paste0(" . ", i ), "", ln.E.string)
   
   for ( j in 1:length(S.n) ) {
     S.n[[j]] <- gsub(paste0(i, " . " ), "", S.n[[j]])
     S.n[[j]] <- gsub(paste0(" . ", i ), "", S.n[[j]])
   }
   
   # Because the + could be before or after the term
}

}


# TODO: Note: we have no provision for a mixture between the y's and q's for 
# the singular terms


if (length(singular.terms)>0 && all(grepl("q", singular.terms)) ) {

# + zeta.02.02 * log(q02) * log(q02)

square.q.index <- grep("I[(]0.5 [*] log[(]q[0-9][0-9][)].2[)]", singular.terms)

# singular.terms <- gsub("(I[(]0.5 . )|([)])|(log[(])", "", singular.terms )

for (i in seq_along(square.q.index) ) {

  q.to.elim <- str_extract(singular.terms[square.q.index[i]], "[0-9][0-9]")
  
  target.q <- paste0("+ zeta.", q.to.elim, ".", q.to.elim, " * log(q", q.to.elim, 
    ") * log(q", q.to.elim, ")" )

  ln.E.string <- gsub(target.q, "", ln.E.string,  fixed=TRUE)
  
}

}






























all.vars(as.formula(paste("ln.E.data ~", ln.E.string)))

# TODO: Where is alpha.01.01? Nevermind, it disappeared with the adding up restrictions


linear.names <- names(coef(linear.sur.est))
linear.names <- gsub("cost.fn_", "", linear.names[grepl("cost.fn_", linear.names)])

lin.to.nonlin.crossref.df <- data.frame(linear=linear.names, nonlinear=linear.names, stringsAsFactors=FALSE)

for ( i in 1:M) {
  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
    lin.to.nonlin.crossref.df$nonlinear,
    paste0("^y", lead.zero(i), "$"), paste0("alpha", lead.zero(i)))
}

for ( i in 1:N) {
  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
    lin.to.nonlin.crossref.df$nonlinear,
    paste0("^log[(]w", lead.zero(i), "[)]$"), paste0("beta", lead.zero(i)))
}



for ( i in 1:M) {
  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
    lin.to.nonlin.crossref.df$nonlinear,
    paste0("^I[(]0.5 . y", lead.zero(i), ".2[)]$"), paste0("alpha.", lead.zero(i), ".", lead.zero(i)))
    # paste0("^I[(] y", lead.zero(i), ".2[)]$")
}


#for ( i in 1:length( M.2.dim )) {
#  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
#    lin.to.nonlin.crossref.df$nonlinear,
#    paste0(y.perm[[1]][i], ":", y.perm[[2]][i]), 
#    paste0("alpha", M.2.dim[i]) )
#}

for ( i in 1:length( M.2.dim )) {
  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
    lin.to.nonlin.crossref.df$nonlinear,
    paste0(y.perm[[2]][i], ":", y.perm[[1]][i]), 
    paste0("alpha", M.2.dim[i]) )
    # paste0(y.perm[[2]][i], ":", "I[(]0.5 . ", y.perm[[1]][i], "[)]"), 
}


# y01:I(0.5 * y02)
#paste0("^I[(]0.5 . log[(]w", lead.zero(i), "[)].I[(]0.5 . log[)]$")
#"log(w03):I(0.5 * log(w01))"






N.2.dim.fixed <- lapply(N.2.dim, FUN=gsub, pattern="[.]", replacement="")

#for ( i in 1:length( N.2.dim.fixed[[1]] )) {
#  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
#    lin.to.nonlin.crossref.df$nonlinear,
#    paste0("log[(]w", N.2.dim.fixed[[1]][i], "[)]:log[(]w", N.2.dim.fixed[[2]][i], "[)]"), 
#    paste0("beta.", N.2.dim.fixed[[1]][i], ".", N.2.dim.fixed[[2]][i]) )
#}

for ( i in 1:length( N.2.dim.fixed[[1]] )) {
  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
    lin.to.nonlin.crossref.df$nonlinear,
    paste0("log[(]w", N.2.dim.fixed[[1]][i], "[)]:log[(]w", N.2.dim.fixed[[2]][i], "[)]"), 
    paste0("beta.", N.2.dim.fixed[[1]][i], ".", N.2.dim.fixed[[2]][i]) )
    #    paste0("log[(]w", N.2.dim.fixed[[1]][i], "[)]:I[(]0.5 . log[(]w", N.2.dim.fixed[[2]][i], "[)][)]"), 
}


# I(0.5 * log(w01)^2)

for ( i in 1:N ) {
  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
    lin.to.nonlin.crossref.df$nonlinear,
    paste0("I[(]0.5 . log[(]w", lead.zero(i), "[)].2[)]"), 
    paste0("beta.", lead.zero(i), ".", lead.zero(i)) )
}



for ( i in 1:J ) {
  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
    lin.to.nonlin.crossref.df$nonlinear,
    paste0("I[(]0.5 . log[(]q", lead.zero(i), "[)].2[)]"), 
    paste0("zeta.", lead.zero(i), ".", lead.zero(i)) )
}




J.2.dim.fixed <- lapply(J.2.dim, FUN=gsub, pattern="[.]", replacement="")

for ( i in 1:length( J.2.dim.fixed[[1]] )) {
  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
    lin.to.nonlin.crossref.df$nonlinear,
    paste0("log[(]q", J.2.dim.fixed[[1]][i], "[)]:log[(]q", J.2.dim.fixed[[2]][i], "[)]"), 
    paste0("zeta.", J.2.dim.fixed[[1]][i], ".", J.2.dim.fixed[[2]][i]) )
    #    paste0("log[(]w", N.2.dim.fixed[[1]][i], "[)]:I[(]0.5 . log[(]w", N.2.dim.fixed[[2]][i], "[)][)]"), 
}

# TODO: need to impose symmetry for the zetas










M.N.dim.fixed <- lapply(M.N.dim, FUN=gsub, pattern="[.]", replacement="")

for ( i in 1:length( M.N.dim.fixed[[1]] )) {
  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
    lin.to.nonlin.crossref.df$nonlinear,
    paste0("y", M.N.dim.fixed[[1]][i], ":log[(]w", M.N.dim.fixed[[2]][i], "[)]"), 
    paste0("gamma.", M.N.dim.fixed[[1]][i], ".", M.N.dim.fixed[[2]][i]) )
}




# kappa.01.04 * log(q01) * log(w04 * theta04)


J.N.dim.fixed <- lapply(J.N.dim, FUN=gsub, pattern="[.]", replacement="")

for ( i in 1:length( J.N.dim.fixed[[1]] )) {
  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
    lin.to.nonlin.crossref.df$nonlinear,
    paste0("log[(]w", J.N.dim.fixed[[2]][i], "[)]:log[(]q", J.N.dim.fixed[[1]][i], "[)]"), 
    paste0("kappa.", J.N.dim.fixed[[1]][i], ".", J.N.dim.fixed[[2]][i]) )
}






M.J.dim.fixed <- lapply(M.J.dim, FUN=gsub, pattern="[.]", replacement="")

for ( i in 1:length( M.J.dim.fixed[[1]] )) {
  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
    lin.to.nonlin.crossref.df$nonlinear,
    paste0("y", M.J.dim.fixed[[1]][i], ":log[(]q", M.J.dim.fixed[[2]][i], "[)]"), 
    paste0("delta.", M.J.dim.fixed[[1]][i], ".", M.J.dim.fixed[[2]][i]) )
}



for ( i in 1:J ) {
  lin.to.nonlin.crossref.df$nonlinear <- str_replace_all(
    lin.to.nonlin.crossref.df$nonlinear,
    paste0("log[(]q", lead.zero(i), "[)]"), 
    paste0("zeta", lead.zero(i) ) )
}

#log(q01)







lin.to.nonlin.crossref.df$nonlinear[lin.to.nonlin.crossref.df$nonlinear=="(Intercept)"] <- "beta0"

# crossref done


ln.E.vars <- lin.to.nonlin.crossref.df$nonlinear



# ln.E.vars <- all.vars(nls.formula.ln.E)
#ln.E.vars <- all.vars(as.formula(paste("ln.E.data ~", ln.E.string)))
#ln.E.vars <- ln.E.vars[ !grepl("(w[0-9])|(y[0-9])|(ln.E.data)", ln.E.vars ) ]
ln.E.start.vals <- vector(mode="numeric", length=length(ln.E.vars))
ln.E.start.vals <- coef(linear.sur.est)[grepl("cost.fn_", names(coef(linear.sur.est)))]
# ln.E.vars <- sort(ln.E.vars)
names(ln.E.start.vals) <- ln.E.vars
#ln.E.start.vals[grepl("(beta)|(gamma)|(alpha)", names(ln.E.start.vals))] <- 5
#ln.E.start.vals[grepl("theta", names(ln.E.start.vals))] <- 1

# NOTE: Would need to fix the below if M=1
# ln.E.start.vals <- ln.E.start.vals[!grepl("(beta01)|(beta....01)|(gamma....01)", 
#   names(ln.E.start.vals))]


# This is to handle the fact that some of these drop out with adding-up restrictions:
ln.E.start.vals <- ln.E.start.vals[!grepl("(beta01)|(beta....01)|(gamma....01)", 
  names(ln.E.start.vals))]
  # (alpha.03.01)|
# alpha.03.01 <- 0   

#TODO: for now we will kill alpha.03.01 since it only pertains to one observation and it will screw up gradient



# theta.starts <- c(.75, 1, 1.25)
 
#theta.starts <- c(.8, 1, 1.2)

theta.starts <- rep(1, times=N-1)
names(theta.starts) <- paste0("theta", lead.zero(1:(N-1)))
theta06 <- 1

# Now making manure numeraire and changing to price translation:
# theta.starts <- rep(0, times=N-1)
# names(theta.starts) <- paste0("theta", lead.zero(c(1,2,4)))
# theta03 <- 0



ln.E.start.vals <-c(ln.E.start.vals, theta.starts)


first.line <- paste0( "args <- c(\"", paste(names(ln.E.start.vals), sep="\", \"", collapse="\", \""), "\")\nfor ( i in 1:length(args)) { assign(args[i], x[i])} ; ")

eval(parse(text=paste0("mod.predicted <- function(x) {", first.line, 
"  ret <- ", ln.E.string, "; ifelse(is.finite(ret), ret, 10^300) }")))

mod.predicted(ln.E.start.vals)

names(list(x01, x02))

x.s.list <- paste0("x", lead.zero(1:N), collapse=", ")
w.s.list <- paste0("w", lead.zero(1:N), collapse=", ")
y.s.list <- paste0("y", lead.zero(1:M), collapse=", ")
q.s.list <- paste0("q", lead.zero(1:J), collapse=", ")

eval( parse(text=paste0("args.list <- list(", x.s.list, ", ", w.s.list , 
  ", ", y.s.list  , ", ", q.s.list , ", ln.E.data)") ) )
  
names(args.list) <- c(
  paste0("x", lead.zero(1:N)),
  paste0("w", lead.zero(1:N)),
  paste0("y", lead.zero(1:M)),
  paste0("q", lead.zero(1:J)),
  "ln.E.data"
)
  

#args.list <- list(x01=x01, x02=x02, x03=x03, x04=x04, x05=x05, w01=w01, w02=w02, w03=w03,
# w04=w04, w05=w05, y01=y01, y02=y02, y03=y03, y04=y04, y05=y05, y06=y06, y07=y07, 
# y08=y08, y09=y09, y10=y10, y11=y11, y12=y12, ln.E.data=ln.E.data)






