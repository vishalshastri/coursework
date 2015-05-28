





##### START FOR LINEAR::::


# as.data.frame(region.matrix) 



#exists.vec <- Vectorize( exists)

# M <- 1
# N <- 6
# J <- 3

combined.df <- data.frame(mget(c("y01", paste0("x", lead.zero(1:N)), 
  paste0("w", lead.zero(1:N)),  paste0("q", lead.zero(1:J)) )))
  
# NOTE: IMPORTANT: Below is a fix since y01 was placed into log terms for the translog specification

  
# TRANSLOG region.matrix.df <-   as.data.frame(region.matrix)


# TRANSLOG colnames(region.matrix.df) <- iconv(colnames(region.matrix.df), to="ASCII//TRANSLIT")
# TRANSLOG colnames(region.matrix.df) <- gsub("'", "", colnames(region.matrix.df) )
# TRANSLOG colnames(region.matrix.df) <- gsub("[.]", "", colnames(region.matrix.df) )
  
# TRANSLOG combined.df <- cbind(combined.df, region.matrix.df)


log10_ceiling <- function(x) {
    10^(ceiling(log10(x)))
}
# Thanks to http://stackoverflow.com/questions/7906996/algorithm-to-round-to-the-next-order-of-magnitude-in-r


# For now, let's not scale
#for ( i in 1:N) {

#  if (scale.vars.on.orig.data) { 
#    input.scaling  <- input.scaling.orig[i] 
#  } else {
#    input.scaling  <- log10_ceiling(
#      sqrt(sum((c(combined.df[, paste0("x", lead.zero(i))], 
#      combined.df[, paste0("w", lead.zero(i))])^2)/(nrow(combined.df)-1)))
#    )
  # Got this idea from scale() function
#  }
  
#  input.scaling <- input.scaling / 100
  
#  combined.df[, paste0("x", lead.zero(i))] <- combined.df[, paste0("x", lead.zero(i))] / input.scaling
#  combined.df[, paste0("w", lead.zero(i))] <- combined.df[, paste0("w", lead.zero(i))] / input.scaling

#}










# TRANSLOG for (i in 1:length(S.n)) {
# TRANSLOG   combined.df[, paste0("s", i)] <- with(combined.df, eval( parse(text=gsub("~.*$", "", S.n[[i]]) ) ))
# TRANSLOG }

if ( !only.cost.fn) {
  for (i in 1:N) {
	combined.df[, paste0("dem", i)] <- with(combined.df, eval(parse(text=paste0("x", lead.zero(i), "/y01" ) ) ))
  }

  combined.df[, paste0("dem", N+1)] <- E.y01.data
  
} else {
  combined.df[, paste0("dem", 1)] <- E.y01.data
}




# combined.df <- scale(combined.df, center=FALSE)

top.before.data <- c(
"sets ",
paste0("  t number of observations  / 1 * ", nrow(combined.df), " /"),
paste0("  d number of variables in datafile  / 1 * ", ncol(combined.df), " /"),
paste0("  ss dimension of S matrix / 1 * ", N-1, "/"),
paste0("  sss second dimension of S matrix / 1 * ", N-1, "/"),
paste0("  cc dimension of C matrix / 1 * ", J, "/"),
paste0("  ccc second dimension of C matrix / 1 * ", J, "/"),
"  m number of points in interval z / 1 * ", length(other.param.support), "/",
"  j number of points in interval v / 1 * 3 /;",
"",
#"alias (s, ss);",
"",
"parameter",
"  datafile(t,d)   raw data")

# cost.dep.line <- "  cost(t)          log of cost"

# cost.share.parm.lines <- paste0("  s", 1:length(S.n),"(t)          cost share ", 1:length(S.n))

combined.df.GAMS <- make.GAMS.data(combined.df)

combined.df.GAMS <- c(combined.df.GAMS, ";")

# TRANSLOG colnames(combined.df)[colnames(combined.df)=="ln.E.data"] <- "cost"

data.declaration.lines <-  c( paste0(colnames(combined.df), "(t)"))
# Eliminated theta here




# TODO: do I need   indic(k)        copy of set k? Seems no.

top.before.data <- c(top.before.data,
  data.declaration.lines,
  "  MM              number of points in support",
  "  JJ              number of points in support",
  ";",
  "",
  "MM=", length(other.param.support), ";",
  "JJ=3;", # Eliminated theta here
  "", 
  "table datafile(t,d)"
)




data.alloc.lines <- c()

for (i in 1:ncol(combined.df) ) {
  data.alloc.lines <- c(data.alloc.lines, 
    paste0(colnames(combined.df)[i], "(t) = datafile(t,\"", i, "\");")
  )
}


#other.param.support cost.err.support share.err.support

vdemsupp.string <- c()

for ( i in 1:length(err.support.dem.eqns)) {

  vdemsupp.string <- c(vdemsupp.string, 
    c(paste0("parameter vdemsupp", i, "(j)    support points "),
    "/",
    paste0(1:length(err.support.dem.eqns[[i]]), "  ", err.support.dem.eqns[[i]]),
    "/;")
  )
}


CE.q.supp.string <- c()

for ( i in 1:length(CE.q.support.dem.eqns)) {

  CE.q.supp.string <- c(CE.q.supp.string, 
    c(paste0("parameter ceqdemsupp", i, "(j)    support points "),
    "/",
    paste0(1:length(CE.q.support.dem.eqns[[i]]), "  ", CE.q.support.dem.eqns[[i]]),
    "/;")
  )
}




param.support.simple.lines <- c( # Eliminated theta here
"parameter zother(m)    support points",
"/",
paste0(1:length(other.param.support), "  ", other.param.support),
"/;",
vdemsupp.string,
CE.q.supp.string
# TRANSLOG "parameter vcost(j)    support points ",
# TRANSLOG "/",
# TRANSLOG paste0(1:length(cost.err.support), "  ", cost.err.support),
# TRANSLOG "/;"
)


greeks.support.simple.lines <- paste0(
"parameter inputmean", lead.zero(1:N), "    input mean value", lead.zero(1:N),  "\n",
"/", "\n",
mean.of.inputs, "\n",
"/;", "\n"
)


#mean.of.inputs
#psi
#beta
#delta
#eta




vsupport.lines <- c() 

for ( i in 1:length(demand.eqns))  {

  vsupport.lines <- c(vsupport.lines, 
  paste0("parameter vdem", i, "(j)  support space for error term;"),
  paste0("vdem", i, "(j) = vdemsupp", i, "(j);")
  )
  
}
# NOTE: maybe we do not want to same bounds on the error terms for each demand equation


CE.q.support.lines <- c() 

for ( i in 1:length(demand.eqns))  {

  CE.q.support.lines <- c(CE.q.support.lines, 
  paste0("parameter ceqdem", i, "(j)  support space for cross entropy;"),
  paste0("ceqdem", i, "(j) = ceqdemsupp", i, "(j);")
  )
  
}


# all.params <- gsub("[.]", "", names(ln.E.start.vals)[!grepl("region", names(ln.E.start.vals))] )

# TRANSLOG ln.E.string <- nls.formula.ln.E.region


#all.params <- unique(str_extract_all(ln.E.string, 
#"(theta[0-9][0-9])|(beta0)|(alpha[0-9][0-9])|(alpha[.][0-9][0-9][.][0-9][0-9])|(beta[0-9][0-9])|(beta[.][0-9][0-9][.][0-9][0-9])|(gamma[.][0-9][0-9][.][0-9][0-9])|(zeta[.][0-9][0-9][.][0-9][0-9])|(zeta[0-9][0-9])|(kappa[.][0-9][0-9][.][0-9][0-9])|(delta[.][0-9][0-9][.][0-9][0-9])|(region[0-9][0-9])"
#  )[[1]]
#)

all.params <- unique(unlist(str_extract_all(unlist(demand.eqns), 
"(s.[0-9][0-9].[0-9][0-9])|(b.y.[0-9][0-9])|(b.[0-9][0-9])|(b.y.y)|(d.[0-9][0-9].[0-9][0-9])|(c.[0-9][0-9] )|(c.[0-9][0-9].[0-9][0-9])"
  ))
)

# (psi.[0-9][0-9])| (beta.[0-9][0-9])| (delta.[0-9][0-9])| (eta.[0-9][0-9])|

all.params <- gsub("([.])|( )", "", all.params)


# TRANSLOG all.params <- all.params[!grepl("theta", all.params)]

non.theta.param.support.lines <- c() 

for ( i in all.params[!grepl("theta", all.params)] ) {

  non.theta.param.support.lines <- c(non.theta.param.support.lines, 
  paste0("parameter z", i, "(m)  support space for params;"),
  paste0("z", i, "(m) = zother(m);")
  )
  
}

#theta.param.support.lines <- c() 

#for ( i in all.params[grepl("theta", all.params)] ) {

#  theta.param.support.lines <- c(theta.param.support.lines, 
#  paste0("parameter z", i, "(m)  support space for params;"),
#  paste0("z", i, "(m) = ztheta(m);")
#  )
  
#}




#parameter z(k,m) support space for betas;
#z(k,m) = z1(m);
#z("2",m) = z2(m);
#z("3",m) = z2(m);
#z("4",m) = z2(m);
#z("5",m) = z2(m);
#z("6",m) = z2(m);
#z("7",m) = z2(m);
#parameter v(j)    poi




# TRANSLOG all.eqns <- c("cost", paste0("s", 1:length(S.n)))
all.eqns <- paste0("dem", 1:length(demand.eqns))


cov.var.declarations <- paste0( "surdelta", expand.grid(1:length(all.eqns), 1:length(all.eqns))[, 1],
  expand.grid(1:length(all.eqns), 1:length(all.eqns))[, 2] )

cov.var.declarations.mat <- matrix(cov.var.declarations, nrow=length(all.eqns))

diag(cov.var.declarations.mat) <- ""
cov.var.declarations <- cov.var.declarations.mat
cov.var.declarations  <- cov.var.declarations[cov.var.declarations!= ""]  
  
cov.var.declarations <- paste0("  ", cov.var.declarations,  "    SUR covar parameter")

if (!do.SUR) { cov.var.declarations <- c() }
  

variable.declaration.lines <- c("variables",
  paste0("  ", all.params, "   parameters to be estimated"),
ifelse(concave.in.prices,  "  Smat(ss,sss)   S matrix to make cost function concave", ""),
ifelse(concave.in.prices,   "  SmatT(sss,ss)   transpose of S matrix to make cost function concave", ""),
ifelse(convex.in.f.inputs,  "  Cmat(cc,ccc)   C matrix to make cost function convex in fixed inputs", ""),
ifelse(convex.in.f.inputs,  "  CmatT(ccc,cc)   transpose of C matrix to make cost function convex in fixed inputs", ""),
#  "  errorrelax(t) small value to accomodate the zero error adding up restriction",
  cov.var.declarations,
  paste0("  p", all.params, "(m)    probability corresponding param"),
  paste0("  w", all.eqns, "(t,j)    probability corresponding error term"),
  "  h           gme objective value",
  "",
  "positive variable",
  paste0("  p", all.params, "(m)"),
  paste0("  w", all.eqns, "(t,j)"), # eliminated thetas here
  ";"
)




objective.fn.lines <- c(
paste0("-sum(m, p", all.params, "(m)*log(p", all.params, "(m)+1.e-8) )"),
paste0("-sum((t,j), w", all.eqns, "(t,j)*log(w", all.eqns, "(t,j)+1.e-8) )"),
paste0("+sum((t,j), w", all.eqns, "(t,j)*log(ceq", all.eqns, "(j)+1.e-8) )"),
";"
)




objective.fn.lines[1] <- paste0( "object..           h =e= ", objective.fn.lines[1])
  




#add.data.subscripts <- function(x) {
#  for (i in 1:99) {
#    ii <- formatC(i, width = 2, flag = "0")  
#    x <- gsub(paste0("w", ii), paste0("w", ii, "(t)"), x)
#    x <- gsub(paste0("q", ii), paste0("q", ii, "(t)"), x)
#    x <- gsub(paste0("x", ii), paste0("x", ii, "(t)"), x)
#    x <- gsub(paste0("y", ii), paste0("y", ii, "(t)"), x)
#  }
#  x
#}


# S.n.GAMS <- lapply(S.n, FUN=add.data.subscripts)

# S.n.GAMS <- lapply(S.n.GAMS, FUN=function(x) gsub(pattern="[.]", replacement="", x=x))



# TRANSLOG S.n.linear <- lapply(S.n, FUN=function(x) gsub(
# TRANSLOG   pattern="[)] [/] [(][(]w[0-9][0-9].*$", replacement=")", x=x))

# TRANSLOG S.n.linear <- lapply(S.n.linear, FUN=add.data.subscripts)

# TRANSLOG S.n.linear <- lapply(S.n.linear, FUN=function(x) gsub(pattern="[.]", replacement="", x=x))

# TRANSLOG S.n.GAMS <- gsub(" [*] theta[0-9][0-9]", "", S.n.linear)




model.restrictions <- list()


GAMS.demand.eqns <- lapply(demand.eqns, FUN=add.data.subscripts)

GAMS.demand.eqns <- lapply(GAMS.demand.eqns, FUN=function(x) gsub(pattern="[.]", replacement="", x=x))
GAMS.demand.eqns <- lapply(GAMS.demand.eqns, FUN=function(x) gsub(pattern="[{^]", replacement="**", x=x))


for ( i in 1:length(demand.eqns)) {

  first.eq.line.a <- paste0("restr", i, "dema(t)$(dem", i, "(t) gt 0)..") 
  first.eq.line.b <- paste0("restr", i, "demb(t)$(dem", i, "(t) eq 0)..")
  
  second.eq.line.a <- paste0("dem", i, "(t) =e= ", GAMS.demand.eqns[[i]],
    " + sum(j, vdem", i, "(j) * wdem", i,"(t, j))",  ";") 
  second.eq.line.b <- paste0("dem", i, "(t) =g= ", GAMS.demand.eqns[[i]], 
    " + sum(j, vdem", i, "(j) * wdem", i,"(t, j))", ";" )
  
  second.eq.line.a <- strwrap( second.eq.line.a, indent=12, exdent=19, width=80)
  second.eq.line.b <- strwrap( second.eq.line.b, indent=12, exdent=19, width=80)
  
  model.restrictions[[i]] <- c(first.eq.line.a, second.eq.line.a, first.eq.line.b, second.eq.line.b)
  
}



#ln.E.string.GAMS <- as.character(S.n.H[[length(S.n.H)]])[3]

#ln.E.string.GAMS <- gsub("I", "", ln.E.string.GAMS)

#ln.E.string.GAMS <- gsub("[{^]", "**", ln.E.string.GAMS)
# including "{" because it helps us get a literal "^"

#ln.E.string.GAMS <- gsub("[:]", "*", ln.E.string.GAMS)

#ln.E.string.GAMS <- add.data.subscripts(ln.E.string.GAMS)

#ln.E.string.GAMS <- gsub(pattern="[.]", replacement="", x=ln.E.string.GAMS)


# TRANSLOG ln.E.string.linear <- gsub("[+]  log[(] [(]w01 [/] [(]w01 [*] theta01[)][)].*$", "", ln.E.string)

# TRANSLOG region.tackon.clean <- iconv(region.tackon, to="ASCII//TRANSLIT")
# TRANSLOG region.tackon.clean <- gsub("'", "", region.tackon.clean )
# TRANSLOG region.tackon.clean <- gsub("[.]", "", region.tackon.clean )

# TRANSLOG ln.E.string.linear <- paste0( ln.E.string.linear, " + ", region.tackon.clean)

# TRANSLOG ln.E.string.linear <- add.data.subscripts(ln.E.string.linear)

# TRANSLOG ln.E.string.linear <- gsub(pattern="[.]", replacement="", x=ln.E.string.linear)

# TRANSLOG ln.E.string.GAMS <- gsub(" [*] theta[0-9][0-9]", "", ln.E.string.linear)




# TRANSLOG first.eq.line.a <- "restrcosta(t)$(cost(t) gt 0).."
# TRANSLOG first.eq.line.b <- "restrcostb(t)$(cost(t) eq 0).."
  
# TRANSLOG second.eq.line.a <- paste0("cost(t) =e= ", ln.E.string.GAMS, 
# TRANSLOG   " + sum(j, vcost(j) * wcost(t, j))", ";") 
# TRANSLOG second.eq.line.b <- paste0("cost(t) =g= ", ln.E.string.GAMS, 
# TRANSLOG   " + sum(j, vcost(j) * wcost(t, j))", ";") 
  
# TRANSLOG second.eq.line.a <- strwrap( second.eq.line.a, indent=12, exdent=19, width=80)
# TRANSLOG second.eq.line.b <- strwrap( second.eq.line.a, indent=12, exdent=19, width=80)
  
# TRANSLOG model.restrictions.cost <- c(first.eq.line.a, second.eq.line.a, first.eq.line.b, second.eq.line.b)



#restr2(i,k)..        beta(i,k) =e= sum(m, p(i,k,m) * z(k, m));

#so we need to make this

#restrpbeta01..        beta01 =e= sum(m, pbeta01(m) * zbeta01(m));



prob.weight.param.lines <- c()

for ( i in all.params[!grepl("theta", all.params)]) {
  prob.weight.param.lines <- c(prob.weight.param.lines,  paste0(
    "restrp", i, "..        ", i," =e= sum(m, p", i, "(m) * z", i, "(m));" ),
    paste0(
    "restrpproper", i, "..            1 =e= sum(m, p", i, "(m));" )
  )
}




# TRANSLOG all.eqns <- c("cost", paste0("s", 1:length(S.n)))

prob.weight.error.lines <- c()

for ( i in all.eqns) {
  prob.weight.error.lines <- c(prob.weight.error.lines,  paste0(
    "restrerr", i, "(t)..        1 =e= sum(j, w", i, "(t,j));"
  ))
}




covar.SUR.mat<- matrix("", ncol=length(all.eqns), nrow=length(all.eqns))

for ( i in 1:length(all.eqns)) {
  for ( j in 1:length(all.eqns)) {
    covar.SUR.mat[i,j] <- paste0( "restrcov", i, j, "..        ",
      "0 =e= surdelta", i, j, " * sqrt( ( sum(t, sum(j, v", all.eqns[i], "(j) * w", all.eqns[i], 
      "(t, j)) * sum(j, v", all.eqns[i], "(j) * w", all.eqns[i], "(t, j))) / ", nrow(combined.df), ") * ",
      " ( sum(t, sum(j, v", all.eqns[j], "(j) * w", all.eqns[j], 
      "(t, j)) * sum(j, v", all.eqns[j], "(j) * w", all.eqns[j], "(t, j))) / ", nrow(combined.df), ") ) - ",
      "sum(t, sum(j, v", all.eqns[i], "(j) * w", all.eqns[i], "(t, j)) ",
      " * sum(j, v", all.eqns[j], "(j) * w", all.eqns[j], "(t, j))) / ", nrow(combined.df), ";"
    )
    
  }
}

diag(covar.SUR.mat) <- ""

covar.SUR.v <- c(covar.SUR.mat)

covar.SUR.v <- strwrap( covar.SUR.v, indent=1, exdent=19, width=80)

covar.SUR.lines <- covar.SUR.v

if (!do.SUR) { covar.SUR.lines <- c() }

cov.rest.declarations <- paste0( "restrcov", expand.grid(1:length(all.eqns), 1:length(all.eqns))[, 1],
  expand.grid(1:length(all.eqns), 1:length(all.eqns))[, 2] )
  
cov.rest.declarations.mat <- matrix(cov.rest.declarations, nrow=length(all.eqns))

diag(cov.rest.declarations.mat) <- ""
cov.rest.declarations <- cov.rest.declarations.mat
cov.rest.declarations  <- cov.rest.declarations[cov.rest.declarations != ""]

if (!do.SUR) { cov.rest.declarations <- c() }



double.s.params<- unique(unlist(str_extract_all(gsub("[.]", "", unlist(demand.eqns)), "s[0-9]{4}")))

double.s.indices <- str_extract_all(double.s.params, "[0-9]{2}")

concave.restriction.defn <- c()

for ( i in 1:length(double.s.indices)) {

 first.ind <- as.numeric(double.s.indices[[i]][1])
 second.ind <- as.numeric(double.s.indices[[i]][2])

  concave.restriction.defn <- c(concave.restriction.defn,  paste0( "restrconcave", double.s.params[i], "..      ", double.s.params[i], " =e= - sum(sss, Smat(\"", 
  first.ind - 1, "\",sss)*SmatT(sss,\"", second.ind - 1, "\"));") )
  
#    concave.restriction.defn <- c(concave.restriction.defn,  paste0( 
#    "restrconcavelowertrinonzero", 
#   second.ind, first.ind, "..      ", 1e-08, " =l= sqr(Smat(\"", 
#  second.ind - 1, "\",", "\"", first.ind - 1, "\"));")) 
  # Above is basically specifying the transpose
  
  if (first.ind==second.ind) {next}
  # since we don't want to zero out the diagonal elements
    
  concave.restriction.defn <- c(concave.restriction.defn,  paste0( "restrconcavelowertri", 
  first.ind, second.ind, "..      ", 0, " =e= Smat(\"", 
  first.ind - 1, "\",", "\"", second.ind - 1, "\");")) 
  
}

concave.restriction.declare <- c()

for ( i in 1:length(double.s.indices)) {
  concave.restriction.declare <- c(concave.restriction.declare, 
    paste0( "restrconcave", double.s.params[i]))
    
   first.ind <- as.numeric(double.s.indices[[i]][1])
   second.ind <- as.numeric(double.s.indices[[i]][2])
   
#   concave.restriction.declare <- c(concave.restriction.declare, 
#    paste0( "restrconcavelowertrinonzero", second.ind, first.ind))

  if (first.ind==second.ind) {next}
  
  concave.restriction.declare <- c(concave.restriction.declare, 
    paste0( "restrconcavelowertri", first.ind, second.ind))
    
}

Smat.transpose.restriction.defn <- "restrSmattrans(ss,sss).. Smat(ss,sss) =e= SmatT(sss,ss);"

Smat.transpose.restriction.declare <- "restrSmattrans"








double.c.params<- unique(unlist(str_extract_all(gsub("[.]", "", unlist(demand.eqns)), "c[0-9]{4}")))

double.c.indices <- str_extract_all(double.c.params, "[0-9]{2}")

convex.restriction.defn <- c()

for ( i in 1:length(double.c.indices)) {

 first.ind <- as.numeric(double.c.indices[[i]][1])
 second.ind <- as.numeric(double.c.indices[[i]][2])

  convex.restriction.defn <- c(convex.restriction.defn,  paste0( "restrconvex", double.c.params[i], "..      ", double.c.params[i], " =e= sum(ccc, Cmat(\"", 
  first.ind, "\",ccc)*CmatT(ccc,\"", second.ind, "\"));") )
  # Notice no negative since we want this to be _positive_ semidef
  
 #   convex.restriction.defn <- c(convex.restriction.defn,  paste0( 
 #   "restrconvexlowertrinonzero", 
 #  second.ind, first.ind, "..      ", 1e-08, " =l= sqr(Cmat(\"", 
 # second.ind - 1, "\",", "\"", first.ind - 1, "\"));"))
  # Above is basically specifying the transpose
  
  if (first.ind==second.ind) {next}
  # since we don't want to zero out the diagonal elements
    
  convex.restriction.defn <- c(convex.restriction.defn,  paste0( "restrconvexlowertri", 
  first.ind, second.ind, "..      ", 0, " =e= Cmat(\"", 
  first.ind, "\",", "\"", second.ind, "\");")) 
  
}

convex.restriction.declare <- c()

for ( i in 1:length(double.c.indices)) {
  convex.restriction.declare <- c(convex.restriction.declare, 
    paste0( "restrconvex", double.c.params[i]))
    
   first.ind <- as.numeric(double.c.indices[[i]][1])
   second.ind <- as.numeric(double.c.indices[[i]][2])
   
#   convex.restriction.declare <- c(convex.restriction.declare, 
#    paste0( "restrconvexlowertrinonzero", second.ind, first.ind))

  if (first.ind==second.ind) {next}
  
  convex.restriction.declare <- c(convex.restriction.declare, 
    paste0( "restrconvexlowertri", first.ind, second.ind))
    
}

Cmat.transpose.restriction.defn <- "restrCmattrans(cc,ccc).. Cmat(cc,ccc) =e= CmatT(ccc,cc);"

Cmat.transpose.restriction.declare <- "restrCmattrans"














# TRANSLOG restriction.that.err.sum.to.zero.defn <- paste0("restrerrsumtozero(t)..        0 =e=  ", paste0("sum(j, vs", 1:length(S.n), "(j) * ws", 1:length(S.n), "(t, j))", collapse=" + "), ";")
# errorrelax(t) +

# TRANSLOG restriction.that.err.sum.to.zero.declare <- "restrerrsumtozero(t)"

# NOTE: Here we are assuming that we have dropped no equations above. Otherwise, this will be a big problem

# TRANSLOG errorrelaxrestrict.defn <- "errorrelaxrestrict(t)..    1e-06 =g= sqr(errorrelax(t));"

# TRANSLOG errorrelaxrestrict.declare <- "errorrelaxrestrict(t)"




#concave.restriction.defn <- c()

#for ( i in 1:length(double.beta.indices)) {

# first.ind <- as.numeric(double.beta.indices[[i]][1])
# second.ind <- as.numeric(double.beta.indices[[i]][2])

#  concave.restriction.defn <- c(concave.restriction.defn,  paste0( "restrconcave", double.beta.params[i], "..      ", double.beta.params[i], " =e= - sum(ss, Smat(\"", 
#  first.ind - 1, "\",ss)*SmatT(ss,\"", second.ind - 1, "\"));") )
  
#}




if (!convex.in.f.inputs) {
  convex.restriction.declare <- ""
  Cmat.transpose.restriction.declare <- ""
}

if (!concave.in.prices) {
  concave.restriction.declare <- ""
  Smat.transpose.restriction.declare <- ""
}





equation.declarations <- c(
  "equations",
  "  object             primal GME objective function",
  gsub("[.]{2}.*$", "", prob.weight.param.lines),
  gsub("[.]{2}.*$", "",  prob.weight.error.lines),
# TRANSLOG   "restrcosta(t)",
# TRANSLOG   "restrcostb(t)",
  paste0("restr", 1:length(demand.eqns), "dema(t)"),
  paste0("restr", 1:length(demand.eqns), "demb(t)"),
  concave.restriction.declare,
  convex.restriction.declare,
  Smat.transpose.restriction.declare,
  Cmat.transpose.restriction.declare,
#  restriction.that.err.sum.to.zero.declare,
#  errorrelaxrestrict.declare,
  cov.rest.declarations,
  ";"
)







# Trying to deal with issue where SUR does crazy things

set.seed(100)

error.weights.lines <- vector("character", nrow(combined.df) * 3)


#CE.q.support.dem.eqns

for ( i in 1:length(all.eqns) ) {
  
  err.weight.temp.df <- data.frame(A=jitter(rep(CE.q.support.dem.eqns[[i]][1],
                                       nrow(combined.df))),
                                   B=jitter(rep(CE.q.support.dem.eqns[[i]][2], 
                                       nrow(combined.df))),
                                   C=jitter(rep(CE.q.support.dem.eqns[[i]][3], 
                                       nrow(combined.df))))

#  err.weight.temp.df <- err.weight.temp.df[, -1 ]
  
  err.grid <- expand.grid( 1:3, 1:nrow(combined.df))
  
  for ( j in 1:nrow(err.grid)) {
  
    error.weights.lines[j + (i-1)*nrow(combined.df)]  <- paste0(" w", all.eqns[i], ".l(\"", err.grid[j, 2], "\",\"",  
      err.grid[j, 1], "\") = ", err.weight.temp.df[err.grid[j, 2] , err.grid[j, 1]], ";")
    
    
  }

}







set.seed(100)

Smat.initiation.v <- matrix(rnorm((N-1)^2), nrow=N-1)
Smat.initiation.v[upper.tri(Smat.initiation.v)] <- 0
Smat.initiation.v <- c(Smat.initiation.v)

Smat.initiation.grid <- expand.grid(1:(N-1), 1:(N-1))

Smat.initial.values <- paste0("Smat.L(\"", Smat.initiation.grid[, 1], "\",\"", Smat.initiation.grid[, 2], "\") =  ", Smat.initiation.v, ";")


set.seed(100)

Cmat.initiation.v <- matrix(rnorm(J^2), nrow=J)
Cmat.initiation.v[upper.tri(Cmat.initiation.v)] <- 0
Cmat.initiation.v <- c(Cmat.initiation.v)

Cmat.initiation.grid <- expand.grid(1:J, 1:J)

Cmat.initial.values <- paste0("Cmat.L(\"", Cmat.initiation.grid[, 1], "\",\"", Cmat.initiation.grid[, 2], "\") =  ", Cmat.initiation.v, ";")


set.seed(100)













cov.var.display <- paste0( "surdelta", expand.grid(1:length(all.eqns), 1:length(all.eqns))[, 1],
  expand.grid(1:length(all.eqns), 1:length(all.eqns))[, 2] )

cov.var.display.mat <- matrix(cov.var.display, nrow=length(all.eqns))

diag(cov.var.display.mat) <- ""
cov.var.display <- cov.var.display.mat
cov.var.display  <- cov.var.display[cov.var.display!= ""]  

cov.var.display  <- paste0("display ", cov.var.display, ".l;")

if (!do.SUR) { cov.var.display <- c() }


  
parameter.display.lines <- c( paste0("display ", all.params, ".l;"),
  paste0("display p", all.params, ".l;"),
  paste0("display w", all.eqns, ".l;"),
  cov.var.display ,
ifelse(concave.in.prices,   paste0("display Smat.l"), ""),
ifelse(convex.in.f.inputs,  paste0("display Cmat.l"), "")
#  paste0("display errorrelax.l")
  )




if (!convex.in.f.inputs) {
  Cmat.initial.values <- ""
}

if (!concave.in.prices) {
  Smat.initial.values <- ""
}


final.lines <- 
c(
"*Initial conditions",
paste0("  p", all.params, ".l(m) = 1/MM;"),
# paste0("  w", all.eqns, ".l(t,j) = 1/JJ;"),
Smat.initial.values,
Cmat.initial.values,
error.weights.lines,
"* primal approach",
"model gme /all/;",
"options domlim=5000;",
"*options seed=5;",
"*options iterlim=0;",
"*option bratio=1;",
"options iterlim=90000;",
"options reslim=470000;",
"*options work=900000;",
"* conopt optimizer option is below",
"*options solprint=off, nlp=conopt;",
"*options  nlp=conopt;",
"options solprint=off;",
"GME.OPTFILE=1; ",
"* OPTION NLP=MINOS5;",
" OPTION NLP=CONOPTD;",
"$inlinecom /* */",
"",
"/* Turn off the listing of the input file */",
"$offlisting",
"",
"/* Turn off the listing and cross-reference of the symbols used */",
"$offsymxref offsymlist",
"",
"option",
"    limrow = 0,     /* equations listed per block */",
"    limcol = 0,     /* variables listed per block */",
"    solprint = off,     /* solver's solution output printed */",
"    sysout = off;       /* solver's system output printed */",
" ",
"solve gme using nlp maximizing h; ",
"options decimals = 7;"
)
# Help from http://support.gams.com/doku.php?id=gams:how_do_i_reduce_the_size_of_my_listing_.lst_file
# on reducing size of list file

if (!convex.in.f.inputs) {
 convex.restriction.defn  <- ""
 Cmat.transpose.restriction.defn <- "" 
}

if (!concave.in.prices) {
 concave.restriction.defn  <- ""
 Smat.transpose.restriction.defn <- "" 

}


completed.GAMS.file <-  c(
  top.before.data, " ", 
  combined.df.GAMS, " ", 
  data.alloc.lines, " ", 
  param.support.simple.lines, " ", 
  greeks.support.simple.lines, " ",
  vsupport.lines, " ", 
  CE.q.support.lines, " ",
  non.theta.param.support.lines, " ",  # Eliminated thetas here
  variable.declaration.lines, " ", 
  equation.declarations, " ",
  objective.fn.lines, " ", 
  unlist(model.restrictions), " ", 
# TRANSLOG  model.restrictions.cost, " ", 
  prob.weight.param.lines, " ", 
  prob.weight.error.lines, " ", 
  concave.restriction.defn, " ",
  Smat.transpose.restriction.defn, " ",
  convex.restriction.defn,
  Cmat.transpose.restriction.defn,
#  restriction.that.err.sum.to.zero.defn, " ",
#  errorrelaxrestrict.defn, " ",
  covar.SUR.lines,
  final.lines, " ",
  parameter.display.lines 
)








if (linear.GAMS.output ) { 

  cat(completed.GAMS.file, 
    file=paste0(GAMS.projdir, "sgmGMElinear", strsplit(target.crop, " ")[[1]][1], 
     formatC(bootstrap.iter, width = 5, flag = "0"), file.flavor , ".gms"), 
    sep="\n")
  
}




# "(recall that uninitialized parameters take on value zero)."
# http://www.me.utexas.edu/~bard/LP/LP%20Handouts/gams_intro.pdf

# ** Warning **  Memory Limit for Hessians exceeded.
# You can use the Conopt option "rvhess"

# rvhess = maxdouble




