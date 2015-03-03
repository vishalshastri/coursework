









##### START FOR NONLINEAR::::





#exists.vec <- Vectorize( exists)

# M <- 1
# N <- 6
# J <- 3

combined.df <- data.frame(mget(c("y01", paste0("x", lead.zero(1:N)), 
  paste0("w", lead.zero(1:N)),  paste0("q", lead.zero(1:J)) )))
  
# TRANSLOG region.matrix.df <-   as.data.frame(region.matrix)


# TRANSLOG colnames(region.matrix.df) <- iconv(colnames(region.matrix.df), to="ASCII//TRANSLIT")
# TRANSLOG colnames(region.matrix.df) <- gsub("'", "", colnames(region.matrix.df) )
# TRANSLOG colnames(region.matrix.df) <- gsub("[.]", "", colnames(region.matrix.df) )
  
# TRANSLOG combined.df <- cbind(combined.df, region.matrix.df)


log10_ceiling <- function(x) {
    10^(ceiling(log10(x)))
}
# Thanks to http://stackoverflow.com/questions/7906996/algorithm-to-round-to-the-next-order-of-magnitude-in-r


# TRANSLOG for ( i in 1:N) {

# TRANSLOG   if (scale.vars.on.orig.data) { 
# TRANSLOG     input.scaling  <- input.scaling.orig[i] 
# TRANSLOG   } else {
# TRANSLOG     input.scaling  <- log10_ceiling(
# TRANSLOG       sqrt(sum((c(combined.df[, paste0("x", lead.zero(i))], 
# TRANSLOG       combined.df[, paste0("w", lead.zero(i))])^2)/(nrow(combined.df)-1)))
# TRANSLOG     )
  # Got this idea from scale() function
# TRANSLOG   }
  
# TRANSLOG   input.scaling <- input.scaling / 100
  
# TRANSLOG   combined.df[, paste0("x", lead.zero(i))] <- combined.df[, paste0("x", lead.zero(i))] / input.scaling
# TRANSLOG   combined.df[, paste0("w", lead.zero(i))] <- combined.df[, paste0("w", lead.zero(i))] / input.scaling

# TRANSLOG }

# TRANSLOG if (log.plus.one.cost) {
# TRANSLOG   ln.E.data.scaled <- with(combined.df, 
# TRANSLOG     log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + w05*x05 + w06*x06 + 1  )
# TRANSLOG   )
# TRANSLOG } else {
# TRANSLOG   ln.E.data.scaled <- with(combined.df, 
# TRANSLOG     log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + w05*x05 + w06*x06  )
# TRANSLOG   )
# TRANSLOG }

# TRANSLOG combined.df <- cbind(ln.E.data.scaled, combined.df)


# TRANSLOG colnames(combined.df)[colnames(combined.df)=="ln.E.data.scaled" ] <- "ln.E.data"
 


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
# This was bad bad

combined.df


top.before.data <- c(
"sets ",
paste0("  t number of observations  / 1 * ", nrow(combined.df), " /"),
paste0("  d number of variables in datafile  / 1 * ", ncol(combined.df), " /"),
paste0("  ss dimension of S matrix / 1 * ", N-1, "/"),
paste0("  sss second dimension of S matrix / 1 * ", N-1, "/"),
paste0("  cc dimension of C matrix / 1 * ", J, "/"),
paste0("  ccc second dimension of C matrix / 1 * ", J, "/"),
"  m number of points in interval z / 1 * ", length(other.param.support), "/",
"  h number of points in interval z / 1 * ", length(xi.param.support), "/",
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

data.declaration.lines <-  c( paste0(colnames(combined.df), "(t)"), paste0("xi", lead.zero(N)))




# TODO: do I need   indic(k)        copy of set k?

top.before.data <- c(top.before.data,
  data.declaration.lines,
  "  MM              number of points in support",
  "  JJ              number of points in support",
  "  HH              number of points in xi support",
  ";",
  "",
  "MM=", length(other.param.support), ";",
  "HH=", length(xi.param.support), ";",
  "JJ=3;",
  paste0("xi", lead.zero(N), "=1;"),
  #   paste0("xi", lead.zero(N), "=0;"),
  "", 
  "table datafile(t,d)"
)




data.alloc.lines <- c()

for (i in 1:ncol(combined.df) ) {
  data.alloc.lines <- c(data.alloc.lines, 
    paste0(colnames(combined.df)[i], "(t) = datafile(t,\"", i, "\");")
  )
}



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
"parameter zxi(h)    support points",
"/",
paste0(1:length(xi.param.support), "  ", xi.param.support),
"/;",
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



vsupport.lines <- c() 

for ( i in 1:length(demand.eqns))  {

  vsupport.lines <- c(vsupport.lines, 
  paste0("parameter vdem", i, "(j)  support space for error term;"),
  paste0("vdem", i, "(j) = vdemsupp", i, "(j);")
  )
  
}
# NOTE: maybe we do not want to same bounds on the error terms for each demand equation - this has been fixed above


CE.q.support.lines <- c() 

for ( i in 1:length(demand.eqns))  {

  CE.q.support.lines <- c(CE.q.support.lines, 
  paste0("parameter ceqdem", i, "(j)  support space for cross entropy;"),
  paste0("ceqdem", i, "(j) = ceqdemsupp", i, "(j);")
  )
  
}



# all.params <- gsub("[.]", "", names(ln.E.start.vals)[!grepl("region", names(ln.E.start.vals))] )

all.params <- unique(unlist(str_extract_all(unlist(demand.eqns.nonlinear), 
"(xi.[0-9][0-9])|(s.[0-9][0-9].[0-9][0-9])|(b.y.[0-9][0-9])|(b.[0-9][0-9])|(b.y.y)|(d.[0-9][0-9].[0-9][0-9])|(c.[0-9][0-9] )|(c.[0-9][0-9].[0-9][0-9])"
  ))
)

all.params <- all.params[!grepl(paste0("xi.", lead.zero(N)), all.params)]

all.params <- gsub("([.])|( )", "", all.params)


non.theta.param.support.lines <- c() 

for ( i in all.params[!grepl("xi", all.params)] ) {

  non.theta.param.support.lines <- c(non.theta.param.support.lines, 
  paste0("parameter z", i, "(m)  support space for params;"),
  paste0("z", i, "(m) = zother(m);")
  )
  
}

xi.param.support.lines <- c() 

for ( i in all.params[grepl("xi", all.params)] ) {

  xi.param.support.lines <- c(xi.param.support.lines, 
  paste0("parameter z", i, "(h)  support space for params;"),
  paste0("z", i, "(h) = zxi(h);")
  )
  
}




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
all.eqns <- paste0("dem", 1:length(demand.eqns.nonlinear))



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
  "  Smat(ss,sss)   S matrix to make cost function concave",
  "  SmatT(sss,ss)   transpose of S matrix to make cost function concave",
ifelse(convex.in.f.inputs,  "  Cmat(cc,ccc)   C matrix to make cost function convex in fixed inputs", ""),
ifelse(convex.in.f.inputs,  "  CmatT(ccc,cc)   transpose of C matrix to make cost function convex in fixed inputs", ""),
#  "  errorrelax(t) small value to accomodate the zero error adding up restriction",
  cov.var.declarations,
  paste0("  p", all.params[!grepl("xi", all.params)], "(m)    probability corresponding param"),
  paste0("  p", all.params[grepl("xi", all.params)], "(h)    probability corresponding param"),
  paste0("  w", all.eqns, "(t,j)    probability corresponding error term"),
#  "longlogsection(t)",
#  "sharedenom(t)",
  "  g           gme objective value",
  "",
  "positive variable",
  paste0("  p", all.params[!grepl("xi", all.params)], "(m)"),
  paste0("  p", all.params[grepl("xi", all.params)], "(h)"),
  paste0("  w", all.eqns, "(t,j)"),
#  paste0("  ", all.params[grepl("theta", all.params)], "   parameters to be estimated"),
#  "longlogsection(t)",
  ";"
)




objective.fn.lines <- c(
paste0("-sum(m, p", all.params[!grepl("xi", all.params)], "(m)*log(p", all.params[!grepl("xi", all.params)], "(m)+1.e-8) )"),
paste0("-sum(h, p", all.params[grepl("xi", all.params)], "(h)*log(p", all.params[grepl("xi", all.params)], "(h)+1.e-8) )"),
paste0("-sum((t,j), w", all.eqns, "(t,j)*log(w", all.eqns, "(t,j)+1.e-8) )"),
paste0("+sum((t,j), w", all.eqns, "(t,j)*log(ceq", all.eqns, "(j)+1.e-8) )"),
";"
)






objective.fn.lines[1] <- paste0( "object..           g =e= ", objective.fn.lines[1])
  



# TRANSLOG S.n.GAMS <- S.n

# TRANSLOG for (j in 1:length(S.n.GAMS)) {
# TRANSLOG for ( i in 1:N) {

# TRANSLOG   S.n.GAMS[[j]] <- gsub(paste0("w", lead.zero(i), " / [(]w", lead.zero(i), " [*] theta", 
# TRANSLOG     lead.zero(i), "[)]"), paste0("1 / theta", lead.zero(i)),  S.n.GAMS[[j]])
    
# TRANSLOG   S.n.GAMS[[j]] <- gsub(paste0("log[(]w", lead.zero(i), " [*] theta", lead.zero(i), "[)]"),
# TRANSLOG    paste0("(log(w", lead.zero(i), ") + log(theta", lead.zero(i), "))"),  S.n.GAMS[[j]])

# TRANSLOG }
# TRANSLOG }





# TRANSLOG S.n.GAMS <- lapply(S.n.GAMS, FUN=add.data.subscripts)

# TRANSLOG S.n.GAMS <- lapply(S.n.GAMS, FUN=function(x) gsub(pattern="[.]", replacement="", x=x))

#share.denom <- paste0("(", strsplit(S.n.GAMS[[1]], "[)] / [(]")[[1]][2] )

# TRANSLOG for (i in 1:length(S.n.GAMS)) {

# TRANSLOG  S.n.GAMS[[i]] <- paste0(strsplit(S.n.GAMS[[i]], "[)] / [(]")[[1]][1], ") / longlogsection(t)")

# TRANSLOG }


model.restrictions <- list()

GAMS.demand.eqns.nonlinear <- lapply(demand.eqns.nonlinear, FUN=add.data.subscripts)

GAMS.demand.eqns.nonlinear <- lapply(GAMS.demand.eqns.nonlinear, FUN=function(x) gsub(pattern="[.]", replacement="", x=x))
GAMS.demand.eqns.nonlinear <- lapply(GAMS.demand.eqns.nonlinear, FUN=function(x) gsub(pattern="[{^]", replacement="**", x=x))


for ( i in 1:length(GAMS.demand.eqns.nonlinear)) {

  first.eq.line.a <- paste0("restr", i, "dema(t)$(dem", i, "(t) gt 0)..") 
  first.eq.line.b <- paste0("restr", i, "demb(t)$(dem", i, "(t) eq 0)..")
  
  second.eq.line.a <- paste0("dem", i, "(t) =e= ", GAMS.demand.eqns.nonlinear[[i]],
    " + sum(j, vdem", i, "(j) * wdem", i,"(t, j))",  ";") 
  second.eq.line.b <- paste0("dem", i, "(t) =g= ", GAMS.demand.eqns.nonlinear[[i]], 
    " + sum(j, vdem", i, "(j) * wdem", i,"(t, j))", ";" )
  
  second.eq.line.a <- strwrap( second.eq.line.a, indent=12, exdent=19, width=80)
  second.eq.line.b <- strwrap( second.eq.line.b, indent=12, exdent=19, width=80)
  
  model.restrictions[[i]] <- c(first.eq.line.a, second.eq.line.a, first.eq.line.b, second.eq.line.b)
  
}




# TODO: change (w01 / (w01 * theta01)) to ( 1 / theta01). It is in cost share and cost function



# TRANSLOG ln.E.string.GAMS <- ln.E.string


# TRANSLOG for ( i in 1:N) {

# TRANSLOG   ln.E.string.GAMS <- gsub(paste0("w", lead.zero(i), " / [(]w", lead.zero(i), " [*] theta", 
# TRANSLOG     lead.zero(i), "[)]"), paste0("1 / theta", lead.zero(i)), ln.E.string.GAMS)
    
# TRANSLOG   ln.E.string.GAMS <- gsub(paste0("log[(]w", lead.zero(i), " [*] theta", lead.zero(i), "[)]"),
# TRANSLOG    paste0("(log(w", lead.zero(i), ") + log(theta", lead.zero(i), "))"),  ln.E.string.GAMS)

# TRANSLOG }


# TRANSLOG ln.E.string.GAMS <- gsub(pattern="[.]", replacement="", x=ln.E.string.GAMS)

# TRANSLOG ln.E.string.GAMS <- add.data.subscripts(ln.E.string.GAMS)


# TRANSLOG big.log.posi.constraint<- str_extract( ln.E.string.GAMS, "log[(] [(]1 / theta01[)].*")

# TRANSLOG big.log.posi.constraint <- gsub(" [+] region.*$", "", big.log.posi.constraint)



# TRANSLOG ln.E.string.linear <- gsub("[+]  log[(] [(]w01 [/] [(]w01 [*] theta01[)][)].*$", "", ln.E.string)
# TRANSLOG region.tackon.clean <- iconv(region.tackon, to="ASCII//TRANSLIT")
# TRANSLOG region.tackon.clean <- gsub("'", "", region.tackon.clean )
# TRANSLOG region.tackon.clean <- gsub("[.]", "", region.tackon.clean )


# TRANSLOG ln.E.string.GAMS <- str_replace(ln.E.string.GAMS, "log[(] [(]1 / theta01[)].*", "log(longlogsection(t))")

# TRANSLOG ln.E.string.GAMS<- paste0( ln.E.string.GAMS, " + ", add.data.subscripts(region.tackon.clean))

# TRANSLOG big.log.posi.constraint <- sub("log[(]", "", big.log.posi.constraint) 
# TRANSLOG big.log.posi.constraint <- sub("[)]$", "", big.log.posi.constraint) 

# TRANSLOG first.eq.line.a <- "restrcosta(t)$(cost(t) gt 0).."
# TRANSLOG first.eq.line.b <- "restrcostb(t)$(cost(t) eq 0).."
  
# TRANSLOG second.eq.line.a <- paste0("cost(t) =e= ", ln.E.string.GAMS, 
# TRANSLOG   " + sum(j, vcost(j) * wcost(t, j))", ";") 
# TRANSLOG second.eq.line.b <- paste0("cost(t) =g= ", ln.E.string.GAMS, 
# TRANSLOG   " + sum(j, vcost(j) * wcost(t, j))", ";") 
  
# TRANSLOG second.eq.line.a <- gsub("log[(]", "log(1.e-8 + ", second.eq.line.a)
# TRANSLOG second.eq.line.b <- gsub("log[(]", "log(1.e-8 + ", second.eq.line.b)

  
# TRANSLOG second.eq.line.a <- strwrap( second.eq.line.a, indent=12, exdent=19, width=80)
# TRANSLOG second.eq.line.b <- strwrap( second.eq.line.a, indent=12, exdent=19, width=80)
  
# TRANSLOG model.restrictions.cost <- c(first.eq.line.a, second.eq.line.a, first.eq.line.b, second.eq.line.b)




# theta01  =g= 0;
#theta02  =g= 0;
#theta03  =g= 0;
#theta04  =g= 0;
#theta05  =g= 0;

#restrpbeta0..        beta0 =e= sum(m, pbeta0(m) * zbeta0(m));

# TRANSLOG theta.positivity.restriction <- 
# TRANSLOG   paste0("restrthetaposi", lead.zero(1:(N-1)), "..      theta", lead.zero(1:(N-1)), "  =g= 0;")


# TRANSLOG big.log.posi.constraint.lines <- c("restrbiglogposi(t)..   " ,
# TRANSLOG    strwrap( paste0("longlogsection(t) =e= ", big.log.posi.constraint, ";"), indent=12, exdent=19, width=80)
# TRANSLOG    )


#share.denom.constraint.lines <- c("restrsharedenom(t)..   " ,
#   strwrap( paste0("sharedenom(t) =e= ", share.denom, ";"), indent=12, exdent=19, width=80)
#   )





#restr2(i,k)..        beta(i,k) =e= sum(m, p(i,k,m) * z(k, m));

#so we need to make this

#restrpbeta01..        beta01 =e= sum(m, pbeta01(m) * zbeta01(m));



prob.weight.param.lines <- c()

for ( i in all.params[!grepl("xi", all.params)]) {
  prob.weight.param.lines <- c(prob.weight.param.lines,  paste0(
    "restrp", i, "..        ", i," =e= sum(m, p", i, "(m) * z", i, "(m));" ),
    paste0(
    "restrpproper", i, "..            1 =e= sum(m, p", i, "(m));" )
  )
}

for ( i in all.params[grepl("xi", all.params)]) {
  prob.weight.param.lines <- c(prob.weight.param.lines,  paste0(
    "restrp", i, "..        ", i," =e= sum(h, p", i, "(h) * z", i, "(h));" ),
    paste0(
    "restrpproper", i, "..            1 =e= sum(h, p", i, "(h));" )
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







# TRANSLOG restriction.that.err.sum.to.zero.defn <- paste0("restrerrsumtozero(t)..        0 =e= ", paste0("sum(j, vs", 1:length(S.n), "(j) * ws", 1:length(S.n), "(t, j))", collapse=" + "), ";")
# errorrelax(t) + 

# TRANSLOG restriction.that.err.sum.to.zero.declare <- "restrerrsumtozero(t)"

# NOTE: Here we are assuming that we have dropped no equations above. Otherwise, this will be a big problem

# TRANSLOG errorrelaxrestrict.defn <- "errorrelaxrestrict(t)..    1e-06 =g= sqr(errorrelax(t));"

# TRANSLOG errorrelaxrestrict.declare <- "errorrelaxrestrict(t)"













if (!convex.in.f.inputs) {
  convex.restriction.declare <- ""
  Cmat.transpose.restriction.declare <- ""
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


  





# install.packages("stringr")
library("stringr")


GAMS.linear.results<- readLines(paste0(GAMS.projdir, "sgmGMElinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".lst"))


GAMS.linear.results <- GAMS.linear.results[
  -(1:grep("S O L V E      S U M M A R Y", GAMS.linear.results)) ]
  
# GAMS.linear.results <- gsub("(^1)|(   2 )|(   3 )", "", GAMS.linear.results)

param.gather.regex<- paste0("(^1)|", paste0("(   ", 2:length(other.param.support), " )", collapse="|"))

GAMS.linear.results <- gsub(param.gather.regex, "", GAMS.linear.results)

GAMS.linear.results.extracted <- str_extract(GAMS.linear.results, " p.*[.]L")

prob.names <- GAMS.linear.results.extracted[!is.na(GAMS.linear.results.extracted)]

prob.numbers <- GAMS.linear.results[which(!is.na(GAMS.linear.results.extracted)) + 2]

set.seed(100)
start.vals.lines <- paste0("xi", lead.zero(1:(N-1)), ".l = 1;")
# start.vals.lines <- paste0("xi", lead.zero(1:(N-1)), ".l = ", jitter(1), ";")
# start.vals.lines <- paste0("xi", lead.zero(1:(N-1)), ".l = 0;")
# added this to have correct (non-zero) starting vals for theta

if ( start.at.true.xi ) {
  start.vals.lines <- paste0("xi", lead.zero(1:(N-1)), ".l = ", 
    synthetic.params[grepl("xi", names(synthetic.params)) ][-N], ";")
}


for ( i in 1:length(prob.names)) {

  start.vals.lines <- c( start.vals.lines, 
    paste0( prob.names[i], "(\"", 1:length(other.param.support), "\") = ",  
      strsplit(prob.numbers[i], ",")[[1]], ";")
  )

}


GAMS.linear.results<- readLines(paste0(GAMS.projdir, "sgmGMElinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".lst"))

# GAMS.linear.results<-readLines("/Users/travismcarthur/Desktop/Dropbox/entropytestlinear.lst")

#GAMS.linear.results <- GAMS.linear.results[
#  -(1:grep("S O L V E      S U M M A R Y", GAMS.linear.results)) ]

begin.err.weight <- grep("L  probability corresponding error term", GAMS.linear.results)
end.err.weight  <- grep(paste0("^", nrow(combined.df), " "), GAMS.linear.results)

error.weights.lines <- c()


if ( !start.nonlin.from.ignorance) {

for ( i in 1:length(all.eqns) ) {
  
  err.weight.temp.df <- read.table( paste0(GAMS.projdir, "sgmGMElinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".lst"), 
    skip = begin.err.weight[i] + 3,  nrows= nrow(combined.df))

  err.weight.temp.df <- err.weight.temp.df[, -1 ]
  
  err.grid <- expand.grid( 1:3, 1:nrow(combined.df))
  
  for ( j in 1:nrow(err.grid)) {
  
    error.weights.lines  <- c(error.weights.lines, paste0(" w", all.eqns[i], ".l(\"", err.grid[j, 2], "\",\"",  
      err.grid[j, 1], "\") = ", err.weight.temp.df[err.grid[j, 2] , err.grid[j, 1]], ";")
    )
    
  }

}

}


#error.weights.lines  <- paste0(" w", all.eqns[i], ".l(\"", err.grid[, 2], "\",\"",  
#      err.grid[, 1], "\") = ", err.weight.temp.df[err.grid[, 2] , err.grid[, 1]], ";")







#  ptheta01.l(m) = 1/MM;
#  ptheta02.l(m) = 1/MM;
#  ptheta03.l(m) = 1/MM;
#  ptheta04.l(m) = 1/MM;
#  ptheta05.l(m) = 1/MM;
  
  
#  ptheta01.l("1") = .96
#  ptheta01.l("2") = .04
#  ptheta01.l("3") = 1.e-6

#theta.weight.lines <-  c( paste0("ptheta", lead.zero(1:(N-1)), ".l(\"1\") = 0.8000018;"),
#  paste0("ptheta", lead.zero(1:(N-1)), ".l(\"2\") = 0.1999972;"),
#  paste0("ptheta", lead.zero(1:(N-1)), ".l(\"3\") = 1.e-6;")
#  )

xi.weight.grid<- expand.grid(1:(N-1), 1:length(xi.param.support))

xi.weight.lines <-  paste0("pxi", lead.zero(xi.weight.grid[,1]), ".l(\"", xi.weight.grid[,2], 
  "\") = 1/", length(xi.param.support),";")

if ( start.at.true.xi ) {

xi.weight.lines <- c()

library("alabama")

for ( i in 1:(N-1) ) {

  desired.xi <- synthetic.params[grepl("xi", names(synthetic.params)) ][i]

  max.ent.fn <- function(x) {
    if (any(x<=0)) {return(500) }
    sum(x * log(x))
    
  }

  max.ent.constraints <- function(x) {
    c(sum(x) - 1, x %*% xi.param.support - desired.xi)
  }

  maxed.xi.weight <- auglag(par=rep(1/length(xi.param.support), length(xi.param.support)), 
    fn=max.ent.fn, heq=max.ent.constraints)$par
    
  xi.weight.lines <-  c(xi.weight.lines,
    paste0("pxi", lead.zero(i), ".l(\"", 1:length(xi.param.support), 
       "\") = ",  maxed.xi.weight    ,";")
    )
  
}


}

  
  

#theta.weight.lines 

# (4+1.e-6*10 ) / (5+1.e-6) = 0.8000018
 
# 1 - 0.8000018 - 1.e-6
 
# c( 0.8000018, 0.1999972, 1.e-6 ) %*% c(1.e-6, 5, 10)

# 1.e-6
# 25
# 50



# parameters to





GAMS.linear.results<- readLines(paste0(GAMS.projdir, "sgmGMElinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".lst"))


GAMS.linear.results.params<- GAMS.linear.results[grep("(parameters to be estimated$)|(SUR covar parameter$)", GAMS.linear.results)]

GAMS.linear.results.params <- GAMS.linear.results.params[grep("VARIABLE", GAMS.linear.results.params)]

GAMS.linear.results.params.names <- gsub("[.]L", "", str_extract(GAMS.linear.results.params, "[^ ]*[.]L") )


GAMS.linear.results.params.numbers <- as.numeric(gsub("(  parameters to be estimated)|(  SUR covar parameter)", "",
  str_extract(GAMS.linear.results.params, "([^ ]*  parameters to be estimated)|([^ ]*  SUR covar parameter)") ) )
  
  
combined.w.params.df <- as.list(as.data.frame(combined.df))

for ( i in 1:length(GAMS.linear.results.params.names)) {
  combined.w.params.df[[ GAMS.linear.results.params.names[i] ]] <- GAMS.linear.results.params.numbers[i]
}

for ( i in 1:N) {
  combined.w.params.df[[ paste0("xi", lead.zero(i)) ]] <- 1
}








Smat.start.vals.mat <- read.fwf( 
  file=paste0(GAMS.projdir, "sgmGMElinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".lst"), 
   widths=c(2, rep(12, N-1) ),
    skip = (grep("VARIABLE Smat.L", GAMS.linear.results)+3),  
    nrows= N-1)
    

    
Smat.start.vals.mat <- as.matrix(Smat.start.vals.mat[, -1])
    

Smat.initiation.v <- Smat.start.vals.mat
Smat.initiation.v[upper.tri(Smat.initiation.v)] <- 0
Smat.initiation.v[is.na(Smat.initiation.v)] <- 0
# sometimes zeros come back as blanks from GAMS, so need above line

Smat.initiation.v <- c(Smat.initiation.v)

if ( any(grepl("(Cmat)|(TIME)", gsub(" ", "", Smat.initiation.v)))) { Smat.initiation.v <- 0 }
# This is to deal with cse when whole matrix is zeroes

Smat.initiation.grid <- expand.grid(1:(N-1), 1:(N-1))

Smat.initial.values <- paste0("Smat.L(\"", Smat.initiation.grid[, 1], "\",\"", Smat.initiation.grid[, 2], "\") =  ", Smat.initiation.v, ";")

if(convex.in.f.inputs) {

Cmat.start.vals.mat <- read.fwf( 
  file=paste0(GAMS.projdir, "sgmGMElinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".lst"), 
   widths=c(2, rep(12, J) ),
    skip = (grep("VARIABLE Cmat.L", GAMS.linear.results)+3),  
    nrows= J)
    
Cmat.start.vals.mat <- as.matrix(Cmat.start.vals.mat[, -1])


Cmat.initiation.v <- Cmat.start.vals.mat
Cmat.initiation.v[upper.tri(Cmat.initiation.v)] <- 0
Cmat.initiation.v[ is.na(Cmat.initiation.v)] <- 0
Cmat.initiation.v <- c(Cmat.initiation.v)

if ( any(grepl("TIME", gsub(" ", "", Cmat.initiation.v)))) { Cmat.initiation.v <- 0 }

Cmat.initiation.grid <- expand.grid(1:J, 1:J)

Cmat.initial.values <- paste0("Cmat.L(\"", Cmat.initiation.grid[, 1], "\",\"", Cmat.initiation.grid[, 2], "\") =  ", Cmat.initiation.v, ";")


} else {
  Cmat.initial.values <- ""
}




# TRANSLOG combined.w.params.df <- as.list(as.data.frame(combined.df))

# TRANSLOG for ( i in 1:length(GAMS.linear.results.params.names)) {
# TRANSLOG   combined.w.params.df[[ GAMS.linear.results.params.names[i] ]] <- GAMS.linear.results.params.numbers[i]
# TRANSLOG }

# TRANSLOG for ( i in 1:N) {
# TRANSLOG   combined.w.params.df[[ paste0("xi", lead.zero(i)) ]] <- 1
# TRANSLOG }


# TRANSLOG modified.ln.E.string <- str_extract( ln.E.string, "log[(] [(]w01 / [(]w01 [*] theta01[)][)].*")

# TRANSLOG modified.ln.E.string <- gsub(" [+] region.*$", "", modified.ln.E.string)

# TRANSLOG modified.ln.E.string <- gsub(pattern="[.]", replacement="", x=modified.ln.E.string )

# TRANSLOG modified.ln.E.string <- sub("log[(]", "", modified.ln.E.string)
# TRANSLOG modified.ln.E.string <- sub("[)]$", "", modified.ln.E.string) 

# TRANSLOG modified.ln.E.string.evaled <- with(combined.w.params.df, eval(parse(text=modified.ln.E.string )))


# TRANSLOG longlogsection.initial <- paste0("longlogsection.l(\"", 1:nrow(combined.df), "\") = ", modified.ln.E.string.evaled, ";")




# TRANSLOG S.n.GAMS <- S.n

# TRANSLOG for (j in 1:length(S.n.GAMS)) {
# TRANSLOG for ( i in 1:N) {

# TRANSLOG   S.n.GAMS[[j]] <- gsub(paste0("w", lead.zero(i), " / [(]w", lead.zero(i), " [*] theta", 
# TRANSLOG     lead.zero(i), "[)]"), paste0("1 / theta", lead.zero(i)),  S.n.GAMS[[j]])
    
# TRANSLOG   S.n.GAMS[[j]] <- gsub(paste0("log[(]w", lead.zero(i), " [*] theta", lead.zero(i), "[)]"),
# TRANSLOG    paste0("(log(w", lead.zero(i), ") + log(theta", lead.zero(i), "))"),  S.n.GAMS[[j]])

# TRANSLOG }
# TRANSLOG }

#share.denom.to.eval <- paste0("(", strsplit(S.n.GAMS[[1]], "[)] / [(]")[[1]][2] )

#share.denom.to.eval <- gsub(pattern="[.]", replacement="", x=share.denom.to.eval )

#share.denom.evaled <- with(combined.w.params.df, eval(parse(text=share.denom.to.eval )))


#share.denom.initial <- paste0("sharedenom.l(\"", 1:nrow(combined.df), "\") = ", share.denom.evaled, ";")




param.starting.vals <- paste0(GAMS.linear.results.params.names, ".l = ", GAMS.linear.results.params.numbers, ";")



if ( start.nonlin.from.ignorance) {



 start.vals.lines <- c( paste0("xi", lead.zero(1:(N-1)), ".l = 1;"), 
   paste0("  p", all.params[!grepl("xi", all.params)], ".l(m) = 1/MM;"),
   paste0("  p", all.params[grepl("xi", all.params)], ".l(h) = 1/HH;")  )

 

set.seed(global.max.seed)

Smat.initiation.v <- matrix(rnorm((N-1)^2), nrow=N-1)
Smat.initiation.v[upper.tri(Smat.initiation.v)] <- 0
Smat.initiation.v <- c(Smat.initiation.v)

Smat.initiation.grid <- expand.grid(1:(N-1), 1:(N-1))

Smat.initial.values <- paste0("Smat.L(\"", Smat.initiation.grid[, 1], "\",\"", Smat.initiation.grid[, 2], "\") =  ", Smat.initiation.v, ";")


#set.seed(100)

Cmat.initiation.v <- matrix(rnorm(J^2), nrow=J)
Cmat.initiation.v[upper.tri(Cmat.initiation.v)] <- 0
Cmat.initiation.v <- c(Cmat.initiation.v)

Cmat.initiation.grid <- expand.grid(1:J, 1:J)

Cmat.initial.values <- paste0("Cmat.L(\"", Cmat.initiation.grid[, 1], "\",\"", Cmat.initiation.grid[, 2], "\") =  ", Cmat.initiation.v, ";")


#set.seed(100)




# Trying to deal with issue where SUR does crazy things

#set.seed(100)

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





if (global.max.seed!=0 ) {



#all.params.set.aside <- unique(unlist(str_extract_all(unlist(demand.eqns.nonlinear), 
#"(xi.[0-9][0-9])|(s.[0-9][0-9].[0-9][0-9])|(b.y.[0-9][0-9])|(b.[0-9][0-9])|(b.y.y)|(d.[0-9][0-9].[0-9][0-9])|(c.[0-9][0-9] )|(c.[0-9][0-9].[0-9][0-9])"
#  ))
#)




#all.params.set.aside <- gsub("( )|([.])", "", all.params.set.aside)
#all.params.set.aside <- all.params.set.aside[!grepl(paste0("xi", lead.zero(N)), all.params.set.aside)]

#all.params.set.aside <- c(all.params.set.aside[!grepl("xi", all.params.set.aside)],
#  all.params.set.aside[grepl("xi", all.params.set.aside)])

#param.starting.vals <- paste0(all.params.set.aside[!grepl("xi", all.params.set.aside)], ".l = ", 
#  rnorm(sum(!grepl("xi", all.params.set.aside))), ";")
  
#param.starting.vals <- c(param.starting.vals, 
#  paste0(all.params.set.aside[grepl("xi", all.params.set.aside)], ".l = ", 
#  rlnorm(sum(grepl("xi", all.params.set.aside))), ";") )
  
  

#chosen.global.search.params <- as.numeric(gsub("(.+=)|(;)", "", param.starting.vals))

#names(chosen.global.search.params) <- all.params.set.aside


# Above is only method to choose params. Below is new method, adopted from the synthetic params


all.params.set.aside <- unique(unlist(str_extract_all(unlist(demand.eqns), 
"(s.[0-9][0-9].[0-9][0-9])|(b.y.[0-9][0-9])|(b.[0-9][0-9])|(b.y.y)|(d.[0-9][0-9].[0-9][0-9])|(c.[0-9][0-9] )|(c.[0-9][0-9].[0-9][0-9])"
  ))
)

all.params.set.aside  <- gsub("([.])|( )", "", all.params.set.aside )


chosen.global.search.params <- rnorm(length(c(all.params.set.aside, paste0("xi", lead.zero(1:N))))) 
chosen.global.search.params <- as.numeric(chosen.global.search.params)
names(chosen.global.search.params) <- c(all.params.set.aside, paste0("xi", lead.zero(1:N)))

synthetic.S.mat <- matrix(0, N-1, N-1)
synthetic.T.mat <- matrix(0, N-1, N-1)

# synthetic.S.mat[lower.tri(synthetic.S.mat, diag=TRUE)] <-  names(  chosen.global.search.params[1:(N*(N-1)/2)] )
# A-ok

synthetic.T.mat[lower.tri(synthetic.T.mat, diag=TRUE)] <- rnorm((N*(N-1)/2))

synthetic.S.mat <-  - synthetic.T.mat %*% t(synthetic.T.mat)

chosen.global.search.params[1:(N*(N-1)/2)] <- synthetic.S.mat[lower.tri(synthetic.S.mat, diag=TRUE)] 


synthetic.C.mat <- matrix(0, J, J)
synthetic.A.mat <- matrix(0, J, J)

chosen.global.search.params <- chosen.global.search.params[names(chosen.global.search.params)!="c0202"] 

chosen.global.search.params <- c(chosen.global.search.params[1:grep(paste0("c01", lead.zero(J)), names(chosen.global.search.params))], 0, 
  chosen.global.search.params[(grep(paste0("c01", lead.zero(J)), names(chosen.global.search.params))+1):length(chosen.global.search.params)])

names(chosen.global.search.params)[grep(paste0("c01", lead.zero(J)), names(chosen.global.search.params))+1] <- "c0202"
# Because "c0202" appear out of order, must fix it with these two lines above



synthetic.A.mat[lower.tri(synthetic.A.mat, diag=TRUE)] <- rnorm((J*(J+1)/2))

synthetic.C.mat <-   synthetic.A.mat %*% t(synthetic.A.mat)

chosen.global.search.params[grepl("c[0-9]{4}", names(chosen.global.search.params))] <- synthetic.C.mat[lower.tri(synthetic.C.mat, diag=TRUE)] 

chosen.global.search.params <- chosen.global.search.params[!grepl(paste0("xi", lead.zero(N)), names(chosen.global.search.params))]

chosen.global.search.params[grepl("xi", names(chosen.global.search.params))] <- rlnorm(N-1)

while ( any(chosen.global.search.params[grepl("xi", names(chosen.global.search.params))] > 10) ) {
  which.invalid<- which(chosen.global.search.params[grepl("xi", names(chosen.global.search.params))] > 10)
  chosen.global.search.params[grepl("xi", names(chosen.global.search.params))][which.invalid] <- rlnorm(length(which.invalid))
}



chosen.global.search.params[grepl("(b[0-9][0-9])|((by[0-9][0-9]))", names(chosen.global.search.params))] <- 
  rlnorm(sum(grepl("(b[0-9][0-9])|(by[0-9][0-9])", names(chosen.global.search.params))))


chosen.global.search.params[grepl("byy", names(chosen.global.search.params))] <- 
  - rlnorm(sum(grepl("byy", names(chosen.global.search.params)))) / 100000
  # byy tends to have very low values in estimation, so divide by 100000

# NOTE: Some of these params are chosen to have good theoretical properties,
# and they mimic the properties in the synthetic params


param.starting.vals <- paste0(names(chosen.global.search.params), ".l = ", chosen.global.search.params, ";")



error.weights.lines <- vector("character", nrow(combined.df) * 3)

library("alabama")

for ( i in 1:length(all.eqns) ) {

#  stopifnot(length(all.eqns)==1) # will have to mess with LHS var if we are not only doing cost fn
  # This includes  E.y01.data below
  # I've fixed this now

  assign(paste0("xi", lead.zero(N)), 1)

  rhs.to.set.error.term    <- with(as.list(chosen.global.search.params) ,
      eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]])))) 
      
  err.term.maxed.ent.ls <- vector("list", length(rhs.to.set.error.term))
  
  
  if ( i == length(all.eqns)) {
    lhs.temp <- E.y01.data
  } else {
    lhs.temp <- get(paste0("x", lead.zero(i)))/y01
  }
  
  
  for ( k in 1:length(rhs.to.set.error.term) ) {

	desired.param.val <- (lhs.temp - rhs.to.set.error.term  )[k]
	desired.param.val <- ifelse( lhs.temp[k]==0 & rhs.to.set.error.term[k]<0, 0 ,  desired.param.val)

	max.ent.fn <- function(x) {
	  if (any(x<=0)) {return(500) }
	  sum(x * log(x)) - sum(x * log( CE.q.support.dem.eqns[[i]]  )) 
	
	}
  
	max.ent.constraints <- function(x) {
		err.off <- x %*% err.support.dem.eqns[[i]] - desired.param.val
		c((sum(x) - 1)*100000, err.off)
		# We scale the equality so that it has "same weight" as the inaccuraccy of the param
		# This avoids the problem where the optimization basically ignores the
		# constraint of having a prob that adds to 1
		# OK, nevermind, that turned out to be a  problem
	  }
	maxed.param.weight <- auglag(par=rep(1/length(err.support.dem.eqns[[i]]), length(err.support.dem.eqns[[i]])), 
		fn=max.ent.fn, 
		hin=function(x) {x*100000},
		heq=max.ent.constraints,
		control.outer=list(trace=FALSE))$par
	  
	err.term.maxed.ent.ls[[k]] <- maxed.param.weight
  }
  
  # NOTE: The above may be fragile if the estimated error terms go out of the error support
  # This operation doesn't 100% get us there since a few error weights come back negitive
 
  err.weight.temp.df <- do.call(rbind, err.term.maxed.ent.ls)

#  err.weight.temp.df <- err.weight.temp.df[, -1 ]
  
  err.grid <- expand.grid( 1:3, 1:nrow(combined.df))
  
  for ( j in 1:nrow(err.grid)) {
  
    error.weights.lines[j + (i-1)*nrow(combined.df)]  <- paste0(" w", all.eqns[i], ".l(\"", err.grid[j, 2], "\",\"",  
      err.grid[j, 1], "\") = ", err.weight.temp.df[err.grid[j, 2] , err.grid[j, 1]], ";")
    
    
  }

}








xi.weight.lines <- c()
start.vals.lines <- c()

library("alabama")

for ( i in 1:length(chosen.global.search.params) ) {

  desired.param.val <- chosen.global.search.params[i]

  max.ent.fn <- function(x) {
    if (any(x<=0)) {return(500) }
    sum(x * log(x))
    
  }
  
  if (grepl("xi", names(chosen.global.search.params)[i])) {
    max.ent.constraints <- function(x) {
      c(sum(x) - 1, x %*% xi.param.support - desired.param.val)
    }
    maxed.param.weight <- auglag(par=rep(1/length(xi.param.support), length(xi.param.support)), 
        fn=max.ent.fn, heq=max.ent.constraints,
        hin=function(x) {x*100000},
		control.outer=list(trace=FALSE))$par
  } else {
    max.ent.constraints <- function(x) {
      c(sum(x) - 1, x %*% other.param.support - desired.param.val)
    }
    maxed.param.weight <- auglag(par=rep(1/length(other.param.support), length(other.param.support)), 
        fn=max.ent.fn, heq=max.ent.constraints,
        hin=function(x) {x*100000},
		control.outer=list(trace=FALSE))$par
  }
  


    
  start.vals.lines <-  c(start.vals.lines,
    paste0("p", names(chosen.global.search.params)[i], ".l(\"", 1:length(maxed.param.weight), 
       "\") = ",  maxed.param.weight ,";")
    )
  
}


if (global.max.seed==0) { param.starting.vals<- c() }

cat(param.starting.vals, sep="\n")

# Problem: with random starting values, almost all error terms go as far negi as possible. 
# May be an issue with byy too high, or others too high

}

}




if (!convex.in.f.inputs) { 
  Cmat.initial.values <- ""
}

final.lines <- 
c(
"*Initial conditions",
# paste0("  p", all.params, ".l(m) = 1/MM;"),
# paste0("  w", all.eqns, ".l(t,j) = 1/JJ;"),
param.starting.vals,
start.vals.lines,
xi.weight.lines,
error.weights.lines,
Smat.initial.values,
Cmat.initial.values,
# TRANSLOG longlogsection.initial,
#share.denom.initial,
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
"OPTION NLP=CONOPTD;",
" ",
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
"solve gme using nlp maximizing g; ",
"options decimals = 7;"
)





cov.var.display <- paste0( "surdelta", expand.grid(1:length(all.eqns), 1:length(all.eqns))[, 1],
  expand.grid(1:length(all.eqns), 1:length(all.eqns))[, 2] )

cov.var.display.mat <- matrix(cov.var.display, nrow=length(all.eqns))

diag(cov.var.display.mat) <- ""
cov.var.display <- cov.var.display.mat
cov.var.display  <- cov.var.display[cov.var.display!= ""]  

cov.var.display  <- paste0("display ", cov.var.display, ".l;")

if (!do.SUR) { cov.var.display  <- c() }


parameter.display.lines <- c( paste0("display ", all.params, ".l;"),
  paste0("display p", all.params, ".l;"),
  paste0("display w", all.eqns, ".l;"),
  cov.var.display,
  paste0("display Smat.l"),
ifelse(convex.in.f.inputs,  paste0("display Cmat.l"), "")
#  paste0("display errorrelax.l")
  )


if (!convex.in.f.inputs) {
  Cmat.transpose.restriction.defn <- ""
  convex.restriction.defn <- ""
}

completed.GAMS.file <-  c(
  top.before.data, " ", 
  combined.df.GAMS, " ", 
  data.alloc.lines, " ", 
  param.support.simple.lines, " ", 
  greeks.support.simple.lines, " ",
  vsupport.lines, " ", 
  CE.q.support.lines, " ",
  non.theta.param.support.lines, " ", 
  xi.param.support.lines, " ", 
  variable.declaration.lines, " ", 
  equation.declarations, " ",
  objective.fn.lines, " ", 
  unlist(model.restrictions), " ", 
# TRANSLOG   model.restrictions.cost, " ", 
# TRANSLOG   theta.positivity.restriction, " ",
# TRANSLOG   big.log.posi.constraint.lines, " ",
#  share.denom.constraint.lines, " ",
  prob.weight.param.lines, " ", 
  prob.weight.error.lines, " ", 
  Smat.transpose.restriction.defn, " ", 
  Cmat.transpose.restriction.defn, " ", 
#  restriction.that.err.sum.to.zero.defn,
#  errorrelaxrestrict.defn,
  concave.restriction.defn, " ", 
  convex.restriction.defn, " ", 
  covar.SUR.lines,
  final.lines, " ",
  parameter.display.lines
)


  
cat(completed.GAMS.file, 
  file=paste0(GAMS.projdir, "sgmGMEnonlinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".gms"), 
  sep="\n")
  
# rvhess = maxdouble



