





##### START FOR LINEAR::::


# as.data.frame(region.matrix) 



#exists.vec <- Vectorize( exists)

# M <- 1
# N <- 6
# J <- 3

combined.df <- data.frame(mget(c("y01", paste0("x", lead.zero(1:N)), 
  paste0("w", lead.zero(1:N)),  paste0("q", lead.zero(1:J)) )))
  
region.matrix.df <-   as.data.frame(region.matrix)


colnames(region.matrix.df) <- iconv(colnames(region.matrix.df), to="ASCII//TRANSLIT")
colnames(region.matrix.df) <- gsub("'", "", colnames(region.matrix.df) )
colnames(region.matrix.df) <- gsub("[.]", "", colnames(region.matrix.df) )
  
combined.df <- cbind(combined.df, region.matrix.df)


log10_ceiling <- function(x) {
    10^(ceiling(log10(x)))
}
# Thanks to http://stackoverflow.com/questions/7906996/algorithm-to-round-to-the-next-order-of-magnitude-in-r


for ( i in 1:N) {

  input.scaling  <- log10_ceiling(
    sqrt(sum((c(combined.df[, paste0("x", lead.zero(i))], 
    combined.df[, paste0("w", lead.zero(i))])^2)/(nrow(combined.df)-1)))
  )
  # Got this idea from scale() function
  
  input.scaling <- input.scaling / 100
  
  combined.df[, paste0("x", lead.zero(i))] <- combined.df[, paste0("x", lead.zero(i))] / input.scaling
  combined.df[, paste0("w", lead.zero(i))] <- combined.df[, paste0("w", lead.zero(i))] / input.scaling

}

if (log.plus.one.cost) {
  ln.E.data.scaled <- with(combined.df, 
    log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + w05*x05 + w06*x06 + 1  )
  )
} else {
  ln.E.data.scaled <- with(combined.df, 
    log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + w05*x05 + w06*x06  )
  )
}



combined.df <- cbind(ln.E.data.scaled, combined.df)


 colnames(combined.df)[colnames(combined.df)=="ln.E.data.scaled" ] <- "ln.E.data"


for (i in 1:length(S.n)) {

  combined.df[, paste0("s", i)] <- with(combined.df, eval( parse(text=gsub("~.*$", "", S.n[[i]]) ) ))

}



# combined.df <- scale(combined.df, center=FALSE)

top.before.data <- c(
"sets ",
paste0("  t number of observations  / 1 * ", nrow(combined.df), " /"),
paste0("  d number of variables in datafile  / 1 * ", ncol(combined.df), " /"),
"  m number of points in interval z / 1 * ", length(other.param.support), "/",
"  j number of points in interval v / 1 * 3 /;",
"",
"parameter",
"  datafile(t,d)   raw data")

# cost.dep.line <- "  cost(t)          log of cost"

# cost.share.parm.lines <- paste0("  s", 1:length(S.n),"(t)          cost share ", 1:length(S.n))

combined.df.GAMS <- make.GAMS.data(combined.df)

combined.df.GAMS <- c(combined.df.GAMS, ";")

colnames(combined.df)[colnames(combined.df)=="ln.E.data"] <- "cost"

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

param.support.simple.lines <- c( # Eliminated theta here
"parameter zother(m)    support points",
"/",
paste0(1:length(other.param.support), "  ", other.param.support),
"/;",
"parameter vshare(j)    support points ",
"/",
paste0(1:length(share.err.support), "  ", share.err.support),
"/;",
"parameter vcost(j)    support points ",
"/",
paste0(1:length(cost.err.support), "  ", cost.err.support),
"/;"
)


vsupport.lines <- c() 

for ( i in paste0("s", 1:length(S.n)))  {

  vsupport.lines <- c(vsupport.lines, 
  paste0("parameter v", i, "(j)  support space for error term;"),
  paste0("v", i, "(j) = vshare(j);")
  )
  
}




# all.params <- gsub("[.]", "", names(ln.E.start.vals)[!grepl("region", names(ln.E.start.vals))] )

ln.E.string <- nls.formula.ln.E.region


all.params <- unique(str_extract_all(ln.E.string, 
"(theta[0-9][0-9])|(beta0)|(alpha[0-9][0-9])|(alpha[.][0-9][0-9][.][0-9][0-9])|(beta[0-9][0-9])|(beta[.][0-9][0-9][.][0-9][0-9])|(gamma[.][0-9][0-9][.][0-9][0-9])|(zeta[.][0-9][0-9][.][0-9][0-9])|(zeta[0-9][0-9])|(kappa[.][0-9][0-9][.][0-9][0-9])|(delta[.][0-9][0-9][.][0-9][0-9])|(region[0-9][0-9])"
  )[[1]]
)

all.params <- gsub("[.]", "", all.params)


all.params <- all.params[!grepl("theta", all.params)]

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




all.eqns <- c("cost", paste0("s", 1:length(S.n)))

cov.var.declarations <- paste0( "surdelta", expand.grid(1:length(all.eqns), 1:length(all.eqns))[, 1],
  expand.grid(1:length(all.eqns), 1:length(all.eqns))[, 2] )

cov.var.declarations.mat <- matrix(cov.var.declarations, nrow=length(all.eqns))

diag(cov.var.declarations.mat) <- ""
cov.var.declarations <- cov.var.declarations.mat
cov.var.declarations  <- cov.var.declarations[cov.var.declarations!= ""]  
  
cov.var.declarations <- paste0("  ", cov.var.declarations,  "    SUR covar parameter")

  

variable.declaration.lines <- c("variables",
  paste0("  ", all.params, "   parameters to be estimated"),
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



S.n.linear <- lapply(S.n, FUN=function(x) gsub(
  pattern="[)] [/] [(][(]w[0-9][0-9].*$", replacement=")", x=x))

S.n.linear <- lapply(S.n.linear, FUN=add.data.subscripts)

S.n.linear <- lapply(S.n.linear, FUN=function(x) gsub(pattern="[.]", replacement="", x=x))

S.n.GAMS <- gsub(" [*] theta[0-9][0-9]", "", S.n.linear)




model.restrictions <- list()


for ( i in 1:length(S.n.GAMS)) {

  first.eq.line.a <- paste0("restr", i, "sa(t)$(s", i, "(t) gt 0)..") 
  first.eq.line.b <- paste0("restr", i, "sb(t)$(s", i, "(t) eq 0)..")
  
  second.eq.line.a <- paste0("s", i, "(t) =e= ", gsub("^.*~", "", S.n.GAMS[[i]]),
    " + sum(j, vs", i, "(j) * ws", i,"(t, j))",  ";") 
  second.eq.line.b <- paste0("s", i, "(t) =g= ", gsub("^.*~", "", S.n.GAMS[[i]]), 
    " + sum(j, vs", i, "(j) * ws", i,"(t, j))", ";" )
  
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


ln.E.string.linear <- gsub("[+]  log[(] [(]w01 [/] [(]w01 [*] theta01[)][)].*$", "", ln.E.string)

region.tackon.clean <- iconv(region.tackon, to="ASCII//TRANSLIT")
region.tackon.clean <- gsub("'", "", region.tackon.clean )
region.tackon.clean <- gsub("[.]", "", region.tackon.clean )

ln.E.string.linear <- paste0( ln.E.string.linear, " + ", region.tackon.clean)

ln.E.string.linear <- add.data.subscripts(ln.E.string.linear)

ln.E.string.linear <- gsub(pattern="[.]", replacement="", x=ln.E.string.linear)

ln.E.string.GAMS <- gsub(" [*] theta[0-9][0-9]", "", ln.E.string.linear)




first.eq.line.a <- "restrcosta(t)$(cost(t) gt 0).."
first.eq.line.b <- "restrcostb(t)$(cost(t) eq 0).."
  
second.eq.line.a <- paste0("cost(t) =e= ", ln.E.string.GAMS, 
  " + sum(j, vcost(j) * wcost(t, j))", ";") 
second.eq.line.b <- paste0("cost(t) =g= ", ln.E.string.GAMS, 
  " + sum(j, vcost(j) * wcost(t, j))", ";") 
  
second.eq.line.a <- strwrap( second.eq.line.a, indent=12, exdent=19, width=80)
second.eq.line.b <- strwrap( second.eq.line.a, indent=12, exdent=19, width=80)
  
model.restrictions.cost <- c(first.eq.line.a, second.eq.line.a, first.eq.line.b, second.eq.line.b)



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




all.eqns <- c("cost", paste0("s", 1:length(S.n)))

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


cov.rest.declarations <- paste0( "restrcov", expand.grid(1:length(all.eqns), 1:length(all.eqns))[, 1],
  expand.grid(1:length(all.eqns), 1:length(all.eqns))[, 2] )
  
cov.rest.declarations.mat <- matrix(cov.rest.declarations, nrow=length(all.eqns))

diag(cov.rest.declarations.mat) <- ""
cov.rest.declarations <- cov.rest.declarations.mat
cov.rest.declarations  <- cov.rest.declarations[cov.rest.declarations != ""]


equation.declarations <- c(
  "equations",
  "  object             primal GME objective function",
  gsub("[.]{2}.*$", "", prob.weight.param.lines),
  gsub("[.]{2}.*$", "",  prob.weight.error.lines),
  "restrcosta(t)",
  "restrcostb(t)",
  paste0("restr", 1:length(S.n), "sa(t)"),
  paste0("restr", 1:length(S.n), "sb(t)"),
  cov.rest.declarations,
  ";"
)







# Trying to deal with issue where SUR does crazy things

error.weights.lines <- c()

for ( i in 1:length(all.eqns) ) {
  
  err.weight.temp.df <- data.frame(A=jitter(rep(1/3, nrow(combined.df))),
                                   B=jitter(rep(1/3, nrow(combined.df))),
                                   C=jitter(rep(1/3, nrow(combined.df))))

#  err.weight.temp.df <- err.weight.temp.df[, -1 ]
  
  err.grid <- expand.grid( 1:3, 1:nrow(combined.df))
  
  for ( j in 1:nrow(err.grid)) {
  
    error.weights.lines  <- c(error.weights.lines, paste0(" w", all.eqns[i], ".l(\"", err.grid[j, 2], "\",\"",  
      err.grid[j, 1], "\") = ", err.weight.temp.df[err.grid[j, 2] , err.grid[j, 1]], ";")
    )
    
  }

}























cov.var.display <- paste0( "surdelta", expand.grid(1:length(all.eqns), 1:length(all.eqns))[, 1],
  expand.grid(1:length(all.eqns), 1:length(all.eqns))[, 2] )

cov.var.display.mat <- matrix(cov.var.display, nrow=length(all.eqns))

diag(cov.var.display.mat) <- ""
cov.var.display <- cov.var.display.mat
cov.var.display  <- cov.var.display[cov.var.display!= ""]  


  
parameter.display.lines <- c( paste0("display ", all.params, ".l;"),
  paste0("display p", all.params, ".l;"),
  paste0("display w", all.eqns, ".l;"),
  paste0("display ", cov.var.display, ".l;")
  )



final.lines <- 
c(
"*Initial conditions",
paste0("  p", all.params, ".l(m) = 1/MM;"),
paste0("  w", all.eqns, ".l(t,j) = 1/JJ;"),
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


completed.GAMS.file <-  c(
  top.before.data, " ", 
  combined.df.GAMS, " ", 
  data.alloc.lines, " ", 
  param.support.simple.lines, " ", 
  vsupport.lines, " ", 
  non.theta.param.support.lines, " ",  # Eliminated thetas here
  variable.declaration.lines, " ", 
  equation.declarations, " ",
  objective.fn.lines, " ", 
  unlist(model.restrictions), " ", 
  model.restrictions.cost, " ", 
  prob.weight.param.lines, " ", 
  prob.weight.error.lines, " ", 
  covar.SUR.lines,
  final.lines, " ",
  parameter.display.lines 
)


 

cat(completed.GAMS.file, 
  file=paste0(GAMS.projdir, "GMElinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".gms"), 
  sep="\n")
  
  
# "(recall that uninitialized parameters take on value zero)."
# http://www.me.utexas.edu/~bard/LP/LP%20Handouts/gams_intro.pdf

# ** Warning **  Memory Limit for Hessians exceeded.
# You can use the Conopt option "rvhess"

# rvhess = maxdouble




