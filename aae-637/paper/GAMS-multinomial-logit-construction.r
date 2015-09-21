


##### START FOR MULTINOMIAL LOGIT::::

# NOTE: I used the translog preparation as the basis for
# The code below, rather than the SGM. Probably should
# have used the SGM. I edited this a bit
# to bring in the SGM code in some places.


# as.data.frame(region.matrix) 

file.flavor <- "typical"


#exists.vec <- Vectorize( exists)

# M <- 1
# N <- 6
# J <- 3

if (FALSE) { y01 <- y01/10 }
if (FALSE) { y01 <- y01*10 }

combined.df <- data.frame(mget(c("y01", paste0("x", lead.zero(1:N)), 
  paste0("w", lead.zero(1:N)),  paste0("q", lead.zero(1:J)),
  paste0("mode", 1:nalts) )))
  
#region.matrix.df <-   as.data.frame(region.matrix)


#colnames(region.matrix.df) <- iconv(colnames(region.matrix.df), to="ASCII//TRANSLIT")
#colnames(region.matrix.df) <- gsub("'", "", colnames(region.matrix.df) )
#colnames(region.matrix.df) <- gsub("[.]", "", colnames(region.matrix.df) )
  
#combined.df <- cbind(combined.df, region.matrix.df)




log10_ceiling <- function(x) {
    10^(ceiling(log10(x)))
}
# Thanks to http://stackoverflow.com/questions/7906996/algorithm-to-round-to-the-next-order-of-magnitude-in-r




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
paste0("  s dimension of S matrix / 1 * ", N-1, "/"),
paste0("  ss second dimension of S matrix / 1 * ", N-1, "/"),
"",
#"alias (s, ss);",
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
  "table datafile(t,d)"
)




data.alloc.lines <- c()

for (i in 1:ncol(combined.df) ) {
  data.alloc.lines <- c(data.alloc.lines, 
    paste0(colnames(combined.df)[i], "(t) = datafile(t,\"", i, "\");")
  )
}


greeks.support.simple.lines <- paste0(
"parameter inputmean", lead.zero(1:N), "    input mean value", lead.zero(1:N),  "\n",
"/", "\n",
mean.of.inputs, "\n",
"/;", "\n"
)



#other.param.support cost.err.support share.err.support




# all.params <- gsub("[.]", "", names(ln.E.start.vals)[!grepl("region", names(ln.E.start.vals))] )

all.params <- unique(unlist(str_extract_all(unlist(demand.eqns), 
"(s.[0-9][0-9].[0-9][0-9])|(b.y.[0-9][0-9])|(b.[0-9][0-9])|(b.y.y)|(d.[0-9][0-9].[0-9][0-9])|(c.[0-9][0-9] )|(c.[0-9][0-9].[0-9][0-9])"
  ))
)







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



all.params <- gsub("[.]", "", all.params)


alt.params <- list()

for ( i in 1:(nalts-1) )  {
  alt.params[[i]] <- all.params
  space.index <- grepl(" ", alt.params[[i]])
  alt.params[[i]] <- gsub(" ", "", alt.params[[i]])
  alt.params[[i]] <- paste0(alt.params[[i]], "alt", i)
  alt.params[[i]][ space.index ] <- paste0(alt.params[[i]][ space.index ], " ")
}



# all.params <- gsub(" ", "", all.params)

  

variable.declaration.lines <- c("variables",
  paste0("  ", unlist(alt.params), "   parameters to be estimated"),
#  "  Smat(s,ss)   S matrix to make cost function concave",
#  "  SmatT(ss,s)   transpose of S matrix to make cost function concave",
#  "  errorrelax(t) small value to accomodate the zero error adding up restriction",
  "  h           MLE objective value",
  "  overflow_protect a trick to protect the exp from overflow",
  "  inner2(t)   a part of MLE function",
  "  inner1(t) b part of MLE function",
  "  part2(t) c part of MLE function",
  paste0("  costfnexpralt", 1:nalts, "(t)    intermediate expression"),
  ";"
)







objective.fn.lines <- c("object..           h =e= sum(t, inner1(t)-part2(t) ) ", ";")





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



#S.n.linear <- lapply(S.n, FUN=function(x) gsub(
#  pattern="[)] [/] [(][(]w[0-9][0-9].*$", replacement=")", x=x))

#S.n.linear <- lapply(S.n.linear, FUN=add.data.subscripts)

#S.n.linear <- lapply(S.n.linear, FUN=function(x) gsub(pattern="[.]", replacement="", x=x))

#S.n.GAMS <- gsub(" [*] theta[0-9][0-9]", "", S.n.linear)




cost.fn.expr.w.subscripts <- add.data.subscripts(cost.fn.expr)

cost.fn.expr.w.subscripts <- gsub("[.]", "", cost.fn.expr.w.subscripts)
# IMPORTANT: NOTE: This order of the two lines above matters or else there will be 
# a big problem where the by.01 param gets interpreted as if it is data

cost.fn.expr.w.subscripts <- gsub(pattern="[{^]", replacement="**", x=cost.fn.expr.w.subscripts)


alt.cost.fn.expr.ls <- list()

for ( i in 1:(nalts-1) )  {
  # reverse the order of the params so that the wrong params do not get replaced
  targ.order <- rev( order( nchar(all.params) ) )
  all.params.to.replace <- all.params[ targ.order ]
  all.alt.params.to.replace <- alt.params[[i]][ targ.order ]
  alt.cost.fn.expr.ls[[ i ]] <- cost.fn.expr.w.subscripts
  for ( j in 1:length( all.params ))  {
  alt.cost.fn.expr.ls[[ i ]]  <- gsub(
      all.params.to.replace[j], all.alt.params.to.replace[j], 
      alt.cost.fn.expr.ls[[ i ]], fixed=TRUE
    )
    # fixed= TRUE so that the . do not get interpreted as any character
  }
}

alt.cost.fn.expr.ls[[length(alt.cost.fn.expr.ls)+1]] <- 0

# 0 since the last option is [all zeros] because of exclusion of last category



cost.fn.alt.expressions.ls <- list()
# Make list above for this

for ( i in 1:length(alt.cost.fn.expr.ls) )  {

  cost.fn.alt.expressions.ls[[i]] <- c( paste0( "costeqnalt", i, "(t).."),
    strwrap( paste0("costfnexpralt", i, "(t) =e= ",
       alt.cost.fn.expr.ls[[ i ]]), indent=12, exdent=19, width=80), ";" ) 

}



#overflow.protect.eqn <- 
#  c(paste0( "overflow_protect_eqn..  overflow_protect =e= max(", paste0( "costfnexpralt", 1:(length(alt.cost.fn.expr.ls)-1), "(t)", collapse=", "), ")"), ";")



overflow.protect.eqn <-   paste0( "overflow_protect_eqn", 1:(length(alt.cost.fn.expr.ls)-1), "(t)..  overflow_protect =g= ", 
    "costfnexpralt", 1:(length(alt.cost.fn.expr.ls)-1), "(t)", "\n ; \n")

# Much thanks to http://jblevins.org/log/log-sum-exp

# Also thanks to http://stackoverflow.com/questions/24588946/gams-maximum-element

#objVal =e= smax(p, abs( y(p) - (m * x(p) + b) ));

#objFun1(p) ..  objVal =g= y(p) - (m * x(p) + b);
#objFun2(p) ..  objVal =g= -( y(p) - (m * x(p) + b) );

#objFun1(p) ..  objVal + x(p) * m + b =g= y(p);
#objFun2(p) ..  objVal - x(p) * m - b =g= - y(p);

#y(p) - (m * x(p) + b)
#y(p) - m * x(p) + b











inner.2.eqn <- c("inner2eqn(t)..    inner2(t)     =e= ", 
  strwrap( paste0("overflow_protect + log(", paste0("exp(costfnexpralt", 1:length(alt.cost.fn.expr.ls), "(t) - overflow_protect)", collapse= " + "), ")"), 
  indent=12, exdent=19, width=80), ";" )
  # Placing log here instead of as part of part.2 so that the inner2 var does not hit the
  # GAMS "infinity" upper limit

part.2.eqn <- c("part2eqn(t)..     part2(t)    =e= ", 
  strwrap( paste0("mode", 1:length(alt.cost.fn.expr.ls), "(t) * inner2(t)", collapse= " + "), 
  indent=12, exdent=19, width=80), ";" )
  
# NOTE: This part is redundant, I think, since the indicator variable will
# end up being 1 once for each observation

inner.1.eqn <- c("inner1eqn(t)..    inner1(t)     =e= ", 
  strwrap( paste0("mode", 1:length(alt.cost.fn.expr.ls), "(t) * costfnexpralt", 1:length(alt.cost.fn.expr.ls),
  "(t)", collapse= " + "), 
  indent=12, exdent=19, width=80), ";" )

#  * log(  )


model.restrictions <- list(cost.fn.alt.expressions.ls, inner.2.eqn, part.2.eqn, inner.1.eqn, overflow.protect.eqn)




#for ( i in 1:length(S.n.GAMS)) {

#  first.eq.line.a <- paste0("restr", i, "sa(t)$(s", i, "(t) gt 0)..") 
#  first.eq.line.b <- paste0("restr", i, "sb(t)$(s", i, "(t) eq 0)..")
  
#  second.eq.line.a <- paste0("s", i, "(t) =e= ", gsub("^.*~", "", S.n.GAMS[[i]]),
#    " + sum(j, vs", i, "(j) * ws", i,"(t, j))",  ";") 
#  second.eq.line.b <- paste0("s", i, "(t) =g= ", gsub("^.*~", "", S.n.GAMS[[i]]), 
#    " + sum(j, vs", i, "(j) * ws", i,"(t, j))", ";" )
  
#  second.eq.line.a <- strwrap( second.eq.line.a, indent=12, exdent=19, width=80)
#  second.eq.line.b <- strwrap( second.eq.line.b, indent=12, exdent=19, width=80)
  
#  model.restrictions[[i]] <- c(first.eq.line.a, second.eq.line.a, first.eq.line.b, second.eq.line.b)
  
#}



#ln.E.string.GAMS <- as.character(S.n.H[[length(S.n.H)]])[3]

#ln.E.string.GAMS <- gsub("I", "", ln.E.string.GAMS)

#ln.E.string.GAMS <- gsub("[{^]", "**", ln.E.string.GAMS)
# including "{" because it helps us get a literal "^"

#ln.E.string.GAMS <- gsub("[:]", "*", ln.E.string.GAMS)

#ln.E.string.GAMS <- add.data.subscripts(ln.E.string.GAMS)

#ln.E.string.GAMS <- gsub(pattern="[.]", replacement="", x=ln.E.string.GAMS)


# ln.E.string.linear <- gsub("[+]  log[(] [(]w01 [/] [(]w01 [*] theta01[)][)].*$", "", ln.E.string)

# region.tackon.clean <- iconv(region.tackon, to="ASCII//TRANSLIT")
# region.tackon.clean <- gsub("'", "", region.tackon.clean )
# region.tackon.clean <- gsub("[.]", "", region.tackon.clean )

#ln.E.string.linear <- paste0( ln.E.string.linear, " + ", region.tackon.clean)

#ln.E.string.linear <- add.data.subscripts(ln.E.string.linear)

#ln.E.string.linear <- gsub(pattern="[.]", replacement="", x=ln.E.string.linear)

# ln.E.string.GAMS <- gsub(" [*] theta[0-9][0-9]", "", ln.E.string.linear)




#first.eq.line.a <- "restrcosta(t)$(cost(t) gt 0).."
#first.eq.line.b <- "restrcostb(t)$(cost(t) eq 0).."
  
#second.eq.line.a <- paste0("cost(t) =e= ", ln.E.string.GAMS, 
#  " + sum(j, vcost(j) * wcost(t, j))", ";") 
#second.eq.line.b <- paste0("cost(t) =g= ", ln.E.string.GAMS, 
#  " + sum(j, vcost(j) * wcost(t, j))", ";") 
  
#second.eq.line.a <- strwrap( second.eq.line.a, indent=12, exdent=19, width=80)
#second.eq.line.b <- strwrap( second.eq.line.a, indent=12, exdent=19, width=80)
  
#model.restrictions.cost <- c(first.eq.line.a, second.eq.line.a, first.eq.line.b, second.eq.line.b)



#restr2(i,k)..        beta(i,k) =e= sum(m, p(i,k,m) * z(k, m));

#so we need to make this

#restrpbeta01..        beta01 =e= sum(m, pbeta01(m) * zbeta01(m));



# CONCAVE RESTRICTIONS BELOW: 


# double.beta.params<- unique(str_extract_all(gsub("[.]", "", ln.E.string), "beta[0-9]{4}")[[1]])

# double.beta.indices <- str_extract_all(double.beta.params, "[0-9]{2}")

# concave.restriction.defn <- c()

# for ( i in 1:length(double.beta.indices)) {

#  first.ind <- as.numeric(double.beta.indices[[i]][1])
#  second.ind <- as.numeric(double.beta.indices[[i]][2])

#   concave.restriction.defn <- c(concave.restriction.defn,  paste0( "restrconcave", double.beta.params[i], "..      ", double.beta.params[i], " =e= - sum(ss, Smat(\"", 
#   first.ind - 1, "\",ss)*SmatT(ss,\"", second.ind - 1, "\"));") )
  

  
#   if (first.ind==second.ind) {next}
  # since we don't want to zero out the diagonal elements
    
#   concave.restriction.defn <- c(concave.restriction.defn,  paste0( "restrconcavelowertri", 
#   first.ind, second.ind, "..      ", 0, " =e= Smat(\"", 
#   first.ind - 1, "\",", "\"", second.ind - 1, "\");")) 
  
# }

# concave.restriction.declare <- c()

# for ( i in 1:length(double.beta.indices)) {
#   concave.restriction.declare <- c(concave.restriction.declare, 
#     paste0( "restrconcave", double.beta.params[i]))
    
#    first.ind <- as.numeric(double.beta.indices[[i]][1])
#    second.ind <- as.numeric(double.beta.indices[[i]][2])
   


#   if (first.ind==second.ind) {next}
  
#   concave.restriction.declare <- c(concave.restriction.declare, 
#     paste0( "restrconcavelowertri", first.ind, second.ind))
    
# }

# Smat.transpose.restriction.defn <- "restrSmattrans(s,ss).. Smat(s,ss) =e= SmatT(ss,s);"

# Smat.transpose.restriction.declare <- "restrSmattrans"


# restriction.that.err.sum.to.zero.defn <- paste0("restrerrsumtozero(t)..        0 =e=  ", paste0("sum(j, vs", 1:length(S.n), "(j) * ws", 1:length(S.n), "(t, j))", collapse=" + "), ";")


# restriction.that.err.sum.to.zero.declare <- "restrerrsumtozero(t)"

# NOTE: Here we are assuming that we have dropped no equations above (for the translog adding up restrictions of error term). Otherwise, this will be a big problem

# errorrelaxrestrict.defn <- "errorrelaxrestrict(t)..    1e-06 =g= sqr(errorrelax(t));"

# errorrelaxrestrict.declare <- "errorrelaxrestrict(t)"




#concave.restriction.defn <- c()

#for ( i in 1:length(double.beta.indices)) {

# first.ind <- as.numeric(double.beta.indices[[i]][1])
# second.ind <- as.numeric(double.beta.indices[[i]][2])

#  concave.restriction.defn <- c(concave.restriction.defn,  paste0( "restrconcave", double.beta.params[i], "..      ", double.beta.params[i], " =e= - sum(ss, Smat(\"", 
#  first.ind - 1, "\",ss)*SmatT(ss,\"", second.ind - 1, "\"));") )
  
#}






equation.declarations <- c(
  "equations",
  "  object             MLE objective function",
  paste0("costeqnalt", 1:length(alt.cost.fn.expr.ls), "(t)"),
  "inner2eqn(t)",
  "part2eqn(t)",
  "inner1eqn(t)",
  paste0("overflow_protect_eqn", 1:(length(alt.cost.fn.expr.ls)-1) ),
#  gsub("[.]{2}.*$", "", prob.weight.param.lines),
#  gsub("[.]{2}.*$", "",  prob.weight.error.lines),
#  "restrcosta(t)",
#  "restrcostb(t)",
#  paste0("restr", 1:length(S.n), "sa(t)"),
#  paste0("restr", 1:length(S.n), "sb(t)"),
#  concave.restriction.declare,
#  Smat.transpose.restriction.declare,
#  restriction.that.err.sum.to.zero.declare,
#  errorrelaxrestrict.declare,
#  cov.rest.declarations,
  ";"
)





# MORE CONCAVE RESTRICTION CODE BELOW:

set.seed(100)

#Smat.initiation.v <- matrix(rnorm((N-1)^2), nrow=N-1)
#Smat.initiation.v[upper.tri(Smat.initiation.v)] <- 0
#Smat.initiation.v <- c(Smat.initiation.v)

#Smat.initiation.grid <- expand.grid(1:(N-1), 1:(N-1))

#Smat.initial.values <- paste0("Smat.L(\"", Smat.initiation.grid[, 1], "\",\"", Smat.initiation.grid[, 2], "\") =  ", #Smat.initiation.v, ";")


set.seed(100)











  
parameter.display.lines <- c( paste0("display ", gsub(" ", "", unlist(alt.params)), ".l;"),
"display overflow_protect.l;"

#  paste0("display p", all.params, ".l;"),
#  paste0("display w", all.eqns, ".l;"),
#  paste0("display ", cov.var.display, ".l;")
#  paste0("display Smat.l")
#  paste0("display errorrelax.l")
  )

# set.seed(100)
# paste0(gsub(" ", "", unlist(alt.params)), ".l = ", rnorm(length(unlist(alt.params)), mean=0, sd=5), ";"),
# Ok, it seems that GAMS hits non-feasibility pretty quick if I just randomize params

final.lines <- 
c(
"*Initial conditions",
"inner2.l(t) = 1.1;", # To avoid log of zero
paste0("costfnexpralt", 1:nalts, ".l(t) = 1.1;"), # To avoid log of zero


#paste0("  p", all.params, ".l(m) = 1/MM;"),
#paste0("  w", all.eqns, ".l(t,j) = 1/JJ;"),
#Smat.initial.values,
# error.weights.lines,
"* final model setup",
"model mle /all/;",
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
"MLE.OPTFILE=1; ", # Change to MLE.OPTFILE instead of GME.OPTFILE
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
"solve mle using nlp maximizing h; ",
"options decimals = 7;"
)
# Help from http://support.gams.com/doku.php?id=gams:how_do_i_reduce_the_size_of_my_listing_.lst_file
# on reducing size of list file

# " RTMAXV = 1.00E+20",

completed.GAMS.file <-  c(
  top.before.data, " ", 
  combined.df.GAMS, " ", 
  data.alloc.lines, " ", 
  greeks.support.simple.lines, " ",
#  param.support.simple.lines, " ", 
#  vsupport.lines, " ", 
#  non.theta.param.support.lines, " ",  # Eliminated thetas here
  variable.declaration.lines, " ", 
  equation.declarations, " ",
  objective.fn.lines, " ", 
  unlist(model.restrictions), " ", 
#  model.restrictions.cost, " ", 
#  prob.weight.param.lines, " ", 
#  prob.weight.error.lines, " ", 
#  concave.restriction.defn, " ",
#  Smat.transpose.restriction.defn, " ",
#  restriction.that.err.sum.to.zero.defn, " ",
#  errorrelaxrestrict.defn, " ",
#  covar.SUR.lines,
  final.lines, " ",
  parameter.display.lines 
)


mle.GAMS.output <- TRUE


if ( mle.GAMS.output ) { 

  cat(completed.GAMS.file, 
    file=paste0(GAMS.projdir, "MLEmultinomiallogit", strsplit(target.crop, " ")[[1]][1], 
     formatC(bootstrap.iter, width = 5, flag = "0"), file.flavor ,".gms"), 
    sep="\n")
  
}



# "(recall that uninitialized parameters take on value zero)."
# http://www.me.utexas.edu/~bard/LP/LP%20Handouts/gams_intro.pdf

# ** Warning **  Memory Limit for Hessians exceeded.
# You can use the Conopt option "rvhess"

# rvhess = maxdouble



if ( FALSE ) {
  run.multinom.logit.from.shell <-paste0("cd ", GAMS.projdir, "\n", 
    GAMS.exe.path, " ", 
    "MLEmultinomiallogit", strsplit(target.crop, " ")[[1]][1], 
    formatC(bootstrap.iter, width = 5, flag = "0"), file.flavor , ".gms", 
    " Ps=0 suppress=1")
  system(run.multinom.logit.from.shell)
}

# , file.flavor
