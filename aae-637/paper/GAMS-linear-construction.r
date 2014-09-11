


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

ln.E.data.scaled <- with(combined.df, 
  log(w01*x01 + w02*x02 + w03*x03 + w04*x04 + w05*x05 + w06*x06 + 1  )
  )

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
"  m number of points in interval z / 1 * 3/",
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




# TODO: do I need   indic(k)        copy of set k?

top.before.data <- c(top.before.data,
  data.declaration.lines,
  "  MM              number of points in support",
  "  JJ              number of points in support",
  ";",
  "",
  "MM=3;",
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


param.support.simple.lines <- c( # Eliminated theta here
"parameter zother(m)    support points",
"/",
paste0("1 ", -round( max(abs(coef(linear.sur.est))) * 3 )),
"2  0",
paste0("3  ", round( max(abs(coef(linear.sur.est))) * 3 )),
"/;",
"parameter vshare(j)    support points ",
"/",
"1 -1.2",
"2  0",
"3  1.2",
"/;",
"parameter vcost(j)    support points ",
"/",
paste0("1 ", -12), # -round( max(combined.df$cost) * 5 )
"2  0",
paste0("3  ", 12), #round( max(combined.df$cost) * 5 )
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

variable.declaration.lines <- c("variables",
  paste0("  ", all.params, "   parameters to be estimated"),
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



equation.declarations <- c(
  "equations",
  "  object             primal GME objective function",
  gsub("[.]{2}.*$", "", prob.weight.param.lines),
  gsub("[.]{2}.*$", "",  prob.weight.error.lines),
  "restrcosta(t)",
  "restrcostb(t)",
  paste0("restr", 1:length(S.n), "sa(t)"),
  paste0("restr", 1:length(S.n), "sb(t)"),
  ";"
)
  
  
parameter.display.lines <- c( paste0("display ", all.params, ".l;"),
  paste0("display p", all.params, ".l;"),
  paste0("display w", all.eqns, ".l;")
  )



final.lines <- 
c(
"*Initial conditions",
paste0("  p", all.params, ".l(m) = 1/MM;"),
paste0("  w", all.eqns, ".l(t,j) = 1/JJ;"),
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
" ",
"solve gme using nlp maximizing h; ",
"options decimals = 7;"
)



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
  final.lines, " ",
  parameter.display.lines 
)

cat(completed.GAMS.file, 
  file="/Users/travismcarthur/Desktop/Metrics (637)/Final paper/GAMS work/entropytestlinear.gms", sep="\n")
  
  



