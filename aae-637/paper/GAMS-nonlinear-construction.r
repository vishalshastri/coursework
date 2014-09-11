




##### START FOR NONLINEAR::::





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
# This was bad bad



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

data.declaration.lines <-  c( paste0(colnames(combined.df), "(t)"), paste0("theta", lead.zero(N)))




# TODO: do I need   indic(k)        copy of set k?

top.before.data <- c(top.before.data,
  data.declaration.lines,
  "  MM              number of points in support",
  "  JJ              number of points in support",
  ";",
  "",
  "MM=3;",
  "JJ=3;",
  paste0("theta", lead.zero(N), "=1;"),
  "", 
  "table datafile(t,d)"
)




data.alloc.lines <- c()

for (i in 1:ncol(combined.df) ) {
  data.alloc.lines <- c(data.alloc.lines, 
    paste0(colnames(combined.df)[i], "(t) = datafile(t,\"", i, "\");")
  )
}


 
 


param.support.simple.lines <- c(
"parameter ztheta(m)    support points",
"/",
#"1  1.e-6",
#"2  5",
#"3  10",
"1  -4",
"2  1",
"3  6",
"/;",

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


all.params <- unique(str_extract_all(ln.E.string, 
"(theta[0-9][0-9])|(beta0)|(alpha[0-9][0-9])|(alpha[.][0-9][0-9][.][0-9][0-9])|(beta[0-9][0-9])|(beta[.][0-9][0-9][.][0-9][0-9])|(gamma[.][0-9][0-9][.][0-9][0-9])|(zeta[.][0-9][0-9][.][0-9][0-9])|(zeta[0-9][0-9])|(kappa[.][0-9][0-9][.][0-9][0-9])|(delta[.][0-9][0-9][.][0-9][0-9])|(region[0-9][0-9])"
  )[[1]]
)

all.params <- all.params[!grepl(paste0("theta", lead.zero(N)), all.params)]

all.params <- gsub("[.]", "", all.params)


non.theta.param.support.lines <- c() 

for ( i in all.params[!grepl("theta", all.params)] ) {

  non.theta.param.support.lines <- c(non.theta.param.support.lines, 
  paste0("parameter z", i, "(m)  support space for params;"),
  paste0("z", i, "(m) = zother(m);")
  )
  
}

theta.param.support.lines <- c() 

for ( i in all.params[grepl("theta", all.params)] ) {

  theta.param.support.lines <- c(theta.param.support.lines, 
  paste0("parameter z", i, "(m)  support space for params;"),
  paste0("z", i, "(m) = ztheta(m);")
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




all.eqns <- c("cost", paste0("s", 1:length(S.n)))

variable.declaration.lines <- c("variables",
  paste0("  ", all.params, "   parameters to be estimated"),
  paste0("  p", all.params, "(m)    probability corresponding param"),
  paste0("  w", all.eqns, "(t,j)    probability corresponding error term"),
  "longlogsection(t)",
#  "sharedenom(t)",
  "  h           gme objective value",
  "",
  "positive variable",
  paste0("  p", all.params, "(m)"),
  paste0("  w", all.eqns, "(t,j)"),
  paste0("  ", all.params[grepl("theta", all.params)], "   parameters to be estimated"),
  "longlogsection(t)",
  ";"
)




objective.fn.lines <- c(
paste0("-sum(m, p", all.params, "(m)*log(p", all.params, "(m)+1.e-8) )"),
paste0("-sum((t,j), w", all.eqns, "(t,j)*log(w", all.eqns, "(t,j)+1.e-8) )"),
";"
)


objective.fn.lines[1] <- paste0( "object..           h =e= ", objective.fn.lines[1])
  




add.data.subscripts <- function(x) {
  for (i in 1:99) {
    ii <- formatC(i, width = 2, flag = "0")  
    x <- gsub(paste0("w", ii), paste0("w", ii, "(t)"), x)
    x <- gsub(paste0("q", ii), paste0("q", ii, "(t)"), x)
    x <- gsub(paste0("x", ii), paste0("x", ii, "(t)"), x)
    x <- gsub(paste0("y", ii), paste0("y", ii, "(t)"), x)
    
  }
  regions.to.fix <- str_extract_all(x, "region[[:alpha:]]+")[[1]]
  
  for ( j in seq_along(regions.to.fix) ) {
    x <- gsub(regions.to.fix[j], paste0(regions.to.fix[j], "(t)"), x)
  }
  x
}


S.n.GAMS <- S.n

for (j in 1:length(S.n.GAMS)) {
for ( i in 1:N) {

  S.n.GAMS[[j]] <- gsub(paste0("w", lead.zero(i), " / [(]w", lead.zero(i), " [*] theta", 
    lead.zero(i), "[)]"), paste0("1 / theta", lead.zero(i)),  S.n.GAMS[[j]])
    
  S.n.GAMS[[j]] <- gsub(paste0("log[(]w", lead.zero(i), " [*] theta", lead.zero(i), "[)]"),
   paste0("(log(w", lead.zero(i), ") + log(theta", lead.zero(i), "))"),  S.n.GAMS[[j]])

}
}





S.n.GAMS <- lapply(S.n.GAMS, FUN=add.data.subscripts)

S.n.GAMS <- lapply(S.n.GAMS, FUN=function(x) gsub(pattern="[.]", replacement="", x=x))

#share.denom <- paste0("(", strsplit(S.n.GAMS[[1]], "[)] / [(]")[[1]][2] )

for (i in 1:length(S.n.GAMS)) {

 S.n.GAMS[[i]] <- paste0(strsplit(S.n.GAMS[[i]], "[)] / [(]")[[1]][1], ") / longlogsection(t)")

}


model.restrictions <- list()


for ( i in 1:length(S.n.GAMS)) {

  first.eq.line.a <- paste0("restr", i, "sa(t)$(s", i, "(t) gt 0)..") 
  first.eq.line.b <- paste0("restr", i, "sb(t)$(s", i, "(t) eq 0)..")
  
  second.eq.line.a <- paste0("s", i, "(t) =e= ", gsub("^.*~", "", S.n.GAMS[[i]]),
    " + sum(j, vs", i, "(j) * ws", i,"(t, j))",  ";") 
  second.eq.line.b <- paste0("s", i, "(t) =g= ", gsub("^.*~", "", S.n.GAMS[[i]]), 
    " + sum(j, vs", i, "(j) * ws", i,"(t, j))", ";" )
    
  second.eq.line.a <- gsub("log[(]", "log(1.e-8 + ", second.eq.line.a)
  second.eq.line.b <- gsub("log[(]", "log(1.e-8 + ", second.eq.line.b)
  
  second.eq.line.a <- strwrap( second.eq.line.a, indent=12, exdent=19, width=80)
  second.eq.line.b <- strwrap( second.eq.line.b, indent=12, exdent=19, width=80)
  
  model.restrictions[[i]] <- c(first.eq.line.a, second.eq.line.a, first.eq.line.b, second.eq.line.b)
  
  
  
}




# TODO: change (w01 / (w01 * theta01)) to ( 1 / theta01). It is in cost share and cost function



ln.E.string.GAMS <- ln.E.string


for ( i in 1:N) {

  ln.E.string.GAMS <- gsub(paste0("w", lead.zero(i), " / [(]w", lead.zero(i), " [*] theta", 
    lead.zero(i), "[)]"), paste0("1 / theta", lead.zero(i)), ln.E.string.GAMS)
    
  ln.E.string.GAMS <- gsub(paste0("log[(]w", lead.zero(i), " [*] theta", lead.zero(i), "[)]"),
   paste0("(log(w", lead.zero(i), ") + log(theta", lead.zero(i), "))"),  ln.E.string.GAMS)

}


ln.E.string.GAMS <- gsub(pattern="[.]", replacement="", x=ln.E.string.GAMS)

ln.E.string.GAMS <- add.data.subscripts(ln.E.string.GAMS)


big.log.posi.constraint<- str_extract( ln.E.string.GAMS, "log[(] [(]1 / theta01[)].*")

big.log.posi.constraint <- gsub(" [+] region.*$", "", big.log.posi.constraint)



ln.E.string.linear <- gsub("[+]  log[(] [(]w01 [/] [(]w01 [*] theta01[)][)].*$", "", ln.E.string)
region.tackon.clean <- iconv(region.tackon, to="ASCII//TRANSLIT")
region.tackon.clean <- gsub("'", "", region.tackon.clean )
region.tackon.clean <- gsub("[.]", "", region.tackon.clean )


ln.E.string.GAMS <- str_replace(ln.E.string.GAMS, "log[(] [(]1 / theta01[)].*", "log(longlogsection(t))")

ln.E.string.GAMS<- paste0( ln.E.string.GAMS, " + ", add.data.subscripts(region.tackon.clean))

big.log.posi.constraint <- sub("log[(]", "", big.log.posi.constraint) 
big.log.posi.constraint <- sub("[)]$", "", big.log.posi.constraint) 

first.eq.line.a <- "restrcosta(t)$(cost(t) gt 0).."
first.eq.line.b <- "restrcostb(t)$(cost(t) eq 0).."
  
second.eq.line.a <- paste0("cost(t) =e= ", ln.E.string.GAMS, 
  " + sum(j, vcost(j) * wcost(t, j))", ";") 
second.eq.line.b <- paste0("cost(t) =g= ", ln.E.string.GAMS, 
  " + sum(j, vcost(j) * wcost(t, j))", ";") 
  
second.eq.line.a <- gsub("log[(]", "log(1.e-8 + ", second.eq.line.a)
second.eq.line.b <- gsub("log[(]", "log(1.e-8 + ", second.eq.line.b)

  
second.eq.line.a <- strwrap( second.eq.line.a, indent=12, exdent=19, width=80)
second.eq.line.b <- strwrap( second.eq.line.a, indent=12, exdent=19, width=80)
  
model.restrictions.cost <- c(first.eq.line.a, second.eq.line.a, first.eq.line.b, second.eq.line.b)




# theta01  =g= 0;
#theta02  =g= 0;
#theta03  =g= 0;
#theta04  =g= 0;
#theta05  =g= 0;

#restrpbeta0..        beta0 =e= sum(m, pbeta0(m) * zbeta0(m));

theta.positivity.restriction <- 
  paste0("restrthetaposi", lead.zero(1:(N-1)), "..      theta", lead.zero(1:(N-1)), "  =g= 0;")


big.log.posi.constraint.lines <- c("restrbiglogposi(t)..   " ,
   strwrap( paste0("longlogsection(t) =e= ", big.log.posi.constraint, ";"), indent=12, exdent=19, width=80)
   )


#share.denom.constraint.lines <- c("restrsharedenom(t)..   " ,
#   strwrap( paste0("sharedenom(t) =e= ", share.denom, ";"), indent=12, exdent=19, width=80)
#   )





#restr2(i,k)..        beta(i,k) =e= sum(m, p(i,k,m) * z(k, m));

#so we need to make this

#restrpbeta01..        beta01 =e= sum(m, pbeta01(m) * zbeta01(m));



prob.weight.param.lines <- c()

for ( i in all.params) {
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
  paste0("restrthetaposi", lead.zero(1:(N-1))), # Added this for theta posi restrictions
  "restrbiglogposi(t)",
#  "restrsharedenom(t)",
  ";"
)
  





# install.packages("stringr")
library("stringr")


GAMS.linear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytestlinear.lst")

GAMS.linear.results <- GAMS.linear.results[
  -(1:grep("S O L V E      S U M M A R Y", GAMS.linear.results)) ]
  
GAMS.linear.results <- gsub("(^1)|(   2 )|(   3 )", "", GAMS.linear.results)

GAMS.linear.results.extracted <- str_extract(GAMS.linear.results, " p.*[.]L")

prob.names <- GAMS.linear.results.extracted[!is.na(GAMS.linear.results.extracted)]

prob.numbers <- GAMS.linear.results[which(!is.na(GAMS.linear.results.extracted)) + 2]

start.vals.lines <- paste0("theta", lead.zero(1:(N-1)), ".l = 1;")
# added this to have correct (non-zero) starting vals for theta

for ( i in 1:length(prob.names)) {

  start.vals.lines <- c( start.vals.lines, 
    paste0( prob.names[i], "(\"", 1:3, "\") = ",  strsplit(prob.numbers[i], ",")[[1]], ";")
  )

}


GAMS.linear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytestlinear.lst")

#GAMS.linear.results <- GAMS.linear.results[
#  -(1:grep("S O L V E      S U M M A R Y", GAMS.linear.results)) ]

begin.err.weight <- grep("L  probability corresponding error term", GAMS.linear.results)
end.err.weight  <- grep(paste0("^", nrow(combined.df), " "), GAMS.linear.results)

error.weights.lines <- c()

for ( i in 1:length(all.eqns) ) {
  
  err.weight.temp.df <- read.table("/Users/travismcarthur/Desktop/Dropbox/entropytestlinear.lst", 
    skip = begin.err.weight[i] + 3,  nrows= nrow(combined.df))

  err.weight.temp.df <- err.weight.temp.df[, -1 ]
  
  err.grid <- expand.grid( 1:3, 1:nrow(combined.df))
  
  for ( j in 1:nrow(err.grid)) {
  
    error.weights.lines  <- c(error.weights.lines, paste0(" w", all.eqns[i], ".l(\"", err.grid[j, 2], "\",\"",  
      err.grid[j, 1], "\") = ", err.weight.temp.df[err.grid[j, 2] , err.grid[j, 1]], ";")
    )
    
  }

}





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


theta.weight.lines <-  c( paste0("ptheta", lead.zero(1:(N-1)), ".l(\"1\") = 1/3;"),
  paste0("ptheta", lead.zero(1:(N-1)), ".l(\"2\") = 1/3;"),
  paste0("ptheta", lead.zero(1:(N-1)), ".l(\"3\") = 1/3;")
  )

theta.weight.lines 

# (4+1.e-6*10 ) / (5+1.e-6) = 0.8000018
 
# 1 - 0.8000018 - 1.e-6
 
# c( 0.8000018, 0.1999972, 1.e-6 ) %*% c(1.e-6, 5, 10)

# 1.e-6
# 25
# 50









GAMS.linear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytestlinear.lst")


GAMS.linear.results.params<- GAMS.linear.results[grep("parameters to be esti$", GAMS.linear.results)]

GAMS.linear.results.params.names <- gsub("[.]L", "", str_extract(GAMS.linear.results.params, "[^ ]*[.]L") )


GAMS.linear.results.params.numbers <- as.numeric(gsub("  parameters to be esti", "",
  str_extract(GAMS.linear.results.params, "[^ ]*  parameters to be esti") ) )
  
combined.w.params.df <- as.list(as.data.frame(combined.df))

for ( i in 1:length(GAMS.linear.results.params.names)) {
  combined.w.params.df[[ GAMS.linear.results.params.names[i] ]] <- GAMS.linear.results.params.numbers[i]
}

for ( i in 1:N) {
  combined.w.params.df[[ paste0("theta", lead.zero(i)) ]] <- 1
}














combined.w.params.df <- as.list(as.data.frame(combined.df))

for ( i in 1:length(GAMS.linear.results.params.names)) {
  combined.w.params.df[[ GAMS.linear.results.params.names[i] ]] <- GAMS.linear.results.params.numbers[i]
}

for ( i in 1:N) {
  combined.w.params.df[[ paste0("theta", lead.zero(i)) ]] <- 1
}


modified.ln.E.string <- str_extract( ln.E.string, "log[(] [(]w01 / [(]w01 [*] theta01[)][)].*")

modified.ln.E.string <- gsub(" [+] region.*$", "", modified.ln.E.string)

modified.ln.E.string <- gsub(pattern="[.]", replacement="", x=modified.ln.E.string )

modified.ln.E.string <- sub("log[(]", "", modified.ln.E.string)
modified.ln.E.string <- sub("[)]$", "", modified.ln.E.string) 

modified.ln.E.string.evaled <- with(combined.w.params.df, eval(parse(text=modified.ln.E.string )))


longlogsection.initial <- paste0("longlogsection.l(\"", 1:nrow(combined.df), "\") = ", modified.ln.E.string.evaled, ";")




S.n.GAMS <- S.n

for (j in 1:length(S.n.GAMS)) {
for ( i in 1:N) {

  S.n.GAMS[[j]] <- gsub(paste0("w", lead.zero(i), " / [(]w", lead.zero(i), " [*] theta", 
    lead.zero(i), "[)]"), paste0("1 / theta", lead.zero(i)),  S.n.GAMS[[j]])
    
  S.n.GAMS[[j]] <- gsub(paste0("log[(]w", lead.zero(i), " [*] theta", lead.zero(i), "[)]"),
   paste0("(log(w", lead.zero(i), ") + log(theta", lead.zero(i), "))"),  S.n.GAMS[[j]])

}
}

#share.denom.to.eval <- paste0("(", strsplit(S.n.GAMS[[1]], "[)] / [(]")[[1]][2] )

#share.denom.to.eval <- gsub(pattern="[.]", replacement="", x=share.denom.to.eval )

#share.denom.evaled <- with(combined.w.params.df, eval(parse(text=share.denom.to.eval )))


#share.denom.initial <- paste0("sharedenom.l(\"", 1:nrow(combined.df), "\") = ", share.denom.evaled, ";")




param.starting.vals <- paste0(GAMS.linear.results.params.names, ".l = ", GAMS.linear.results.params.numbers, ";")



final.lines <- 
c(
"*Initial conditions",
# paste0("  p", all.params, ".l(m) = 1/MM;"),
# paste0("  w", all.eqns, ".l(t,j) = 1/JJ;"),
param.starting.vals,
start.vals.lines,
theta.weight.lines,
error.weights.lines,
longlogsection.initial,
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
"solve gme using nlp maximizing h; ",
"options decimals = 7;"
)

parameter.display.lines <- c( paste0("display ", all.params, ".l;"),
  paste0("display p", all.params, ".l;"),
  paste0("display w", all.eqns, ".l;")
  )




completed.GAMS.file <-  c(
  top.before.data, " ", 
  combined.df.GAMS, " ", 
  data.alloc.lines, " ", 
  param.support.simple.lines, " ", 
  vsupport.lines, " ", 
  non.theta.param.support.lines, " ", 
  theta.param.support.lines, " ", 
  variable.declaration.lines, " ", 
  equation.declarations, " ",
  objective.fn.lines, " ", 
  unlist(model.restrictions), " ", 
  model.restrictions.cost, " ", 
  theta.positivity.restriction, " ",
  big.log.posi.constraint.lines, " ",
#  share.denom.constraint.lines, " ",
  prob.weight.param.lines, " ", 
  prob.weight.error.lines, " ", 
  final.lines, " ",
  parameter.display.lines
)

cat(completed.GAMS.file, 
  file="/Users/travismcarthur/Desktop/Metrics (637)/Final paper/GAMS work/entropytest.gms", sep="\n")
  



