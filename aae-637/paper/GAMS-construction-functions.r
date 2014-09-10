
#  round to 6 significant digits
#  make 4 columns
#  4 spaces between columns


# install.packages("gdata")
library("gdata")




make.GAMS.data <- function(x) {

raw.first.df <- x[, 1:4]

colnames(raw.first.df) <- sprintf("%10s", as.character(1:4))

raw.first.df<- as.data.frame(apply(signif(raw.first.df, digits=5), 2, FUN=as.character))

row.names(raw.first.df) <- paste0( " ", format(1:nrow(raw.first.df), justify="right") )

colnames(raw.first.df)[1] <- paste0(paste0(rep(" ", max(nchar(row.names(raw.first.df)))), collapse=""), colnames(raw.first.df)[1])

file <- tempfile()

write.fwf(raw.first.df, file=file, 
  append=FALSE, quote=FALSE, sep="    ", na="", justify="right", width = c(max(nchar(row.names(raw.first.df))), 10,10,10,10),
  rownames=TRUE, rowCol="")

processed.first.df <- readLines(file)

unlink(file)


#"/Users/travismcarthur/Desktop/Metrics (637)/Final paper/GAMS work/fwf test.gms"

num.body.dataframes <- (ncol(x) - 4) %/% 4

processed.body <- list()

for ( i in 1:num.body.dataframes) {

  raw.body.df <- x[, (1:4)+4*i]

  colnames(raw.body.df) <- sprintf("%10s", as.character((1:4)+4*i))

  raw.body.df<- as.data.frame(apply(signif(raw.body.df, digits=5), 2, FUN=as.character))

  row.names(raw.body.df) <- paste0( " ", format(1:nrow(raw.body.df), justify="right") )

  colnames(raw.body.df)[1] <- paste0(paste0(rep(" ", max(nchar(row.names(raw.body.df)))), collapse=""), colnames(raw.body.df)[1])

  file <- tempfile()

  write.fwf(raw.body.df, file=file, 
    append=FALSE, quote=FALSE, sep="    ", na="", justify="right", width = c(max(nchar(row.names(raw.body.df))), 10,10,10,10),
    rownames=TRUE, rowCol="")

  processed.body[[i]] <- readLines(file)
  
  processed.body[[i]][1] <- sub(" ", "+", processed.body[[i]][1])
  # This only substitutes the first space, since it is not gsub()
  
  processed.body[[i]] <- c(processed.body[[i]][1], 
    paste0(rep(" ", nchar(processed.body[[i]][1])), collapse=""), processed.body[[i]][-1]
    )
  
  unlink(file)

}

if ( ncol(x) > 4*(num.body.dataframes+1) ) {

  raw.last.df <- x[, (4*(num.body.dataframes+1)+1):ncol(x) ]
  
  colnames(raw.last.df) <- sprintf("%10s", (4*(num.body.dataframes+1)+1):ncol(x))

  raw.last.df<- as.data.frame(apply(signif(raw.last.df, digits=5), 2, FUN=as.character))

  row.names(raw.last.df) <- paste0( " ", format(1:nrow(raw.last.df), justify="right") )

  colnames(raw.last.df)[1] <- paste0(paste0(rep(" ", max(nchar(row.names(raw.last.df)))), collapse=""), colnames(raw.last.df)[1])

  file <- tempfile()

  write.fwf(raw.last.df, file=file, 
    append=FALSE, quote=FALSE, sep="    ", na="", justify="right", width = c(max(nchar(row.names(raw.last.df))), 
      rep(10, ncol(raw.last.df))),
    rownames=TRUE, rowCol="")

  processed.last.df <- readLines(file)
  
  processed.last.df[1] <- sub(" ", "+", processed.last.df[1])
  # This only substitutes the first space, since it is not gsub()
  
  processed.last.df <- c(processed.last.df[1], 
    paste0(rep(" ", nchar(processed.last.df[1])), collapse=""), processed.last.df[-1]
    )

  
  unlink(file)

} else { 
  processed.last.df <- NULL
}

ret <- processed.first.df

for ( i in 1:length(processed.body) ) {
  ret <- c(ret,  paste0(rep(" ", max(nchar(ret))), collapse=""),  processed.body[[i]]) 
}

ret <- c(ret, paste0(rep(" ", max(nchar(ret))), collapse=""), processed.last.df)

ret

# TODO: it would be nice to include the column names in the GAMS comments

}




# make.GAMS.data(data.frame(matrix(runif(100*23)+1, ncol=23, nrow=100)))


# TODO: will need to check for any numbers of the form 2.20265e+04







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
  



















linear.sur.est.ols <- systemfit( S.n.H, method="OLS", restrict.matrix = lm.param.restrictions,  maxit = 1 )


linear.sur.est.ols.single <- systemfit( S.n.H[length(S.n.H)], "OLS", restrict.matrix = lm.param.restrictions[36:46],  maxit = 5000 )







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



ln.E.string.GAMS <- ln.E.string


for ( i in 1:N) {

  ln.E.string.GAMS <- gsub(paste0("w", lead.zero(i), " / [(]w", lead.zero(i), " [*] theta", 
    lead.zero(i), "[)]"), paste0("1 / theta", lead.zero(i)), ln.E.string.GAMS)
    
  ln.E.string.GAMS <- gsub(paste0("log[(]w", lead.zero(i), " [*] theta", lead.zero(i), "[)]"),
   paste0("(log(w", lead.zero(i), ") + log(theta", lead.zero(i), "))"),  ln.E.string.GAMS)

}

ln.E.string.GAMS <- gsub(pattern="[.]", replacement="", x=ln.E.string.GAMS)

big.log.posi.constraint.2<- str_extract( ln.E.string.GAMS, "log[(] [(]1 / theta01[)].*")

big.log.posi.constraint.2 <- sub("log[(]", "", big.log.posi.constraint.2) 
big.log.posi.constraint.2 <- sub("[)]$", "", big.log.posi.constraint.2) 


with(combined.w.params.df , eval(parse(text=big.log.posi.constraint.2)))

log(with(combined.w.params.df , eval(parse(text=big.log.posi.constraint.2))))

test.expression <- "(log(w01) + log(theta01)) "

with(combined.w.params.df , eval(parse(text=test.expression)))




for making sure data is a-ok:


parameters
test1(t)
test2(t)
test3(t)
test4(t)
test5(t)
test6(t)
test7(t)
test8(t)
test9(t);

test1(t) =  (w01(t)=0)   ;
test2(t) =  (w02(t)=0)   ;
test3(t) =  (w03(t)=0)   ;
test4(t) =  (w04(t)=0)   ;
test5(t) =  (w05(t)=0)   ;
test6(t) =  (w06(t)=0)   ;
test7(t) =  (q01(t)=1)   ;
* test7(t) =  (q01(t)>1)   ;
test8(t) =  (q02(t)=0)   ;
test9(t) =  (q03(t)=0)   ;

display test1;
display test2;
display test3;
display test4;
display test5;
display test6;
display test7;
display test8;
display test9;
display q01;





OPTION NLP=BARON;
* We only have demo version of this:  PATHNLP
* We only have demo version of this:  OQNLP
* Not suitable: NLPEC
* We only have demo version of this:  MSNLP
* We only have demo version of this: KNITRO
* Not avail: COUENNE
* We only have demo version of this: SNOPT
* We only have demo version of this:  BARON
* No license: LINDOGLOBAL
* No license: LGO
* No licsnes: MOSEK

* Maybe good: received weird error: AMPL

* This ran for a hot sec before choking: COINIPOPT


CONVERT
EMP
EXAMINER
GAMSBAS
GAMSCHK
LINDOWRAP
LINGO
MINOS55
MPECDUMP
MPSWRITE
PATHGMO
PATHNLP
PATHSMAG
TRAMP


# PROMISING!!!!!!:
OPTION NLP=CONOPTD;  

maybe try:
OPTION lfstal=10000;

lmmxsf = 1
lfnicr = 1000
rvhess = 0
0.05 



4.3736

....559


solve gme using nlp maximizing h;
options decimals = 7;

display theta01.l;
display theta02.l;
display theta03.l;
display theta04.l;
display theta05.l;
display theta06;























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


# TODO: change (w01 / (w01 * theta01)) to ( 1 / theta01). It is in cost share and cost function


# TODO: leaving off geography for now...

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
  
  











p.l(i,k,m) = 1/MM;
pp.l(m) = 1/ MM;
w.l(t,i,j) = 1/JJ;


z("7",m) = z2(m);



  
1 0.3288806,    2 0.3333134,    3 0.3378060







----  11589 VARIABLE pdelta0103.L  probability corresponding param

1 0.3346314,    2 0.3333317,    3 0.3320370






----  11588 VARIABLE pbeta0.L  probability corresponding param


















Powers:
**
http://support.gams.com/doku.php?id=gams:difference_between_the_-operator_and_the_power_function










ln.E.string.linear <- gsub("[+]  log[(] [(]w01 [/] [(]w01 [*] theta01[)][)].*$", "", ln.E.string)

ln.E.string.linear <- gsub(" [*] theta[0-9][0-9]", "", ln.E.string.linear)



, 















































#display beta.l;
#scalar alpha   the constant term in aggregrate price equation;
#  alpha = sum(m, pp.l(m)*zz(m));
#  display alpha;























restrerror(t)..        1 =e= sum(j, w(t));


restr3b..            1 =e= sum(m, pp(m));















restr2(i,k)..        beta(i,k) =e= sum(m, p(i,k,m) * z(k, m));





parameter z1(m)    support points
/
1 -1
2  0
3  1
/;
parameter z2(m)    support points 
/
1 -0.2
2  0
3  0.2
/;
parameter zz(m)    support points 
/
1 -20
2  0
3  20
/;



This is for the non-theta params:
round( max(abs(coef(linear.sur.est))) * 5 )

Then the thetas:
1.e-6 to 50

error terms:
cost share:
-10 to 10

cost fn:
round( max(ln.E.data) * 5 )



+












s(t,"1") = datafile(t,"1");
s(t,"2") = datafile(t,"2");
s(t,"3") = datafile(t,"3");
s(t,"4") = datafile(t,"4");
s(t,"5") = datafile(t,"5");






#parameter
#  datafile(t,d)   raw data from SAS
#  s(t,i)          dependent variable share of budege
#  x(t,i,k)        explanatory variables
#  price1(t)       price level in log form
#  price2(t)       price level in log form
#  price3(t)       price level in log form
#  price4(t)       price level in log form
#  price5(t)       price level in log form
#  apple(t)        weighted average price level
#  indic(k)        copy of set k
#  MM              number of points in support
#  JJ              number of points in support
#;



# So main variables we need to combine are: top.before.data; combined.df.GAMS; 














library("numDeriv")


GAMS.nonlinear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytest.lst")

GAMS.nonlinear.results <- GAMS.nonlinear.results[
  -(1:grep("S O L V E      S U M M A R Y", GAMS.nonlinear.results)) ]
  
GAMS.nonlinear.results <- gsub("(^1)|(   2 )|(   3 )", "", GAMS.nonlinear.results)

GAMS.nonlinear.results.extracted <- str_extract(GAMS.nonlinear.results, " p.*[.]L")

prob.names <- GAMS.nonlinear.results.extracted[!is.na(GAMS.nonlinear.results.extracted)]

prob.numbers <- GAMS.nonlinear.results[which(!is.na(GAMS.nonlinear.results.extracted)) + 2]

start.vals.lines <- paste0("theta", lead.zero(1:(N-1)), ".l = 1;")
# added this to have correct (non-zero) starting vals for theta

for ( i in 1:length(prob.names)) {

  start.vals.lines <- c( start.vals.lines, 
    paste0( prob.names[1], "(\"", 1:3, "\") = ",  strsplit(prob.numbers[i], ",")[[1]], ";")
  )

}









GAMS.nonlinear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytest.lst")


GAMS.nonlinear.results.params<- GAMS.nonlinear.results[grep("parameters to be esti$", GAMS.nonlinear.results)]

GAMS.nonlinear.results.params.names <- gsub("[.]L", "", str_extract(GAMS.nonlinear.results.params, "[^ ]*[.]L") )


GAMS.nonlinear.results.params.numbers <- as.numeric(gsub("  parameters to be esti", "",
  str_extract(GAMS.nonlinear.results.params, "[^ ]*  parameters to be esti") ) )





GAMS.nonlinear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytest.lst")

#GAMS.nonlinear.results <- GAMS.nonlinear.results[
#  -(1:grep("S O L V E      S U M M A R Y", GAMS.nonlinear.results)) ]

begin.err.weight <- grep("L  probability corresponding error term", GAMS.nonlinear.results)
end.err.weight  <- grep(paste0("^", nrow(combined.df), " "), GAMS.nonlinear.results)

error.weight.eq.ls<-list()

for ( i in 1:length(all.eqns) ) {
  
  err.weight.temp.df <- read.table("/Users/travismcarthur/Desktop/Dropbox/entropytest.lst", 
    skip = begin.err.weight[i] + 3,  nrows= nrow(combined.df))

  error.weight.eq.ls[[i]] <- err.weight.temp.df[, -1 ]

}

names(error.weight.eq.ls) <- all.eqns




error.collapsed.eq.ls <- list()

for ( i in all.eqns ) {

  if (i=="cost") {
     error.collapsed.eq.ls[[i]] <- as.matrix(error.weight.eq.ls[[i]]) %*% 
     c(-12, 0, 12)
      # c(-round( max(ln.E.data) * 5 ), 0, round( max(ln.E.data) * 5 ))
  } else {
     error.collapsed.eq.ls[[i]] <- as.matrix(error.weight.eq.ls[[i]]) %*% 
     c(-1.2, 0, 1.2)
      # c(-10, 0, 10) 
  }
  
}


big.sigma <- cov(do.call( cbind, error.collapsed.eq.ls))




combined.df <- data.frame(mget(c("y01", paste0("x", lead.zero(1:N)), 
  paste0("w", lead.zero(1:N)),  paste0("q", lead.zero(1:J)) )))
  
region.matrix.df <-   as.data.frame(region.matrix)


colnames(region.matrix.df) <- iconv(colnames(region.matrix.df), to="ASCII//TRANSLIT")
colnames(region.matrix.df) <- gsub("'", "", colnames(region.matrix.df) )
colnames(region.matrix.df) <- gsub("[.]", "", colnames(region.matrix.df) )
  
combined.df <- cbind(combined.df, region.matrix.df)



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




combined.w.params.df <- as.list(as.data.frame(combined.df))

for ( i in 1:length(GAMS.nonlinear.results.params.names)) {
  combined.w.params.df[[ GAMS.nonlinear.results.params.names[i] ]] <- GAMS.nonlinear.results.params.numbers[i]
}

combined.w.params.df[[ paste0("theta", lead.zero(N)) ]] <- 1








#combined.w.params.for.deriv.df <- as.list(as.data.frame(combined.df))

#for ( i in 1:length(GAMS.nonlinear.results.params.names)) {
#  combined.w.params.for.deriv.df[[ GAMS.nonlinear.results.params.names[i] ]] <- GAMS.nonlinear.results.params.numbers[i] + 1e-8
#}

#combined.w.params.for.deriv.df[[ paste0("theta", lead.zero(N)) ]] <- 1 + 1e-8


GAMS.nonlinear.results.params.full <- GAMS.nonlinear.results.params.numbers
names(GAMS.nonlinear.results.params.full) <- GAMS.nonlinear.results.params.names
GAMS.nonlinear.results.params.full[ paste0("theta", lead.zero(N)) ] <- 1



modified.ln.E.string <- ln.E.string # str_extract( ln.E.string, "log[(] [(]w01 / [(]w01 [*] theta01[)][)].*")

region.tackon.clean <- iconv(region.tackon, to="ASCII//TRANSLIT")
region.tackon.clean <- gsub("'", "", region.tackon.clean )
region.tackon.clean <- gsub("[.]", "", region.tackon.clean )


modified.ln.E.string <- gsub(pattern="[.]", replacement="", x=modified.ln.E.string )

modified.ln.E.string<- paste0(modified.ln.E.string, " + ", region.tackon.clean)

# modified.ln.E.string <- sub("log[(]", "", modified.ln.E.string)
# modified.ln.E.string <- sub("[)]$", "", modified.ln.E.string) 

#modified.ln.E.string.evaled.deriv <- (with(combined.w.params.df, eval(parse(text=modified.ln.E.string ))) -
#  with(combined.w.params.for.deriv.df, eval(parse(text=modified.ln.E.string ))) ) / 1e-8
  
  
  
## This is to test if we can get rid of noninvertibility by having more reasonable vals
# GAMS.nonlinear.results.params.full[grepl("theta", names(GAMS.nonlinear.results.params.full))] <- 1
  
temp.deriv.fn <- function(x, data) { 
    x <- c(as.list(x), as.list(data))
    with(x, eval(parse(text=modified.ln.E.string )) )
  }
  
temp.deriv.fn(  GAMS.nonlinear.results.params.full, as.data.frame(combined.df))

modified.ln.E.string.evaled.deriv <- jacobian(temp.deriv.fn, 
    x=GAMS.nonlinear.results.params.full, method="complex", 
    data=as.data.frame(combined.df) )

modified.S.n.string.evaled.deriv <- list()

# install.packages("numDeriv")
library("numDeriv")

for ( i in 1:(length(all.eqns)-1) ) {

  modified.S.n.string <- str_extract( S.n[[i]], "~.*")

  modified.S.n.string <- gsub(pattern="[.]", replacement="", x=modified.S.n.string )

  modified.S.n.string <- sub("~", "", modified.S.n.string)
  
  temp.deriv.fn <- function(x, data) { 
    x <- c(as.list(x), as.list(data))
    with(x, eval(parse(text=modified.ln.E.string )) )
  }

  modified.S.n.string.evaled.deriv[[i]] <- jacobian(temp.deriv.fn, 
    x=GAMS.nonlinear.results.params.full, method="complex", 
    data=as.data.frame(combined.df) )
  
  #(with(combined.w.params.df, eval(parse(text=modified.S.n.string ))) -
  #with(combined.w.params.for.deriv.df, eval(parse(text=modified.S.n.string ))) ) / 1e-8


}


temp.deriv.fn(  GAMS.nonlinear.results.params.full, as.data.frame(combined.df))


stacked.jacobian <- rbind(  modified.ln.E.string.evaled.deriv, 
  do.call(rbind, modified.S.n.string.evaled.deriv) )


# install.packages("matrixcalc")
library("matrixcalc")

is.positive.definite(big.sigma)
is.positive.definite(test.matrix)


test.matrix <-  solve(
  t( stacked.jacobian ) %*% 
    kronecker( solve(big.sigma), diag(nrow(combined.df)) ) %*% 
    stacked.jacobian
  )


diag(test.matrix)




GAMS.nonlinear.results.params.full /  diag(test.matrix)



# (1 / nrow(combined.df))  *

test.matrix <-   t( stacked.jacobian ) %*% 
    kronecker( solve(big.sigma), diag(nrow(combined.df)) ) %*% 
    stacked.jacobian
    
   testcols <- function(ee) {
       ## split eigenvector matrix into a list, by columns
       evecs <- split(zapsmall(ee$vectors),col(ee$vectors))
       ## for non-zero eigenvalues, list non-zero evec components
       mapply(function(val,vec) {
           if (val!=0) NULL else which(vec!=0)
       },zapsmall(ee$values),evecs)
   }
# Thanks to http://stackoverflow.com/questions/12304963/using-eigenvalues-to-test-for-singularity-identifying-collinear-columns

testcols(eigen(test.matrix))

svd(test.matrix)


test.matrix.scaled <- scale(test.matrix)


p<-ncol(test.matrix.scaled)

A<-svd(test.matrix.scaled,nu=0)
#x is ill-conditioned: this ratio is larger 
#than 10. (step 1)
2*log(A$d[1]/A$d[p])

#check what is causing it: (step 2)
round(A$v[,ncol(A$v)],2)
#you can write the last variable as (.23*x_1+.5*x_2-.45*x_3)/(-.7) [1]

min(A$d)


n<-100
p<-20
#non-ill conditioned part of the dataset.
x<-matrix(rnorm(n*p),nc=p)
x<-scale(x)
#introduce a variable that causes x to be 
#ill conditioned.
y<-x%*%c(rnorm(1),0, 0, 0, rnorm(1), 0, rnorm(1), rnorm(1), rep(0,p))[1:p]
#y<-scale(y)
#x<-cbind(x,y)
p<-ncol(x)

A<-svd(x,nu=0)
#x is ill-conditioned: this ratio is larger 
#than 10. (step 1)
2*log(A$d[1]/A$d[p])

#check what is causing it: (step 2)
round(A$v[,ncol(A$v)],2)
#you can write the last variable as (.23*x_1+.5*x_2-.45*x_3)/(-.7) [1]

min(A$d)



















#solve( scale(test.matrix, center=FALSE) )

solve( test.matrix / 10e10 )

det(test.matrix / 10e10 )


test.matrix <- solve(
  t( stacked.jacobian ) %*% 
    kronecker( solve(big.sigma), diag(nrow(combined.df)) ) %*% 
    stacked.jacobian
  )

diag(test.matrix)


GAMS.nonlinear.results.params.full /  diag(test.matrix)


  


with(combined.w.params.df, eval(parse(text=modified.ln.E.string))) + 
  error.collapsed.eq.ls[["cost"]] - 
  combined.w.params.df$cost


ln.E.data - 




parameter vshare(j)    support points 
/
1 -10
2  0
3  10
/;
parameter vcost(j)    support points 
/
1 -97
2  0
3  97
/;




modified.ln.E.string.evaled.deriv.complex <- jacobian(temp.deriv.fn, 
    x=GAMS.nonlinear.results.params.full, method="complex", 
    data=as.data.frame(combined.df) )


modified.ln.E.string.evaled.deriv.richardson <- jacobian(temp.deriv.fn, 
    x=GAMS.nonlinear.results.params.full, method="Richardson", 
    data=as.data.frame(combined.df) )



summary(modified.ln.E.string.evaled.deriv.complex - modified.ln.E.string.evaled.deriv.richardson)
max(modified.ln.E.string.evaled.deriv.complex - modified.ln.E.string.evaled.deriv.richardson)
min(modified.ln.E.string.evaled.deriv.complex - modified.ln.E.string.evaled.deriv.richardson)








