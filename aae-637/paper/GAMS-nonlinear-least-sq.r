





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

  if (scale.vars.on.orig.data) { 
    input.scaling  <- input.scaling.orig[i] 
  } else {
    input.scaling  <- log10_ceiling(
      sqrt(sum((c(combined.df[, paste0("x", lead.zero(i))], 
      combined.df[, paste0("w", lead.zero(i))])^2)/(nrow(combined.df)-1)))
    )
  # Got this idea from scale() function
  }
  
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
# This was bad bad



top.before.data <- c(
"sets ",
paste0("  t number of observations  / 1 * ", nrow(combined.df), " /"),
paste0("  d number of variables in datafile  / 1 * ", ncol(combined.df), " /"),
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
  ";",
  "",
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
""
)








vsupport.lines <- c("") 



# all.params <- gsub("[.]", "", names(ln.E.start.vals)[!grepl("region", names(ln.E.start.vals))] )



region.tackon.clean <- iconv(region.tackon, to="ASCII//TRANSLIT")
region.tackon.clean <- gsub("'", "", region.tackon.clean )
region.tackon.clean <- gsub("[.]", "", region.tackon.clean )


all.params <- unique(str_extract_all(paste0(ln.E.string, " + ", region.tackon.clean), 
"(theta[0-9][0-9])|(beta0)|(alpha[0-9][0-9])|(alpha[.][0-9][0-9][.][0-9][0-9])|(beta[0-9][0-9])|(beta[.][0-9][0-9][.][0-9][0-9])|(gamma[.][0-9][0-9][.][0-9][0-9])|(zeta[.][0-9][0-9][.][0-9][0-9])|(zeta[0-9][0-9])|(kappa[.][0-9][0-9][.][0-9][0-9])|(delta[.][0-9][0-9][.][0-9][0-9])|(region[0-9][0-9])"
  )[[1]]
)

all.params <- all.params[!grepl(paste0("theta", lead.zero(N)), all.params)]

all.params <- gsub("[.]", "", all.params)


non.theta.param.support.lines <- c("") 

theta.param.support.lines <- c("") 




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
  "longlogsection(t)",
#  "sharedenom(t)",
  "residual(t)",
  "  sse           nonlin least sq objective value",
  "",
  "positive variable",
  paste0("  ", all.params[grepl("theta", all.params)], "   parameters to be estimated"),
  "longlogsection(t)",
  ";"
)







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




main.objective <- c("obj.. sse =e= sum(t, sqr(residual(t)));",
"fit(t).. cost(t)  =e=",
ln.E.string.GAMS,
"+ residual(t);")


main.objective <- strwrap( main.objective, indent=12, exdent=19, width=80)








# theta01  =g= 0;
#theta02  =g= 0;
#theta03  =g= 0;
#theta04  =g= 0;
#theta05  =g= 0;

#restrpbeta0..        beta0 =e= sum(m, pbeta0(m) * zbeta0(m));

# I think I can ignore these posi constraints below since I declare them as posi vars above
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



prob.weight.param.lines <- c("")


all.eqns <- c("cost", paste0("s", 1:length(S.n)))

prob.weight.error.lines <- c("")







equation.declarations <- c(
  "equations",
  "  obj             nonlin least sq objective function",
  "fit(t)",
  "restrbiglogposi(t)",
#  "restrsharedenom(t)",
  ";"
)
  





# install.packages("stringr")
library("stringr")



#GAMS.linear.results.params <- GAMS.linear.results.params[grep("VARIABLE", GAMS.linear.results.params)]



S.n.H.region <- S.n.H

S.n.H.region[[length(S.n.H.region)]] <- 
  as.formula( paste0("ln.E.data ~ ", as.character(S.n.H[[length(S.n.H)]])[[3]], " + region" ) )
  
#lm.param.restrictions.cost.fn <- gsub("cost[.]fn_I[(]0[.]5", "2 * cost.fn_I(0.5", lm.param.restrictions[!grepl("S.n.H", lm.param.restrictions)])

lm.param.restrictions.cost.fn <-lm.param.restrictions[!grepl("S.n.H", lm.param.restrictions)]
  
linear.sur.est.region <- systemfit( S.n.H.region[length(S.n.H.region)], "SUR", restrict.matrix = lm.param.restrictions.cost.fn  ,  maxit = 5000, data=data.frame(combined.df, ln.E.data=combined.df$cost ))
# Note: data=combined.df is new
# Note here that I am only estimating one equation, the cost function

# Below should be numerically zero
sum(coef(linear.sur.est.region)[grepl("log[(]w[0-9][0-9][)][:]log[(]q01[)]", 
  names(coef(linear.sur.est.region)))])
  


region.rearrange <- coef(linear.sur.est.region)[grepl("region", names(coef(linear.sur.est.region)))]
linear.sur.est.region$coefficients <- c(coef(linear.sur.est.region)[
  !grepl("region", names(coef(linear.sur.est.region)))], region.rearrange)


ln.E.vars <- lin.to.nonlin.crossref.df$nonlinear


ln.E.start.vals <- vector(mode="numeric", length=length(coef(linear.sur.est.region)))
ln.E.start.vals <- coef(linear.sur.est.region)[grepl("cost.fn_", names(coef(linear.sur.est.region)))]
names(ln.E.start.vals) <- ln.E.vars
names(ln.E.start.vals)[is.na(names(ln.E.start.vals)) ] <- 
  paste0("region",  lead.zero(1:ncol(region.matrix)))

# This is to handle the fact that some of these drop out with adding-up restrictions:
ln.E.start.vals <- ln.E.start.vals[!grepl("(beta01)|(beta....01)|(gamma....01)|(kappa....01)", 
  names(ln.E.start.vals))]

#for ( i in 1:length(ln.E.start.vals)) {
#  if (!grepl("(beta[.])|(zeta[.])", names(ln.E.start.vals)[i])) { next}
#  split.param.name<- strsplit(names(ln.E.start.vals)[i], "[.]")[[1]]
#  if(split.param.name[2] != split.param.name[3]) {
#    ln.E.start.vals[i] <- ln.E.start.vals[i] / 2
#    cat(names(ln.E.start.vals[i]), "\n")
#  }
#}
# Apparently I didnt need this


set.seed(100)

#theta.starts <- rep(1, times=N-1) + rnorm(N-1, sd=.01)
theta.starts <- rep(1, times=N-1)
names(theta.starts) <- paste0("theta", lead.zero(1:(N-1)))
theta06 <- 1

ln.E.start.vals <-c(ln.E.start.vals, theta.starts)

ln.E.start.vals <- ln.E.start.vals[!grepl("beta[.]01[.][0-9][0-9]", names(ln.E.start.vals))]

# TODO: Double check - we are taking these out since they do not appear in the nonlinear
# specification due to homogeneity restrictions. But is this ok? Need to make sure that
# the proper restrictions were put into the OLS model


# ln.E.start.vals <- fm1DNase1$coefficients


ln.E.start.vals <- ln.E.start.vals + rnorm(length(ln.E.start.vals), sd=.01)

GAMS.linear.results.params.names <- gsub("[.]", "", names(ln.E.start.vals))


GAMS.linear.results.params.numbers <- unname(ln.E.start.vals)
  
  
combined.w.params.df <- as.list(as.data.frame(combined.df))

for ( i in 1:length(GAMS.linear.results.params.names)) {
  combined.w.params.df[[ GAMS.linear.results.params.names[i] ]] <- GAMS.linear.results.params.numbers[i]
}

#for ( i in 1:N) {
#  combined.w.params.df[[ paste0("theta", lead.zero(i)) ]] <- 1
#}



modified.ln.E.string <- str_extract( ln.E.string, "log[(] [(]w01 / [(]w01 [*] theta01[)][)].*")

modified.ln.E.string <- gsub(" [+] region.*$", "", modified.ln.E.string)

modified.ln.E.string <- gsub(pattern="[.]", replacement="", x=modified.ln.E.string )

modified.ln.E.string <- sub("log[(]", "", modified.ln.E.string)
modified.ln.E.string <- sub("[)]$", "", modified.ln.E.string) 

modified.ln.E.string.evaled <- with(combined.w.params.df, eval(parse(text=modified.ln.E.string )))


longlogsection.initial <- paste0("longlogsection.l(\"", 1:nrow(combined.df), "\") = ", modified.ln.E.string.evaled, ";")

summary(modified.ln.E.string.evaled)



ln.E.string.w.region <- paste0(ln.E.string, " + ", region.tackon.clean)

#rm(list=names(combined.df))

ln.E.string.evaled <- with(combined.w.params.df, eval(parse(text=gsub(pattern="[.]", replacement="", x=ln.E.string.w.region ) )))


#ln.E.string.linear.w.region <- paste0(ln.E.string.linear, " + ", region.tackon.clean)


#ln.E.string.evaled <- with(combined.w.params.df, eval(parse(text=gsub(pattern="[.]", replacement="", x=ln.E.string.linear.w.region) )))







sum( (combined.w.params.df$cost - ln.E.string.evaled )^2) / sum(resid(linear.sur.est.region)[[1]]^2)

sum( (combined.w.params.df$cost - mean(combined.w.params.df$cost) )^2)


#data.frame(SUR=predict(linear.sur.est.region), nonlin=ln.E.string.evaled)

#data.frame(SUR=coef(linear.sur.est.region), nonlin=c(ln.E.start.vals[!grepl("theta", names(ln.E.start.vals))], rep(NA, 73-62)), 
#nonlin.names=c(names(ln.E.start.vals)[!grepl("theta", names(ln.E.start.vals))], rep(NA, 73-62)))

#t(t(coef(linear.sur.est.region)))


sum( predict(linear.sur.est.region) - resid(linear.sur.est.region) - combined.w.params.df$cost)
# ok, so A-ok, i.e. it's predicting on the right data

# ln.E.string.GAMS


#linear.sur.est.region <- systemfit( S.n.H.region[length(S.n.H.region)], "SUR", restrict.matrix = lm.param.restrictions.cost.fn  ,  maxit = 5000, data=data.frame(combined.df, ln.E.data=combined.df$cost ))
# Note: data=combined.df is new

#linear.sur.est.region$eq[[1]]



#predict(linear.sur.est.region, formula= ln.E.data ~ y01 + log(w01) + log(w02) + log(w03) + log(w04) + log(w05) + log(w06) )



longlogsection.initial <- paste0("longlogsection.l(\"", 1:nrow(combined.df), "\") = ", modified.ln.E.string.evaled, ";")

#residual.initial <- paste0("residual.l(\"", 1:nrow(combined.df), "\") = ", resid(linear.sur.est.region)$cost.fn, ";")


resid.evaluated <- with(data.frame(combined.w.params.df, ln.E.data=combined.w.params.df$cost), 
eval(parse(text=paste0(gsub("[.]", "", ln.E.string), " + ", region.tackon.clean, " - ln.E.data"))))



residual.initial <- paste0("residual.l(\"", 1:nrow(combined.df), "\") = ", resid.evaluated , ";")


#residual(t)

#longlogsection.l("1")






param.starting.vals <- paste0(GAMS.linear.results.params.names, ".l = ", GAMS.linear.results.params.numbers, ";")



final.lines <- 
c(
"*Initial conditions",
# paste0("  p", all.params, ".l(m) = 1/MM;"),
# paste0("  w", all.eqns, ".l(t,j) = 1/JJ;"),
param.starting.vals,
#start.vals.lines,
longlogsection.initial,
residual.initial,
#share.denom.initial,
"* primal approach",
#"model nls /all/;",
#"model nls /obj,fit/;", # Ok, it seems we _really_ need the obj,fit version
"model nls /obj,fit,restrbiglogposi/;", # try it different...
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
"NLS.OPTFILE=1; ",
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
#"solve gme using nlp maximizing g; ",
"solve nls minimizing sse using nlp;",
"options decimals = 7;"
)




parameter.display.lines <- c( paste0("display ", all.params, ".l;")
  )




completed.GAMS.file <-  c(
  top.before.data, " ", 
  combined.df.GAMS, " ", 
  data.alloc.lines, " ", 
#  param.support.simple.lines, " ", 
#  vsupport.lines, " ", 
#  non.theta.param.support.lines, " ", 
#  theta.param.support.lines, " ", 
  variable.declaration.lines, " ", 
  equation.declarations, " ",
#  objective.fn.lines, " ", 
  main.objective, "",
#  unlist(model.restrictions), " ", 
#  model.restrictions.cost, " ", 
#  theta.positivity.restriction, " ",
  big.log.posi.constraint.lines, " ",
#  share.denom.constraint.lines, " ",
#  prob.weight.param.lines, " ", 
#  prob.weight.error.lines, " ", 
#  covar.SUR.lines,
  final.lines, " ",
  parameter.display.lines
)



  cat(completed.GAMS.file, 
    file=paste0(GAMS.projdir, "NLS", boot.or.jacknife,  strsplit(target.crop, " ")[[1]][1], 
     formatC(bootstrap.iter, width = 5, flag = "0"), ".gms"), 
    sep="\n")
  

