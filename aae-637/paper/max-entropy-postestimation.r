



#theta.param.support <- c(
#-4, 
#1, 
#6
#)


#other.param.support <- c(
#-round( max(abs(coef(linear.sur.est))) * 3 ), 
#0, 
#round( max(abs(coef(linear.sur.est))) * 3 )
#)


#cost.err.support <- c(
#-12,
#0,
#12)
# -round( max(combined.df$cost) * 5 )

#share.err.support <- c(
#-1.2,
#0,
#1.2
#)




#  bootstrap.iter <- 0


library("numDeriv")


# GAMS.nonlinear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytest.lst")

GAMS.nonlinear.results<- readLines(paste0(GAMS.projdir, "GMEnonlinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".lst"))




GAMS.nonlinear.results <- GAMS.nonlinear.results[
  -(1:grep("S O L V E      S U M M A R Y", GAMS.nonlinear.results)) ]
  
param.gather.regex<- paste0("(^1)|", paste0("(   ", 2:length(other.param.support), " )", collapse="|"))

GAMS.nonlinear.results <- gsub(param.gather.regex, "", GAMS.nonlinear.results)
  
#GAMS.nonlinear.results <- gsub("(^1)|(   2 )|(   3 )", "", GAMS.nonlinear.results)

GAMS.nonlinear.results.extracted <- str_extract(GAMS.nonlinear.results, " p.*[.]L")

prob.names <- GAMS.nonlinear.results.extracted[!is.na(GAMS.nonlinear.results.extracted)]

prob.numbers <- GAMS.nonlinear.results[which(!is.na(GAMS.nonlinear.results.extracted)) + 2]

#start.vals.lines <- paste0("theta", lead.zero(1:(N-1)), ".l = 1;")
# added this to have correct (non-zero) starting vals for theta

#for ( i in 1:length(prob.names)) {

#  start.vals.lines <- c( start.vals.lines, 
#    paste0( prob.names[1], "(\"", 1:3, "\") = ",  strsplit(prob.numbers[i], ",")[[1]], ";")
#  )

#}









# GAMS.nonlinear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytest.lst")

GAMS.nonlinear.results<- readLines(paste0(GAMS.projdir, "GMEnonlinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".lst"))


GAMS.nonlinear.results.params<- GAMS.nonlinear.results[grep("parameters to be estimated$", GAMS.nonlinear.results)]

GAMS.nonlinear.results.params <- GAMS.nonlinear.results.params[grep("VARIABLE", GAMS.nonlinear.results.params)]

GAMS.nonlinear.results.params.names <- gsub("[.]L", "", str_extract(GAMS.nonlinear.results.params, "[^ ]*[.]L") )


GAMS.nonlinear.results.params.numbers <- as.numeric(gsub("  parameters to be estimated", "",
  str_extract(GAMS.nonlinear.results.params, "[^ ]*  parameters to be estimated") ) )





# GAMS.nonlinear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytest.lst")

GAMS.nonlinear.results<- readLines(paste0(GAMS.projdir, "GMEnonlinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".lst"))

#home.mac <- TRUE

#if (home.mac) {

#  annoying.lines<- grep(
#    "GAMS 24.1.3  r41464 Released Jul 26, 2013 XXX-DEG Mac x86_64/Darwin", 
#    GAMS.nonlinear.results)
  
#  annoying.lines <- eval(parse(text=paste0("c(", paste0(annoying.lines, ":", annoying.lines+8, collapse=", "), ")")))
  
#  cat(GAMS.nonlinear.results[-annoying.lines], 
#    file="/Users/travismcarthur/Desktop/Dropbox/entropytesttemp.lst",
#    sep="\n")
  
  
  
#}

#GAMS.nonlinear.results <- GAMS.nonlinear.results[
#  -(1:grep("S O L V E      S U M M A R Y", GAMS.nonlinear.results)) ]

begin.err.weight <- grep("L  probability corresponding error term", GAMS.nonlinear.results)
end.err.weight  <- grep(paste0("^", nrow(combined.df), " "), GAMS.nonlinear.results)

error.weight.eq.ls<-list()

for ( i in 1:length(all.eqns) ) {
  
  err.weight.temp.df <- read.table(paste0(GAMS.projdir, "GMEnonlinear", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".lst"), 
    skip = begin.err.weight[i] + 3,  nrows= nrow(combined.df))
    # "/Users/travismcarthur/Desktop/Dropbox/entropytest.lst"

  error.weight.eq.ls[[i]] <- err.weight.temp.df[, -1 ]

}

names(error.weight.eq.ls) <- all.eqns




error.collapsed.eq.ls <- list()

for ( i in all.eqns ) {

  if (i=="cost") {
     error.collapsed.eq.ls[[i]] <- as.matrix(error.weight.eq.ls[[i]]) %*% 
     cost.err.support
      # c(-round( max(ln.E.data) * 5 ), 0, round( max(ln.E.data) * 5 ))
  } else {
     error.collapsed.eq.ls[[i]] <- as.matrix(error.weight.eq.ls[[i]]) %*% 
     share.err.support
      # c(-10, 0, 10) 
  }
  
}


big.sigma <- cov(do.call( cbind, error.collapsed.eq.ls))

error.collapsed.eq.df <- do.call( cbind, error.collapsed.eq.ls)

summary(error.collapsed.eq.df)
hist(error.collapsed.eq.df)



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
#is.positive.definite(test.matrix)


#param.covar.mat <-  solve(
#  t( stacked.jacobian ) %*% 
#    kronecker( solve(big.sigma), diag(nrow(combined.df)) ) %*% 
#    stacked.jacobian
#  )


stacked.jacobian.last.theta.dropped <- stacked.jacobian[, -ncol(stacked.jacobian)]


param.covar.mat <-  solve(
  t( stacked.jacobian.last.theta.dropped ) %*% 
    kronecker( solve(big.sigma), diag(nrow(combined.df)) ) %*% 
    stacked.jacobian.last.theta.dropped
  )

diag(param.covar.mat)




GAMS.nonlinear.results.params.full[-length(GAMS.nonlinear.results.params.full)] /  diag(param.covar.mat)

t(t(GAMS.nonlinear.results.params.full ))

cat(diag(param.covar.mat), sep="\n")

















GAMS.nonlinear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytest.lst")

# GAMS.linear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytestlinear.lst")

GAMS.nonlinear.results <- GAMS.nonlinear.results[
  -(1:grep("S O L V E      S U M M A R Y", GAMS.nonlinear.results)) ]
  
  
param.gather.regex<- paste0("(^1)|", paste0("(   ", 2:length(other.param.support), " )", collapse="|"))

GAMS.nonlinear.results <- gsub(param.gather.regex, "", GAMS.nonlinear.results)

#GAMS.nonlinear.results <- gsub("(^1)|(   2 )|(   3 )", "", GAMS.nonlinear.results)

GAMS.nonlinear.results.extracted <- str_extract(GAMS.nonlinear.results, " p.*[.]L")

prob.names <- GAMS.nonlinear.results.extracted[!is.na(GAMS.nonlinear.results.extracted)]

prob.numbers <- GAMS.nonlinear.results[which(!is.na(GAMS.nonlinear.results.extracted)) + 2]


# TODO: Do we need to add Theta06 to this goodness-of-fit measure?

resultant.param.probs.v <- eval(parse(text=paste0("c(", paste0(prob.numbers, collapse=", "), ")")))


(-resultant.param.probs.v %*% log(resultant.param.probs.v)) / 
  (length(prob.numbers) * log(3))
# Goodness-of-fit measure from GJM (1996) p. 93, eqn 6.4.26




# rvhess = maxdouble

# max(abs(do.call( cbind, error.collapsed.eq.ls)))





