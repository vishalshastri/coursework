




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

