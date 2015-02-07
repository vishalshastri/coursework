
# Synthetic data construction

#M <- 1
#N <- 6
#J <- 3

# intended.obs <- length(y01)

#intended.obs <- 4000
#intended.obs <- 2000
#intended.obs <- 8000
intended.obs <- 1000

#set.seed(100)
set.seed(intended.seed)

all.params <- unique(unlist(str_extract_all(unlist(demand.eqns), 
"(s.[0-9][0-9].[0-9][0-9])|(b.y.[0-9][0-9])|(b.[0-9][0-9])|(b.y.y)|(d.[0-9][0-9].[0-9][0-9])|(c.[0-9][0-9] )|(c.[0-9][0-9].[0-9][0-9])"
  ))
)

all.params  <- gsub("([.])|( )", "", all.params )


for ( i in 1:N) {
  assign(paste0("w", lead.zero(i)), rlnorm(intended.obs)*10)
}

for ( i in 1:J) {
  assign(paste0("q", lead.zero(i)), rlnorm(intended.obs)*10)
}
q02 <- sample(0:1, intended.obs, replace=TRUE)

 y01 <- rlnorm(intended.obs)*10
#y01 <- rlnorm(intended.obs, meanlog= -5)


synthetic.params <- rnorm(length(c(all.params, paste0("xi", lead.zero(1:N))))) 
synthetic.params <- as.numeric(synthetic.params)
names(synthetic.params) <- c(all.params, paste0("xi", lead.zero(1:N)))


synthetic.S.mat <- matrix(0, N-1, N-1)
synthetic.T.mat <- matrix(0, N-1, N-1)

# synthetic.S.mat[lower.tri(synthetic.S.mat, diag=TRUE)] <-  names(  synthetic.params[1:(N*(N-1)/2)] )
# A-ok

synthetic.T.mat[lower.tri(synthetic.T.mat, diag=TRUE)] <- rnorm((N*(N-1)/2))

synthetic.S.mat <-  - synthetic.T.mat %*% t(synthetic.T.mat)

synthetic.params[1:(N*(N-1)/2)] <- synthetic.S.mat[lower.tri(synthetic.S.mat, diag=TRUE)] 


synthetic.C.mat <- matrix(0, J, J)
synthetic.A.mat <- matrix(0, J, J)

synthetic.params <- synthetic.params[names(synthetic.params)!="c0202"] 

synthetic.params <- c(synthetic.params[1:grep(paste0("c01", lead.zero(J)), names(synthetic.params))], 0, 
  synthetic.params[(grep(paste0("c01", lead.zero(J)), names(synthetic.params))+1):length(synthetic.params)])

names(synthetic.params)[grep(paste0("c01", lead.zero(J)), names(synthetic.params))+1] <- "c0202"
# Because "c0202" appear out of order, must fix it with these two lines above


# synthetic.C.mat[lower.tri(synthetic.C.mat, diag=TRUE)] <-  names(  synthetic.params[grepl("c[0-9]{4}", names(synthetic.params))] )
# A-ok

synthetic.A.mat[lower.tri(synthetic.A.mat, diag=TRUE)] <- rnorm((J*(J+1)/2))

synthetic.C.mat <-   synthetic.A.mat %*% t(synthetic.A.mat)

synthetic.params[grepl("c[0-9]{4}", names(synthetic.params))] <- synthetic.C.mat[lower.tri(synthetic.C.mat, diag=TRUE)] 


synthetic.params[grepl("xi", names(synthetic.params))] <- c(rlnorm(N-1), 1)

while ( any(synthetic.params[grepl("xi", names(synthetic.params))] > 10) ) {
  which.invalid<- which(synthetic.params[grepl("xi", names(synthetic.params))] > 10)
  synthetic.params[grepl("xi", names(synthetic.params))][which.invalid] <- rlnorm(length(which.invalid))
}

# synthetic.params[grepl("xi", names(synthetic.params))] <- 1
# Test it with no true nonlinearity

#synthetic.params[grepl("b[0-9][0-9]", names(synthetic.params))] <-  runif(N, 250, 500)

#synthetic.params[grepl("d[0-9]{4}", names(synthetic.params))] <-  runif(N, -1.7, -0)
#synthetic.params[grepl("c[0-9]{2}$", names(synthetic.params))] <-  runif(sum(grepl("c[0-9]{2}$", names(synthetic.params))), -1.7, -0)
#synthetic.params[grepl("c0202$", names(synthetic.params))] <-  runif(sum(grepl("c0202$", names(synthetic.params))), -1.7, -0)

synthetic.params[grepl("(b[0-9][0-9])|((by[0-9][0-9]))", names(synthetic.params))] <- 
  rlnorm(sum(grepl("(b[0-9][0-9])|(by[0-9][0-9])", names(synthetic.params))))
  
#synthetic.params[grepl("(c[0-9][0-9]$)|(c0202)", names(synthetic.params))] <- 
#   (-1) * rlnorm(sum(grepl("(c[0-9][0-9]$)|(c0202)", names(synthetic.params))))

synthetic.params[grepl("byy", names(synthetic.params))] <- 
  rlnorm(sum(grepl("byy", names(synthetic.params))))


for ( i in 1:N) {
  assign( paste0("inputmean", lead.zero(i)), mean.of.inputs[i])
}

dep.var.ls <- vector(mode= "list", length=N)


# for ( iter in 1:10000) {

for ( i in 1:N) {
  dep.var.ls[[i]] <- 
    with(as.list(synthetic.params) ,
      eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]])))) 
  
  dep.var.ls[[i]] <- dep.var.ls[[i]] + rnorm(length(dep.var.ls[[i]]), sd=sd(dep.var.ls[[i]]) / 3)
 
  dep.var.ls[[i]] <- dep.var.ls[[i]] * y01
  
  dep.var.ls[[i]] <- ifelse(dep.var.ls[[i]] > 0, dep.var.ls[[i]], 0)
  # This censors it
  
  assign(paste0("x", lead.zero(i)), dep.var.ls[[i]])
  
}


#mean.of.inputs <- colMeans(data.frame(mget(paste0("x", lead.zero(1:N)))))

#for ( i in 1:N) {
#  assign( paste0("inputmean", lead.zero(i)), mean.of.inputs[i])
#}

#cat(inputmean01, "\n")

#}





1 - mean(x01 == 0)
1 - mean(x02 == 0)
1 - mean(x03 == 0)
1 - mean(x04 == 0)
1 - mean(x05 == 0)
1 - mean(x06 == 0)

# Do the same trimming I do for the real dataset



#price.trim.quantile <- 0.99
#demand.var.trim.quantile <- 0.95


price.trim.criteria <- apply(firm.df[, price.to.trim], 2, FUN=function(x) x < quantile(x, probs=price.trim.quantile) )
price.trim.criteria <- apply(price.trim.criteria, 1, FUN=all)
firm.df <- firm.df[price.trim.criteria, ]


synth.demand.var.trim.criteria.ls <- list()

for ( i in 1:N) {
  temp.fun <- function(x) {x < quantile(x[x>0], probs=price.trim.quantile)  }
  synth.demand.var.trim.criteria.ls[[i]] <- temp.fun(get(paste0("w", lead.zero(i))))
}

# data.frame(a=1:10, b=101:110)/(1:10) is ok, so the above operation works
synth.demand.var.trim.criteria <- apply(do.call(cbind, synth.demand.var.trim.criteria.ls), 1, FUN=all)

for ( i in 1:N) {
  assign(paste0("x", lead.zero(i)), get(paste0("x", lead.zero(i)))[synth.demand.var.trim.criteria])
  assign(paste0("w", lead.zero(i)), get(paste0("w", lead.zero(i)))[synth.demand.var.trim.criteria])
}

for ( i in 1:J) {
  assign(paste0("q", lead.zero(i)), get(paste0("q", lead.zero(i)))[synth.demand.var.trim.criteria])
}

y01 <- y01[synth.demand.var.trim.criteria]





synth.demand.var.trim.criteria.ls <- list()

for ( i in 1:N) {
  temp.fun <- function(x) {x < quantile(x[x>0], probs=demand.var.trim.quantile)  }
  synth.demand.var.trim.criteria.ls[[i]] <- temp.fun(get(paste0("x", lead.zero(i)))/y01)
}

# data.frame(a=1:10, b=101:110)/(1:10) is ok, so the above operation works
synth.demand.var.trim.criteria <- apply(do.call(cbind, synth.demand.var.trim.criteria.ls), 1, FUN=all)

for ( i in 1:N) {
  assign(paste0("x", lead.zero(i)), get(paste0("x", lead.zero(i)))[synth.demand.var.trim.criteria])
  assign(paste0("w", lead.zero(i)), get(paste0("w", lead.zero(i)))[synth.demand.var.trim.criteria])
}


for ( i in 1:J) {
  assign(paste0("q", lead.zero(i)), get(paste0("q", lead.zero(i)))[synth.demand.var.trim.criteria])
}

y01 <- y01[synth.demand.var.trim.criteria]



E.y01.data <- (w01*x01 + w02*x02 + w03*x03 + w04*x04 + w05*x05 + w06*x06) / y01



if (generate.synth.data.from.cost.fn ) {

  E.y01.data <- 
    with(as.list(synthetic.params) ,
      eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[N+1]])))) 
  
  E.y01.data <- E.y01.data + rnorm(length(E.y01.data), sd=sd(E.y01.data) )
  
  E.y01.data <- ifelse(E.y01.data > 0, E.y01.data, 0)
  

}





t(t(synthetic.params))


if ( FALSE ) {




# GAMS.nonlinear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytest.lst")

grab.linear.results <- FALSE

GAMS.nonlinear.results<- readLines(paste0(GAMS.projdir, ifelse(grab.linear.results, "sgmGMElinear", "sgmGMEnonlinear"), # "GMEnonlinear", 
strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".lst"))


GAMS.nonlinear.results.params<- GAMS.nonlinear.results[grep("parameters to be estimated$", GAMS.nonlinear.results)]

GAMS.nonlinear.results.params <- GAMS.nonlinear.results.params[grep("VARIABLE", GAMS.nonlinear.results.params)]

GAMS.nonlinear.results.params.names <- gsub("[.]L", "", str_extract(GAMS.nonlinear.results.params, "[^ ]*[.]L") )


GAMS.nonlinear.results.params.numbers <- as.numeric(gsub("  parameters to be estimated", "",
  str_extract(GAMS.nonlinear.results.params, "[^ ]*  parameters to be estimated") ) )



GAMS.nonlinear.results.params.full <- GAMS.nonlinear.results.params.numbers
names(GAMS.nonlinear.results.params.full) <- GAMS.nonlinear.results.params.names


param.check.df <- data.frame(est.params=GAMS.nonlinear.results.params.full, param.names=names(GAMS.nonlinear.results.params.full))
param.check.synth.df <- data.frame(synth.params=synthetic.params, param.names=names(synthetic.params))
param.check.df <- merge(param.check.df, param.check.synth.df)
cor.test(param.check.df$est.params , param.check.df$synth.params)
cor.test(param.check.df$est.params[grepl("xi", param.check.df$param.names)] , 
  param.check.df$synth.params[grepl("xi", param.check.df$param.names)])

# 0.27



}


# Check impact of shadow price on demand

if ( FALSE ) {
  
  for ( i in 1:7) {
  set.seed(100)
  
  synthetic.params[grepl("xi", names(synthetic.params))] <- c(rlnorm(N-1), 1)

  aloc.inef.eval  <-  with(as.list(synthetic.params) ,
      eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]])))) 

  synthetic.params[grepl("xi", names(synthetic.params))] <- 1

  no.aloc.inef.eval<-  with(as.list(synthetic.params) ,
      eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[i]])))) 
      
  print(cor.test(aloc.inef.eval, no.aloc.inef.eval))

  print(summary(log(aloc.inef.eval / no.aloc.inef.eval)))
  
  }




}

# Check homogeneity of degree 1 in prices


if ( FALSE ) {

  evalued.cost.fn.orig <- with(as.list(synthetic.params) ,
      eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[N+1]])))) 

  for ( i in 1:N) {
    assign(paste0("w", lead.zero(i)), get(paste0("w", lead.zero(i))) * 2)
  }
  
  evalued.cost.fn.doubled <- with(as.list(synthetic.params) ,
      eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[N+1]])))) 
      
  for ( i in 1:N) {
    assign(paste0("w", lead.zero(i)), get(paste0("w", lead.zero(i))) / 2)
  }
  
  summary(evalued.cost.fn.doubled / evalued.cost.fn.orig)
  # Seems A-ok


}


# Check homogeneity of degree 0 in shadow prices


if ( FALSE ) {

  synthetic.params[grepl("xi", names(synthetic.params)) ] <- 
    synthetic.params[grepl("xi", names(synthetic.params)) ] * 2

  evalued.cost.fn.orig <- with(as.list(synthetic.params) ,
      eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[N+1]])))) 


  evalued.cost.fn.doubled <- with(as.list(synthetic.params) ,
      eval(parse(text=gsub("[.]", "", demand.eqns.nonlinear[[N+1]])))) 
      
  synthetic.params[grepl("xi", names(synthetic.params)) ] <- 
    synthetic.params[grepl("xi", names(synthetic.params)) ] / 2
  
  summary(evalued.cost.fn.doubled / evalued.cost.fn.orig)
  # Seems A-ok


}

 












