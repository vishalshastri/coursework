

fsdfsa


# fty


M <- 1
N <- 4
J <- 3

inef.cost.1 <- paste0( "beta", lead.zero(1:N), " * log(theta", lead.zero(1:N), ")", collapse=" + ")


#beta.special<-c()
# beta.mat<-matrix(1:(N*N), nrow=N, ncol=N)
#for ( i in 1:N) {

inef.cost.2 <- paste0(
    paste0("beta", do.call(paste0, N.2.dim[2:1]) , " * ", 
    "log(w", gsub("[.]", "", N.2.dim[[2]]),
       ") * log(theta", gsub("[.]", "", N.2.dim[[1]]), ")"
    ),
    collapse=" + " )
#}

# gsub(" [*] ", ") * log(",  gsub("[*] $", "", ln.sh.w.grid[[1]])) 


# We need to re-impose adding-up constraints and symmetry constraints

inef.cost.3 <- paste0(
    paste0("beta", do.call(paste0, N.2.dim[2:1]) , " * ", 
    "log(theta", gsub("[.]", "", N.2.dim[[2]]),
       ") * log(theta", gsub("[.]", "", N.2.dim[[1]]), ")"
    ),
    collapse=" + " )
    

inef.cost.4 <- paste0( "gamma.01.", lead.zero(1:N), 
  " * log(y01)", 
  " * log(theta", lead.zero(1:N), ")", collapse=" + ")


inef.cost.full <- paste0(
  inef.cost.1, " + ",
  inef.cost.2, " + ", 
  "(1/2) * (", inef.cost.3, ") + ",
  inef.cost.4)
  
  






