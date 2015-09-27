

#GAMS.multinom.logit.results<- readLines(paste0(GAMS.projdir, "MLEmultinomiallogit", strsplit(target.crop, " ")[[1]][1], 
#   formatC(bootstrap.iter, width = 5, flag = "0"), file.flavor, ".lst"))


#GAMS.multinom.logit.results<- readLines(paste0(GAMS.projdir, "MLEmultinomiallogit", strsplit(target.crop, " ")[[1]][1], 
#   formatC(bootstrap.iter, width = 5, flag = "0"), "saved.lst"))


#GAMS.multinom.logit.results<- readLines(paste0(GAMS.projdir,"sgmGMEnonlinearHaba00000 before soil and elev.lst"))



#GAMS.multinom.logit.results.params<- GAMS.multinom.logit.results[grep("parameters to be estimated$", GAMS.multinom.logit.results)]

#GAMS.multinom.logit.results.params <- GAMS.multinom.logit.results.params[grep("VARIABLE", #GAMS.multinom.logit.results.params)]

#GAMS.multinom.logit.results.params.names <- gsub("[.]L", "", #str_extract(GAMS.multinom.logit.results.params, "[^ ]*[.]L") )


#GAMS.multinom.logit.results.params.numbers <- as.numeric(gsub("  parameters to be estimated", "",
#  str_extract(GAMS.multinom.logit.results.params, "[^ ]*  parameters to be estimated") ) )

#GAMS.multinom.logit.results.params.full <- GAMS.multinom.logit.results.params.numbers
#names(GAMS.multinom.logit.results.params.full) <- GAMS.multinom.logit.results.params.names

#overflow_protect <- GAMS.multinom.logit.results[grep("a trick to protect the exp from overflow", GAMS.multinom.logit.results)]

#overflow_protect <- as.numeric(gsub("  a trick to protect the exp from overflow", "",
#  str_extract(overflow_protect, "[^ ]*  a trick to protect the exp from overflow") ) )

# So the functions below will just access the value of overflow protect from the 
# global environment here



gdx.params.df <- read.csv(file=paste0(GAMS.projdir, "MLEmultinomiallogit", strsplit(target.crop, " ")[[1]][1], 
     formatC(bootstrap.iter, width = 5, flag = "0"), file.flavor  , "-param-output.txt"),
     head=FALSE, stringsAsFactors=FALSE)
   
names(gdx.params.df) <- c("param.names", "param.values")  
     
GAMS.multinom.logit.results.params.full <- 
  gdx.params.df$param.values[gdx.params.df$param.names!="overflow_protect"]
names(GAMS.multinom.logit.results.params.full) <- 
  gdx.params.df$param.names[gdx.params.df$param.names!="overflow_protect"]
  
overflow_protect <-
  gdx.params.df$param.values[gdx.params.df$param.names=="overflow_protect"]








cond_logit_predict <- function(b0, outcome) {

  start.params <- b0
#  names(start.params) <- rep(all.params, nalts)
  start.params.ls<- as.list(start.params)
  
  alt.cost.fn.expr.ls.no.subscripts <- lapply(alt.cost.fn.expr.ls, FUN=gsub, pattern="(t)", replacement="", fixed=TRUE)
  

#  start.params.ls<- as.list(as.data.frame(matrix(start.params, ncol=nalts-1)))
#  for ( i in 1:(nalts-1)) {
#    names(start.params.ls[[i]]) <- all.params
#  }
  
#  start.params.ls[[ nalts ]] <- rep(0, length(all.params) )
#  names(start.params.ls[[ nalts ]]) <- all.params
  

  inner_2 <- 0
  for (i in 1:nalts)  {     # %*** Create LLF denom. ***    
 #     inner_2 = inner_2 + exp(with(start.params.ls, eval(parse(text= alt.cost.fn.expr.ls.no.subscripts[[i]] ))) -
  #        overflow_protect )     
        inner_2=inner_2 + exp(with(start.params.ls, eval(parse(text=alt.cost.fn.expr.ls.no.subscripts[[i]]))))
  }
#  cat(inner_2)
  
  # Just show prob of being in 2nd category at first (i.e., use fert)
  exp( with(start.params.ls, eval(parse(text=alt.cost.fn.expr.ls.no.subscripts[[outcome]]))) ) / inner_2
}  

#      part_2=part_2 + dummy_j * ( log(inner_2) + overflow_protect );
#   	  inner_1=inner_1 + dummy_j * with(start.params.ls, eval(parse(text= alt.cost.fn.expr.ls.no.subscripts[[j]] ))) 




#predicted.regime <- 5

#summary(cond_logit_predict(GAMS.multinom.logit.results.params.full, outcome=1) +
#cond_logit_predict(GAMS.multinom.logit.results.params.full, outcome=2) +
#cond_logit_predict(GAMS.multinom.logit.results.params.full, outcome=3) +
#cond_logit_predict(GAMS.multinom.logit.results.params.full, outcome=4) #+
#cond_logit_predict(GAMS.multinom.logit.results.params.full, outcome=5) 
#)

#summary(cond_logit_predict(GAMS.multinom.logit.results.params.full, outcome=predicted.regime))


J <- J + 1
# This make room for the correction factor

source(paste0(code.dir, "sgm-linear-sur-building.r"))  





for ( i in 1:nalts) {

  temp.outcome.pred <- cond_logit_predict(
    GAMS.multinom.logit.results.params.full, outcome=i)
    
  temp.outcome.pred <- ifelse(
    temp.outcome.pred < 0.001, 0.001, temp.outcome.pred)
  temp.outcome.pred <- ifelse(
    temp.outcome.pred > 0.999, 0.999, temp.outcome.pred)
  # This prevents divide-by-zero

  assign( paste0("P", lead.zero(i)), temp.outcome.pred)
  
  assign( paste0("RInd", lead.zero(i)), get(paste0("mode", i) )) 
    
}




#correction.factor <- paste0("lambda", lead.zero(1:nalts), " * ( log(P", lead.zero(1:nalts), ") / (1-P", lead.zero(1:nalts), 
#  ") ) * (P", lead.zero(1:nalts), " - RInd", lead.zero(1:nalts), ")", collapse=" + " )
# 2nd line of eqn 31 in Dubin & Mcfadden (1984)

#correction.factor <- paste0("lambda", lead.zero(1:(nalts-1)), " * ( log(P", lead.zero(1:(nalts-1)), ") / (1-P", lead.zero(1:(nalts-1)), 
#  ") ) * (P", lead.zero(1:(nalts-1)), " - RInd", lead.zero(1:(nalts-1)), ")", collapse=" + " )
# Do above if want to eliminate last lambda if you think there are still colinearity problems
  
correction.factor <- paste0("lambda", lead.zero(1:nalts), " * ( log(P", lead.zero(1:nalts), ") / (1-P", lead.zero(1:nalts), 
  ") ) * (P", lead.zero(1:nalts), " - RInd", lead.zero(1:nalts), ")", collapse=" + " )
# NOTE: DON'T CHANGE THIS to 1:(nalts-1), for example, unless 
# you go to the "all.params <- c(all.params, paste0("lambda", lead.zero(1:nalts)) )" line
# in the sgm-GAMS-regimes-construction script file
  
# 2nd line of eqn 31 in Dubin & Mcfadden (1984)




# Need to double-check what the below would do. Seems like it is designed to not have
# a parameter associated with the correction factor, so the expression is probably normalized
# already. So for now we are sticking with the Perraudin & Sorensen (2000) way, which is above.
# The code below also "inverted" RInd01, etc., which actually badly messed up the scheme 
# of technology-switching.
# Ok, trying a different way:

#for ( i in 1:nalts) {
#  assign( paste0("RInd", lead.zero(i)), ifelse(get(paste0("mode", i) )==1, 0, 1) ) 
#}
# So the above assigns an "inverse dummy" so we can do the below:

#correction.factor <- paste0("RInd", lead.zero(1:nalts), " * lambda", lead.zero(1:nalts), 
#  " * ( ( (P", lead.zero(1:nalts), " * log(P", lead.zero(1:nalts), ")) / (1-P", lead.zero(1:nalts), 
#  ") ) + log(P", lead.zero(1:nalts), ") )", collapse=" + " )
  
# 2nd line of eqn 31 in Dubin & Mcfadden (1984),
# but really reflects eqn 33
# RInd01 * lambda01 * ( ( (P01 * log(P01)) / (1-P01) ) + log(P01) ) 

# NOTE: DON'T CHANGE THIS to 1:(nalts-1), for example, unless 
# you go to the "all.params <- c(all.params, paste0("lambda", lead.zero(1:nalts)) )" line
# in the sgm-GAMS-regimes-contruction script file


correction.factor  <- paste0("( ", correction.factor, " )")






all.params <- unique(unlist(str_extract_all(unlist(demand.eqns), 
"(s.[0-9][0-9].[0-9][0-9])|(b.y.[0-9][0-9])|(b.[0-9][0-9])|(b.y.y)|(d.[0-9][0-9].[0-9][0-9])|(c.[0-9][0-9] )|(c.[0-9][0-9].[0-9][0-9])"
  ))
)

#all.params <- gsub("[.]", "", all.params)

all.params <- gsub(" ", "", all.params)


# WARNING: seems like demand.eqns.nonlinear has not been stripped of its 
# dots in the params at this point. But that is the right thing to do so that
# by.01 does not get replaced by the subscript when it sees by01

demand.eqns.alt.ls <- list()

for ( targ.regime in 1:nalts) {

  demand.eqns.alt <- demand.eqns.nonlinear # gsub( "[.]", "", 

  for (targ.param in all.params) {
    demand.eqns.alt <- gsub(paste0(targ.param, " "), 
      paste0(targ.param, "R", lead.zero(targ.regime), " "), demand.eqns.alt, fixed=TRUE)
      # The addition of space is so that e.g. c.02 doesnt replace the c.02.02 erroneously, 
    if (targ.param %in% paste0("b.", lead.zero(1:N)) ) {
      demand.eqns.alt <- gsub(paste0(targ.param, "/"), 
      paste0(targ.param, "R", lead.zero(targ.regime), "/"), demand.eqns.alt, fixed=TRUE)
    
    }
  }
  
  demand.eqns.alt.ls[[targ.regime]] <- demand.eqns.alt

}

demand.eqns.alt.mat <- do.call(cbind, demand.eqns.alt.ls)




demand.eqns.alt <- apply(demand.eqns.alt.mat, 1, function(x) {
  paste0( "RInd", lead.zero(1:nalts), " * ( ", x, " ) ", collapse= " + ")
} )

demand.eqns.alt <- as.list(demand.eqns.alt)


#  assign( paste0("q", lead.zero(J)), eval(parse(text=correction.factor)) )

if ( !set.exp.correction.as.q07 ) {
  demand.eqns.alt <- gsub(paste0("q", lead.zero(J)), correction.factor,  demand.eqns.alt)
}

# save.image("/Users/travismcarthur/Desktop/gamsdir/projdir2/R workspace right before test of correction factor model.Rdata")


 

if ( normalize.cond.exp.coefs ) { 

normalize.replacement.search.string <- paste0("(d[.][0-9][0-9][.]", lead.zero(J), ")|(c[.]", lead.zero(J), 
  ")|(c[.][0-9][0-9][.]", lead.zero(J), ")|(c[.]", lead.zero(J), "[.][0-9][0-9])")

normalize.replacement.naked.params <- all.params[grepl(normalize.replacement.search.string, all.params)]

demand.eqns.alt <- lapply(demand.eqns.alt, function(x) {
  for ( i in 1:length(normalize.replacement.naked.params) ) {
    x <- gsub(paste0(normalize.replacement.naked.params[i], "R", lead.zero(nalts)), "(-1)", x)
  }
  x
} )

}

# We do normalize.cond.exp.coefs because otherwise the params associated with the
# conditional expectation correction factor are not identified. See footnote 21 on
# page 125 of Perraudin & Sorensen (2000). This was a major source of a "bug" in
# previous versions of this code.





lambda.param.support <- c(-100, 0, 100)



start.nonlin.from.ignorance <- FALSE
# Not sure why I need this line here, but it was here in the previous iteration


# Ok, so for now the code does regime-specific params, but 
# a common shadow price param








