

GAMS.multinom.logit.results<- readLines(paste0(GAMS.projdir, "MLEmultinomiallogit", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), file.flavor, ".lst"))


#GAMS.multinom.logit.results<- readLines(paste0(GAMS.projdir, "MLEmultinomiallogit", strsplit(target.crop, " ")[[1]][1], 
#   formatC(bootstrap.iter, width = 5, flag = "0"), "saved.lst"))


#GAMS.multinom.logit.results<- readLines(paste0(GAMS.projdir,"sgmGMEnonlinearHaba00000 before soil and elev.lst"))



GAMS.multinom.logit.results.params<- GAMS.multinom.logit.results[grep("parameters to be estimated$", GAMS.multinom.logit.results)]

GAMS.multinom.logit.results.params <- GAMS.multinom.logit.results.params[grep("VARIABLE", GAMS.multinom.logit.results.params)]

GAMS.multinom.logit.results.params.names <- gsub("[.]L", "", str_extract(GAMS.multinom.logit.results.params, "[^ ]*[.]L") )


GAMS.multinom.logit.results.params.numbers <- as.numeric(gsub("  parameters to be estimated", "",
  str_extract(GAMS.multinom.logit.results.params, "[^ ]*  parameters to be estimated") ) )

GAMS.multinom.logit.results.params.full <- GAMS.multinom.logit.results.params.numbers
names(GAMS.multinom.logit.results.params.full) <- GAMS.multinom.logit.results.params.names

overflow_protect <- GAMS.multinom.logit.results[grep("a trick to protect the exp from overflow", GAMS.multinom.logit.results)]

overflow_protect <- as.numeric(gsub("  a trick to protect the exp from overflow", "",
  str_extract(overflow_protect, "[^ ]*  a trick to protect the exp from overflow") ) )

# So the functions below will just access the value of overflow protect from the 
# global environment here



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
  
  assign( paste0("RInd", lead.zero(i)), paste0("mode", i) ) 
    
}




#correction.factor <- paste0("lambda", lead.zero(1:nalts), " * ( log(P", lead.zero(1:nalts), ") / (1-P", lead.zero(1:nalts), 
#  ") ) * (P", lead.zero(1:nalts), " - RInd", lead.zero(1:nalts), ")", collapse=" + " )
# 2nd line of eqn 31 in Dubin & Mcfadden (1984)

correction.factor <- paste0("lambda", lead.zero(1:(nalts-1)), " * ( log(P", lead.zero(1:(nalts-1)), ") / (1-P", lead.zero(1:(nalts-1)), 
  ") ) * (P", lead.zero(1:(nalts-1)), " - RInd", lead.zero(1:(nalts-1)), ")", collapse=" + " )
# 2nd line of eqn 31 in Dubin & Mcfadden (1984)

correction.factor  <- paste0("( ", correction.factor, " )")





demand.eqns.alt.ls <- list()

for ( targ.regime in 1:nalts) {

  demand.eqns.alt <- demand.eqns.nonlinear

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

demand.eqns.alt <- gsub(paste0("q", lead.zero(J)), correction.factor,  demand.eqns.alt)

# save.image("/Users/travismcarthur/Desktop/gamsdir/projdir2/R workspace right before test of correction factor model.Rdata")


lambda.param.support <- c(-100, 0, 100)



start.nonlin.from.ignorance <- FALSE
# Not sure why I need this line here, but it was here in the previous iteration











