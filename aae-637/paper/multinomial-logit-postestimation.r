



# GAMS.nonlinear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytest.lst")




GAMS.nonlinear.results<- readLines(paste0(GAMS.projdir, "MLEmultinomiallogit", strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), file.flavor, ".lst"))


#GAMS.nonlinear.results<- readLines(paste0(GAMS.projdir, "MLEmultinomiallogit", strsplit(target.crop, " ")[[1]][1], 
#   formatC(bootstrap.iter, width = 5, flag = "0"), "saved.lst"))


#GAMS.nonlinear.results<- readLines(paste0(GAMS.projdir,"sgmGMEnonlinearHaba00000 before soil and elev.lst"))



GAMS.nonlinear.results.params<- GAMS.nonlinear.results[grep("parameters to be estimated$", GAMS.nonlinear.results)]

GAMS.nonlinear.results.params <- GAMS.nonlinear.results.params[grep("VARIABLE", GAMS.nonlinear.results.params)]

GAMS.nonlinear.results.params.names <- gsub("[.]L", "", str_extract(GAMS.nonlinear.results.params, "[^ ]*[.]L") )


GAMS.nonlinear.results.params.numbers <- as.numeric(gsub("  parameters to be estimated", "",
  str_extract(GAMS.nonlinear.results.params, "[^ ]*  parameters to be estimated") ) )

GAMS.nonlinear.results.params.full <- GAMS.nonlinear.results.params.numbers
names(GAMS.nonlinear.results.params.full) <- GAMS.nonlinear.results.params.names

overflow_protect <- GAMS.nonlinear.results[grep("a trick to protect the exp from overflow", GAMS.nonlinear.results)]

overflow_protect <- as.numeric(gsub("  a trick to protect the exp from overflow", "",
  str_extract(overflow_protect, "[^ ]*  a trick to protect the exp from overflow") ) )

# So the functions below will just access the value of overflow protect from the 
# global environment here


# NEW WAY TO GET PARAM VALUES BELOW

gdx.params.df <- read.csv(file=paste0(GAMS.projdir, "MLEmultinomiallogit", strsplit(target.crop, " ")[[1]][1], 
     formatC(bootstrap.iter, width = 5, flag = "0"), file.flavor  , "-param-output.txt"),
     head=FALSE, stringsAsFactors=FALSE)
   
names(gdx.params.df) <- c("param.names", "param.values")  
     
GAMS.nonlinear.results.params.full <- 
  gdx.params.df$param.values[gdx.params.df$param.names!="overflow_protect"]
names(GAMS.nonlinear.results.params.full) <- 
  gdx.params.df$param.names[gdx.params.df$param.names!="overflow_protect"]
  
overflow_protect <-
  gdx.params.df$param.values[gdx.params.df$param.names=="overflow_protect"]




cond_logit_llf <- function(b0,  data) {
#   global rhsvar nalts mode_id mode ;

  start.params <- b0
#  names(start.params) <- rep(all.params, nalts)
  start.params.ls<- as.list(start.params)
  start.params.ls <- c(start.params.ls, as.list(data) )
  
  alt.cost.fn.expr.ls.no.subscripts <- lapply(alt.cost.fn.expr.ls, FUN=gsub, pattern="(t)", replacement="", fixed=TRUE)
  
 # cat(alt.cost.fn.expr.ls.no.subscripts[[1]])
  
#  for ( i in 1:(nalts-1)) {
#    names(start.params.ls[[i]]) <- all.params
#  }
  
#  start.params.ls[[ nalts ]] <- rep(0, length(all.params) )
#  names(start.params.ls[[ nalts ]]) <- all.params
  
  #start.params.ls<- list(start.params[1:length(all.params)], start.params[(length(all.params)+1):length(start.params)])
#print(start.params.ls)
 
  part_2=0
  inner_1=0
  inner_2=0
  for (i in 1:nalts)  {     # %*** Create LLF denom. ***         
        inner_2=inner_2 + exp(with(start.params.ls, eval(parse(text= alt.cost.fn.expr.ls.no.subscripts[[i]] ))) -
          overflow_protect ) #exp("EVALUATE i'th expression" ) #    %*** (T x 1) ***     
  }
  
#  print(log(inner_2))
  for (j in 1:nalts)  {                         #%*** No. of Alternatives ***
 
      dummy_j= get(paste0("mode", j)) # ; %**Dummy var.,jth model chosen **
      # May want to change this to the mode1, etc., variable.
      # OK, now this should probs work
      
      part_2=part_2 + dummy_j * ( log(inner_2) + overflow_protect );
   	  inner_1=inner_1 + dummy_j * with(start.params.ls, eval(parse(text= alt.cost.fn.expr.ls.no.subscripts[[j]] ))) # ("EVALUATE j'th expression") #; %***Sum if j consumed (T x 1) ***
   }  
#   print(inner_1)
   
    sum(inner_1-part_2)
   
}

# as.data.frame(apply(signif(raw.first.df, digits=5), 2, FUN=as.character))




with(as.data.frame(apply(signif(combined.df, digits=5), 2, FUN=as.character)), 
  cond_logit_llf(GAMS.nonlinear.results.params.full )
)


with( as.data.frame(apply(signif(combined.df, digits=5), 2, FUN=as.character)), 
  cond_logit_llf(GAMS.nonlinear.results.params.full )
)



cond_logit_llf(GAMS.nonlinear.results.params.full,
  data= as.data.frame(apply(signif(combined.df, digits=5), 2, FUN=as.character))) 


cond_logit_llf(GAMS.nonlinear.results.params.full)
  

cond_logit_llf(GAMS.nonlinear.results.params.full,
  data=  signif(combined.df, digits=5))


# w01.save <- w01






#install.packages("numDeriv")
library("numDeriv")


z = grad(cond_logit_llf, GAMS.nonlinear.results.params.full, method="complex", 
  data=  signif(combined.df, digits=5)) # ; % Numerical Gradients   
  
#valid.cov.names <- names(GAMS.nonlinear.results.params.full)[is.finite(z)]

z[!is.finite(z)] <- rnorm(sum(!is.finite(z)), mean=mean(z, na.rm=TRUE), sd=sd(z, na.rm=TRUE) )
# just trying out this very hacky non-legit thing

# Or can do this:
z <- z[is.finite(z)]
      
H = z %o% z  # outer product
cov_betas=solve(H, tol=10^-50)
# Above is no good.
"simple"
# , method="complex"

diag.cov <- diag(cov_betas)
names(diag.cov) <- names(GAMS.nonlinear.results.params.full)

sort(   sqrt(diag.cov)/sqrt(length(x01))  ) 


sort(  GAMS.nonlinear.results.params.full / ( sqrt(diag.cov)/sqrt(length(x01)) )  ) 



summary(sqrt(diag(cov_betas))/sqrt(length(x01)))








cond_logit_predict <- function(b0, outcome, data) {

  start.params <- b0
#  names(start.params) <- rep(all.params, nalts)
  start.params.ls<- as.list(start.params)
  start.params.ls <- c(start.params.ls, as.list(data) )
  
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




predicted.regime <- 5

summary(cond_logit_predict(GAMS.nonlinear.results.params.full, outcome=1, data=  signif(combined.df, digits=5)) +
cond_logit_predict(GAMS.nonlinear.results.params.full, outcome=2, data=  signif(combined.df, digits=5)) +
cond_logit_predict(GAMS.nonlinear.results.params.full, outcome=3, data=  signif(combined.df, digits=5)) +
cond_logit_predict(GAMS.nonlinear.results.params.full, outcome=4, data=  signif(combined.df, digits=5)) +
cond_logit_predict(GAMS.nonlinear.results.params.full, outcome=5, data=  signif(combined.df, digits=5)) 
)

summary(cond_logit_predict(GAMS.nonlinear.results.params.full, outcome=predicted.regime, data=  signif(combined.df, digits=5)))

predictions <- cond_logit_predict(GAMS.nonlinear.results.params.full, outcome=predicted.regime, data=  signif(combined.df, digits=5))

table( regime.cut == predicted.regime, predictions> mean(predictions) )

table( regime.cut == predicted.regime, predictions> 0.5 )

cor( regime.cut == predicted.regime, predictions> mean(predictions) )
cor( regime.cut == predicted.regime, predictions> 0.5 )


predicted.regime <- 1
predictions <- cond_logit_predict(GAMS.nonlinear.results.params.full, outcome=predicted.regime, data=  signif(combined.df, digits=5))

plot( predictions, jitter(as.numeric(regime.cut == predicted.regime )))

# But really, how do we do prediction for the residual category?

# McFadden's R squared is defined as 1-l_mod/l_null,
# http://stats.stackexchange.com/questions/82105/mcfaddens-pseudo-r2-interpretation
# McFadden states "while the R2 index is a more familiar concept to planner who 
# are experienced in OLS, it is not as well behaved as the rho-squared measure, for 
# ML estimation. Those unfamiliar with rho-squared should be forewarned that its 
# values tend to be considerably lower than those of the R2 index...For example,
# values of 0.2 to 0.4 for rho-squared represent EXCELLENT fit."

# Another reference says: "Simulations by Domenich and McFadden (1975) 
# equivalence this range to 0.7 to 0.9 for a linear function"
# - Louviere et al (2000). "Stated Choice Methods: Analysis and Applications"


L_C = -length(x01) * log(nalts)   #; % Greene 7th ed. p. 807

1 - cond_logit_llf(GAMS.nonlinear.results.params.full)/L_C

# 1 - (-1) * cond_logit_llf(GAMS.nonlinear.results.params.full)/L_C

# TODO: Do a strict test of whether the model has explanatory power

pchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)
qchisq(p, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)


# log likelihood if there were no covariates or constants:
L_c = sum( table(regime.cut)*log(1/(nalts+1)) )
# Greene page 803, I think
# table(regime.cut) is just a way of getting the number of occurrences

# Log likelihood if there are no covariates but there are constants:
L_c = sum( table(regime.cut) * log(table(regime.cut)/length(x01)) )

chi.sq.stat <- (-2) * ( L_c - cond_logit_llf(GAMS.nonlinear.results.params.full, data=  signif(combined.df, digits=5))  )


1 - pchisq(q=chi.sq.stat, df=length(GAMS.nonlinear.results.params.full) )


1 - pchisq(q=chi.sq.stat, df=length(GAMS.nonlinear.results.params.full)*2 )



1 - cond_logit_llf(GAMS.nonlinear.results.params.full, data=  signif(combined.df, digits=5))/L_c





#x01 = firm.df$x19.fertilizante.cantidad.kg
#x02 = firm.df$x19.sem.comprada.cantidad.kg
#x03 = firm.df$tractor.hrs.final
#x04 = firm.df$x19.plagicidas.cantidad.kg
#x05 = firm.df$paid.hours.spread 
#x06 = firm.df$x19.abono.cantidad.kg






for (targ.regime in 1:nalts ) {

just.posi.vars.df.postestimation <- just.posi.vars.df

colnames(just.posi.vars.df.postestimation) <- c("Fert", "Seed", "Tractor", "Plag", "Labor", "Abono")

just.posi.vars.df.postestimation <- just.posi.vars.df.postestimation[, 
  rev(colnames(just.posi.vars.df.postestimation)) ]


regime.table.prep <- as.data.frame( ftable(just.posi.vars.df.postestimation[regime.cut==targ.regime, ]) )


regime.table.prep <- regime.table.prep[regime.table.prep$Freq!=0, ]

regime.table.prep <- regime.table.prep[nrow(regime.table.prep):1, ]

regime.table.prep <- regime.table.prep[, c(6:1, 7)]
# To undo reverse of columns above. Helps group the "True" better for the fertilizer examination

#regime.table.prep[, 1:6][as.logical(as.matrix(regime.table.prep[, 1:6]))] <- "X"
#regime.table.prep[, 1:6][regime.table.prep==FALSE] <- ""
# Old attempt is above

regime.table.prep <- as.data.frame( lapply(regime.table.prep, FUN=function(x) {
  if ( is.numeric(x) ) {return(x)}
  ifelse(as.logical(as.character(x)), "X", "")
  } ) )
# A little hacky, above


library("xtable")


xtab.output <- print(xtable(regime.table.prep,
  caption=paste0("Regime breakdown: ", strsplit(target.crop, " ")[[1]][1], ", regime ", targ.regime)) ,
  caption.placement = "top")
  
cat(xtab.output, sep="n",
      file=paste0("/Users/travismcarthur/Desktop/Bolivia project/Multinomial logit results/regimes-breakdown-",
      strsplit(target.crop, " ")[[1]][1], "-regime-", targ.regime, ".tex")
      )

}



regime.corr.df <- data.frame(Regime=NA, Correlation=NA )[-1, ]


for ( predicted.regime in 1:nalts) {

  predictions <- cond_logit_predict(GAMS.nonlinear.results.params.full, outcome=predicted.regime, data=  signif(combined.df, digits=5))

  regime.corr.df <- rbind(regime.corr.df, c(predicted.regime, cor( regime.cut == predicted.regime, predictions) ) )

}


colnames(regime.corr.df) <- c("Regime", "Correlation")

library("xtable")

xtab.output <- print(xtable(regime.corr.df,
  caption=paste0("Correlation between predicted and actual regime from multinomial logit: ", strsplit(target.crop, " ")[[1]][1])) ,
  caption.placement = "top")
  
cat(xtab.output, sep="n",
      file=paste0("/Users/travismcarthur/Desktop/Bolivia project/Multinomial logit results/regimes-prediction-",
      strsplit(target.crop, " ")[[1]][1], ".tex")
      )













predicted.regime <- 1

summary(cond_logit_predict(GAMS.nonlinear.results.params.full, outcome=1, data=  signif(combined.df, digits=5)) +
cond_logit_predict(GAMS.nonlinear.results.params.full, outcome=2, data=  signif(combined.df, digits=5)) +
cond_logit_predict(GAMS.nonlinear.results.params.full, outcome=3, data=  signif(combined.df, digits=5)) +
cond_logit_predict(GAMS.nonlinear.results.params.full, outcome=4, data=  signif(combined.df, digits=5)) +
cond_logit_predict(GAMS.nonlinear.results.params.full, outcome=5, data=  signif(combined.df, digits=5)) 
)

summary(cond_logit_predict(GAMS.nonlinear.results.params.full, outcome=predicted.regime, data=  signif(combined.df, digits=5)))

predictions <- cond_logit_predict(GAMS.nonlinear.results.params.full, outcome=predicted.regime, data=  signif(combined.df, digits=5))

table( regime.cut == predicted.regime, predictions> mean(predictions) )

table( regime.cut == predicted.regime, predictions> 0.5 )

cor( regime.cut == predicted.regime, predictions> mean(predictions) )
cor( regime.cut == predicted.regime, predictions> 0.5 )


cor( regime.cut == predicted.regime, predictions)






























xtab.output <- print(xtable(shadow.price.params.extracted(params=results.ls$params),
  caption=paste0("Shadow price parameters for ", text.crop)),
  caption.placement = "top")
  
cat(xtab.output, sep="n",
      file=paste0("/Users/travismcarthur/Desktop/Proposal course/Materials for 3-31 meeting with Brad/shadow-price-",
      text.crop, "-", J, "-fixed-inputs.tex")
      )












