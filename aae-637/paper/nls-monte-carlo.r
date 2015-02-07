

library("minpack.lm")

# THis code below may have some loose ends


# source(paste0(code.dir, "sgm-linear-sur-building.r"))

all.params <- unique(unlist(str_extract_all(unlist(demand.eqns[[1]]), 
"(s.[0-9][0-9].[0-9][0-9])|(b.y.[0-9][0-9])|(b.[0-9][0-9])|(b.y.y)|(d.[0-9][0-9].[0-9][0-9])|(c.[0-9][0-9] )|(c.[0-9][0-9].[0-9][0-9])"
  ))
)

all.params <- gsub(" ", "", all.params)

linear.nls.formula <- as.formula( paste0( "E.y01.data ~ ", demand.eqns[[1]]))
#linear.nls.formula <- as.formula( paste0( "I(x01/y01) ~ ", demand.eqns[[1]]))

start.linear.ls <- vector("list", length(all.params))
start.linear.ls[] <- 0
names(start.linear.ls) <- all.params


linear.nls.fit <- nlsLM(linear.nls.formula , start=start.linear.ls, 
  control = nls.lm.control(maxiter = 1024), trace = TRUE)


#cat(coef(linear.nls.fit ), sep="\n")

xi.start.vals.nls <- rep(1, N-1)

names(xi.start.vals.nls) <- paste0("xi.", lead.zero(1:(N-1)))

assign(paste0("xi.", lead.zero(N)),  1)

start.nonlinear.ls <- as.list(c(coef(linear.nls.fit), xi.start.vals.nls)) 

nonlinear.nls.formula <- as.formula( paste0( "E.y01.data ~ ", demand.eqns.nonlinear[[1]]))
#nonlinear.nls.formula <- as.formula( paste0( "I(x01/y01) ~ ", demand.eqns.nonlinear[[1]]))


nonlinear.nls.fit <- nlsLM(nonlinear.nls.formula , start=start.nonlinear.ls, 
  control = nls.lm.control(maxiter = 1024, maxfev=10^9), trace = TRUE,
  lower=ifelse(grepl("xi", names(start.nonlinear.ls)), .0001, -Inf))
  
  
summary(nonlinear.nls.fit)

nonlinear.nls.formula <- as.formula( paste0( "E.y01.data ~ ", gsub("[.]", "", demand.eqns.nonlinear[[1]])))
assign(paste0("xi", lead.zero(N)),  1)
  
nonlinear.nls.fit <- nlsLM(nonlinear.nls.formula , start=as.list(synthetic.params)[-length(synthetic.params)], 
  control = nls.lm.control(maxiter = 1024, maxfev=10^9), trace = TRUE)
  
  
summary(nonlinear.nls.fit)



start.nonlinear.ls[!grepl("xi", names(start.nonlinear.ls))] <- 0

nonlinear.nls.fit <- nlsLM(nonlinear.nls.formula , start=start.nonlinear.ls, 
  control = nls.lm.control(maxiter = 1024, maxfev=10^9), trace = TRUE)




param.check.df <- data.frame(est.params=coef(nonlinear.nls.fit), param.names=gsub("[.]", "", names(coef(nonlinear.nls.fit))))
param.check.synth.df <- data.frame(synth.params=synthetic.params, param.names=names(synthetic.params))
param.check.df <- merge(param.check.df, param.check.synth.df)


nls.all.param.corr.ls[[intended.seed]] <- cor(param.check.df$est.params , 
  param.check.df$synth.params)
  
nls.xi.param.corr.ls[[intended.seed]] <- cor(param.check.df$est.params[grepl("xi", param.check.df$param.names)] , 
  param.check.df$synth.params[grepl("xi", param.check.df$param.names)])


 r.sq.val <-  1 - sum(resid(linear.nls.fit)^2)/sum(E.y01.data^2)
  
 r.sq.val - (1- r.sq.val) * length(coef(linear.nls.fit))/(length(E.y01.data)-length(coef(linear.nls.fit))-1)


summary(linear.nls.fit)
summary(nonlinear.nls.fit)










  

