
# THESE ARE IMPORTANT PARAMS IMMEDIATELY BELOW:
mfx.on.posi.median <- FALSE
mean.of.MP <- TRUE
# NOTE, IMPORTANT: It seems that under a quadratic specification, the mean of the marginal
# products is equal to the marginal product of the mean, so the above is somewhat redundant.
# The elasticities differ, however.


local.source.evaluation <- FALSE
dropped.cost.share.eq <- 10
# anything >6 means that no equation gets dropped


saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/GAMS work/saved workspace.Rdata"

saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF.Rdata"



GAMS.projdir <-  "/Users/travismcarthur/Desktop/gamsdir/projdir/"

GAMS.exe.path <- "/Applications/GAMS/gams24.1_osx_x64_64_sfx/gams"

code.dir <- "/Users/travismcarthur/git/coursework/aae-637/paper/"

# GAMS.projdir.subdir <-  "/Users/travismcarthur/Desktop/gamsdir/projdir/bootstrap/"

if (Sys.info()['sysname']=="Linux") {

saved.workspace.path <- "/home/c/cschmidt/TravisImInYourInternets/bootstrap-output/saved workspace.Rdata"

GAMS.projdir <-  "/home/c/cschmidt/TravisImInYourInternets/gamsdir/projdir/"

GAMS.exe.path <- "/home/c/cschmidt/TravisImInYourInternets/gams24.1_linux_x64_64_sfx/gams"

code.dir <- "/home/c/cschmidt/TravisImInYourInternets/bootstrap-R-code/"

.libPaths("/home/c/cschmidt/TravisImInYourInternets/Rlib")

#detach("package:Matrix", unload = TRUE, force=TRUE)
#detach("package:lattice", unload = TRUE, force=TRUE)

#unloadNamespace("lattice")

#install.packages("lattice", repos="http://cran.us.r-project.org", 
#        lib="/home/c/cschmidt/TravisImInYourInternets/Rlib")

library(lattice, lib.loc ="/home/c/cschmidt/TravisImInYourInternets/Rlib")

library(Matrix)

  for ( i in c("gdata", "stringr", "systemfit") ) {
    if(!require(i, character.only=TRUE, lib.loc ="/home/c/cschmidt/TravisImInYourInternets/Rlib")) {
      install.packages(i, repos="http://cran.us.r-project.org", 
        lib="/home/c/cschmidt/TravisImInYourInternets/Rlib")
      while(!require(i, character.only=TRUE, lib.loc ="/home/c/cschmidt/TravisImInYourInternets/Rlib")) {
        Sys.sleep(1)
  	    require(i, character.only=TRUE, lib.loc ="/home/c/cschmidt/TravisImInYourInternets/Rlib")
  	  }
    }
  }

}


# target.top.crop.number <- 1

bootstrapped.marg.results.ls <- list()
r.sq.list <- list()

for ( target.top.crop.number in 1:5) {

#Papa (patatas)    3155 
#Maiz combined   1838 
#Cebada combined   950 
#Trigo             475 
#Haba (verde)       641 
#Oca               240 
#Arveja (verde)     217 
#Hoja de coca       363 
#Arroz con cascara          264
#Quinua            284 



load(saved.workspace.path)










log.plus.one.cost <- FALSE

bootstrap.iter <- 1
# NOTE: Bootstrap iter = 0 means actual estimate
bootstrap.selection.v <- TRUE
source(paste0(code.dir, "build-model-extract-parcels.r"))


combined.df <- data.frame(mget(c("y01", paste0("x", lead.zero(1:N)), 
  paste0("w", lead.zero(1:N)),  paste0("q", lead.zero(1:J)) )))
  
#region.matrix.df <-   as.data.frame(region.matrix)
#colnames(region.matrix.df) <- iconv(colnames(region.matrix.df), to="ASCII//TRANSLIT")
#colnames(region.matrix.df) <- gsub("'", "", colnames(region.matrix.df) )
#colnames(region.matrix.df) <- gsub("[.]", "", colnames(region.matrix.df) )
  
#combined.df <- cbind(combined.df, region.matrix.df)
#combined.df <- cbind(combined.df, region)


combined.df$y01 <- exp(combined.df$y01)
# need to convert back to original

combined.df.orig <- combined.df
combined.df.orig.quad <- combined.df.orig
combined.df.orig.quad$q02 <- log(combined.df.orig.quad$q02)
# To change irrigation back to zero-one

for ( i in paste0("x", lead.zero(1:N))) {
  combined.df[, i] <- combined.df[, i] + min(combined.df[combined.df[, i]!=0, i])
}
# S0 no zeros when we take logs


region.matrix.df <-   as.data.frame(region.matrix)
colnames(region.matrix.df) <- iconv(colnames(region.matrix.df), to="ASCII//TRANSLIT")
colnames(region.matrix.df) <- gsub("'", "", colnames(region.matrix.df) )
colnames(region.matrix.df) <- gsub("[.]", "", colnames(region.matrix.df) )
  
combined.df.orig.quad <- cbind(combined.df.orig.quad, region.matrix.df)



# install.packages("micEcon")
library("micEcon")

# paste0("q", lead.zero(1:J)))
# c(paste0("x", lead.zero(1:N)), "q01", "q03")
# Take out irrigation for now




boot.replications <- 2000

# as.data.frame(t(apply(combined.df.orig.quad, 2, function(x) median(x[x>0]) )))

set.seed(100)


bootstraps <- apply(matrix(
    sample(1:nrow(combined.df.orig.quad), 
    size = boot.replications*nrow(combined.df.orig.quad), replace=TRUE), 
    nrow=boot.replications),
    1, FUN=function(x) {
    
    data.df <- combined.df.orig.quad[x, ]
    estResult <- quadFuncEst(  "y01", 
      c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
          data=data.df, shifterNames=colnames(region.matrix.df) ) 
          
    estResult$coef[is.na(estResult$coef)] <- 0
    
    if (mfx.on.posi.median ) {
      combined.mean.temp <- as.data.frame(t(apply(data.df, 2, function(x) median(x[x>0], na.rm=TRUE) )))
      combined.mean.temp[,apply(combined.mean.temp, 1, is.na)] <- 0
      # Above is to deal with case when a region drops out of the bootstrap
      # Careful: This sets all regions dummies to 1, but this does not affect our
      # marginal effects calculation here    
    } else {
      combined.mean.temp <- as.data.frame(t(colMeans(data.df)))
    }
    
    if (mean.of.MP) {
      margProducts <- colMeans(quadFuncDeriv( 
        c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
          data.df, coef( estResult ) ), na.rm=TRUE)
    } else {
      margProducts <- quadFuncDeriv( 
      c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
          combined.mean.temp, coef( estResult ) ) 
    }
          
    names(margProducts) <- paste0("margProduct.",  names(margProducts))
    
    if (mean.of.MP) {
      estElasticities <- colMeans(elasticities(estResult, data=data.df), na.rm=TRUE)
    } else {
      estElasticities <- elasticities(estResult, data=combined.mean.temp)
    }
    
    names(estElasticities) <- paste0("prodElast.",  names(estElasticities))
    
    c(unlist(margProducts), unlist(estElasticities))
    
    }
)

set.seed(100)

point.est <- apply(matrix(
    1:nrow(combined.df.orig.quad), 
#    sample(1:nrow(combined.df.orig.quad), replace=TRUE), 
    nrow=1),
    1, FUN=function(x) {
    
    data.df <- combined.df.orig.quad[x, ]
    estResult <- quadFuncEst(  "y01", 
      c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
          data=data.df, shifterNames=colnames(region.matrix.df) ) 
          
    estResult$coef[is.na(estResult$coef)] <- 0

    if (mfx.on.posi.median ) {
      combined.mean.temp <- as.data.frame(t(apply(data.df, 2, function(x) median(x[x>0], na.rm=TRUE) )))
      combined.mean.temp[,apply(combined.mean.temp, 1, is.na)] <- 0
      # Above is to deal with case when a region drops out of the bootstrap
      # Careful: This sets all regions dummies to 1, but this does not affect our
      # marginal effects calculation here    
    } else {
      combined.mean.temp <- as.data.frame(t(colMeans(data.df)))
    }
    
   if (mean.of.MP) {
      margProducts <- colMeans(quadFuncDeriv( 
        c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
          data.df, coef( estResult ) ), na.rm=TRUE)
    } else {
      margProducts <- quadFuncDeriv( 
      c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
          combined.mean.temp, coef( estResult ) ) 
    }
          
    names(margProducts) <- paste0("margProduct.",  names(margProducts))
    
    if (mean.of.MP) {
      estElasticities <- colMeans(elasticities(estResult, data=data.df), na.rm=TRUE)
    } else {
      estElasticities <- elasticities(estResult, data=combined.mean.temp)
    }
    
    names(estElasticities) <- paste0("prodElast.",  names(estElasticities))
    
    c(unlist(margProducts), unlist(estElasticities))
    
    }
)


#bootstraps.mat <- do.call(rbind, bootstraps)

#CI.mat <- matrix(apply(bootstraps, 1, FUN=quantile, probs=c(0.025, 0.975)), ncol=2, byrow=TRUE)

CI.mat <- matrix(apply(bootstraps, 1, FUN=quantile, probs=c(0.05, 0.95), na.rm=TRUE), ncol=2, byrow=TRUE)

input.desc <- c("Inorganic Fert", "Seeds", "Tractor Hrs", "Plaguicidas", "Hired Labor", "Organic Fert", "Land", "Irrigation", "Family Labor")

marg.prod.row.names <- rep(input.desc, 2)

marg.prod.measure <- c(rep("Marginal Prod", length(input.desc)), rep("Output elasticity", length(input.desc)))

input.units <- c("kg", "kg", "hours", "kg", "hours", "kg", "hectares", "binary", "num persons", rep("", length(input.desc)))



if (mfx.on.posi.median ) {
   data.means.v <- c(apply(combined.df.orig.quad[, c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J)))], 2, function(x) median(x[x>0], na.rm=TRUE)), rep(NA, length(input.desc)))
   # Careful: This sets all regions dummies to 1, but this does not affect our
   # marginal effects calculation here    
} else {
  data.means.v <- c(colMeans(combined.df.orig.quad[, c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J)))] ), rep(NA, length(input.desc)))
}


bootstrapped.results.df <- data.frame(Measure=marg.prod.measure, Input.Name=marg.prod.row.names , stringsAsFactors=FALSE) 

bootstrapped.results.df$Point.Estimate <- point.est
bootstrapped.results.df$Units <- input.units
bootstrapped.results.df$Lower.90.CI <- CI.mat[, 1]
bootstrapped.results.df$Upper.90.CI <- CI.mat[, 2]
bootstrapped.results.df$Data.mean <- data.means.v 
if (mfx.on.posi.median) {
  colnames(bootstrapped.results.df)[colnames(bootstrapped.results.df)=="Data.mean"] <- "Data.median|x>0"
}



bootstrapped.marg.results.ls[[target.crop]] <- bootstrapped.results.df
    
r.sq.list[[target.crop]]<- quadFuncEst(  "y01", 
      c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
          data=combined.df.orig.quad, shifterNames=colnames(region.matrix.df) )$r2bar

}




library("stargazer")
library("xtable")

for ( i in 1:length(bootstrapped.marg.results.ls) ) {

  crop.english<- c("Potatoes", "Maize", "Barley", "Wheat", "Fava Beans")

#  test <- stargazer(bootstrapped.marg.results.ls[[i]], 
#  title=paste0("Marginal Product and Output Elasticities for ", crop.english[i], 
#  ", $R^2$ for model is ", round(r.sq.list[[i]], digits=3)),
#  summary=FALSE, rownames=FALSE, align=TRUE, no.space=TRUE,
#  out = paste0("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Marginal products/table", i, ".tex" ))
  

  
  xtab.output <- print(xtable(bootstrapped.marg.results.ls[[i]], ,
    caption=paste0("Marginal Product and Output Elasticities for ", crop.english[i], 
  "; $R^2$ for model is ", round(r.sq.list[[i]], digits=3))),
  hline.after=0:nrow(bootstrapped.marg.results.ls[[i]]),
  caption.placement="top")
  
  xtab.output <- paste(xtab.output, "\\vspace{2em}")
  if (mfx.on.posi.median) {
    cat(xtab.output, sep="\n",
      file=paste0("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Marginal products/tableatmedian", i, ".tex" ))
  } else {
  	if (mean.of.MP) {
      cat(xtab.output, sep="\n",
        file=paste0("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Marginal products/tablemeanofMP", i, ".tex" ))  	
  	} else {
      cat(xtab.output, sep="\n",
        file=paste0("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Marginal products/table", i, ".tex" ))
  	}
  }
  # thanks to http://stackoverflow.com/questions/7160754/adding-a-horizontal-line-between-the-rows-in-a-latex-table-using-r-xtable?rq=1
  
}


# END CODE











  
  library(xtable)
df <- data.frame(name=rep(letters[1:3],each=24),salary=runif(24*3,100,200))
lst <- tapply(df$salary,df$name,matrix,nrow=4,byrow=T)
xlst <- lapply(lst,function(x)print(xtable(x), hline.after=1:nrow(x)))
xlst <- lapply(xlst, paste, "\\vspace{2em}")
xlst


}

xtable(bootstrapped.marg.results.ls[[i]], 
  caption=paste0("Marginal Product and Output Elasticities for ", crop.english[i], 
  "; $R^2$ for model is ", round(r.sq.list[[i]], digits=3)))

























    estResult <- quadFuncEst(  "y01", 
      c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
          data=combined.df.orig.quad, shifterNames=colnames(region.matrix.df) ) 





summary.translogEst <- function (object, ...) 
{
    object$coefTable <- miscTools::coefTable(coef(object)[!is.na(coef(object))], diag(vcov(object))^0.5, 
        df.residual(object$est))
    class(object) <- "summary.translogEst"
    return(object)
}



summary(estResult <- quadFuncEst(  "y01", c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
          data=combined.df.orig.quad, shifterNames="region"  ) )
      
estResult$r2bar    

estResult$coef[is.na(estResult$coef)] <- 0
          
 margProducts <- quadFuncDeriv( c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
          combined.df.orig.quad, coef( estResult ) )  
# vcov( estResult )

 margProducts <- quadFuncDeriv( xNames=c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
          data=as.data.frame(t(colMeans(combined.df.orig.quad[, -ncol(combined.df)]))),
          coef=coef( estResult ),  homWeights = NULL )  
          # coefCov =vcov( estResult ),

colMeans(margProducts)



combined.mean.temp <- as.data.frame(t(colMeans(combined.df.orig.quad[, -ncol(combined.df.orig.quad)])))

combined.mean.temp$region <- names(sort(table(combined.df.orig.quad$region), decreasing=TRUE))[1]

estResult$coef <- estResult$coef[!grepl("^d", names(estResult$coef))]




region.matrix.df <-   as.data.frame(region.matrix)
colnames(region.matrix.df) <- iconv(colnames(region.matrix.df), to="ASCII//TRANSLIT")
colnames(region.matrix.df) <- gsub("'", "", colnames(region.matrix.df) )
colnames(region.matrix.df) <- gsub("[.]", "", colnames(region.matrix.df) )
  
combined.df.orig.quad <- cbind(combined.df.orig.quad, region.matrix.df)

summary(estResult <- quadFuncEst(  "y01", c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
          data=combined.df.orig.quad, shifterNames=colnames(region.matrix.df) ) )
          
estResult$coef[is.na(estResult$coef)] <- 0

combined.mean.temp <- as.data.frame(t(colMeans(combined.df.orig.quad)))

elasticities(estResult, data=combined.mean.temp)




quadFuncEla( c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))), 
data=combined.mean.temp, coef(estResult), yName = NULL,
       shifterNames = colnames(region.matrix.df), homWeights = NULL )


# Bug:

# Error in .quadFuncCheckCoefNames(names(coef), nExog = length(xNames),  : 
#  following coefficients in argument 'coef' are missing: d_1










summary( translog.prod <- 
  translogEst( "y01", c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
          data=combined.df, shifterNames="region" ))

translog.prod$r2

translog.prod$coef[is.na(translog.prod$coef)] <- 0


margProductsObs <- translogDeriv( c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
          combined.df, coef( translog.prod )[!grepl("^d", names(coef( translog.prod )))],
          vcov( translog.prod )[!grepl("^d", names(coef( translog.prod ))), 
            !grepl("^d", names(coef( translog.prod )))],
             "y01" )


colMeans(margProductsObs$deriv)


margProducts <- translogDeriv( c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
          combined.df, 
          coef( translog.prod )[!grepl("^d", names(coef( translog.prod )))],
          vcov( translog.prod )[!grepl("^d", names(coef( translog.prod ))), 
            !grepl("^d", names(coef( translog.prod )))]
             )

colMeans(margProducts$deriv)

margProducts <- translogDeriv( c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
          as.data.frame(t(colMeans(combined.df.orig[, -ncol(combined.df)]))), 
          coef( translog.prod )[!grepl("^d", names(coef( translog.prod )))],
          vcov( translog.prod )[!grepl("^d", names(coef( translog.prod ))), 
            !grepl("^d", names(coef( translog.prod )))],
            "y01"
             )

colMeans(margProducts$deriv)


# Examples:

   # compute the marginal products of the inputs (with "fitted" Output)
       margProducts <- translogDeriv( c( "qLabor", "land", "qVarInput", "time" ),
          germanFarms, coef( estResult ), vcov( estResult ) )
       margProducts$deriv
       # compute the marginal products of the inputs (with observed Output)
       margProductsObs <- translogDeriv( c( "qLabor", "land", "qVarInput", "time" ),
          germanFarms, coef( estResult ), vcov( estResult ), "qOutput" )
       margProductsObs$deriv




summary( translog.prod <- 
  translogEst( "y01", c(paste0("x", lead.zero(1:N)), "q01", "q03"),
          data=combined.df, shifterNames="region" ))
          
elasticities(translog.prod, data=as.data.frame(t(colMeans(combined.df.orig[, -ncol(combined.df)]))))

margProducts <- translogDeriv( c(paste0("x", lead.zero(1:N)), "q01", "q03"),
          as.data.frame(t(colMeans(combined.df.orig[, -ncol(combined.df)]))), 
          coef( translog.prod )[!grepl("^d", names(coef( translog.prod )))],
          vcov( translog.prod )[!grepl("^d", names(coef( translog.prod ))), 
            !grepl("^d", names(coef( translog.prod )))],
            "y01"
             )

colMeans(margProducts$deriv)

translog.prod$r2

summary(estResult <- quadFuncEst(  "y01", c(paste0("x", lead.zero(1:N)), "q01", "q03"),
          data=combined.df.orig, shifterNames="region"  ) )
          
          
 margProducts <- quadFuncDeriv( c(paste0("x", lead.zero(1:N)), "q01", "q03"),
          combined.df.orig, coef( estResult ), vcov( estResult ) )  

 margProducts <- quadFuncDeriv( xNames=c(paste0("x", lead.zero(1:N)), "q01", "q03"),
          data=as.data.frame(t(colMeans(combined.df.orig[, -ncol(combined.df)]))),
          coef=coef( estResult ), coefCov =vcov( estResult ), homWeights = NULL )  

colMeans(margProducts)


















