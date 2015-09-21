
# THESE ARE IMPORTANT PARAMS IMMEDIATELY BELOW:
mfx.on.posi.median <- TRUE
mean.of.MP <- FALSE
# NOTE, IMPORTANT: It seems that under a quadratic specification, the mean of the marginal
# products is equal to the marginal product of the mean, so the above is somewhat redundant.
# The elasticities differ, however.


local.source.evaluation <- FALSE
dropped.cost.share.eq <- 10
# anything >6 means that no equation gets dropped


saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/GAMS work/saved workspace.Rdata"

saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF.Rdata"

saved.workspace.path <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Rdata results files/saved workspace only inputsDF with soil and rain.Rdata"
# with soil and rain and elevation

saved.workspace.path <- "/Users/travismcarthur/Desktop/Bolivia project/Data/saved workspace only inputsDF with soil and rain and advanced drive time fixed.Rdata"


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
nobs <- list()
MP.var.ls <- list()


M <- 1
N <- 6
# J <- 3
 J <- 6

price.trim.quantile <- 0.99
demand.var.trim.quantile <- 0.95
functional.form <- "SGM"
#demand.var.trim.quantile <- 1

  intended.seed <- 100 
  start.nonlin.from.ignorance <- FALSE
#  start.nonlin.from.ignorance <- TRUE
  global.max.seed <- 4
  do.SUR <- TRUE
  include.cost.fn <- TRUE
  only.cost.fn <- FALSE
  generate.synth.data.from.cost.fn <- FALSE
  start.at.true.xi <- FALSE
 synthetic.data <-FALSE
if (!exists("global.max.seed")) { global.max.seed <- 0}


do.yield <- TRUE

aggregate.regions <- TRUE







#  target.top.crop.number <- 1

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


# Do this section above just so we can extract the regions where the crops are planted

unique.regions <- as.character( unique(region) )

if (aggregate.regions) {
  unique.regions[grepl("ALTIPLANO", unique.regions)] <- "ALTIPLANO"
  unique.regions[grepl("VALLES", unique.regions)] <- "VALLES"
  unique.regions <- unique(unique.regions)
}

region.list <- as.list( unique.regions )

names(region.list) <- unique.regions




region.list <- c(list(ALL.REGIONS=unique.regions), region.list)


for ( target.region in names(region.list) ) {


load(saved.workspace.path)

log.plus.one.cost <- FALSE

bootstrap.iter <- 1
# NOTE: Bootstrap iter = 0 means actual estimate
bootstrap.selection.v <- TRUE
source(paste0(code.dir, "build-model-extract-parcels.r"))



if (aggregate.regions) {
  region <- as.character( region )
  region[grepl("ALTIPLANO", region)] <- "ALTIPLANO"
  region[grepl("VALLES", region)] <- "VALLES"
}


combined.df <- data.frame(mget(c("y01", paste0("x", lead.zero(1:N)), 
  paste0("w", lead.zero(1:N)),  paste0("q", lead.zero(1:J)) )))
  
combined.df <- cbind(combined.df, data.frame(region) )
  
if (do.yield) {
  combined.df$yield <- combined.df$y01/combined.df$q01
  combined.df$x01 <- combined.df$x01/combined.df$q01
  combined.df$x02 <- combined.df$x02/combined.df$q01
  combined.df$x03 <- combined.df$x03/combined.df$q01
  combined.df$x04 <- combined.df$x04/combined.df$q01
  combined.df$x05 <- combined.df$x05/combined.df$q01
  combined.df$x06 <- combined.df$x06/combined.df$q01
  combined.df$q03 <- combined.df$q03/combined.df$q01
# Oh well; I'm hardcoding the numbers in 

  combined.df <- combined.df[combined.df$yield < quantile(combined.df$yield, .99), ]
  # Cutting out strange observations

}
  
#region.matrix.df <-   as.data.frame(region.matrix)
#colnames(region.matrix.df) <- iconv(colnames(region.matrix.df), to="ASCII//TRANSLIT")
#colnames(region.matrix.df) <- gsub("'", "", colnames(region.matrix.df) )
#colnames(region.matrix.df) <- gsub("[.]", "", colnames(region.matrix.df) )
  
#combined.df <- cbind(combined.df, region.matrix.df)
#combined.df <- cbind(combined.df, region)

if (functional.form!="SGM") {
  combined.df$y01 <- exp(combined.df$y01)
  # need to convert back to original
  combined.df.orig.quad$q02 <- log(combined.df.orig.quad$q02)
  # To change irrigation back to zero-one
}


combined.df <- combined.df[combined.df$region %in% region.list[[target.region]], ]

if (nrow(combined.df) < 78 ) { next }
# The number of params in the quadratic production function is 78, so this avoids the 
# Problem of negative degrees of freedom

combined.df.orig <- combined.df
combined.df.orig.quad <- combined.df.orig


#for ( i in paste0("x", lead.zero(1:N))) {
#  combined.df[, i] <- combined.df[, i] + min(combined.df[combined.df[, i]!=0, i])
#}
# S0 no zeros when we take logs. NOTE: Not sure why this was done earlier

# NOTE: BElow, I'm taking out the region dummies
#region.matrix.df <-   as.data.frame(region.matrix)
#colnames(region.matrix.df) <- iconv(colnames(region.matrix.df), to="ASCII//TRANSLIT")
#colnames(region.matrix.df) <- gsub("'", "", colnames(region.matrix.df) )
#colnames(region.matrix.df) <- gsub("[.]", "", colnames(region.matrix.df) )
  
#combined.df.orig.quad <- cbind(combined.df.orig.quad, region.matrix.df)



# install.packages("micEcon")
library("micEcon")

# paste0("q", lead.zero(1:J)))
# c(paste0("x", lead.zero(1:N)), "q01", "q03")
# Take out irrigation for now




boot.replications <- 100

# as.data.frame(t(apply(combined.df.orig.quad, 2, function(x) median(x[x>0]) )))

set.seed(100)


bootstraps <- apply(matrix(
    sample(1:nrow(combined.df.orig.quad), 
    size = boot.replications*nrow(combined.df.orig.quad), replace=TRUE), 
    nrow=boot.replications),
    1, FUN=function(x) {
    
    data.df <- combined.df.orig.quad[x, ]
    data.df <- data.df[, colnames(data.df)!="region"]
    
    if ( do.yield) {
      estResult <- quadFuncEst(  "yield", 
        c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(2:J))),
           data=data.df)
    } else {
      estResult <- quadFuncEst(  "y01", 
        c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
            data=data.df) #, shifterNames=colnames(region.matrix.df) )  # NOTE: TAKING out region
    } 
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
      if ( do.yield) {
        margProducts <- colMeans(quadFuncDeriv( 
          c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(2:J))),
            data.df, coef( estResult ) ), na.rm=TRUE)
        } else {
          margProducts <- colMeans(quadFuncDeriv( 
            c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
              data.df, coef( estResult ) ), na.rm=TRUE)
        }
    } else {
      if ( do.yield) {
      margProducts <- quadFuncDeriv( 
      c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(2:J))),
          combined.mean.temp, coef( estResult ) ) 
      } else {
        margProducts <- colMeans(quadFuncDeriv( 
          c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
            data.df, coef( estResult ) ), na.rm=TRUE)
      }
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
    
    data.df <- data.df[, colnames(data.df)!="region"]
    
    if( do.yield) {
      estResult <- quadFuncEst(  "yield", 
        c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(2:J))),
           data=data.df)
    } else {
      estResult <- quadFuncEst(  "y01", 
        c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
            data=data.df) #, shifterNames=colnames(region.matrix.df) )  # NOTE: TAKING out region
    } 
          
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
      if ( do.yield) {
        margProducts <- colMeans(quadFuncDeriv( 
          c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(2:J))),
            data.df, coef( estResult ) ), na.rm=TRUE)
        } else {
          margProducts <- colMeans(quadFuncDeriv( 
            c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
              data.df, coef( estResult ) ), na.rm=TRUE)
        }
    } else {
      if ( do.yield) {
      margProducts <- quadFuncDeriv( 
      c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(2:J))),
          combined.mean.temp, coef( estResult ) ) 
      } else {
        margProducts <- colMeans(quadFuncDeriv( 
          c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
            data.df, coef( estResult ) ), na.rm=TRUE)
      }
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
MP.var.ls[[target.crop]][[target.region]] <- apply(bootstraps, 1, FUN=var, na.rm=TRUE)

input.desc <- c("Inorganic Fert", "Purchased Seeds", "Tractor Hrs", "Plaguicidas", "Hired Labor", "Organic Fert", "Land", "Irrigation", "Family Labor", "Soil", "Elevation", "Rainfall")


marg.prod.row.names <- rep(input.desc, 2)

marg.prod.measure <- c(rep("Marginal Prod", length(input.desc)), rep("Output elasticity", length(input.desc)))

if (do.yield) {
  input.units <- c("kg/ha", "kg/ha", "hours/ha", "kg/ha", "hours/ha", "kg/ha", "binary", "num persons/ha", "index", "km", "decimeters", 
    rep("", length(input.desc)-1))
} else{
  input.units <- c("kg", "kg", "hours", "kg", "hours", "kg", "hectares", "binary", "num persons", "index", "km", "decimeters", 
    rep("", length(input.desc)))
}




if (mfx.on.posi.median ) {
   data.means.v <- c(apply(combined.df.orig.quad[, c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J)))], 2, function(x) median(x[x>0], na.rm=TRUE)), rep(NA, length(input.desc)))
   # Careful: This sets all regions dummies to 1, but this does not affect our
   # marginal effects calculation here    
} else {
  data.means.v <- c(colMeans(combined.df.orig.quad[, c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J)))] ), rep(NA, length(input.desc)))
}


bootstrapped.results.df <- data.frame(Measure=marg.prod.measure, Input.Name=marg.prod.row.names , stringsAsFactors=FALSE) 

if (do.yield) {
  bootstrapped.results.df <- bootstrapped.results.df[!grepl("Land", bootstrapped.results.df$Input.Name), ]
  data.means.v <- data.means.v[input.desc!="Land"] 
  # Caution: We are using the vector recycling feature here
}

bootstrapped.results.df$Point.Estimate <- point.est
bootstrapped.results.df$Units <- input.units
bootstrapped.results.df$Lower.90.CI <- CI.mat[, 1]
bootstrapped.results.df$Upper.90.CI <- CI.mat[, 2]
bootstrapped.results.df$Data.mean <- data.means.v 
if (mfx.on.posi.median) {
  colnames(bootstrapped.results.df)[colnames(bootstrapped.results.df)=="Data.mean"] <- "Data.median|x>0"
}

bootstrapped.results.df$Input.Name <- with(bootstrapped.results.df,
 ifelse((Lower.90.CI > 0 & Upper.90.CI > 0) | (Lower.90.CI < 0 & Upper.90.CI < 0), 
  paste0(Input.Name, "*"), Input.Name)
)



bootstrapped.marg.results.ls[[target.crop]][[target.region]] <- bootstrapped.results.df

if (do.yield) {
  r.sq.list[[target.crop]][[target.region]]<- quadFuncEst(  "yield", 
        c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(2:J))),
            data=combined.df.orig.quad )$r2bar #, shifterNames=colnames(region.matrix.df) )$r2bar # TAKING OUT REGION
} else {
  r.sq.list[[target.crop]][[target.region]]<- quadFuncEst(  "y01", 
        c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
            data=combined.df.orig.quad )$r2bar
}


nobs[[target.crop]][[target.region]] <- nrow(combined.df.orig.quad)

cat(target.top.crop.number, "\n")

}

}


# END ESTIMATION











library("stargazer")
library("xtable")



for ( target.crop.number in 1:length(bootstrapped.marg.results.ls) ) {

for (target.region in names(bootstrapped.marg.results.ls[[target.crop.number]]) ) {

#for ( i in 1:length(bootstrapped.marg.results.ls) ) {

  crop.english<- c("Potatoes", "Maize", "Barley", "Wheat", "Fava Beans")

#  test <- stargazer(bootstrapped.marg.results.ls[[i]], 
#  title=paste0("Marginal Product and Output Elasticities for ", crop.english[i], 
#  ", $R^2$ for model is ", round(r.sq.list[[i]], digits=3)),
#  summary=FALSE, rownames=FALSE, align=TRUE, no.space=TRUE,
#  out = paste0("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Marginal products/table", i, ".tex" ))
  
  # TODO: Need to change table titles according to mode
  
  xtab.output <- print(xtable(bootstrapped.marg.results.ls[[target.crop.number]][[target.region]], 
    caption=paste0("Marginal Product and Output Elasticities for ", crop.english[target.crop.number], " in ",
    target.region,
    "; $R^2$ for model is ", round(r.sq.list[[target.crop.number]][[target.region]], digits=3), 
    "; N = ", nobs[[target.crop.number]][[target.region]] )),
    hline.after=0:nrow(bootstrapped.marg.results.ls[[target.crop.number]][[target.region]]),
    caption.placement="top")
    
#  xtab.output <- print(xtable(bootstrapped.marg.results.ls[[target.crop.number]][[target.region]],
#    caption=paste0("Marginal Product and Output Elasticities for ", crop.english[target.crop.number], " in ",
#    target.region)),
#    hline.after=0:nrow(bootstrapped.marg.results.ls[[target.crop.number]][[target.region]]),
#    caption.placement="top") 
  
  xtab.output <- paste(xtab.output, "\\vspace{2em}")
  if (mfx.on.posi.median) {
     if ( do.yield) {
  	    cat(xtab.output, sep="\n",
          file=paste0("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Marginal products/NEWyieldtableatmedian_", crop.english[target.crop.number], "_", target.region, ".tex" ))
      } else {
        cat(xtab.output, sep="\n",
          file=paste0("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Marginal products/NEWtableatmedian_", 
          crop.english[target.crop.number], "_", target.region, ".tex" ))
      }
  } else {
  	if (mean.of.MP) {
      cat(xtab.output, sep="\n",
        file=paste0("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Marginal products/NEWtablemeanofMP_", 
        crop.english[target.crop.number], "_", target.region, ".tex" ))  	
  	} else {
  	  if ( do.yield) {
  	    cat(xtab.output, sep="\n",
          file=paste0("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Marginal products/NEWyieldtable_",
           crop.english[target.crop.number], "_", target.region, ".tex" ))
      } else {
        cat(xtab.output, sep="\n",
          file=paste0("/Users/travismcarthur/Desktop/Metrics (637)/Final paper/Marginal products/NEWtable_", 
          crop.english[target.crop.number], "_", target.region, ".tex" ))
      }
  	}
  }
  # thanks to http://stackoverflow.com/questions/7160754/adding-a-horizontal-line-between-the-rows-in-a-latex-table-using-r-xtable?rq=1
  # ABove is a total mess of "if else"
  

}

}

# END CODE

#install.packages("aod")
library("aod")



wald.result.ls <- list()


for ( target.crop.number in 1:length(bootstrapped.marg.results.ls) ) {


  region.combns <- t(  combn(names( bootstrapped.marg.results.ls[[target.crop.number]][-1] ), 2) )
  # Taking [-1] since all.regions is the first element
  
  for ( target.pair in 1:nrow(region.combns) ) {
  
    first.region  <- region.combns[ target.pair, 1]
    second.region <- region.combns[ target.pair, 2]
    
    first.point <- bootstrapped.marg.results.ls[[target.crop.number]][[  first.region  ]]$Point.Estimate["margProduct.x01",]
    second.point <- bootstrapped.marg.results.ls[[target.crop.number]][[  second.region ]]$Point.Estimate["margProduct.x01",]

    first.var <- MP.var.ls[[target.crop.number]][[  first.region  ]]["margProduct.x01"]
    second.var <- MP.var.ls[[target.crop.number]][[  second.region  ]]["margProduct.x01"]    
    
    
    mp.sigma <- matrix(c(first.var, 0, 0, second.var), ncol=2)
    # Note that the covariance is zero since these region-specific samples are independent
    
    wald.result.ls[[as.character(target.crop.number)]][[target.pair]] <- 
      wald.test(mp.sigma, c( first.point, second.point), L=matrix(c(1, -1), ncol=2))$result$chi2["P"]
    


  }

}




wald.test(mp.sigma, c( first.point, second.point), L=diag(2))













wald.test(Sigma, b)








# Ok, I have 3 different flavors of iterators here, which is quite dangerous:
# target.crop.number
# target.top.crop.number
# target.crop
# That could lead to big trouble































estResult

#combined.df.orig.quad$yield <- combined.df.orig.quad$y01 / combined.df.orig.quad$q01

    estResult <- quadFuncEst(  "yield", 
      c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(2:J))),
          data=combined.df.orig.quad) # , shifterNames=colnames(region.matrix.df) ) # NOTE: taking out region
          
    estResult$coef[is.na(estResult$coef)] <- 0


replacement.cov <- matrix(0, nrow=1, ncol=ncol(estResult$coefCov))
rownames(replacement.cov) <- "b_7_7"

cov.rownames <- rownames(estResult$coefCov)
cov.targ.ind <- which(cov.rownames== "b_6_11")

estResult$coefCov <- rbind(estResult$coefCov[1:cov.targ.ind,], 
  replacement.cov, 
  estResult$coefCov[(cov.targ.ind+1):length(cov.rownames),] )

replacement.cov <- matrix(0, nrow=nrow(estResult$coefCov), ncol=1)

colnames(replacement.cov) <- "b_7_7"

estResult$coefCov <- cbind(estResult$coefCov[, 1:cov.targ.ind], 
  replacement.cov, 
  estResult$coefCov[, (cov.targ.ind+1):length(cov.rownames)] )




test <- quadFuncDeriv( 
      c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(2:J))),
          as.data.frame(t(colMeans(combined.df.orig.quad))), coef( estResult ), coefCov=vcov( estResult ) ) 



plot(quadFuncDeriv( 
      c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(2:J))),
          cbind(x01=0:100, combined.mean.temp[, -2]), coef( estResult ) )$x01 #, vcov( estResult ) ) 
)



library("effects")

quad.est <- quadFuncEst(  "y01", 
      c(paste0("x", lead.zero(1:N)), paste0("q", lead.zero(1:J))),
          data=combined.df.orig.quad )
          
quad.est <- lm(y01 ~ (x01 + x02 + x03 + x04 + x05 + x06 + 
  q01 + q02 + q03 + q04 + q05 + q06)^2 + I(x01^2) + I(x02^2) + I(x03^2) + I(x04^2) + I(x05^2) +
  I(x06^2) + I(q01^2)  + I(q03^2) + I(q04^2) + I(q05^2) + I(q06^2), # + I(q02^2) Since irrigation
  data=combined.df)

quad.est <- lm(y01 ~ (x01 + x02 + x03 + x04 + x05 + x06 + 
  q01 + q02 + q03 + q04 + q05 + q06)^2 + poly(x01,2) + poly(x02, 2) + poly(x03,2) + poly(x04,2) + poly(x05,2) +
  poly(x06,2) + poly(q01,2)  + poly(q03,2) + poly(q04,2) + poly(q05,2) + poly(q06,2), # + I(q02^2) Since irrigation
  data=combined.df)


quad.est <- lm(y01 ~ x01 + x02 + x03 + x04 + x05 + x06 + 
  q01 + q02 + q03 + q04 + q05 + q06, # + I(q02^2) Since irrigation
  data=combined.df)

summary(quad.est)

plot(effect(c("x01"), quad.est, xlevels=list(x01=seq(0, quantile(x01, .95), by=1)),
  given.values=colMeans(combined.df[, names(combined.df) %in% names(coef(quad.est))])), ask = FALSE, rescale.axis = FALSE)
     
plot(Effect(c("x01"), quad.est, xlevels=list(x01=seq(0, quantile(x01, .95), by=1))), ask = FALSE, rescale.axis = FALSE)

plot(allEffects( quad.est))
, "x01:x02"
"I(x01^2)"


price.lm <- lm( x01 ~ (w01 + w02 + w03 + w04 + w05 + w06 + y01 + I(y01^2))^2 + 
  I(w01^2) + I(w02^2) + I(w03^2) + I(w04^2) + I(w05^2) + I(w06^2) +
  (q01 + q02 + q03)^2  , subset=x01>0   
   )
summary(price.lm)

plot(Effect("w01", price.lm), ask = FALSE, rescale.axis = FALSE)






  
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


















