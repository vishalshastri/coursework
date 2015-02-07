#synthetic-monte-carlo


code.dir <- "/Users/travismcarthur/git/coursework/aae-637/paper/"

global.max.seed <- 0

all.param.corr.ls <- vector("list", 100)
xi.param.corr.ls <- vector("list", 100)
percent.lhs.posi <- vector("numeric", 100)

nls.all.param.corr.ls <- vector("list", 100)
nls.xi.param.corr.ls <- vector("list", 100)


for ( intended.seed in 1:100) {

source(paste0(code.dir, "max-entropy-bootstrap.r"))




# GAMS.nonlinear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytest.lst")

grab.linear.results <- FALSE

GAMS.nonlinear.results<- readLines(paste0(GAMS.projdir, ifelse(grab.linear.results, "sgmGMElinear", "sgmGMEnonlinear"), # "GMEnonlinear", 
strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".lst"))
   
gme.obj.value<- GAMS.nonlinear.results[grepl("[*][*][*][*] OBJECTIVE VALUE", GAMS.nonlinear.results)]
gsub("[^0-9.]", "", gme.obj.value)

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

cor.test(param.check.df$est.params[grepl("xi", param.check.df$param.names)] , 
  param.check.df$synth.params[grepl("xi", param.check.df$param.names)])
  
all.param.corr.ls[[intended.seed]] <- cor(param.check.df$est.params , param.check.df$synth.params, method="kendall", use="pairwise") 
#xi.param.corr.ls[[intended.seed]] <- cor(param.check.df$est.params[grepl("xi", param.check.df$param.names)] , 
#  param.check.df$synth.params[grepl("xi", param.check.df$param.names)])

cat("\n", "\n", "\n", "\n", "\n", "WE ARE ON SEED", intended.seed, "\n", "\n", "\n", "\n", "\n")

#source(paste0(code.dir, "nls-monte-carlo.r"))

xi.param.corr.ls[[intended.seed]] <- cor(param.check.df$est.params[grepl("xi", param.check.df$param.names)] , 
  param.check.df$synth.params[grepl("xi", param.check.df$param.names)], method="kendall", use="pairwise") 



print(summary(unlist(all.param.corr.ls)))
print(summary(unlist(xi.param.corr.ls)))
print(summary(unlist(nls.all.param.corr.ls)))
print(summary(unlist(nls.xi.param.corr.ls)))
percent.lhs.posi[intended.seed] <- mean(E.y01.data>0)
print(mean(percent.lhs.posi[1:intended.seed]))

}


summary(unlist(all.param.corr.ls))
summary(unlist(xi.param.corr.ls))


all.param.corr.ls.save <- all.param.corr.ls
xi.param.corr.ls.save <- xi.param.corr.ls

# GME does poorly on seed 21



