# global-max-search.r

# if global.max.seed!=0, then it will implement global max search



code.dir <- "/Users/travismcarthur/git/coursework/aae-637/paper/"



intended.seed <-  17

gme.obj.value.v <- vector("numeric", 100)
param.est.ls <- vector("list", 100)

for (global.max.seed in 1:100) {


source(paste0(code.dir, "max-entropy-bootstrap.r"))




# GAMS.nonlinear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytest.lst")

grab.linear.results <- FALSE

GAMS.nonlinear.results<- readLines(paste0(GAMS.projdir, ifelse(grab.linear.results, "sgmGMElinear", "sgmGMEnonlinear"), # "GMEnonlinear", 
strsplit(target.crop, " ")[[1]][1], 
   formatC(bootstrap.iter, width = 5, flag = "0"), ".lst"))
   
gme.obj.value.temp <- GAMS.nonlinear.results[grepl("[*][*][*][*] OBJECTIVE VALUE", GAMS.nonlinear.results)]
gme.obj.value.v[global.max.seed] <- as.numeric(gsub("[^0-9.]", "", gme.obj.value.temp) )


GAMS.nonlinear.results.params<- GAMS.nonlinear.results[grep("parameters to be estimated$", GAMS.nonlinear.results)]
GAMS.nonlinear.results.params <- GAMS.nonlinear.results.params[grep("VARIABLE", GAMS.nonlinear.results.params)]
GAMS.nonlinear.results.params.names <- gsub("[.]L", "", str_extract(GAMS.nonlinear.results.params, "[^ ]*[.]L") )
GAMS.nonlinear.results.params.numbers <- as.numeric(gsub("  parameters to be estimated", "",
  str_extract(GAMS.nonlinear.results.params, "[^ ]*  parameters to be estimated") ) )


param.est.ls[[global.max.seed]] <- GAMS.nonlinear.results.params.numbers



cat("\n", "\n", "\n", "\n", "\n", "WE ARE ON SEED", global.max.seed, "\n", "\n", "\n", "\n", "\n")
print(gme.obj.value.v[global.max.seed])

#source(paste0(code.dir, "nls-monte-carlo.r"))

}

summary(gme.obj.value.v[gme.obj.value.v!=0])
table(gme.obj.value.v)

do.call(cbind, param.est.ls)




