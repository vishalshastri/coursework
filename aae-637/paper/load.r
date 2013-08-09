
# PACKAGE LOADING

# Must also put in livestock.wide.df due to way formula in logit is

if (load.files) {
  load(file=paste0(work.dir, "crop wide df2.Rdata"))
  load(file=paste0(work.dir, "prod01.df imputed prices.Rdata"))
}


#install.packages("foreign")
library("foreign")
#install.packages("VGAM")
library("VGAM")
#install.packages("AER")
library("AER")
#install.packages("MASS")
library("MASS")
#install.packages("censReg")
library("censReg")
#install.packages("knitr")
library("knitr")
#install.packages("erer")
library("erer")
#install.packages("maptools")
library("maptools")
#install.packages("splancs")
library("splancs")
#install.packages("PBSmapping")
library("PBSmapping")
#install.packages("spdep")
library("spdep")
#install.packages("spatial")
library("spatial")
#install.packages("gstat")
library("gstat")
#install.packages("rgdal")
library("rgdal")
#install.packages("Hmisc")
library("Hmisc")
#install.packages("pgirmess")
library("pgirmess")
#install.packages("rjson")
library("rjson")
#install.packages("httr")
library("httr")
#install.packages("VIM")
library("VIM")
#install.packages("osmar")
library("osmar")
#install.packages("spatstat")
library("spatstat")
#install.packages("relaimpo")
library("relaimpo")
#install.packages("car")
library("car")
#install.packages("bbmle")
library("bbmle")




