
library("foreign")

#install.packages("AER")
library(AER)

#install.packages("ivpack")
library(ivpack)

#install.packages("rms")
library(rms)

library(stargazer)


bang.df <- read.dta("/Users/travismcarthur/Desktop/Dev - AAE 730/Problem sets/PS 3/BGD_2009_HIEQMP_v01_M_STATA8/hh_98.dta")

table(bang.df$thanaid)
table(bang.df$villid)

# 2.a.i

bang.df$hhland.log <- log(bang.df$hhland)
bang.df$exptot.log <- log(bang.df$exptot)

# 2.a.2

par(mfcol=c(2,2))

graph.labels <- c("HH expenditure", "Log of HH expenditure", "HH land", "Log of HH land")
targ.vars <- c("exptot", "exptot.log", "hhland", "hhland.log")

for (i in 1:4) {

  plot(density(bang.df[, targ.vars[i]]), main=paste0(graph.labels[i] ), col="white") # "Kernel Density of ", 
  polygon(density(bang.df[, targ.vars[i]]), col="red", border="blue")
  
}

# 2.bi

bang.df$vill.comb.id <- bang.df$thanaid*10 + bang.df$villid

# TODO: DELETE:
#gen vill=thanaid*10+villid;

vill.prog.ag<- aggregate(cbind(dmmfd, dfmfd) ~ vill.comb.id, data=bang.df, FUN=max)

names(vill.prog.ag) <- c("vill.comb.id", "male.credit.vill", "female.credit.vill")

bang.df <- merge(bang.df, vill.prog.ag)




# 2.b.ii

summary( prog.on.exp.lm<- lm(exptot ~ female.credit.vill, data=bang.df, weight=weight ) )


summary(  prog.on.exp.w.controls.lm <- lm(
  exptot ~ female.credit.vill  + sexhead + educhead + hhland + vaccess +
  pcirr + rice + wheat + milk + egg + oil, data=bang.df, weight=weight ) )








