
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

pdf(file="/Users/travismcarthur/Desktop/Dev - AAE 730/Problem sets/PS 3/kernelDensity.pdf", width=5, height=5)


par(mfcol=c(2,2))

graph.labels <- c("HH expenditure", "Log of HH expenditure", "HH land", "Log of HH land")
targ.vars <- c("exptot", "exptot.log", "hhland", "hhland.log")

for (i in 1:4) {

  plot(density(bang.df[, targ.vars[i]]), main=paste0(graph.labels[i] ), col="white") # "Kernel Density of ", 
  polygon(density(bang.df[, targ.vars[i]]), col="red", border="blue")
  
}

dev.off()

# 2.b.i

bang.df$vill.comb.id <- bang.df$thanaid*10 + bang.df$villid

vill.prog.ag<- aggregate(cbind(dmmfd, dfmfd) ~ vill.comb.id, data=bang.df, FUN=max)

names(vill.prog.ag) <- c("vill.comb.id", "male.credit.vill", "female.credit.vill")

bang.df <- merge(bang.df, vill.prog.ag)

with(bang.df , {
  weighted.mean(exptot[female.credit.vill==1]  , weight[female.credit.vill==1]) - 
    weighted.mean(exptot[female.credit.vill==0]  , weight[female.credit.vill==0])
}
)

with(bang.df , {
  weighted.mean(exptot.log[female.credit.vill==1]  , weight[female.credit.vill==1]) - 
    weighted.mean(exptot.log[female.credit.vill==0]  , weight[female.credit.vill==0])
}
)




# 2.b.ii

summary( prog.on.exp.lm<- lm(exptot ~ female.credit.vill, data=bang.df, weight=weight ) )

summary( prog.on.exp.log.lm<- lm(exptot.log ~ female.credit.vill, data=bang.df, weight=weight ) )


# 2.b.iii

summary(  prog.on.exp.w.controls.lm <- lm(
  exptot ~ female.credit.vill  + sexhead + agehead + educhead + hhland + vaccess +
  pcirr + rice + wheat + milk + egg + oil, data=bang.df, weight=weight ) )

summary(  prog.on.exp.log.w.controls.lm <- lm(
  exptot.log ~ female.credit.vill  + sexhead + agehead + educhead + hhland + vaccess +
  pcirr + rice + wheat + milk + egg + oil, data=bang.df, weight=weight ) )

stargazer( prog.on.exp.lm, prog.on.exp.log.lm, prog.on.exp.w.controls.lm, prog.on.exp.log.w.controls.lm ,
  se=list(sqrt(diag(vcovHC( prog.on.exp.lm, type = "HC0"))),
    sqrt(diag(vcovHC( prog.on.exp.log.lm, type = "HC0"))),
    sqrt(diag(vcovHC( prog.on.exp.w.controls.lm, type = "HC0"))),
    sqrt(diag(vcovHC( prog.on.exp.log.w.controls.lm, type = "HC0")))
  ),
  align=TRUE, no.space=FALSE,  single.row=FALSE, font.size="small",
  notes="White-corrected standard errors are in parentheses")


# 2.b.iv


with(bang.df , {
  weighted.mean(exptot[dfmfd==1]  , weight[dfmfd==1]) - 
    weighted.mean(exptot[dfmfd==0]  , weight[dfmfd==0])
}
)

with(bang.df , {
  weighted.mean(exptot.log[dfmfd==1]  , weight[dfmfd==1]) - 
    weighted.mean(exptot.log[dfmfd==0]  , weight[dfmfd==0])
}
)


summary( partic.on.exp.lm<- lm(exptot ~ dfmfd, data=bang.df, weight=weight ) )

summary( partic.on.exp.log.lm<- lm(exptot.log ~ dfmfd, data=bang.df, weight=weight ) )

summary(  partic.on.exp.w.controls.lm <- lm(
  exptot ~ dfmfd  + sexhead + agehead + educhead + hhland + vaccess +
  pcirr + rice + wheat + milk + egg + oil, data=bang.df, weight=weight ) )

summary(  partic.on.exp.log.w.controls.lm <- lm(
  exptot.log ~ dfmfd  + sexhead + agehead + educhead + hhland + vaccess +
  pcirr + rice + wheat + milk + egg + oil, data=bang.df, weight=weight ) )

stargazer( partic.on.exp.lm, partic.on.exp.log.lm, partic.on.exp.w.controls.lm, partic.on.exp.log.w.controls.lm ,
  se=list(sqrt(diag(vcovHC( partic.on.exp.lm, type = "HC0"))),
    sqrt(diag(vcovHC( partic.on.exp.log.lm, type = "HC0"))),
    sqrt(diag(vcovHC( partic.on.exp.w.controls.lm, type = "HC0"))),
    sqrt(diag(vcovHC( partic.on.exp.log.w.controls.lm, type = "HC0")))
  ),
  align=TRUE, no.space=FALSE,  single.row=FALSE, font.size="small",
  notes="White-corrected standard errors are in parentheses")

# 2.b.v

summary( partic.on.exp.subsample.lm<- lm(exptot ~ dfmfd, data=bang.df, weight=weight, subset= female.credit.vill==1 ) )

summary( partic.on.exp.log.subsample.lm<- lm(exptot.log ~ dfmfd, data=bang.df, weight=weight, subset= female.credit.vill==1 ) )


summary(  partic.on.exp.w.controls.subsample.lm <- lm(
  exptot ~ dfmfd  + sexhead + agehead + educhead + hhland + vaccess +
  pcirr + rice + wheat + milk + egg + oil, data=bang.df, weight=weight, subset= female.credit.vill==1 ) )

summary(  partic.on.exp.log.w.controls.subsample.lm <- lm(
  exptot.log ~ dfmfd  + sexhead + agehead + educhead + hhland + vaccess +
  pcirr + rice + wheat + milk + egg + oil, data=bang.df, weight=weight, subset= female.credit.vill==1 ) )

stargazer( partic.on.exp.subsample.lm, partic.on.exp.log.subsample.lm, partic.on.exp.w.controls.subsample.lm, partic.on.exp.log.w.controls.subsample.lm ,
  se=list(sqrt(diag(vcovHC( partic.on.exp.subsample.lm, type = "HC0"))),
    sqrt(diag(vcovHC( partic.on.exp.log.subsample.lm, type = "HC0"))),
    sqrt(diag(vcovHC( partic.on.exp.w.controls.subsample.lm, type = "HC0"))),
    sqrt(diag(vcovHC( partic.on.exp.log.w.controls.subsample.lm, type = "HC0")))
  ),
  align=TRUE, no.space=FALSE,  single.row=FALSE, font.size="small",
  notes="White-corrected standard errors are in parentheses")

# 2.b.vi


summary( spillover.on.exp.lm<- lm(exptot ~ female.credit.vill, data=bang.df, weight=weight, subset= dfmfd==0 ) )

summary( spillover.on.exp.log.lm<- lm(exptot.log ~ female.credit.vill, data=bang.df, weight=weight, subset= dfmfd==0 ) )


summary(  spillover.on.exp.w.controls.lm <- lm(
  exptot ~ female.credit.vill  + sexhead + agehead + educhead + hhland + vaccess +
  pcirr + rice + wheat + milk + egg + oil, data=bang.df, weight=weight, subset= dfmfd==0 ) )

summary(  spillover.on.exp.log.w.controls.lm <- lm(
  exptot.log ~ female.credit.vill  + sexhead + agehead + educhead + hhland + vaccess +
  pcirr + rice + wheat + milk + egg + oil, data=bang.df, weight=weight, subset= dfmfd==0) )

stargazer( spillover.on.exp.lm, spillover.on.exp.log.lm, spillover.on.exp.w.controls.lm, spillover.on.exp.log.w.controls.lm ,
  se=list(sqrt(diag(vcovHC( spillover.on.exp.lm, type = "HC0"))),
    sqrt(diag(vcovHC( spillover.on.exp.log.lm, type = "HC0"))),
    sqrt(diag(vcovHC( spillover.on.exp.w.controls.lm, type = "HC0"))),
    sqrt(diag(vcovHC( spillover.on.exp.log.w.controls.lm, type = "HC0")))
  ),
  align=TRUE, no.space=FALSE,  single.row=FALSE, font.size="small",
  notes="White-corrected standard errors are in parentheses")



