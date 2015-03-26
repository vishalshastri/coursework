results.ls <- extract.gams.est()
gc()

demand.curve.slope(1, params=results.ls$params)
demand.curve.slope(2, params=results.ls$params)
demand.curve.slope(3, params=results.ls$params)
demand.curve.slope(4, params=results.ls$params)
demand.curve.slope(5, params=results.ls$params)
demand.curve.slope(6, params=results.ls$params)


cost.elast.output(eval.point=median(combined.df$y01),  params=results.ls$params)

for ( targ.quantile in seq(.1, .9, by=.1)) {
  print(
    cost.elast.output(eval.point=quantile(combined.df$y01, probs=targ.quantile),  params=results.ls$params)
  )
}

for ( targ.quantile in seq(.1, .9, by=.1)) {
  print(
    cost.elast.output(eval.point=quantile(combined.df$y01, probs=targ.quantile),  
      params=results.ls$params,
      data=apply(combined.df, 2, FUN=quantile, probs=targ.quantile))
  )
}


for ( targ.quantile in seq(.1, .9, by=.1)) {
  print(
    cost.elast.output(eval.point=quantile(combined.df$y01, probs=targ.quantile),  
      params=results.ls$params,
      data=combined.df)
  )
}


all.obs.cost.elast<- cost.elast.output.all.obs(params=results.ls$params)
summary(all.obs.cost.elast[is.finite(all.obs.cost.elast)])
quantile(all.obs.cost.elast[is.finite(all.obs.cost.elast)], probs=seq(0, 1, by=.1))

# Many observations have zero cost
#TOOD should we use predicted cost or actual cost? Probbaly predicted. we are using actual cost now.


