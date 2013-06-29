# GEOGRAPHIC MARKET INTEGRATION EXPLORATION



top.crops<-names(rev(sort(table(prod01.df$crop))))[1:12]


by(prod01.df[prod01.df$crop %in% top.crops, ], 
	INDICES=factor(prod01.df$crop[prod01.df$crop %in% top.crops]),
	FUN = function(x) {
		par(mfrow=c(1,2))
		plot(x$sales.value, x$sales.quant.r, 
		main=paste(x$crop[1], "cor=", round(cor(x$sales.value, x$sales.quant.r, use="pairwise.complete.obs"), 2)) )
		plot(log(x$sales.value), log(x$sales.quant.r), 
		main=paste(x$crop[1], "(log) cor=", round(cor(log(x$sales.value), log(x$sales.quant.r), use="pairwise.complete.obs"), 2)) )
		par(mfrow=c(1,1))
		NULL
	}
)


price.cor<-by(prod01.df[prod01.df$crop %in% top.crops, ], 
	INDICES=factor(prod01.df$crop[prod01.df$crop %in% top.crops]),
	FUN = function(x) {
		ret<-c(0,0)
		names(ret)<-c("cor", "log.cor")
		ret[1]<-cor(x$sales.value, x$sales.quant.r, use="pairwise.complete.obs")
		ret[2]<-cor(log(x$sales.value), log(x$sales.quant.r), use="pairwise.complete.obs")
		ret
	}
)

price.cor<-as.data.frame(do.call(rbind, price.cor))

round(price.cor[order(price.cor$cor), ], 2)
round(price.cor[order(price.cor$log.cor), ], 2)

# Yield

yield.ls<-by(prod01.df[prod01.df$crop %in% top.crops, ], 
	INDICES=factor(prod01.df$crop[prod01.df$crop %in% top.crops]),
	FUN = function(x) {
		x$harvest.r/x$area.r
	}
)

lapply(yield.ls, FUN=function(x) print(summary(x)))

lapply(yield.ls, FUN=function(x) boxplot(x, main=x$crop[1]))

by(prod01.df[prod01.df$crop %in% top.crops, ], 
	INDICES=factor(prod01.df$crop[prod01.df$crop %in% top.crops]),
	FUN = function(x) {
		boxplot(x$harvest.r/x$area.r, main=x$crop[1])
	}
)

by(prod01.df[prod01.df$crop %in% top.crops, ], 
	INDICES=factor(prod01.df$crop[prod01.df$crop %in% top.crops]),
	FUN = function(x) {
		quantile(x$area.r, probs=c(.025, .975), na.rm=TRUE)
	}
)

by(prod01.df[prod01.df$crop %in% top.crops, ], 
	INDICES=factor(prod01.df$crop[prod01.df$crop %in% top.crops]),
	FUN = function(x) {
		ret<-quantile(x$area.r, probs=c(.025, .975), na.rm=TRUE)
		ret[2]/ret[1]
	}
)

by(prod01.df[prod01.df$crop %in% top.crops, ], 
	INDICES=factor(prod01.df$crop[prod01.df$crop %in% top.crops]),
	FUN = function(x) {
		crop.quantiles<-quantile(x$area.r, probs=c(.05, .95), na.rm=TRUE)
		yield.quantiles<-quantile(x$harvest.r/x$area.r, probs=c(.05, .95), na.rm=TRUE)
		print(yield.quantiles)
		yield<-x$harvest.r/x$area.r
		keep.index<-x$area.r>crop.quantiles[1]*10000 & !is.na(x$area.r) & 
			(yield*10000>yield.quantiles[1] & yield*10000<yield.quantiles[2])
		if (all(!keep.index)) return(NULL)
		data.frame(a=x$area.r[keep.index], 
			b=yield[keep.index]*10000, 
			c=median(x$harvest.r/x$area.r, na.rm=TRUE))
	}
)




false situations:
High yield, low area: big problem; can deal with
High yield, high area
Low yield, low area
Low yield, high area

We would be off by a factor of 10,000. take like 5% and 95% percentiles and blow up and shrink by 10,000

