

#START NEW DATA

library("foreign")

work.dir <- "/Users/travismcarthur/Desktop/Metrics (637)/Final paper/"



options(encoding = "CP850")
prod01.df<-read.spss(paste0(work.dir, "bd18 (2001).zip Folder/agricola.sav"), to.data.frame = TRUE, reencode="CP850")
prod01.df[, 3]<-gsub("(^[[:space:]]+)|([[:space:]]+$)", "", prod01.df[, 3])
#write.csv(rev(sort(table(prod01.df[, 3]))), file=paste0(work.dir, "unclean cropnames.csv"), fileEncoding="CP850")


hogar01.df<-read.spss(paste0(work.dir, "bd18 (2001).zip Folder/hogar.sav"), to.data.frame = TRUE, reencode="CP850")






colnames(prod01.df)<-c("FOLIO", "CODA", "crop", "crop.code", "area", "area.unit", "harvest", "harvest.unit", "sales.quant", "sales.value", "consumption", "seeds", "animal.or.subproducts", "bartered", "lost")

prod01.df<-within(prod01.df, {
	# TODO: fix the is.na after we have imputed the values
  area.r<-0
	area.r[area.unit==2 & !is.na(area.unit)] <- area[area.unit==2 & !is.na(area.unit)]
	area.r[area.unit==1 & !is.na(area.unit)] <- area[area.unit==1 & !is.na(area.unit)] / 10000
} )
	
quant.to.conv<-c("harvest", "sales.quant", "consumption", "seeds", "animal.or.subproducts", "bartered", "lost")
for (i in quant.to.conv) {
	prod01.df[, paste0(i, ".r")]<-0
	prod01.df[prod01.df$harvest.unit=="Arroba" & !is.na(prod01.df$harvest.unit), paste0(i, ".r")] <- 
		prod01.df[prod01.df$harvest.unit=="Arroba" & !is.na(prod01.df$harvest.unit), i]*11.5
	prod01.df[prod01.df$harvest.unit=="Quintal" & !is.na(prod01.df$harvest.unit), paste0(i, ".r")] <- 
		prod01.df[prod01.df$harvest.unit=="Quintal" & !is.na(prod01.df$harvest.unit), i]*46
	prod01.df[prod01.df$harvest.unit=="Libra" & !is.na(prod01.df$harvest.unit), paste0(i, ".r")] <- 
		prod01.df[prod01.df$harvest.unit=="Libra" & !is.na(prod01.df$harvest.unit), i]*0.453592
	prod01.df[prod01.df$harvest.unit=="Unidad" & !is.na(prod01.df$harvest.unit), paste0(i, ".r")] <- 
		prod01.df[prod01.df$harvest.unit=="Unidad" & !is.na(prod01.df$harvest.unit), i]
		prod01.df[prod01.df$harvest.unit=="Kilogramo" & !is.na(prod01.df$harvest.unit), paste0(i, ".r")] <- 
		prod01.df[prod01.df$harvest.unit=="Kilogramo" & !is.na(prod01.df$harvest.unit), i]
}




cropname.corrections<-as.matrix( read.csv(paste0(work.dir, "unclean cropnames01 corrections.csv"), header=FALSE, stringsAsFactors=FALSE, encoding = "UTF-8") )

# MAKE SURE TO ENCODE THE csv in EXCEL AS csv (MS-DOS)

prod01.df$crop.name.cleaned<-FALSE

prod01.df$crop.r<-prod01.df$crop

for (i in 1:nrow(cropname.corrections)) {
  if ( (sum(cropname.corrections[i, ]=="")+1)==ncol(cropname.corrections)) { next }
  clean.index<-prod01.df$crop.r %in% cropname.corrections[i, -1]
  prod01.df$crop.r[clean.index] <- cropname.corrections[i, 1]
  prod01.df$crop.name.cleaned[clean.index] <- TRUE
}
  
prod01.df$crop.r <- as.factor(prod01.df$crop.r)





prod.geog.df<-hogar01.df[, c("FOLIO", "ID01", "ID02", "ID03", "ID04", "ID05")]
names(prod.geog.df)<-c("FOLIO", "department", "province", "seccion", "canton", "village")
prod.geog.df$village<-do.call(paste0, prod.geog.df[, c("department", "province", "seccion", "canton", "village")])
prod.geog.df$canton<-do.call(paste0, prod.geog.df[, c("department", "province", "seccion", "canton")])
prod.geog.df$seccion<-do.call(paste0, prod.geog.df[, c("department", "province", "seccion")])
prod.geog.df$province<-do.call(paste0, prod.geog.df[, c("department",  "province")])
prod.geog.df$nation<-1


prod01.df<-merge(prod01.df, prod.geog.df, all.x=TRUE)



# TODO: Make sure that prices are matched by units or weight
# TODO: Make sure the three-HH rule is not overcome by imputation as we're going along


prod01.df$total.value<-0
prod01.df$impute.level<-""
prod01.df$price<-0

impute.levels<-c("household", "village", "canton", "seccion", "province", "department", "nation")

for (i in 1:nrow(prod01.df)) {

for (impute.level in impute.levels) {
  
  if (impute.level=="household" & !is.na(prod01.df$sales.value[i]) & prod01.df$sales.quant.r[i]!=0) {
    prod01.df$total.value[i] <- 
      (prod01.df$harvest.r[i]-prod01.df$lost.r[i]) * prod01.df$sales.value[i]/prod01.df$sales.quant.r[i]
    prod01.df$price[i] <- prod01.df$sales.value[i]/prod01.df$sales.quant.r[i]
    prod01.df$impute.level[i]<-impute.level
    prod01.df$impute.sample.size[i]<-0
    break
  }
  if (impute.level=="household") {next}
  
  #target.crop<-prod01.df$crop[i]
  match.index<-prod01.df[, impute.level] == prod01.df[i, impute.level] & 
    prod01.df$crop == prod01.df$crop[i] & !is.na(prod01.df$sales.value) & prod01.df$sales.quant.r!=0
  
  impute.sample.size <- sum(match.index)
  
  if (impute.sample.size>=3) {
    prod01.df$total.value[i] <- 
      (prod01.df$harvest.r[i]-prod01.df$lost.r[i]) * 
        median(prod01.df$sales.value[match.index]/prod01.df$sales.quant.r[match.index])
    prod01.df$price[i] <- median(prod01.df$sales.value[match.index]/prod01.df$sales.quant.r[match.index])
    prod01.df$impute.level[i]<-impute.level
    prod01.df$impute.sample.size[i] <- impute.sample.size
    break
  }
  
}
  
}























prod01.df$crop.hh.id<-1:nrow(prod01.df)

top.crops<-names(rev(sort(table(prod01.df$crop.r))))[1:12]

# plot(rev(sort(table(prod01.df$crop.r)))[1:20], ylim=c(0, 1600))

farm.types.ls <- by(prod01.df$crop.r, INDICES=prod01.df$FOLIO, FUN=function(x) sort(as.character(x)))

length(unique( farm.types.ls))

#There are at least that many farms that we would not have full coverage for 

#sort(table( sapply(farm.types.ls, FUN=paste, collapse=" " )))

#install.packages("sets")
library("sets")

#<- as.integer(as.numeric(prod01.df$crop.r))

#s <- set(1L, 2L, 3L)

#test.set <-  2 ^ s

#rep(s, 2)

#s2 <- set(rep(list(1:3), length(test.set)))

#gset_contains_element(test.set , s)

#attributes(test.set) <- NULL

#set_is_subset(test.set, rep(s, each=length(test.set)))

#set_is_subset(test.set, list(s, s))








largest.set <- 15



crop.integers <- as.integer(prod01.df$crop.r[
  prod01.df$crop.r %in% names(rev(sort(table(prod01.df$crop.r))))[1:largest.set] 
  ] )

#farm.types.int.ls <- by(prod01.df$crop.r, INDICES=prod01.df$FOLIO, FUN=
#  function(x) paste(sort(as.integer(x)), collapse=" ") )

reduced.crops <- prod01.df$crop.r %in% names(rev(sort(table(prod01.df$crop.r)[table(prod01.df$crop.r)>20])))
  
farm.types.int.ls <- by(prod01.df$crop.r[reduced.crops], INDICES=prod01.df$FOLIO[reduced.crops], FUN=function(x) as.set(as.integer(x)) ) # as.integer 

farm.types.char.ls <- by(prod01.df$crop.r[reduced.crops], INDICES=prod01.df$FOLIO[reduced.crops], FUN=function(x) paste(sort(as.integer(x)), collapse=" "))

names(farm.types.int.ls) <- NULL

farm.types.int.ls <- farm.types.int.ls[order(farm.types.char.ls)]

farm.types.int.ls <- unique(farm.types.int.ls)

farm.types.char.ls <- farm.types.char.ls[order(farm.types.char.ls)]

apparance.times.tab <- table(sapply(farm.types.char.ls, FUN=function(x) paste(sort(x), collapse=" ")) )


crop.integers <- sort(unique(crop.integers))

crop.powerset <- 2 ^ as.set(crop.integers)

attributes(crop.powerset) <- NULL

crop.powerset.cardinality <- sapply(crop.powerset, set_cardinality)

#crop.powerset <- lapply(crop.powerset, as.integer)

subset.count <- rep(0, length(crop.powerset))

library("compiler")

#set_is_subset.cmp <- cmpfun(set_is_subset)

#sapply(crop.powerset,  FUN = function(a) set_is_empty(set_complement(farm.types.int.ls[i], a)) )

#set_is_empty(set_complement(farm.types.int.ls[i], crop.powerset))

enableJIT(3)


#for ( i in 1:length(farm.types.int.ls)) {
#  subset.count <- subset.count + is.element(list(farm.types.int.ls[[i]]), crop.powerset)
  # grepl(farm.types.int.ls[[i]], crop.powerset )
  # set_is_subset(list(farm.types.int.ls[i]), crop.powerset)
#  cat(i, "\n")
#}

#length(farm.types.int.ls)

set.to.iter <- (1:length(farm.types.int.ls))[
  sapply(farm.types.int.ls, FUN= function(x) (all(is.element(x, crop.integers))))
  ]
  
cat("Number of iterations: ", length(set.to.iter), "\n\n", 
  "Max listed number: ", max(set.to.iter), "\n\n")

for ( i in set.to.iter) {
  subset.count <- subset.count + 
    set_is_subset(list(farm.types.int.ls[[i]]), crop.powerset) * as.vector(apparance.times.tab)[i]
  # grepl(farm.types.int.ls[[i]], crop.powerset )
  # set_is_subset(list(farm.types.int.ls[i]), crop.powerset)
  cat(i, " ", sum(subset.count),  "\n")
}


largest.crop.sets <-c()

for (i in 1:largest.set ) {

 largest.crop.sets <- c(largest.crop.sets, 
   which.max(subset.count[crop.powerset.cardinality<=i]) )

}

top.sets <- crop.powerset[largest.crop.sets]

added.element <- as.integer(top.sets[[1]])

for ( i in 2:length(top.sets)) {
  added.element <- c(added.element , as.integer(top.sets[[i]] - top.sets[[i-1]]) )
}

added.element <- unique(added.element)

levels(prod01.df$crop.r[reduced.crops])[added.element]

subset.count[largest.crop.sets]



top.crops<-names(rev(sort(table(prod01.df$crop.r))))[1:largest.set]

crop.comparison <-list()

for (i in 1:largest.set) {
  crop.comparison[[i]] <- data.frame(sort(top.crops[1:i]), 
  levels(prod01.df$crop.r[reduced.crops])[ as.integer(top.sets[[i]]) ]
  )
  colnames(crop.comparison[[i]]) <- c("naive", "power.set")
}


# Let's say highest average percentage of revenue



crop.comparison.revenue.median<- list()
crop.comparison.revenue.mean<- list()
crop.comparison.revenue <- list()

for (i in 1:largest.set) {

 naive.total <- by( prod01.df, INDICES=prod01.df$FOLIO, function (x) {
   sum(x$total.value[x$crop.r %in% top.crops[1:i] ]) / 
   sum(x$total.value) } )

 power.set.total <- by( prod01.df, INDICES=prod01.df$FOLIO, function (x) {
   sum(x$total.value[x$crop.r %in% 
     levels(prod01.df$crop.r[reduced.crops])[ as.integer(top.sets[[i]]) ] 
       ]) / 
   sum(x$total.value) } )
 
 crop.comparison.revenue.median[[i]] <- c(naive=
   median(unlist(naive.total), na.rm=TRUE), 
 power.set= 
   median(unlist(power.set.total), na.rm=TRUE)
 )
 
 crop.comparison.revenue.mean[[i]] <- c(naive=
   mean(unlist(naive.total), na.rm=TRUE), 
 power.set= 
   mean(unlist(power.set.total), na.rm=TRUE)
 )
 
 crop.comparison.revenue[[i]] <- data.frame(
   naive=unclass(unlist(naive.total)), 
   power.set= unclass(unlist(power.set.total))
 )

}


crop.comparison.revenue.median <- do.call(rbind, crop.comparison.revenue.median)


crop.comparison.revenue.mean <- do.call(rbind, crop.comparison.revenue.mean)

par(mfcol=c(3, 5))

for (i in 1:largest.set) {
  hist(crop.comparison.revenue[[i]]$power.set, ylab=NULL, xlab=NULL, main=i)
}



par(mfcol=c(3, 5))

for (i in 1:largest.set) {
  hist(crop.comparison.revenue[[i]]$naive, ylab=NULL, xlab=NULL, main=i)
}



#j <- 8
#num.clust <- 


deg.free.max.mat <- matrix(0, ncol=10, nrow=length(3:15))


for (j in 3:15) {

# What are our goals here? 
# max crop coverage
# min num of crops
# How about max degrees of freedom?
# so the number of M

# 1  44
# 2  54
# 3  65
# 4  77
# 5  90
# 6 104
# 7 119
# 8 135
# 9 154
#10 170
#11 189
#12 209
#13 230
#14 252
#15 275

# TODO: Im pretty sure that these counts below are the "theoretical" number of parameters, which is what we want, but I'm not sure if the number of terms was cleaned of non-identified parameters

parameters.to.est <- c(
44,
54,
65,
77,
90,
104,
119,
135,
154,
170,
189,
209,
230,
252,
275)

 

crops.to.include <- levels(prod01.df$crop.r[reduced.crops])[ as.integer(top.sets[[j]]) ] 

crop.coverage.ls <- by( prod01.df, INDICES=prod01.df$FOLIO, function (x) {
   sum(x$total.value[x$crop.r %in% crops.to.include ]) / 
   sum(x$total.value) } )

included.firms.df <- prod01.df[ prod01.df$FOLIO %in%
  names(crop.coverage.ls[sapply(crop.coverage.ls, FUN=function(x) x >= 0.75)]),
  ]


crop.wide.df<-reshape(included.firms.df[ included.firms.df$crop %in% crops.to.include, 
	!names(included.firms.df) %in% c("CODA", "area", "crop.code", "harvest", "sales.quant", "consumption", "seeds", "animal.or.subproducts", "bartered", "lost")], timevar="crop", idvar="FOLIO", direction="wide")

crop.wide.df[is.na(crop.wide.df)] <- 0 



harvest.yes.no.df <- apply(crop.wide.df[, grepl("harvest.r", colnames(crop.wide.df))], 2, FUN=function(x) ifelse(x>0, 1, 0) )


par(mfcol=c(1,1))

prop.table(table(rowSums(harvest.yes.no.df)))

for ( num.clust in 1:10 ) {
# 1:largest.set

d <- dist(harvest.yes.no.df , method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
#plot(fit) # display dendogram
groups <- cutree(fit, k=num.clust) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
#rect.hclust(fit, k=6, border="red")

aggregate(harvest.yes.no.df,  by=list(groups), FUN=sum)

t(aggregate(harvest.yes.no.df,  by=list(groups), FUN=sum))
#t(aggregate(harvest.yes.no.df,  by=list(kmeans(harvest.yes.no.df, 6)$cluster), FUN=sum))

# number of crop types per cluster
sort(colSums((t(aggregate(harvest.yes.no.df,  by=list(groups), FUN=function(x) ifelse(sum(x)>0, 1, 0))[,-1]))))
#sort(colSums(t(aggregate(harvest.yes.no.df,  by=list(kmeans(harvest.yes.no.df, 5)$cluster), FUN=function(x) ifelse(sum(x)>0, 1, 0))[,-1])))

# Total number of farms in each category

sort(table(groups))
#sort(table(kmeans(harvest.yes.no.df, 5)$cluster))


deg.of.freedom <- table(groups) - parameters.to.est[
colSums((t(aggregate(harvest.yes.no.df,  by=list(groups), FUN=function(x) ifelse(sum(x)>0, 1, 0))[,-1])))
]


#cat( c(num.clust,  sum( c(deg.of.freedom) ), "\n"))

deg.free.max.mat[j-2, num.clust]  <- ifelse( all(c(deg.of.freedom) - 54 >0), sum( c(deg.of.freedom) ), NA)
# Substract 3 because need to account for shadow price parameters
# ok, let's ensure at least 50 degrees of freedom
# deg.free.max.mat[j-2, num.clust]  <- ifelse( all(c(deg.of.freedom) - 4 >0), sum( c(deg.of.freedom) ), NA)

}

}

cluster.benefit.mat <- matrix(rep(1:10, length(3:15)) , ncol=10, nrow=length(3:15), byrow=TRUE)

deg.free.max.mat.2 <- deg.free.max.mat + cluster.benefit.mat * 100

deg.free.max.mat.2[is.na(deg.free.max.mat.2)] <- 0


deg.free.max.mat.2[max.col(t(deg.free.max.mat.2)), max.col(deg.free.max.mat.2)]


deg.free.max.mat.2[which.max(deg.free.max.mat.2)]

opt.clusters <- matrix(rep(1:10, length(3:15)) , ncol=10, nrow=length(3:15), byrow=TRUE)[which.max(deg.free.max.mat.2)]

opt.crops <- matrix(rep(1:length(3:15), 10) , ncol=10, nrow=length(3:15), byrow=FALSE)[which.max(deg.free.max.mat.2)] + 2





crops.to.include <- levels(prod01.df$crop.r[reduced.crops])[ as.integer(top.sets[[opt.crops]]) ] 
crop.coverage.2.ls <- by( prod01.df, INDICES=prod01.df$FOLIO, function (x) {
   sum(x$total.value[x$crop.r %in% crops.to.include ]) / 
   sum(x$total.value) } )
included.firms.df <- prod01.df[ prod01.df$FOLIO %in%
  names(crop.coverage.2.ls[sapply(crop.coverage.2.ls, FUN=function(x) x >= 0.75)]),
  ]
crop.wide.df<-reshape(included.firms.df[ included.firms.df$crop %in% crops.to.include, 
	!names(included.firms.df) %in% c("CODA", "area", "crop.code", "harvest", "sales.quant", "consumption", "seeds", "animal.or.subproducts", "bartered", "lost")], timevar="crop", idvar="FOLIO", direction="wide")
crop.wide.df[is.na(crop.wide.df)] <- 0 
harvest.yes.no.df <- apply(crop.wide.df[, grepl("harvest.r", colnames(crop.wide.df))], 2, FUN=function(x) ifelse(x>0, 1, 0) )

d <- dist(harvest.yes.no.df , method = "euclidean") # distance matrix
fit <- hclust(d, method="ward") 
#plot(fit) # display dendogram
groups <- cutree(fit, k=opt.clusters) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters 
#rect.hclust(fit, k=6, border="red")
t(aggregate(harvest.yes.no.df,  by=list(groups), FUN=sum))

( deg.of.freedom <- table(groups) - parameters.to.est[
colSums((t(aggregate(harvest.yes.no.df,  by=list(groups), FUN=function(x) ifelse(sum(x)>0, 1, 0))[,-1])))
] )

groups.w.folio <- data.frame(FOLIO = crop.wide.df$FOLIO, groups=groups)

firm.df.save <- firm.df

firm.df.save <- merge(firm.df.save , crop.wide.df, all.x=TRUE)

for ( target.group in sort(unique(groups))) {

# target.group <- 1

  firm.df <- firm.df.save[firm.df.save$FOLIO %in% 
    groups.w.folio$FOLIO[groups.w.folio$groups==target.group], ]
    
  firm.df <- merge(firm.df, coverage.to.merge.df, all.x=TRUE)
  
  # This total conversion below may be a bit cavalier
  firm.df[is.na(firm.df)] <- 0
  
  source("/Users/travismcarthur/git/coursework/aae-637/paper/subset-firmbuilding.r")
  
  source("/Users/travismcarthur/git/coursework/aae-637/paper/linear-sur-building.r")

  
linear.sur.est <- systemfit( S.n.H, "SUR", restrict.matrix = lm.param.restrictions,  maxit = 5000 )

summary(linear.sur.est )

summary(lm(S.n.H[[length(S.n.H)]]))


linear.sur.est <- systemfit( S.n.H, "SUR", restrict.matrix = lm.param.restrictions,  maxit = 5000 )



















# NOTE: here we now define the set of crops and the clustered groups


included.firms.df <- prod01.df[ prod01.df$FOLIO %in%
  names(crop.coverage.2.ls[sapply(crop.coverage.2.ls, FUN=function(x) x >= 0.75)]),
  ]





by( prod01.df, INDICES=prod01.df$FOLIO, function (x) {
   sum(x$total.value[x$crop.r %in% crops.to.include ]) / 
   sum(x$total.value) } )

coverage.to.merge.df <- data.frame(
  FOLIO = names(crop.coverage.ls[sapply(crop.coverage.ls, FUN=function(x) x >= 0.75)]), 
  crop.coverage = unclass(unlist(crop.coverage.2.ls[sapply(crop.coverage.2.ls, FUN=function(x) x >= 0.75)])),
  stringsAsFactors=FALSE
)

coverage.to.merge.df <- coverage.to.merge.df[complete.cases(coverage.to.merge.df), ]













































is.element(farm.types.int.ls[i], crop.powerset)

is.element(farm.types.int.ls[[i]], crop.powerset)

setdiff(farm.types.int.ls[[i]], crop.powerset)
setdiff( crop.powerset, farm.types.int.ls[[i]]) == farm.types.int.ls[[i]]

intersect( crop.powerset, farm.types.int.ls[[i]])

intersect(list(farm.types.int.ls[[i]]), crop.powerset) == crop.powerset



set_is_subset(list(s, s), test.set)

lapply(test.set, FUN=

s <= test.set

test.set <= s




for (i in 1:









