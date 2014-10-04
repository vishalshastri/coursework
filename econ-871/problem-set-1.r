
# usa.nz.mex.df <- read.csv("/Users/travismcarthur/Desktop/Econ 871 Trade/Problem sets/PS 1/USA - N Zea and USA  - Mex - 2010.csv", stringsAsFactors=FALSE)




# Thanks to http://comtrade.un.org/data/Doc/api/ex/r

get.Comtrade <- function(url="http://comtrade.un.org/api/get?"
                         ,maxrec=50000
                         ,type="C"
                         ,freq="A"
                         ,px="HS"
                         ,ps="now"
                         ,r
                         ,p
                         ,rg="all"
                         ,cc="TOTAL"
                         ,fmt="json"
)
{
  string<- paste(url
                 ,"max=",maxrec,"&" #maximum no. of records returned
                 ,"type=",type,"&" #type of trade (c=commodities)
                 ,"freq=",freq,"&" #frequency
                 ,"px=",px,"&" #classification
                 ,"ps=",ps,"&" #time period
                 ,"r=",r,"&" #reporting area
                 ,"p=",p,"&" #partner country
                 ,"rg=",rg,"&" #trade flow
                 ,"cc=",cc,"&" #classification code
                 ,"fmt=",fmt        #Format
                 ,sep = ""
  )
  
  if(fmt == "csv") {
    raw.data<- read.csv(string,header=TRUE)
    return(list(validation=NULL, data=raw.data))
  } else {
    if(fmt == "json" ) {
      raw.data<- fromJSON(file=string)
      data<- raw.data$dataset
      validation<- unlist(raw.data$validation, recursive=TRUE)
      ndata<- NULL
      if(length(data)> 0) {
        var.names<- names(data[[1]])
        data<- as.data.frame(t( sapply(data,rbind)))
        ndata<- NULL
        for(i in 1:ncol(data)){
          data[sapply(data[,i],is.null),i]<- NA
          ndata<- cbind(ndata, unlist(data[,i]))
        }
        ndata<- as.data.frame(ndata)
        colnames(ndata)<- var.names
      }
      return(list(validation=validation,data =ndata))
    }
  }
}


# install.packages("rjson")
library("rjson")
usa.nz.mex.df <- get.Comtrade(r="842", p="484,554", ps=2010, px="H1", cc="AG6",fmt="csv", rg="1,2")


usa.nz.mex.df <- usa.nz.mex.df$data

length(unique(usa.nz.mex.df$Commodity.Code))

# QUESTION 1(a)

nonzero.flows  <- as.data.frame.table(
  table(usa.nz.mex.df$Trade.Value..US..>0, usa.nz.mex.df$Partner, usa.nz.mex.df$Trade.Flow)
)

nonzero.flows$Freq <- nonzero.flows$Freq / 5131

nonzero.flows

# QUESTION 1(b)

# install.packages("e1071")
library("e1071")                  

summary.stats.fn <- function(x) {
  data.frame(mean=mean(x), median=median(x), sd=sd(x), skew=skewness(x))
}

summary.stats.ls <- with(usa.nz.mex.df[usa.nz.mex.df$Trade.Flow=="Export", ],
  by(data=Trade.Value..US.., INDICES=list(Partner), FUN=summary.stats.fn)
)

summary.stats.df <- do.call(rbind, summary.stats.ls)

summary.stats.df$Country <- rownames(summary.stats.df)


# QUESTION 1(c)


usa.nz.mex.AG1.df <- get.Comtrade(r="842", p="484,554", ps=2010, px="H1", cc="AG2",fmt="csv", rg="1,2")

usa.nz.mex.AG1.df <- usa.nz.mex.AG1.df$data


by(data=usa.nz.mex.AG1.df, 
    INDICES=list(usa.nz.mex.AG1.df$Partner, usa.nz.mex.AG1.df$Trade.Flow), 
  FUN=function(x) {
    x[order(x$Trade.Value..US.., decreasing=TRUE), ][1:5, c("Commodity", "Trade.Value..US..")]
  }
)

# QUESTION 2(a)


reporters.df <- fromJSON(file="http://comtrade.un.org/data/cache/partnerAreas.json")
reporters.df <- as.data.frame(matrix(unlist(reporters.df$results), ncol=2, byrow=TRUE), stringsAsFactors=FALSE)

colnames(reporters.df) <- c("code", "country")

top.40.countries <- c(
  "China",
  "USA",
  "Germany",
  "Japan",
  "France",
  "Netherlands",
  "Rep. of Korea",
  "Italy",
  "United Kingdom",
  "Belgium",
  "China, Hong Kong SAR",
  "Russian Federation",
  "Canada",
  "Singapore",
  "Mexico",
  "Saudi Arabia",
  "Spain",
  "India",
  "Australia",
  "Malaysia",
  "United Arab Emirates",
  "Brazil",
  "Switzerland",
  "Thailand",
  "Sweden",
  "Indonesia",
  "Poland",
  "Austria",
  "Czech Rep.",
  "Norway",
  "Ireland",
  "Turkey",
  "Denmark",
  "Hungary",
  "Nigeria",
  "Iran",
  "Qatar",
  "Viet Nam",
  "South Africa",
  "Chile"
)

top.40.countries[!top.40.countries %in% reporters.df$country]

top.40.countries.code <- reporters.df[reporters.df$country %in% top.40.countries, "code"]

#top.40.countries.code <- paste0(top.40.countries.code, collapse=",")

top.for.grid <- c(paste0(top.40.countries.code[1:5], collapse=","),
  paste0(top.40.countries.code[6:10], collapse=","),
  paste0(top.40.countries.code[11:15], collapse=","),
  paste0(top.40.countries.code[16:20], collapse=","),
  paste0(top.40.countries.code[21:25], collapse=","),
  paste0(top.40.countries.code[26:30], collapse=","),
  paste0(top.40.countries.code[31:35], collapse=","),
  paste0(top.40.countries.code[36:40], collapse=",")
)

top.grid <- expand.grid(top.for.grid,top.for.grid)

top.40.ls <- list()

for ( i in 1:nrow(top.grid)) {
  
  top.40.ls[[i]] <- get.Comtrade(
    r=top.grid[i, 1], 
    p=top.grid[i, 2], 
    ps=2010, cc="TOTAL",rg="2")$data # Exports now
  
  Sys.sleep(1.2)
  
}

top.40.df <- do.call(rbind, top.40.ls)

#top.40.df <- top.40.df[as.character(top.40.df$rt3ISO) != as.character(top.40.df$pt3ISO), ]
str(top.40.df)

#top.40.df.save <- top.40.df

top.40.df$TradeValue

sum(is.na(as.numeric(as.character(top.40.df$TradeValue))))

# want 1,516 nonzeros
# had 1,554 when imports
# 40* 39 = 1560 possible


if(!require(XML)) {
  install.packages("XML" )
	while(!require(XML)) {
		Sys.sleep(1)
		require(XML)
	}
}

if(!require(plyr)) {
  install.packages("plyr" )
	while(!require(plyr)) {
		Sys.sleep(1)
		require(plyr)
	}
}

gdp.ls<- list()

# Must have pt3ISO since only 39 reporters, but 40 partners

for ( i in unique(top.40.df$pt3ISO)) {

ca.df<-xmlToList(xmlParse(paste0("http://api.worldbank.org/countries/", i, "/indicators/NY.GDP.MKTP.CD?date=2010&format=xml&frequency=Y")))
ca.df<-lapply(ca.df, unlist)
ca.df<-lapply(ca.df, t)
ca.df<-lapply(ca.df, data.frame, stringsAsFactors =FALSE)
gdp.ls[[i]]<-do.call(rbind.fill, ca.df)
  
}

gdp.df <- do.call(rbind.fill, gdp.ls)
gdp.df <- gdp.df[!is.na(gdp.df[, 1]), ]

gdp.r.df <- gdp.df
gdp.p.df <- gdp.df

gdp.r.df$gdp.r <- gdp.r.df$value
gdp.p.df$gdp.p <- gdp.p.df$value

gdp.r.df$rt3ISO <- unique(top.40.df$pt3ISO)
gdp.p.df$pt3ISO <- unique(top.40.df$pt3ISO)

top.40.df <- merge(top.40.df, gdp.r.df[, c("rt3ISO", "gdp.r")],  all.x=TRUE)

top.40.df <- merge(top.40.df, gdp.p.df[, c("pt3ISO", "gdp.p")],  all.x=TRUE)

# r is reporter is exporter




dist.df <- read.csv("/Users/travismcarthur/Desktop/Econ 871 Trade/Problem sets/PS 1/dist_cepii.csv", stringsAsFactors=FALSE)

# dist.df$dist

#probably origin and destination: iso_o iso_d

names(dist.df)[names(dist.df)=="iso_o"] <- "rt3ISO"
names(dist.df)[names(dist.df)=="iso_d"] <- "pt3ISO"

top.40.df <- merge(top.40.df, dist.df[, c("rt3ISO", "pt3ISO", "dist")],  all.x=TRUE)

top.40.df$TradeValue <- as.numeric(as.character(top.40.df$TradeValue))
top.40.df$gdp.r <- as.numeric(as.character(top.40.df$gdp.r))
top.40.df$gdp.p <- as.numeric(as.character(top.40.df$gdp.p))

summary(naive.gravity.lm <- lm( 
  log( TradeValue/(gdp.r * gdp.p) ) ~ log(dist) + rtTitle + ptTitle,  
  data=top.40.df)
)


summary(naive.gravity.lm <- lm( 
  log( TradeValue ) ~ log(gdp.r) + log(gdp.p) + I( - log(dist)) + rtTitle + ptTitle,  
  data=top.40.df)
)




# QUESTION 3(a)

# install.packages("evd")
library("evd")

set.seed(100)

wages=rep(1,N.countries)

#wages=c(5,9,10)

(test.mat<-matrix(1:9, ncol=3))
c(test.mat)



assume norma

library("Matrix")


simplified.EK.fn <- function(wages, tau_n_i, N.countries=3, K.goods=100, T_i = 1.5, distn_theta=4, L_n=rep(1,3), sigma=5, ret.share.mat=FALSE) {
  
  set.seed(100)
  
  p_n_i_k.args.ls <- lapply(as.list(rep(T_i, N.countries)), FUN=function(T_i) {
    list(z_i_k=rfrechet(n=K.goods, loc = 0, scale = T_i^(-distn_theta), shape=distn_theta),
      w_i=0)
  }
  )
  
  for ( i in 1:length(p_n_i_k.args.ls)) {
    p_n_i_k.args.ls[[i]]$w_i <- wages[i]
  }
  
  for ( i in 1:length(p_n_i_k.args.ls)) {
    p_n_i_k.args.ls[[i]]$tau_n_i <- tau_n_i
  }
  # Ok this is assuming matrix is the same for all, whcih is not true. I think maybe true, though
   
  p_n_i_k.ls <- list()
  for ( i in 1:length(p_n_i_k.args.ls)) {
    x <- p_n_i_k.args.ls[[i]]
    
    p_n_i_k.ls[[i]] <-
      (x$w_i / x$z_i_k) * rep(1 + x$tau_n_i[, i], each=length(x$z_i_k))
    
  }
  
  p_n_i_k.mat<- do.call(cbind, p_n_i_k.ls)
  
  # max.col() possibly useful
  
  p_n_k.which.v <- apply(p_n_i_k.mat, 1, which.min) 
  
  p_n_k.which.mat <- matrix(p_n_k.which.v, ncol=N.countries)
  
  #use by()?
  
  p_n_k.v <- apply(p_n_i_k.mat, 1, min) 
  
  p_n_k.mat <- matrix(p_n_k.v, ncol=N.countries)
  # TODO: not sure if the matrix is filling properly yet. I think it is, now that I look at it
  # The lowest price is the same for all countries since no trade costs
  
  
  # http://www.columbia.edu/~jid2106/td/dixitstiglitzbasics.pdf 
  # Marshallian: http://dept.econ.yorku.ca/~sam/5010/consumer/s04.pdf
  # r = 1-sigma  
  
#  p_n_i_k.ls <- lapply(p_n_i_k.args.ls, FUN=function(x) {
#    (x$w_i / x$z_i_k) * c(x$tau_n_i)
    # Must add the tau for future function
    # We are relying heavily on vector replication here
#  })
  
#  min.price.mat<- do.call(cbind, p_n_i_k.ls)
  min.price.mat<-p_n_k.mat
  
  C_n.fn <- function(price.vec, targ.price, sigma, y) {
    (price.vec[targ.price]^-sigma * y) /
      sum(price.vec^(1-sigma))
  }
  
#  test.price <- runif(5)
#  C_n.fn(1:5, test.price, 5, 1)
 # Seems to work OK in a vectorized way 
  
  exp.share.ls <- list()
  
  for ( i in 1:ncol(min.price.mat)) {
    exp.share.ls[[i]]<- 
      C_n.fn(price.vec=min.price.mat[, i], targ.price=1:nrow(min.price.mat), 
        sigma=sigma, y=L_N[i] * wages[i])   
  }
  
  trade.share.ls <- list()
  
  for ( i in 1:N.countries) {
    
    left.matrix <- sparseMatrix(i = p_n_k.which.mat[, i], j=1:nrow(p_n_k.which.mat)  ) 
    
    right.matrix <- matrix(exp.share.ls[[i]], ncol=1)
    
    trade.share.ls[[i]] <- as.matrix(left.matrix %*% right.matrix)
    
  }
  
  trade.share.mat <- do.call(cbind, trade.share.ls)
  # Ok, so here the rows are trading partners, so a sum of a column adds to
  # a country's total expenditure
  
  if (ret.share.mat) {return(trade.share.mat)}
  
  sum( abs( colSums(trade.share.mat) - rowSums(trade.share.mat) ) )
  
}

  
set.seed(100)

system.time(
simplified.EK.fn(wages=rep(1,3), N.countries=3, K.goods=100, T_i = 1.5, distn_theta=4, L_n=rep(1,3), sigma=5)
)

no.trade.costs.wages<- optim(par=rep(1,3), fn=simplified.EK.fn, tau_n_i = matrix(0, nrow=3, ncol=3))$par

simplified.EK.fn(no.trade.costs.wages, matrix(0, nrow=3, ncol=3), ret.share.mat=TRUE)

low.tau.mat <-  matrix(0.1, nrow=3, ncol=3)

diag(low.tau.mat) <- 0

all.0.1.trade.costs.wages <- optim(par=rep(1,3), fn=simplified.EK.fn, tau_n_i = low.tau.mat)$par

simplified.EK.fn(all.0.1.trade.costs.wages , low.tau.mat, ret.share.mat=TRUE)

stochastic.tau.mat <- matrix(runif(9), nrow=3, ncol=3)

stochastic.trade.costs.wages <-optim(par=rep(1,3), fn=simplified.EK.fn, tau_n_i = stochastic.tau.mat )$par

simplified.EK.fn(stochastic.trade.costs.wages, stochastic.tau.mat, ret.share.mat=TRUE)

# Ok, cannot get different trade shares from all-same tau vs. no tau

  
  
  
  apply(min.price.mat, 1, FUN=C_n.fn, targ.price=1:nrow(min.price.mat), sigma=sigma, y=)
  
  C_n.fn(1:nrow(min.price.mat), min.price.mat, sigma, y)
  
  for ( i in )
  
  
  
  wages*L_n
  
  max.
  
  
  
  z_i_k.df.aug <- 
  
  lapply()
  
  z_i_k <- rfrechet(n.countries, location = 0, scale = T_i^(-distn_theta), shape=distn_theta)
  
  p_ni <- wages/
  
  
  
  
}


























any(duplicated(top.40.df))

top.40.df <- get.Comtrade(r=paste0(top.40.countries.code[1:5], collapse=","), 
  p=paste0(top.40.countries.code[1], collapse=","), 
  ps=2010, cc="TOTAL",rg="1")

fmt="csv", 


reporters.df[reporters.df[ ,2] %in%  ,1]



as.matrix(do.call(rbind, reporters.df$results))




# Successful test:
library("Matrix")
sparseMatrix(i = c(1,1,2,3,1), j=1:length(c(1,1,2,3,1))  ) %*% matrix(1:5, ncol=1)






