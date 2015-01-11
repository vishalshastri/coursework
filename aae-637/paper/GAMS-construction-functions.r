
#  round to 6 significant digits
#  make 4 columns
#  4 spaces between columns


# install.packages("gdata")
library("gdata")

# install.packages("stringr")
library("stringr")
library("plyr")

#plyr::round_any(.1234567, accuracy=.0001, f=floor)



make.GAMS.data <- function(x) {



# http://stackoverflow.com/questions/13483430/how-to-make-rounded-percentages-add-up-to-100
# http://stackoverflow.com/questions/23158165/truncate-decimal-to-specified-places?rq=1

trunc.prec <- function(x, ..., prec = 0) base::trunc(x * 10^prec, ...) / 10^prec

largest.remainder.method <- function(x, digits, index=1) {

  x <- as.matrix(x)
  x <- round(x, digits=digits+3)
  # Above is to deal with some weird behavior when the actual value is
  # infinitesiammly different from 1
  
  if (index==2) x <- t(x)
#   plyr::round_any(x, accuracy=1/10^digits, f=floor)
  x.truncated <- trunc.prec(x, prec=digits)
  x.remainder <- x - x.truncated
  
  for ( i in seq_len(nrow(x.truncated))) {
    if(sum(x.truncated[i, ])==1 ) next
    dif.to.make.up <- as.integer(round((1 - sum(x.truncated[i, ]) )*10^digits))
    
    x.remainder.manip <- x.remainder[i, ]
    names(x.remainder.manip) <- seq_len(length(x.remainder.manip ))
    
    # kind of hacky, below
    indices.to.add <- as.numeric(names(
      sort(x.remainder.manip, decreasing=TRUE)[seq_len(dif.to.make.up)]
    ))
    
    x.truncated[i, indices.to.add] <- x.truncated[i, indices.to.add] + 1/10^digits
  }
  
  if (index==2) x.truncated <- t(x.truncated )
  
  round(x.truncated , digits=digits)
 
}

if ( any(grepl("^s[0-9]+$", names(x)))) {
  x[, grepl("^s[0-9]+$", names(x))] <- 
    as.data.frame(largest.remainder.method(x=x[, grepl("^s[0-9]+$", names(x))], digits=5))
}
  
#  x<- x[rowSums(as.data.frame(largest.remainder.method(x=x[, grepl("^s[0-9]+$", names(x))], digits=5)))!=1, grepl("^s[0-9]+$", names(x))][2, ]
  
#  x.fixed <- as.data.frame(largest.remainder.method(x=x[, grepl("^s[0-9]+$", names(x))], digits=5))
  
#  x.fixed[rowSums(x.fixed)!=1, ]
  
raw.first.df <- x[, 1:4]

colnames(raw.first.df) <- sprintf("%10s", as.character(1:4))

raw.first.df<- as.data.frame(apply(signif(raw.first.df, digits=5), 2, FUN=as.character))

row.names(raw.first.df) <- paste0( " ", format(1:nrow(raw.first.df), justify="right") )

colnames(raw.first.df)[1] <- paste0(paste0(rep(" ", max(nchar(row.names(raw.first.df)))), collapse=""), colnames(raw.first.df)[1])

file <- tempfile()

write.fwf(raw.first.df, file=file, 
  append=FALSE, quote=FALSE, sep="    ", na="", justify="right", width = c(max(nchar(row.names(raw.first.df))), 10,10,10,10),
  rownames=TRUE, rowCol="")

processed.first.df <- readLines(file)

unlink(file)


#"/Users/travismcarthur/Desktop/Metrics (637)/Final paper/GAMS work/fwf test.gms"

num.body.dataframes <- (ncol(x) - 4) %/% 4

processed.body <- list()

for ( i in 1:num.body.dataframes) {

  raw.body.df <- x[, (1:4)+4*i]

  colnames(raw.body.df) <- sprintf("%10s", as.character((1:4)+4*i))

  raw.body.df<- as.data.frame(apply(signif(raw.body.df, digits=5), 2, FUN=as.character))

  row.names(raw.body.df) <- paste0( " ", format(1:nrow(raw.body.df), justify="right") )

  colnames(raw.body.df)[1] <- paste0(paste0(rep(" ", max(nchar(row.names(raw.body.df)))), collapse=""), colnames(raw.body.df)[1])

  file <- tempfile()

  write.fwf(raw.body.df, file=file, 
    append=FALSE, quote=FALSE, sep="    ", na="", justify="right", width = c(max(nchar(row.names(raw.body.df))), 10,10,10,10),
    rownames=TRUE, rowCol="")

  processed.body[[i]] <- readLines(file)
  
  processed.body[[i]][1] <- sub(" ", "+", processed.body[[i]][1])
  # This only substitutes the first space, since it is not gsub()
  
  processed.body[[i]] <- c(processed.body[[i]][1], 
    paste0(rep(" ", nchar(processed.body[[i]][1])), collapse=""), processed.body[[i]][-1]
    )
  
  unlink(file)

}

if ( ncol(x) > 4*(num.body.dataframes+1) ) {

  raw.last.df <- x[, (4*(num.body.dataframes+1)+1):ncol(x), drop=FALSE ]
  
  colnames(raw.last.df) <- sprintf("%10s", (4*(num.body.dataframes+1)+1):ncol(x))

  raw.last.df<- as.data.frame(apply(signif(raw.last.df, digits=5), 2, FUN=as.character))

  row.names(raw.last.df) <- paste0( " ", format(1:nrow(raw.last.df), justify="right") )

  colnames(raw.last.df)[1] <- paste0(paste0(rep(" ", max(nchar(row.names(raw.last.df)))), collapse=""), colnames(raw.last.df)[1])

  file <- tempfile()

  write.fwf(raw.last.df, file=file, 
    append=FALSE, quote=FALSE, sep="    ", na="", justify="right", width = c(max(nchar(row.names(raw.last.df))), 
      rep(10, ncol(raw.last.df))),
    rownames=TRUE, rowCol="")

  processed.last.df <- readLines(file)
  
  processed.last.df[1] <- sub(" ", "+", processed.last.df[1])
  # This only substitutes the first space, since it is not gsub()
  
  processed.last.df <- c(processed.last.df[1], 
    paste0(rep(" ", nchar(processed.last.df[1])), collapse=""), processed.last.df[-1]
    )

  
  unlink(file)

} else { 
  processed.last.df <- NULL
}

ret <- processed.first.df

for ( i in 1:length(processed.body) ) {
  ret <- c(ret,  paste0(rep(" ", max(nchar(ret))), collapse=""),  processed.body[[i]]) 
}

ret <- c(ret, paste0(rep(" ", max(nchar(ret))), collapse=""), processed.last.df)

ret

# TODO: it would be nice to include the column names in the GAMS comments

}




# make.GAMS.data(data.frame(matrix(runif(100*23)+1, ncol=23, nrow=100)))


# TODO: will need to check for any numbers of the form 2.20265e+04



log10_ceiling <- function(x) {
    10^(ceiling(log10(x)))
}
# Thanks to http://stackoverflow.com/questions/7906996/algorithm-to-round-to-the-next-order-of-magnitude-in-r








add.data.subscripts <- function(x) {
  for (i in 1:99) {
    ii <- formatC(i, width = 2, flag = "0")  
    x <- gsub(paste0("w", ii), paste0("w", ii, "(t)"), x)
    x <- gsub(paste0("q", ii), paste0("q", ii, "(t)"), x)
    x <- gsub(paste0("x", ii), paste0("x", ii, "(t)"), x)
    x <- gsub(paste0("y", ii), paste0("y", ii, "(t)"), x)
    
  }
  regions.to.fix <- str_extract_all(x, "region[[:alpha:]]+")[[1]]
  
  for ( j in seq_along(regions.to.fix) ) {
    x <- gsub(regions.to.fix[j], paste0(regions.to.fix[j], "(t)"), x)
  }
  x
}







