
#  round to 6 significant digits
#  make 4 columns
#  4 spaces between columns


# install.packages("gdata")
library("gdata")

# install.packages("stringr")
library("stringr")



make.GAMS.data <- function(x) {

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

  raw.last.df <- x[, (4*(num.body.dataframes+1)+1):ncol(x) ]
  
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










































































linear.sur.est.ols <- systemfit( S.n.H, method="OLS", restrict.matrix = lm.param.restrictions,  maxit = 1 )


linear.sur.est.ols.single <- systemfit( S.n.H[length(S.n.H)], "OLS", restrict.matrix = lm.param.restrictions[36:46],  maxit = 5000 )







GAMS.linear.results<- readLines("/Users/travismcarthur/Desktop/Dropbox/entropytestlinear.lst")


GAMS.linear.results.params<- GAMS.linear.results[grep("parameters to be esti$", GAMS.linear.results)]

GAMS.linear.results.params.names <- gsub("[.]L", "", str_extract(GAMS.linear.results.params, "[^ ]*[.]L") )


GAMS.linear.results.params.numbers <- as.numeric(gsub("  parameters to be esti", "",
  str_extract(GAMS.linear.results.params, "[^ ]*  parameters to be esti") ) )
  
combined.w.params.df <- as.list(as.data.frame(combined.df))

for ( i in 1:length(GAMS.linear.results.params.names)) {
  combined.w.params.df[[ GAMS.linear.results.params.names[i] ]] <- GAMS.linear.results.params.numbers[i]
}

for ( i in 1:N) {
  combined.w.params.df[[ paste0("theta", lead.zero(i)) ]] <- 1
}



ln.E.string.GAMS <- ln.E.string


for ( i in 1:N) {

  ln.E.string.GAMS <- gsub(paste0("w", lead.zero(i), " / [(]w", lead.zero(i), " [*] theta", 
    lead.zero(i), "[)]"), paste0("1 / theta", lead.zero(i)), ln.E.string.GAMS)
    
  ln.E.string.GAMS <- gsub(paste0("log[(]w", lead.zero(i), " [*] theta", lead.zero(i), "[)]"),
   paste0("(log(w", lead.zero(i), ") + log(theta", lead.zero(i), "))"),  ln.E.string.GAMS)

}

ln.E.string.GAMS <- gsub(pattern="[.]", replacement="", x=ln.E.string.GAMS)

big.log.posi.constraint.2<- str_extract( ln.E.string.GAMS, "log[(] [(]1 / theta01[)].*")

big.log.posi.constraint.2 <- sub("log[(]", "", big.log.posi.constraint.2) 
big.log.posi.constraint.2 <- sub("[)]$", "", big.log.posi.constraint.2) 


with(combined.w.params.df , eval(parse(text=big.log.posi.constraint.2)))

log(with(combined.w.params.df , eval(parse(text=big.log.posi.constraint.2))))

test.expression <- "(log(w01) + log(theta01)) "

with(combined.w.params.df , eval(parse(text=test.expression)))




for making sure data is a-ok:


parameters
test1(t)
test2(t)
test3(t)
test4(t)
test5(t)
test6(t)
test7(t)
test8(t)
test9(t);

test1(t) =  (w01(t)=0)   ;
test2(t) =  (w02(t)=0)   ;
test3(t) =  (w03(t)=0)   ;
test4(t) =  (w04(t)=0)   ;
test5(t) =  (w05(t)=0)   ;
test6(t) =  (w06(t)=0)   ;
test7(t) =  (q01(t)=1)   ;
* test7(t) =  (q01(t)>1)   ;
test8(t) =  (q02(t)=0)   ;
test9(t) =  (q03(t)=0)   ;

display test1;
display test2;
display test3;
display test4;
display test5;
display test6;
display test7;
display test8;
display test9;
display q01;





OPTION NLP=BARON;
* We only have demo version of this:  PATHNLP
* We only have demo version of this:  OQNLP
* Not suitable: NLPEC
* We only have demo version of this:  MSNLP
* We only have demo version of this: KNITRO
* Not avail: COUENNE
* We only have demo version of this: SNOPT
* We only have demo version of this:  BARON
* No license: LINDOGLOBAL
* No license: LGO
* No licsnes: MOSEK

* Maybe good: received weird error: AMPL

* This ran for a hot sec before choking: COINIPOPT


CONVERT
EMP
EXAMINER
GAMSBAS
GAMSCHK
LINDOWRAP
LINGO
MINOS55
MPECDUMP
MPSWRITE
PATHGMO
PATHNLP
PATHSMAG
TRAMP


# PROMISING!!!!!!:
OPTION NLP=CONOPTD;  

maybe try:
OPTION lfstal=10000;

lmmxsf = 1
lfnicr = 1000
rvhess = 0
0.05 



4.3736

....559


solve gme using nlp maximizing h;
options decimals = 7;

display theta01.l;
display theta02.l;
display theta03.l;
display theta04.l;
display theta05.l;
display theta06;






























p.l(i,k,m) = 1/MM;
pp.l(m) = 1/ MM;
w.l(t,i,j) = 1/JJ;


z("7",m) = z2(m);



  
1 0.3288806,    2 0.3333134,    3 0.3378060







----  11589 VARIABLE pdelta0103.L  probability corresponding param

1 0.3346314,    2 0.3333317,    3 0.3320370






----  11588 VARIABLE pbeta0.L  probability corresponding param


















Powers:
**
http://support.gams.com/doku.php?id=gams:difference_between_the_-operator_and_the_power_function










ln.E.string.linear <- gsub("[+]  log[(] [(]w01 [/] [(]w01 [*] theta01[)][)].*$", "", ln.E.string)

ln.E.string.linear <- gsub(" [*] theta[0-9][0-9]", "", ln.E.string.linear)



, 















































#display beta.l;
#scalar alpha   the constant term in aggregrate price equation;
#  alpha = sum(m, pp.l(m)*zz(m));
#  display alpha;























restrerror(t)..        1 =e= sum(j, w(t));


restr3b..            1 =e= sum(m, pp(m));















restr2(i,k)..        beta(i,k) =e= sum(m, p(i,k,m) * z(k, m));





parameter z1(m)    support points
/
1 -1
2  0
3  1
/;
parameter z2(m)    support points 
/
1 -0.2
2  0
3  0.2
/;
parameter zz(m)    support points 
/
1 -20
2  0
3  20
/;



This is for the non-theta params:
round( max(abs(coef(linear.sur.est))) * 5 )

Then the thetas:
1.e-6 to 50

error terms:
cost share:
-10 to 10

cost fn:
round( max(ln.E.data) * 5 )



+












s(t,"1") = datafile(t,"1");
s(t,"2") = datafile(t,"2");
s(t,"3") = datafile(t,"3");
s(t,"4") = datafile(t,"4");
s(t,"5") = datafile(t,"5");






#parameter
#  datafile(t,d)   raw data from SAS
#  s(t,i)          dependent variable share of budege
#  x(t,i,k)        explanatory variables
#  price1(t)       price level in log form
#  price2(t)       price level in log form
#  price3(t)       price level in log form
#  price4(t)       price level in log form
#  price5(t)       price level in log form
#  apple(t)        weighted average price level
#  indic(k)        copy of set k
#  MM              number of points in support
#  JJ              number of points in support
#;



# So main variables we need to combine are: top.before.data; combined.df.GAMS; 













# (1 / nrow(combined.df))  *

test.matrix <-   t( stacked.jacobian ) %*% 
    kronecker( solve(big.sigma), diag(nrow(combined.df)) ) %*% 
    stacked.jacobian
    
   testcols <- function(ee) {
       ## split eigenvector matrix into a list, by columns
       evecs <- split(zapsmall(ee$vectors),col(ee$vectors))
       ## for non-zero eigenvalues, list non-zero evec components
       mapply(function(val,vec) {
           if (val!=0) NULL else which(vec!=0)
       },zapsmall(ee$values),evecs)
   }
# Thanks to http://stackoverflow.com/questions/12304963/using-eigenvalues-to-test-for-singularity-identifying-collinear-columns

testcols(eigen(test.matrix))

svd(test.matrix)


test.matrix.scaled <- scale(test.matrix)


p<-ncol(test.matrix.scaled)

A<-svd(test.matrix.scaled,nu=0)
#x is ill-conditioned: this ratio is larger 
#than 10. (step 1)
2*log(A$d[1]/A$d[p])

#check what is causing it: (step 2)
round(A$v[,ncol(A$v)],2)
#you can write the last variable as (.23*x_1+.5*x_2-.45*x_3)/(-.7) [1]

min(A$d)


n<-100
p<-20
#non-ill conditioned part of the dataset.
x<-matrix(rnorm(n*p),nc=p)
x<-scale(x)
#introduce a variable that causes x to be 
#ill conditioned.
y<-x%*%c(rnorm(1),0, 0, 0, rnorm(1), 0, rnorm(1), rnorm(1), rep(0,p))[1:p]
#y<-scale(y)
#x<-cbind(x,y)
p<-ncol(x)

A<-svd(x,nu=0)
#x is ill-conditioned: this ratio is larger 
#than 10. (step 1)
2*log(A$d[1]/A$d[p])

#check what is causing it: (step 2)
round(A$v[,ncol(A$v)],2)
#you can write the last variable as (.23*x_1+.5*x_2-.45*x_3)/(-.7) [1]

min(A$d)



















#solve( scale(test.matrix, center=FALSE) )

solve( test.matrix / 10e10 )

det(test.matrix / 10e10 )


test.matrix <- solve(
  t( stacked.jacobian ) %*% 
    kronecker( solve(big.sigma), diag(nrow(combined.df)) ) %*% 
    stacked.jacobian
  )

diag(test.matrix)


GAMS.nonlinear.results.params.full /  diag(test.matrix)


  


with(combined.w.params.df, eval(parse(text=modified.ln.E.string))) + 
  error.collapsed.eq.ls[["cost"]] - 
  combined.w.params.df$cost


ln.E.data - 




parameter vshare(j)    support points 
/
1 -10
2  0
3  10
/;
parameter vcost(j)    support points 
/
1 -97
2  0
3  97
/;




modified.ln.E.string.evaled.deriv.complex <- jacobian(temp.deriv.fn, 
    x=GAMS.nonlinear.results.params.full, method="complex", 
    data=as.data.frame(combined.df) )


modified.ln.E.string.evaled.deriv.richardson <- jacobian(temp.deriv.fn, 
    x=GAMS.nonlinear.results.params.full, method="Richardson", 
    data=as.data.frame(combined.df) )



summary(modified.ln.E.string.evaled.deriv.complex - modified.ln.E.string.evaled.deriv.richardson)
max(modified.ln.E.string.evaled.deriv.complex - modified.ln.E.string.evaled.deriv.richardson)
min(modified.ln.E.string.evaled.deriv.complex - modified.ln.E.string.evaled.deriv.richardson)








