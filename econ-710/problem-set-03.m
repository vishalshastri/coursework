

urlwrite('http://www.ssc.wisc.edu/~bhansen/710/cps85.dat','cps85.dat');
load 'cps85.dat';

cps85(:, 10) = cps85(:, 7).^2  ;
cps85(:, 11) = 1;

% 3.19

beta = ((cps85(:, [1, 7, 10, 11])'*cps85(:, [1, 7, 10, 11]))^-1)*cps85(:, [1, 7, 10, 11])'*cps85(:, 9) 

% a.
e_hat= cps85(:, 9)-cps85(:, [1, 7, 10, 11])*((cps85(:, [1, 7, 10, 11])'*cps85(:, [1, 7, 10, 11]))^-1)*cps85(:, [1, 7, 10, 11])'*cps85(:, 9) ;
sum(e_hat)

% b.
sum(cps85(:, 1)'*e_hat)

% c.
sum(cps85(:, 7)'*e_hat)

% d.
sum((cps85(:, 1).^2)'*e_hat)

% e.
sum(cps85(:, 10)'*e_hat)

% f.
sum((cps85(:, [1, 7, 10, 11])*((cps85(:, [1, 7, 10, 11])'*cps85(:, [1, 7, 10, 11]))^-1)*cps85(:, [1, 7, 10, 11])'*cps85(:, 9) )'*e_hat)

% g.
e_hat'*e_hat

% h.
1-e_hat'*e_hat/sum((cps85(:, 9)-mean(cps85(:, 9)))'*(cps85(:, 9)-mean(cps85(:, 9))))

% 3.20

beta1 = ((cps85(:, [ 7, 10, 11])'*cps85(:, [7, 10, 11]))^-1)*cps85(:, [7, 10, 11])'*cps85(:, 9) 
e_hat1= cps85(:, 9)-cps85(:, [7, 10, 11])*((cps85(:, [7, 10, 11])'*cps85(:, [7, 10, 11]))^-1)*cps85(:, [7, 10, 11])'*cps85(:, 9) ;


beta2 = ((cps85(:, [ 7, 10, 11])'*cps85(:, [7, 10, 11]))^-1)*cps85(:, [7, 10, 11])'*cps85(:, 1) 
e_hat2= cps85(:, 1)-cps85(:, [7, 10, 11])*((cps85(:, [7, 10, 11])'*cps85(:, [7, 10, 11]))^-1)*cps85(:, [7, 10, 11])'*cps85(:, 1) ;

beta3 = ((e_hat2'*e_hat2)^-1)*e_hat2'*e_hat1
e_hat3= e_hat1-e_hat2*((e_hat2'*e_hat2)^-1)*e_hat2'*e_hat1 ;

1-e_hat3'*e_hat3/sum((e_hat1-mean(e_hat1))'*(e_hat1-mean(e_hat1)))

e_hat3'*e_hat3

