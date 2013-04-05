

urlwrite('http://www.ssc.wisc.edu/~bhansen/710/invest.dat','invest.dat');
load 'invest.dat';


%% 8.3.a

invest = horzcat ( invest(:, 1), repmat(1, size(invest, 1), 1), invest(:, 2:end));

beta = ((invest(:, 2:end)'*invest(:, 2:end))^-1)*invest(:, 2:end)'*invest(:, 1) ;

% df = size(invest, 1) - 4; 
df = size(invest, 1) - 3;                       
ehat = invest(:, 1) - invest(:, 2:end) * beta;                          
sse = ehat'*ehat;                         
sighat2 = sse/df;                         
  
covb = sighat2*inv(invest(:, 2:end)'*invest(:, 2:end));          

stbls = sqrt(diag(covb));	        


paramnames = {'constant', 'tobin_Q','cash_asset','debt_asset'}
%paramnames = {'tobin_Q','cash_asset','debt_asset'}

for i=1:length(paramnames)
 fprintf('Param estimate for %s is: %3.4f \n', ...
   paramnames{i}, beta(i)   )
end

for i=1:length(paramnames)
 fprintf('Standard error for %s is: %3.4f \n', ...
   paramnames{i}, stbls(i)   )
end

%% 8.3.b

for i=1:length(paramnames)
 fprintf('Confidence interval for %s is: [ %3.4f , %3.4f] \n', ...
   paramnames{i}, beta(i) - 1.96 * stbls(i)   , beta(i) + 1.96 * stbls(i)   )
end

%% 8.3.c

wald_stat = beta(2:3)' * inv( [zeros(2,1) eye(2)] * ...
   covb * [zeros(2,1) eye(2)]' ) * beta(2:3);

chi_bwg(0.05,7,fluid_wald_stat);

w_test_p_val = 1 - chi2cdf(wald_stat, 2);
fprintf('Wald Stat. (H_0: cash_asset = debt_asset = 0): %10.4f \n', wald_stat);
fprintf('Prob Wald Stat. Assum. H_0:            %10.4f \n', w_test_p_val);
if w_test_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end


tvalue=beta(1)./stbls(1);                      
pvalue=2*(1-tcdf(abs(tvalue),df));  

fprintf('T Stat. (H_0: tobin_Q = 0): %10.4f \n', tvalue);
fprintf('Prob T Stat. Assum. H_0:            %10.4f \n', pvalue);
if pvalue < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end 

%% 8.3.d








% a.
e_hat= invest(:, 9)-invest(:, [1, 7, 10, 11])*((invest(:, [1, 7, 10, 11])'*invest(:, [1, 7, 10, 11]))^-1)*invest(:, [1, 7, 10, 11])'*invest(:, 9) ;
sum(e_hat)

% b.
sum(invest(:, 1)'*e_hat)

% c.
sum(invest(:, 7)'*e_hat)

% d.
sum((invest(:, 1).^2)'*e_hat)

% e.
sum(invest(:, 10)'*e_hat)

% f.
sum((invest(:, [1, 7, 10, 11])*((invest(:, [1, 7, 10, 11])'*invest(:, [1, 7, 10, 11]))^-1)*invest(:, [1, 7, 10, 11])'*invest(:, 9) )'*e_hat)

% g.
e_hat'*e_hat

% h.
1-e_hat'*e_hat/sum((invest(:, 9)-mean(invest(:, 9)))'*(invest(:, 9)-mean(invest(:, 9))))

% 3.20

beta1 = ((invest(:, [ 7, 10, 11])'*invest(:, [7, 10, 11]))^-1)*invest(:, [7, 10, 11])'*invest(:, 9) 
e_hat1= invest(:, 9)-invest(:, [7, 10, 11])*((invest(:, [7, 10, 11])'*invest(:, [7, 10, 11]))^-1)*invest(:, [7, 10, 11])'*invest(:, 9) ;


beta2 = ((invest(:, [ 7, 10, 11])'*invest(:, [7, 10, 11]))^-1)*invest(:, [7, 10, 11])'*invest(:, 1) 
e_hat2= invest(:, 1)-invest(:, [7, 10, 11])*((invest(:, [7, 10, 11])'*invest(:, [7, 10, 11]))^-1)*invest(:, [7, 10, 11])'*invest(:, 1) ;

beta3 = ((e_hat2'*e_hat2)^-1)*e_hat2'*e_hat1
e_hat3= e_hat1-e_hat2*((e_hat2'*e_hat2)^-1)*e_hat2'*e_hat1 ;

1-e_hat3'*e_hat3/sum((e_hat1-mean(e_hat1))'*(e_hat1-mean(e_hat1)))

e_hat3'*e_hat3

