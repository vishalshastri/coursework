

urlwrite('http://www.ssc.wisc.edu/~bhansen/710/invest.dat','invest.dat');
load 'invest.dat';


%% 8.3.a

%invest = horzcat( invest(:, 1), repmat(1, size(invest, 1), 1), invest(:, 2:end));

beta = ((invest(:, 2:end)'*invest(:, 2:end))^-1)*invest(:, 2:end)'*invest(:, 1) ;

% df = size(invest, 1) - 4; 
df = size(invest, 1) - 3;                       
ehat = invest(:, 1) - invest(:, 2:end) * beta;                          
sse = ehat'*ehat;                         
sighat2 = sse/df;                         
  
covb = sighat2*inv(invest(:, 2:end)'*invest(:, 2:end));          

stbls = sqrt(diag(covb));	        


%paramnames = {'constant', 'tobin_Q','cash_asset','debt_asset'}
paramnames = {'tobin_Q','cash_asset','debt_asset'}

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

chi_bwg(0.05,7,wald_stat);

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

invest_quad = horzcat( invest(:, 1), ...
  invest(:, 2).^2, ...
  invest(:, 3).^2, ...
  invest(:, 4).^2, ...
  invest(:, 2) .* invest(:, 3), ...
  invest(:, 2) .* invest(:, 4), ...
  invest(:, 3) .* invest(:, 4), ...  
  );


beta = ((invest_quad(:, 2:end)'*invest_quad(:, 2:end))^-1)*invest_quad(:, 2:end)'*invest_quad(:, 1) ;

df = size(invest, 1) - 9;                       
ehat = invest_quad(:, 1) - invest_quad(:, 2:end) * beta;                          
sse = ehat'*ehat;                         
sighat2 = sse/df;                         
  
covb = sighat2*inv(invest_quad(:, 2:end)'*invest_quad(:, 2:end));          

stbls = sqrt(diag(covb));	

wald_stat = beta(4:end)' * inv( [zeros(6,1) eye(6)] * ...
   covb * [zeros(6,1) eye(6)]' ) * beta(4:end);

chi_bwg(0.05,7,wald_stat);

w_test_p_val = 1 - chi2cdf(wald_stat, 2);
fprintf('Wald Stat. (H_0: All interaction and quadratic coef. are zero): %10.4f \n', wald_stat);
fprintf('Prob Wald Stat. Assum. H_0:            %10.4f \n', w_test_p_val);
if w_test_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end

p. 183
reg for classes


%% 8.4.a


urlwrite('http://www.ssc.wisc.edu/~bhansen/710/nerlov.dat','nerlov.dat');
load 'C:\Users\tdmcarthur\Downloads\nerlov.dat';

nerlov_tranf = horzcat( log(nerlov(:, 1)), ...
  repmat(1, size(nerlov, 1), 1), ...
  log(nerlov(:,2)), ...
  log(nerlov(:,3)), ...
  log(nerlov(:,4)), ...
  log(nerlov(:,5)) ...
  ) ;
  
beta = ((nerlov_tranf(:, 2:end)'*nerlov_tranf(:, 2:end))^-1)*nerlov_tranf(:, 2:end)'*nerlov_tranf(:, 1) ;

df = size(nerlov_tranf, 1) - 5;                       
ehat = nerlov_tranf(:, 1) - nerlov_tranf(:, 2:end) * beta;                          
sse = ehat'*ehat;                         
sighat2 = sse/df;                         

%covb = sighat2*inv(nerlov_tranf(:, 2:end)'*nerlov_tranf(:, 2:end));          
covb = inv(nerlov_tranf(:, 2:end)'*nerlov_tranf(:, 2:end)) * ...
  ( nerlov_tranf(:, 2:end)' * diag(ehat.^2)  * nerlov_tranf(:, 2:end) ) * ...
  inv(nerlov_tranf(:, 2:end)'*nerlov_tranf(:, 2:end)) ... 
   ;  
% No DF?
 


paramnames = {'log_Q','log_PL', 'log_PK', 'log_PF'}

for i=1:length(paramnames)
 fprintf('Param estimate for %s is: %3.4f \n', ...
   paramnames{i}, beta(i)   )
end

for i=1:length(paramnames)
 fprintf('Standard error for %s is: %3.4f \n', ...
   paramnames{i}, stbls(i)   )
end

%% 8.4.c

%b3+b4+b5 = 1

x_mat_temp = nerlov_tranf(:, 2:end);
r_mat = [zeros(3,2) eye(3)]';

beta_cls = beta - inv(x_mat_temp'*x_mat_temp)*r_mat* ...     
  inv(r_mat'*inv(x_mat_temp'*x_mat_temp)*r_mat)* ...
  (r_mat'*beta-1)


%% 8.4.d

%b3+b4+b5 = 1


beta_emd = beta - covb*r_mat* ...     
  inv(r_mat'*covb*r_mat)* ...
  (r_mat'*beta-1)

%% 8.4.e


wald_stat = (r_mat'*beta-1)' * inv( r_mat * ...
   covb * r_mat' ) * (r_mat'*beta-1);

chi_bwg(0.05,7,wald_stat);

w_test_p_val = 1 - chi2cdf(wald_stat, 2);
fprintf('Wald Stat. (H_0: All interaction and quadratic coef. are zero): %10.4f \n', wald_stat);
fprintf('Prob Wald Stat. Assum. H_0:            %10.4f \n', w_test_p_val);
if w_test_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end
	

%% 8.4.f

Test H0 : b3 +b4 +b5 = 1 using a minimum distance statistic

Since the hypothesis is linear, the mimimu distance statistic Jn is the same as the Wald statistic

%% 9.2.a




urlwrite('http://www.ssc.wisc.edu/~bhansen/710/nerlov.dat','nerlov.dat');
load 'C:\Users\tdmcarthur\Downloads\nerlov.dat';

nerlov_tranf = horzcat( log(nerlov(:, 1)), ...
  repmat(1, size(nerlov, 1), 1), ...
  log(nerlov(:,2)), ...
  log(nerlov(:,3)), ...
  log(nerlov(:,4)), ...
  log(nerlov(:,5)), ...
  log(nerlov(:,2)).^2 ...
  ) ;
  
beta = ((nerlov_tranf(:, 2:end)'*nerlov_tranf(:, 2:end))^-1)*nerlov_tranf(:, 2:end)'*nerlov_tranf(:, 1) ;

df = size(nerlov_tranf, 1) - 6;                       
ehat = nerlov_tranf(:, 1) - nerlov_tranf(:, 2:end) * beta;                          
sse = ehat'*ehat;                         
sighat2 = sse/df;                         

%covb = sighat2*inv(nerlov_tranf(:, 2:end)'*nerlov_tranf(:, 2:end));          
covb = inv(nerlov_tranf(:, 2:end)'*nerlov_tranf(:, 2:end)) * ...
  ( nerlov_tranf(:, 2:end)' * diag(ehat.^2)  * nerlov_tranf(:, 2:end) ) * ...
  inv(nerlov_tranf(:, 2:end)'*nerlov_tranf(:, 2:end)) ... 
  ;  

stbls = sqrt(diag(covb));
tvalue=beta./stbls; 
pvalue=2*(1-tcdf(abs(tvalue),df)); 

pvalue(6)

%% 9.2.b

%x_mat_temp = nerlov_tranf(:, 2:end);
%r_mat = [zeros(3,2) eye(3)]';

%beta_cls = beta - inv(x_mat_temp'*x_mat_temp)*r_mat* ...     
%  inv(r_mat'*inv(x_mat_temp'*x_mat_temp)*r_mat)* ...
%  (r_mat'*beta-1)


nerlov_tranf = horzcat( log(nerlov(:, 1)), ...
  repmat(1, size(nerlov, 1), 1), ...
  log(nerlov(:,2)), ...
  log(nerlov(:,3)), ...
  log(nerlov(:,4)), ...
  log(nerlov(:,5)) ...
  ) ;
  


grid_vals = 2:.1:2.5;

min_b7 = 1000;
min_b6  = 1000;
min_sse = 1000;
min_ehat = 1000;


for i=1:length(grid_vals)

  nerlov_nls = horzcat( nerlov_tranf, ...
    log(nerlov(:,2)) .* (1+exp(-log(nerlov(:,2))-grid_vals(i))).^-1 ...
    );


  beta_nls = ((nerlov_nls(:, 2:end)'*nerlov_nls(:, 2:end))^-1)*nerlov_nls(:, 2:end)'*nerlov_nls(:, 1);
  
  x_mat_temp = nerlov_nls(:, 2:end);
  r_mat = [zeros(3,2) eye(3) zeros(3,1)]';

  beta_cls_nls = beta_nls - inv(x_mat_temp'*x_mat_temp)*r_mat* ...     
    inv(r_mat'*inv(x_mat_temp'*x_mat_temp)*r_mat)* ...
    (r_mat'*beta_nls-1);
  
  ehat = nerlov_nls(:, 1) - nerlov_nls(:, 2:end) * beta_cls_nls;                          
  sse = ehat'*ehat;
  
  fprintf('%3.1f: %3.5f\n',grid_vals(i), sse)
  
  if sse < min_sse
    min_b7 = grid_vals(i);
    min_b6 = beta_cls_nls(5);
    min_sse = sse;
    min_ehat = ehat;
  end

end



%% 9.2.c

% z = Grad(betas,func_name,numobs, dh, x_mat);

m_deriv = horzcat(  ...
  repmat(1, size(nerlov, 1), 1), ...
  log(nerlov(:,2)), ...
  log(nerlov(:,3)), ...
  log(nerlov(:,4)), ...
  log(nerlov(:,5)), ...
  log(nerlov(:,2)) .* (1+exp(-log(nerlov(:,2))-min_b7)).^-1 , ...
  min_b7 .* log(nerlov(:,2)) .* - ...
    (nerlov(:,2).*exp(min_b7)./(nerlov(:,2)+exp(min_b7)).^2) ...
  ) 


 
covb = inv(m_deriv' * m_deriv) * (m_deriv' * diag(min_ehat.^2) * m_deriv) * inv(m_deriv' * m_deriv);

stbls = sqrt(diag(covb))



%% 9.3.a.

urlwrite('http://www.ssc.wisc.edu/~bhansen/710/cps78.dat','cps78.dat');
load 'cps78.dat';


cps78_tranf = horzcat(cps78(:,11), cps78(:,[1:10 12:20]) );


beta = ((cps78_tranf(:, 2:end)'*cps78_tranf(:, 2:end))^-1)*cps78_tranf(:, 2:end)'*cps78_tranf(:, 1) ;

% df = size(invest, 1) - 4; 
df = size(cps78_tranf, 1) - size(cps78_tranf(:, 2:end), 2);                       
ehat = cps78_tranf(:, 1) - cps78_tranf(:, 2:end) * beta;                          
sse = ehat'*ehat;                         
sighat2 = sse/df;                         
  
covb = sighat2*inv(cps78_tranf(:, 2:end)'*cps78_tranf(:, 2:end));          

stbls = sqrt(diag(covb));	        


paramnames = {'ED' 'SOUTH' 'NONWH' 'HISP' 'FE' 'MARR' 'MARRFE' 'EX' 'EXSQ' 'UNION' 'AGE' 'NDEP' 'MANUF' 'CONSTR' 'MANAG' 'SALES' 'CLER' 'SERV' 'PROF'}

for i=1:length(paramnames)
 fprintf('Param est. and st. error for %6s is: %6.4f  %6.4f \n', ...
   paramnames{i}, beta(i), stbls(i)   )
end


%% 9.3.b.


cps78_tranf = horzcat(cps78_tranf, ...
  cps78_tranf(:, 2).^2, ...
  cps78_tranf(:, 2) .* cps78_tranf(:, 6), ...
  cps78_tranf(:, 2).^2 .* cps78_tranf(:, 6), ...
  cps78_tranf(:, 6) .* cps78_tranf(:, 13) ...
);


beta = ((cps78_tranf(:, 2:end)'*cps78_tranf(:, 2:end))^-1)*cps78_tranf(:, 2:end)'*cps78_tranf(:, 1) ;

% df = size(invest, 1) - 4; 
df = size(cps78_tranf, 1) - size(cps78_tranf(:, 2:end), 2);                       
ehat = cps78_tranf(:, 1) - cps78_tranf(:, 2:end) * beta;                          
sse = ehat'*ehat;                         
sighat2 = sse/df;                         
  
covb = sighat2*inv(cps78_tranf(:, 2:end)'*cps78_tranf(:, 2:end));          

stbls = sqrt(diag(covb));	

tvalue=beta./stbls; 
pvalue=2*(1-tcdf(abs(tvalue),df)); 


paramnames = {'ED' 'SOUTH' 'NONWH' 'HISP' 'FE' 'MARR' 'MARRFE' 'EX' 'EXSQ' 'UNION' 'AGE' 'NDEP' 'MANUF' 'CONSTR' 'MANAG' 'SALES' 'CLER' 'SERV' 'PROF' 'EDsq' 'EDxFE' 'EDsqxFE' 'NDEPxFE'}

for i=1:length(paramnames)
 fprintf('Beta, sterr and p-val for %7s is: %6.4f  %6.4f %1.4f \n', ...
   paramnames{i}, beta(i), stbls(i), pvalue(i)   )
end


%% 9.3.c.


cps78_tranf = cps78_tranf(:, [1:2 4 6:17 19:24]);


beta = ((cps78_tranf(:, 2:end)'*cps78_tranf(:, 2:end))^-1)*cps78_tranf(:, 2:end)'*cps78_tranf(:, 1) ;

% df = size(invest, 1) - 4; 
df = size(cps78_tranf, 1) - size(cps78_tranf(:, 2:end), 2);                       
ehat = cps78_tranf(:, 1) - cps78_tranf(:, 2:end) * beta;                          
sse = ehat'*ehat;                         
sighat2 = sse/df;                         
  
covb = sighat2*inv(cps78_tranf(:, 2:end)'*cps78_tranf(:, 2:end));          

stbls = sqrt(diag(covb));	

tvalue=beta./stbls; 
pvalue=2*(1-tcdf(abs(tvalue),df)); 


paramnames = {'ED' 'NONWH' 'FE' 'MARR' 'MARRFE' 'EX' 'EXSQ' 'UNION' 'AGE' 'NDEP' 'MANUF' 'CONSTR' 'MANAG' 'SALES'  'SERV' 'PROF' 'EDsq' 'EDxFE' 'EDsqxFE' 'NDEPxFE'};

for i=1:length(paramnames)
 fprintf('Beta, sterr and p-val for %7s is: %6.4f  %6.4f %1.4f \n', ...
   paramnames{i}, beta(i), stbls(i), pvalue(i)   )
end


%% 9.3.d.


z_mat = horzcat( ...
  repmat(1, size(cps78_tranf, 1), 1), ...
  cps78_tranf(:, 4) ...
  );

alpha_tilde = inv(z_mat'*z_mat) * z_mat' * ehat.^2;
% Skedastic regression

df = size(z_mat, 1) - size(z_mat, 2);                       
ehat_sked = cps78_tranf(:, 1) - z_mat * alpha_tilde;                          
sse = ehat_sked'*ehat_sked;                         
sighat2 = sse/df;                         
  
covb = sighat2*inv(z_mat'*z_mat);          

stbls = sqrt(diag(covb));	
tvalue=alpha_tilde(2)./stbls(2);                      
pvalue=2*(1-tcdf(abs(tvalue),df)) 


%% 9.3.e.

z_mat = horzcat( ...
  repmat(1, size(cps78_tranf, 1), 1), ...
  cps78_tranf(:, 3) ...
  );

alpha_tilde = inv(z_mat'*z_mat) * z_mat' * ehat.^2;
% Skedastic regression

df = size(z_mat, 1) - size(z_mat, 2);                       
ehat_sked = cps78_tranf(:, 1) - z_mat * alpha_tilde;                          
sse = ehat_sked'*ehat_sked;                         
sighat2 = sse/df;                         
  
covb = sighat2*inv(z_mat'*z_mat);          

stbls = sqrt(diag(covb));	
tvalue=alpha_tilde(2)./stbls(2);                      
pvalue=2*(1-tcdf(abs(tvalue),df)) 

%% 9.3.f.

z_mat = horzcat( ...
  repmat(1, size(cps78_tranf, 1), 1), ...
  cps78_tranf(:, 7).^2, ...
  cps78_tranf(:, 10).^2, ...
  cps78_tranf(:, 11).^2 ...
  );

alpha_tilde = inv(z_mat'*z_mat) * z_mat' * ehat.^2;
% Skedastic regression

sigma_tilde = z_mat * alpha_tilde;

sigma_tilde( sigma_tilde < sighat2 * .1) = sighat2 * .1;


df = size(z_mat, 1) - size(z_mat, 2);                       
ehat_sked = cps78_tranf(:, 1) - z_mat * alpha_tilde;                          
sse = ehat_sked'*ehat_sked;                         
sighat2 = sse/df;                         
  
covb = sighat2*inv(z_mat'*z_mat);          

stbls = sqrt(diag(covb));	

wald_stat = alpha_tilde(2:4)' * inv( [zeros(3,1) eye(3)] * ...
   covb * [zeros(3,1) eye(3)]' ) * alpha_tilde(2:4);

chi_bwg(0.05,7,wald_stat);

w_test_p_val = 1 - chi2cdf(wald_stat, 2);
fprintf('Wald Stat. (H_0: cash_asset = debt_asset = 0): %10.4f \n', wald_stat);
fprintf('Prob Wald Stat. Assum. H_0:            %10.4f \n', w_test_p_val);
if w_test_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end


%% 9.3.g.



beta = ((cps78_tranf(:, 2:end)'*diag(sigma_tilde)*cps78_tranf(:, 2:end))^-1)*cps78_tranf(:, 2:end)'*diag(sigma_tilde)*cps78_tranf(:, 1);


% df = size(invest, 1) - 4; 
df = size(cps78_tranf, 1) - size(cps78_tranf(:, 2:end), 2);                       
ehat = cps78_tranf(:, 1) - cps78_tranf(:, 2:end) * beta;                          
sse = ehat'*ehat;                         
sighat2 = sse/df;                         
  
covb = sighat2*inv(cps78_tranf(:, 2:end)'*cps78_tranf(:, 2:end));          

stbls = sqrt(diag(covb));	

tvalue=beta./stbls; 
pvalue=2*(1-tcdf(abs(tvalue),df)); 


paramnames = {'ED' 'NONWH' 'FE' 'MARR' 'MARRFE' 'EX' 'EXSQ' 'UNION' 'AGE' 'NDEP' 'MANUF' 'CONSTR' 'MANAG' 'SALES'  'SERV' 'PROF' 'EDsq' 'EDxFE' 'EDsqxFE' 'NDEPxFE'};

for i=1:length(paramnames)
 fprintf('Beta, sterr and p-val for %7s is: %6.4f  %6.4f %1.4f \n', ...
   paramnames{i}, beta(i), stbls(i), pvalue(i)   )
end


%% 9.3.h.







EX age ndep




SOUTH
HISP
CLER





ED sq
ED and FE interact
ED sq and FE interact

NDEP  and FE interact







http://www.ssc.wisc.edu/~bhansen/710/cps78.pdf













paramnames = {'log_Q','log_PL', 'log_PK', 'log_PF'}

for i=1:length(paramnames)
 fprintf('Param estimate for %s is: %3.4f \n', ...
   paramnames{i}, beta(i)   )
end

for i=1:length(paramnames)
 fprintf('Standard error for %s is: %3.4f \n', ...
   paramnames{i}, stbls(i)   )
end











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

