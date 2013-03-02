function[betas,covb,rho]=crm(x,y,names,intercept)     
   [numr,numc]=size(x);                 % Original size of the exogenous data matrix
 %  if intercept == 1;                        % 1 indicates an intercept, 0 no intercept
 %  	    x = horzcat(ones(numr,1),x); % Add a vector of 1's
 %       numc=numc+1;                      % Increase the column count by 1
 %       else;   
 %           names=names(2:numc);   % First coefficient is the intercept
 %  end                                              % Terminate if loop
   betas = inv(x'*x)*x'*y;                   % Estimated Coefficients, CRM Formula 
   df = numr - numc;                       % Degrees of Freedom 
   ehat = y - x*betas;                          % Error Vector
   sse = ehat'*ehat;                          % Sum of Squared Errors 
   sighat2 = sse/df;                         %Unbiased Estimate of Error Variance  
   % Is this sighat2 above right?
   covb = sighat2*inv(x'*x);           % Parameter Covariance Matrix 
   ybar = mean(y);                          % Mean of the dependant variable
   tss = y'*y - numr*(ybar^2);         % Total Sum of Squared Deviations from Mean 
   r2 = 1 - (sse/tss);                        % Coefficient of Determination 
   rbar2 = 1 - (numr-1)*(1-r2)/df;  % Adjusted Coef. of Det.
   stbls = sqrt(diag(covb));	        % Coefficient Standard Errors 
   tvalue=betas./stbls;                        % Coefficient t-values w/ H0:  Beta=0 
   pvalue=2*(1-tcdf(abs(tvalue),df));   % Coefficient p-value (two tail) 

   R = [zeros(numc-1,1) eye(numc-1)];
   
   f_stat= (R * betas)' * inv(R * ((sse/(numr-numc-1)) .* inv(x'*x)) * R') * (R * betas);
   % By eq. 5-16 of Greene, p. 159
   
   dw_stat = sum( (ehat(2:numr)-ehat(1:(numr-1)) .^2)) / sse;
   
   rho = ( ehat(2:numr)' * ehat(1:(numr-1)) ) / sum( ehat(1:(numr-1)) .^2 );
   % by equation 20-19 of Greene (p. 962)
   
   rho_z = rho /  sqrt( (1-rho^2)/numr ) ;
   % Not sure about the above formula
   
   disp('  ');
   disp('  ');
   disp('Final results:');
   results=horzcat(betas,stbls,tvalue,pvalue); % Build results matrix
   table_bwg(names,results,9);
   disp('  ');
   fprintf('Unadjusted R-squared: %1.4f', r2)
   disp('  ');
   fprintf('Adjusted R-squared: %1.4f', rbar2)
   disp('  ');
   fprintf('F-Statistic: %4.4f', f_stat)
   disp('  ');
   fprintf('Durbin-Watson Statistic: %1.4f', dw_stat)
   disp('  ');
   fprintf('rho estimate: %1.4f', rho)
   disp('  ');
   fprintf('rho z-statistic: %3.4f', rho_z)
   disp('  ');
   
   
end