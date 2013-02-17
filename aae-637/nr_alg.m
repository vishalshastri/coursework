
function[b, covb] = nr_alg(betas,y,parname2, critic_limit, iter_limit, do_step, func_name, x_mat)

crit = 1;
iter = 1;

while (critic_limit < crit ) &&  (iter < iter_limit);    % Begin do loop 
     nonlinf = func_name(betas, x_mat);      % Define nonlinear function 
     %z= x_1 + 2 * b0 * x_2;                 % Derivative wrt beta 
     u = y - nonlinf;           % Compute residual vector 
     %numer=u'*z;                     % Numerator of JHGLL,eq.12.2.74
     %denom=z'*z -2*u'*x_2;           % Denomenator of JHGLL,eq.12.2.74
     sse = u'*u;                     % Sum of squared errors
     z = Grad(betas,func_name,size(x_mat,1), .000001, x_mat); % defined at JHGLL, eq. 12.2.34
     targ_grad = -2*z'*u; % defined at 12.2.33. This is not right
     targ_hess = model_hess(func_name, betas, x_mat, y);
     
     sl = inv(targ_hess) * targ_grad;               % Compute full step adjustment
     if do_step == 1;
       s = 1;
       ss1 = 1; ss2 = 2;
      while (ss1 < ss2) % && (s >=.2);  % ** Loop to determine step length
         u1 = y - func_name(betas - s*sl/2, x_mat) ;% ** Error w/SL/2 & curr. betas
         u2 = y - func_name(betas - s*sl, x_mat);  % ** Error w/SL & curr. betas
         ss1 = u1'*u1;                    % ** SSE w/SL/2 & curr. betas
         ss2 = u2'*u2;                    % ** SSE w/SL & curr. betas
         s_star=s;
         s = s/2;                         % ** Update SL for next pass
       end 
%       while (ss1 < ss2) && (v >=.1);
%          v = v/2;
%          b0_1= b0+ v * sl;
%          b0_2= b0 + v* sl/2;
%          nonlinf_1= b0_1 * x_1 + (b0_1^2) * x_2;
%          nonlinf_2= b0_2 * x_1 + (b0_2^2) * x_2;
%          u1 = y - nonlinf_1;
%          u2 = y - nonlinf_2;
%          ss1 = u1'*u1;
%          ss2 = u2'*u2; 
%          v_lag=v;
%       end 
     end   
     b = betas - s_star*sl; % I think we subtract here
     fprintf('i = %3.0f\r', iter);   % Print iteration number
     fprintf('SSE = %8.4f\r', sse);  % Print current SSE
     fprintf('step = %5.4f\r', s);
     fprintf('b = %8.4f\r',betas);      % Print current paramater estimate
     disp(' ')
     if iter == iter_limit-1;
        disp('***Warning:  Exceeded Iteration Limit***'); % May not be at optimum 
     end
     iter = iter + 1;                % Update iteration count
     crit = max(abs((b - betas)./betas));  % Evaluate change in coefficients 
     betas = b;                         % Make curret beta the new beta 
end

  %**** Compute covariance matrix *****
  sighat2 = sse/(size(y, 1) - size(betas, 1));    % Unbiased est of error variance
  covb = inv(z'*z).*sighat2;     % Coefficient cov matrix, JHGLL 12.2.43a             
  % **** Print out Final Results ******/
  fprintf('Final Results:  ');
  disp(' ');
  stbls=sqrt(diag(covb));        % Column vector of param. std. errors
  tvalue=b./stbls;               % Column vector of t-values
  df= size(y, 1)-1;                  % Degrees of freedom for t-value
  pvalue=2*(1-tcdf(abs(tvalue),df));  % Column vector of param p-values 
  results=horzcat(b,stbls,tvalue,pvalue); % Build results matrix
  table_bwg(parname2,results,1);
  disp('  ')
  fprintf('Unbiased estimate of error variance:  %10.4f\r', sighat2);
  ybar = mean(y);
  sst = y'*y - size(y, 1)*(ybar^2);
  r2 = 1 - (sse/sst);
  disp('Note:  The R^2 value may not be between 0-1')
  fprintf('            R-Squared: %4.3f\r', r2);
  disp('  ')
  disp('Variance-Covariance Matrix for Estimated Coeff.:')
  disp(covb);
  %**********************************************************/ 
