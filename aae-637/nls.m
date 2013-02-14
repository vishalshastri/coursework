%{
 ***************************************************************
 **** PROC NLS  ---  Non-linear Least Squares               ****
 ****                using numerical derivatives            ****
 ***************************************************************
%}
function[betas,covb] =  nls(betas,y,names, critic_limit, iter_limit, numobs, do_step, func_name, x_mat, dh)
%    global critic_limit iter_limit z numobs do_step func_name ;
    crit = 1;                   % ** Initialize critical % coef. change **
    s=1;                        % ** Initialize step length **
    iter = 1;                   % ** Initialize iteration count **
    k = length(betas);          % ** Number of parameters **
    while (critic_limit < crit ) &&  (iter < iter_limit); %** Begin loop **
     % ********  Compute numerical gradients **********************
     z = Grad(betas,func_name,numobs, dh, x_mat);
     % ************************************************************
     u_lag = y - func_name(betas, x_mat); %** Compute errors| previous betas **
     sl=(z'*z)\(z'*u_lag);      % ** step length:  JHGLL,eq.12.2.74 *
                                % ** Could have used:  sl=inv(z'*z)*z'*u
     if do_step == 1;           % ** Use a variable step length **
       s = 1;                   % ** Initialize base step length of 1 **
       ss1 = 1; ss2 = 2;        % ** Intialize SSE's under 2 step lengths
       while (ss1 < ss2) && (s >=.2);  % ** Loop to determine step length
         u1 = y - func_name(betas + s*sl/2, x_mat) ;% ** Error w/SL/2 & curr. betas
         u2 = y - func_name(betas + s*sl, x_mat);  % ** Error w/SL & curr. betas
         ss1 = u1'*u1;                    % ** SSE w/SL/2 & curr. betas
         ss2 = u2'*u2;                    % ** SSE w/SL & curr. betas
         s_star=s;
         s = s/2;                         % ** Update SL for next pass
       end 
     end 
     b = betas + s_star*sl;           % ** Update parameters from prevoius **
     u = y - func_name(b, x_mat);      % ** Create errors given current betas **
     sse = u'*u;                 % ** Calc. SSE's given current betas **
     % *** Printing Intermediate Results ****
     diary on;                        % Turn on output to file
     fprintf('i = %3.0f\r', iter);    % Print iteration number
     fprintf('SSE = %8.4f\r', sse);   % Print current SSE
     fprintf('step = %5.4f\r', s_star); % Print "current" step length
     fprintf('Parameters \r');        % Print current paramater estimate
     vv=1;
    	if length(betas)>7;           % Do we have more than 7 parameters?
     		numloop=floor(length(betas)./7); % How many group of 7 parameters?
            resid=length(betas)-numloop*7; % How many parameters
                                           % after the last group of 7
     		while vv<=numloop;
    			disp(b((vv-1)*7+1:vv*7,:)');  % Display 7 parameters 
    			vv=vv+1; 
            end
            if resid>0;     % After the last group of 7 print out remaining
                disp(b((vv-1)*7+1:rows(betas),:)');
                disp(' ')
                disp(' ')
            end
        else
         disp(b');              % Print out all paramatrs if less than 7
        end
     iter = iter + 1;                % ** Update for next iteration**
     crit = max(abs((b - betas)./betas));  % ** Evaluate change in coeff. ** 
     betas = b;                      % ** Create lag betas **
     hessian = model_hess(func_name, betas, x_mat)
     [throwaway,definiteness] = chol(hessian)
     if definiteness==0
       display('*******REACHED CONVEX SECTION********'
       break
     end
    end

  %**** Compute covariance matrix *****
  sighat2 = sse/(numobs - k);    % Unbiased est of error variance
  covb = inv(z'*z).*sighat2;     % Coefficient cov matrix, JHGLL 12.2.43a             
  % **** Print out Final Results ******/
  fprintf('Final Results:  ');
  disp(' ');
  stbls=sqrt(diag(covb));        % Column vector of param. std. errors
  tvalue=b./stbls;               % Column vector of t-values
  df= numobs-1;                  % Degrees of freedom for t-value
  pvalue=2*(1-tcdf(abs(tvalue),df));  % Column vector of param p-values 
  results=horzcat(b,stbls,tvalue,pvalue); % Build results matrix
  table_bwg(names,results,1);
  disp('  ')
  fprintf('Unbiased estimate of error variance:  %10.4f\r', sighat2);
  ybar = mean(y);
  sst = y'*y - numobs*(ybar^2);
  r2 = 1 - (sse/sst);
  disp('Note:  The R^2 value may not be between 0-1')
  fprintf('            R-Squared: %4.3f\r', r2);
  disp('  ')
  disp('Variance-Covariance Matrix for Estimated Coeff.:')
  disp(covb);
  diary off;
  %**********************************************************/ 
end
            