function [bn,cov_p,LLF_vec] = probit_bwg(b0)
%{
**************************************************************************
** At this point there are several ways to proceed.  The general
** optimization algorithms of Ch. 12 in Greene could be used to maximize    
** the log-likelihood func. or an algorithm specifically for probit could  
** be written, which entails programming the first and second derivatives.  
** This program does the later.  PROC PROBIT returns the probit ML        
** estimates and their asymptotic covariance matrix using (JHGLL 19.2.20) 
** and the derivatives given in JHGLL (19.2.19) and (19.2.21).            
** Note: This is the Newton-Raphson Algorithm                                
**************************************************************************
%}
global numobs iter_limit critic_limit func_name do_step parname rhsvar ...
            depend numc2 iter;
   crit = 1;              % Initialize critical value of % beta change
   iter = 1;              % Initialize iteration count
   s = 1;                 % Initialize step length
   disp('Below is the intermediate iteration information:');     
   disp('  ');
   diary on;                     % ***  Turn on output to file ***
       %******* Begin do loop *********
   while ((crit > critic_limit) & (iter<= iter_limit));                 
       arg=rhsvar*b0;
       pdf_arg = normpdf(arg); %*** PDF ***/
       cdf_arg = normcdf(arg); %*** CDF ***/
             
       g1 = depend.*(pdf_arg./cdf_arg);
       g12=repmat(g1,1,numc2).*rhsvar;
       g2 = -(1-depend).*(pdf_arg./(1-cdf_arg));
       g22=repmat(g2,1,numc2).* rhsvar;             
       g=g12+g22;                  %***Gradient vector,JHGLL,(19.2.19)**
       g = sum(g)'; 
            			% **Hessian Matrix. JHGLL, Eq. 19.2.21**
             			% *** H = -X'DX where D is diagonal ****
       D = pdf_arg.*( (depend.*(pdf_arg+(arg).*cdf_arg)./cdf_arg.^2)...
             +((1-depend).*(pdf_arg-(arg).*(1-cdf_arg))./(1-cdf_arg).^2));
       H = -(rhsvar .* repmat(D,1,numc2))'*rhsvar;
       db = -inv(H)*g;                % ***  Full Newton-Raphson step  ***
       if do_step == 1;               % Use a variable step length 
         s = 2;                       % Reset base step length 
         li1 = 0; li2 = 1;            % ** Intialize LLF's under 2 step lengths
           while (li1 <= li2) && (s >=.2); % ** Loop to determine step length
              s = s/2;
              li1 = sum(feval(func_name,(b0 + s*db))); %Eval. LLF at higher SL
              li2 = sum(feval(func_name,(b0 + s*db/2)));%Eval. LLF at lower SL    
           end 
       end   
                                     % ***  Determine step length  ***
       bn = b0 + s*db;               % ***  New estimates          ***
       crit = max(abs((bn-b0)./b0)./s);  % ***  Convergence criterion  ***
                                     % ***  Increment iteration    ***
       if iter == iter_limit;
           disp('   ');
           disp('***************************************************');
           disp('***** WARNING:  You are at the Iteration Limit*****');
           disp('***************************************************');
           disp('  ')
       end;
      disp('  ');                       
      fprintf('i = %3.0f\r', iter);    % Print iteration number
      fprintf('LLF = %8.4f\r', sum(feval(func_name,bn))); % Print curr.LLF
      fprintf('step = %5.4f\r', s);    % Print "current" step length
      fprintf('Paramaters \r');        % Print current paramater estimate
      vv=1;                            % Reset a counter to 1
    	if length(bn)>7;            % Do we have more than 7 parameters?
     		numloop=floor(length(bn)./7); % How many group of 7 parameters?
            resid=length(bn)-numloop*7; % How many parameters
                                        % after the last group of 7
     		while vv<=numloop;          % Number of groups of 7
                disp('  ');
    			fprintf('%8.4f',bn((vv-1)*7+1:vv*7,:)'); % Display 7 param 
                vv=vv+1;                   % Increment numloop counter
            end
            if resid>0;     % After the last group of 7 print out remaining
                disp('  ');
                fprintf('%8.4f',bn((vv-1)*7+1:length(b0),:)');
            end
        else
         fprintf('%8.4f',bn');       % Print out all paramatrs if less than 7
         disp('  ');
        end 
        disp('  ');
       iter = iter + 1;
       b0 = bn;                      % Replace old parm vector w/new
       cov_p = -inv(H);              % ***  Define Covariance matrix  ***/
       std = sqrt(diag(cov_p));      % ***  Asymptotic Std. Errors    ***/  
   end
     if iter < iter_limit;
       disp('   ');
       disp('This is the Probit Maximum Likelihood Result');
       tvalue=bn./std;                  % t-values 
       df= numobs-1;                       % Degrees of freedom for t-value
       pvalue=2*(1-tcdf(abs(tvalue),df));  % Column vector of param p-values 
       results=horzcat(bn,std,tvalue,pvalue); % Build results matrix
       table_bwg(parname,results,1);
       disp('  ');  
     end
     LLF_vec=sum(feval(func_name,bn));
   end  