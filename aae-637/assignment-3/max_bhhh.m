
%{
   ***************************************************************
   ***************************************************************
   ***************************************************************
   ** MAX_BHHH Function -- Maximum likelihood estimation (BHHH) **
   ***************************************************************
%}
function [betas, cov_betas, llf_vec] = max_bhhh(b0, names)
   global numobs iter_limit critic_limit func_name do_step ;
   crit = 1;              % Initialize critical value of % beta change
   iter = 1;              % Initialize iteration count
   s = 1;                 % Initialize step length
   disp('Below is the intermediate iteration information:');     
   disp('  ');
   diary on;     % Turn on output to file
                %******* Begin do loop *********
   while ((crit > critic_limit) & (iter<= iter_limit));                 
      z = Grad(b0,func_name,numobs);  % Numerical Gradients          
      H = z'*z;                       % Compute approx. to Hessian   
      g = sum(z)';                    % Gradient vector (K x 1)  
      db = inv(H)*g;                  % Compute full step adjustment 
      if do_step == 1;                % Use a variable step length 
       s = 1;                         % Reset base step length 
       li1 = 0; li2 = 1;              % ** Intialize LLF's under 2 step lengths
       while (li1 < li2) && (s >=.2); % ** Loop to determine step length
          li1 = sum(feval(func_name,(b0 + s*db)));  %Eval. LLF at higher SL
          li2 = sum(feval(func_name,(b0 + s*db/2)));%Eval. LLF at lower SL    
          s_star=s;
          s = s/2;          
       end 
      else
          s_star = s;
      end 
      betas = b0 + s_star*db;               % Update parameter values            
       
      disp('  ');
      disp('  ');
      fprintf('i = %3.0f\r', iter);    % Print iteration number
      fprintf('LLF = %8.4f\r', sum(feval(func_name,betas))); % Print current LLF
      fprintf('step = %5.3f\r', s_star);    % Print "current" step length
      fprintf('Parameters \r');        % Print current paramater estimate
      vv=1;                            % Reset a counter to 1
    	if length(betas)>7;            % Do we have more than 7 parameters?
     		numloop=floor(length(betas)./7); % How many group of 7 parameters?
            resid=length(betas)-numloop*7; % How many parameters
                                           % after the last group of 7
     		while vv<=numloop;             % Number of groups of 7
                fprintf('%8.4f',betas((vv-1)*7+1:vv*7,:)');  % Display 7 parameters 
    			vv=vv+1;                   % Increment numloop counter
            end
            if resid>0;     % After the last group of 7 print out remaining
                disp('  ');
                fprintf('%8.4f',betas((vv-1)*7+1:length(b0),:)');
            end
        else
         fprintf('%8.4f',betas');       % Print out all paramatrs if less than 7
         disp('  ');
        end 
        
      iter = iter + 1;               % Update iteration count
      crit = abs((betas-b0)./b0)./s_star; % Eval. size adjusted % parm change
      b0 = betas;                    % Replace old parm vector w/new
   end
   llf_vec=feval(func_name,b0); % Vector of Log-Likelihood values
   cov_betas=inv(H);            % BHHH method for Param. Cov. calculation
   std = sqrt(diag(cov_betas)); % Compute estimated std. errors
   zvalue=betas./std;           % t-values 
   df= numobs-length(b0);              % Degrees of freedom for t-value
   pvalue=2*(1-normcdf(abs(zvalue)));  % Column vector of param p-values 
   disp('  ');
   disp('  ');
   disp('Final results:');
   results=horzcat(b0,std,zvalue,pvalue); % Build results matrix
   table_bwg(names,results,9);
   disp('  ');
   
end
 
    
    

