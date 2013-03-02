function [beta_tilde, rho_tilde] = beach_mackinnon_alg(betas, rho, data_mat, func_name, names, critic_limit, iter_limit)

  [nrow,ncol] = size(data_mat);
  
  iter = 1;
  crit = 1;
  
  while ((crit > critic_limit) & (iter<= iter_limit)); 

  y_star = [ sqrt(1-rho^2) .* data_mat(1,1) ; ...
    data_mat(2:nrow,1) - rho .* data_mat(1:(nrow-1),1) ];
    
  x_star = [ sqrt(1-rho^2) .* data_mat(1,2:ncol) ; ...
    data_mat(2:nrow,2:ncol) - rho .* data_mat(1:(nrow-1),2:ncol) ];
    
  beta_tilde = inv(x_star' * x_star) * x_star' * y_star;
  
  e_hat = data_mat(:,1)- data_mat(:,2:ncol) * beta_tilde ;
  
  a_input = -(nrow-2) * sum(e_hat(2:nrow)' * e_hat(1:(nrow-1))) / ...
    ( (nrow-1) * sum( e_hat(2:nrow).^ 2 - e_hat(1).^2 ) );
    
  b_input = ( (nrow-1) * e_hat(1).^2 - nrow * sum(e_hat(1:(nrow-1)).^2) - sum(e_hat(2:nrow).^2) ) / ...
  ( (nrow-1) * sum( e_hat(2:nrow).^ 2 - e_hat(1).^2 ) );
  
  c_input = nrow * sum(e_hat(2:nrow)' * e_hat(1:(nrow-1))) / ...
    ( (nrow-1) * sum( e_hat(2:nrow).^ 2 - e_hat(1).^2 ) );
  
  p_input = b_input - a_input^2 / 3;
  
  q_input = c_input - (a_input * b_input)/3 + (2 * a_input^3) /27;
  
  phi_input = acos( (q_input * sqrt(27) ) / (2 * p_input *sqrt(-p_input) ) );
    
  rho_tilde = -2 * sqrt(-p_input/3) * cos( phi_input/3 + pi/3) - a_input/3;
  
  rho_tilde = real(rho_tilde);
  
  crit = max(abs(([beta_tilde; rho_tilde]-[betas ; rho])./[betas ; rho]));
  iter = iter + 1;

  betas = beta_tilde;
  
  rho = rho_tilde;
  
  betas_rho =[betas;rho]
  
      disp('  ');
      disp('  ');
      fprintf('i = %3.0f\r', iter);    % Print iteration number
      fprintf('LLF = %8.4f\r', sum(func_name(betas_rho, data_mat))); % Print current LLF
      fprintf('Parameters \r');        % Print current paramater estimate
      vv=1;                            % Reset a counter to 1
    	if length([betas;rho])>7;            % Do we have more than 7 parameters?
     		numloop=floor(length(betas_rho)./7); % How many group of 7 parameters?
            resid=length([betas;rho])-numloop*7; % How many parameters
                                           % after the last group of 7
     		while vv<=numloop;             % Number of groups of 7
                fprintf('%8.4f',betas_rho((vv-1)*7+1:vv*7,:)');  % Display 7 parameters 
    			vv=vv+1;                   % Increment numloop counter
            end
            if resid>0;     % After the last group of 7 print out remaining
                disp('  ');
                fprintf('%8.4f',betas_rho((vv-1)*7+1:length(betas_rho),:)');
            end
        else
         fprintf('%8.4f',betas_rho');       % Print out all paramatrs if less than 7
         disp('  ');
        end 
  
  
  end
  
  sig_sq_mu = (data_mat(:,1)-data_mat(:,2:ncol) * betas)' * ...
    (data_mat(:,1)-data_mat(:,2:ncol) * betas) ./ nrow;
  % Still not sure if this is how you compute sig_sq_mu
  
  cov_betas = eye(length(betas)+1);
  
  
  for i=1:length(betas)
    for j=1:length(betas)
      cov_betas(i,j) = ( (1-rho^2) * data_mat(1,i+1) * data_mat(1,j+1)  + ...
        sum( (data_mat(2:nrow,i+1) - rho * data_mat(1:(nrow-1),i+1)) .* ...
          (data_mat(2:nrow,j+1) - rho * data_mat(1:(nrow-1),j+1)) ) ) / ...
        sig_sq_mu;
    end
  end
  
  cov_betas(length(betas)+1,length(betas)+1) = ...
    (1+rho^2)/(1-rho^2)^2 + ...
    sum( (data_mat(2:nrow,1)-data_mat(2:nrow,2:ncol) * betas ).^2) / sig_sq_mu;
    
   cov_betas = inv(cov_betas);
    
   std = sqrt(diag(cov_betas)); % Compute estimated std. errors
   zvalue=[betas; rho]./std;           % t-values 
   df= size(data_mat,1)-length(betas);              % Degrees of freedom for t-value
   pvalue=2*(1-normcdf(abs(zvalue)));  % Column vector of param p-values 
   disp('  ');
   disp('  ');
   disp('Final results:');
   results=horzcat([betas; rho],std,zvalue,pvalue); % Build results matrix
   table_bwg(names,results,9);
   disp('  ');
  

end
  


%  Q_mat = [(1-rho^2)^.5 0 0 0 0 0; ...
%           -rho         1 0 0 0 0; ...
%           0            0 0 0 -rho 1]