function ret = gas_rho_likelihood_fn(betas, data_mat)

  [nrow,ncol] = size(data_mat);
  ncol = ncol-1;
  rho = tanh(betas(ncol+1));
  betas = betas(1:ncol);
  
%  y_star = [ sqrt(1-rho^2) .* data_mat(1,1) ; ...
%    data_mat(2:nrow,1) - rho .* data_mat(1:(nrow-1),1) ];
    
%  x_star = [ sqrt(1-rho^2) .* data_mat(1,2:9) ; ...
%    data_mat(2:nrow,2:9) - rho .* data_mat(1:(nrow-1),2:9) ];

  y_star = [ data_mat(1,1) ; ...
    data_mat(2:nrow,1) - rho .* data_mat(1:(nrow-1),1) ];
    
  x_star = [  data_mat(1,2:9) ; ...
    data_mat(2:nrow,2:9) - rho .* data_mat(1:(nrow-1),2:9) ];

%  sig_sq_mu = (y_star-x_star * betas)' * ...
%    (y_star-x_star * betas) ./ nrow;

  sig_sq_mu = (data_mat(:,1)-data_mat(:,2:9) * betas)' * ...
    (data_mat(:,1)-data_mat(:,2:9) * betas) ./ nrow;
  % Still not sure if this is how you compute sig_sq_mu
  
  ret = [-.5 * log(2*pi) - .5 * log(sig_sq_mu) + .5 * log(1 - rho^2) - ...
    ( (1-rho^2) * (y_star(1,1)-x_star(1, :) * betas)^2 ) / (2*sig_sq_mu);  ...
     -.5 * log(2*pi) - .5 * log(sig_sq_mu) - ...
      (y_star(2:nrow,1)-x_star(2:nrow, :) * betas).^2 / (2*sig_sq_mu) ];
      

end