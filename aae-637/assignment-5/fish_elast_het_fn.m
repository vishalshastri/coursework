function [ret] = fish_elast_het_fn(b0, data_mat)

  x_beta = mean(data_mat(:, [1 2 3 16])) * b0(1:4);

  sigma_est = exp( mean(data_mat(:,16)) * b0(end));

  mu_2 = b0(6);

  ret =  normpdf(  (mu_2 - x_beta)./sigma_est ) * ...
    ( ( b0(4) - (mu_2 - x_beta)*b0(end) ) / sigma_est ) * ...
    mean(data_mat(:,16)) * normcdf(  (mu_2 - x_beta)./sigma_est );
  
end