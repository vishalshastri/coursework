function [ret] = fish_elast_fn(b0, data_mat)

  mean_rhsvar = mean(data_mat(:, 1:4));
  x_beta = mean_rhsvar*b0(1:4);
  mu_1 = b0(5);
  mu_2 = b0(6);
  b0((end-1):end) = round(b0((end-1):end));
  
  j = b0((end-1));
  
  if b0(end)==1
    ret = -normpdf(-x_beta) * b0(j) * ...
      (mean_rhsvar(j) / normcdf(-x_beta) );
    elseif b0(end)==2
      ret = ( normpdf(-x_beta) - normpdf(mu_1 - x_beta ) ) * b0(j) * ...
        (mean_rhsvar(j) / (normcdf(mu_1 - x_beta) - normcdf(-x_beta) ) );
    elseif b0(end)==3
      ret = ( normpdf(mu_1-x_beta) - normpdf(mu_2 - x_beta ) ) * b0(j) * ...
        (mean_rhsvar(j) / (normcdf(mu_2 - x_beta) - normcdf(mu_1-x_beta) ) )  ; 
    elseif b0(end)==4
      ret = ( normpdf(mu_2 - x_beta ) ) * b0(j) * ...
        (mean_rhsvar(j) / (1-normcdf(mu_2 - x_beta)) );

end