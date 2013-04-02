function [ret] = inc_elast_credit_fn(b0, data_mat)

  ret = normpdf(mean(data_mat(:, 2:end)) * b0) * ...
    b0(4)  * ...
    mean(data_mat(:, 5)) / normcdf(mean(data_mat(:, 2:end)) * b0);
  
end