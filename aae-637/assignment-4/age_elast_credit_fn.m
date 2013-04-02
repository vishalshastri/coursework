function [ret] = age_elast_credit_fn(b0, data_mat)

  ret = normpdf(mean(data_mat(:, 2:end)) * b0) * ...
    (b0(2) + 2*b0(3)*mean(data_mat(:, 3)) ) * ...
    mean(data_mat(:, 3)) / normcdf(mean(data_mat(:, 2:end)) * b0);
  
end