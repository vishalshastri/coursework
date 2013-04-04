function [ret] = fluid_inc_elast(b0, data_mat)

  ret = normpdf(mean(data_mat(:, 2:end))*b0) * ...
    (b0(3) + b0(4)*mean(data_mat(:, 3)) ) * ...
    (mean(data_mat(:, 3)) / normcdf(mean(data_mat(:, 2:end))*b0));
  
end