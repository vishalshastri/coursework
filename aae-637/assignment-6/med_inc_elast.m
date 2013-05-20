function [ret] = med_inc_elast(b0, data_mat)

  ret = normpdf(mean(data_mat(:,3:8))*b0(1:6)) * b0(2) * ...
    mean(data_mat(:, 4))/ normcdf(mean(data_mat(:,3:8))*b0(1:6));
   
end
