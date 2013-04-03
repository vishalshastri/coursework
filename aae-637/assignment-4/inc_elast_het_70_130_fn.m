function [ret] = inc_elast_het_70_130_fn(b0, data_mat)


beta_temp = b0(1:8);
x_temp = mean(data_mat(:, 2:9));
x_temp(3:4) = x_temp(3:4)*0.70;
z_temp = mean(data_mat(:, 10:end));
z_temp(4) = z_temp(4)*0.70;
gam_temp = b0(9:end);
inc_mean = mean(data_mat(:, 4)) * 0.7;

inc_elast_het_70 = normpdf( (x_temp * beta_temp ) / exp(z_temp * gam_temp) ) * ...
  (( (beta_temp(2) + beta_temp(3) * inc_mean) - ...
    (x_temp * beta_temp) * gam_temp(4) ) / ...
    exp(z_temp * gam_temp) ) * ...
  normcdf( (x_temp * beta_temp) / exp(z_temp * gam_temp) );
    
beta_temp = b0(1:8);
x_temp = mean(data_mat(:, 2:9));
x_temp(3:4) = x_temp(3:4) * 1.30;
z_temp = mean(data_mat(:, 10:end));
z_temp(4) = z_temp(4) * 1.30;
gam_temp = b0(9:end);
inc_mean = mean(data_mat(:, 4)) * 1.3;

inc_elast_het_130 = normpdf( (x_temp * beta_temp) / exp(z_temp * gam_temp) ) * ...
  (( (beta_temp(2) + beta_temp(3) * inc_mean) - ...
    (x_temp * beta_temp) * gam_temp(4) ) / ...
    exp(z_temp * gam_temp) ) * ...
  normcdf( (x_temp * beta_temp) / exp(z_temp * gam_temp) );
  
ret = inc_elast_het_70 - inc_elast_het_130;
  
end