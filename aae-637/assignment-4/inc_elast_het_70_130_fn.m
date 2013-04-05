function [ret] = inc_elast_het_70_130_fn(b0, data_mat)

less_than_70_inc_index = data_mat(:, 4) <= mean(data_mat(:, 4))*0.7;
beta_temp = b0(1:8);
x_temp = mean(data_mat(less_than_70_inc_index, 2:9));
z_temp = mean(data_mat(less_than_70_inc_index, 10:end));
gam_temp = b0(9:end);
inc_mean = mean(x_temp(:, 4)) ;

inc_elast_het_70 = normpdf( (x_temp * beta_temp ) / exp(z_temp * gam_temp) ) * ...
  (( (beta_temp(3) + beta_temp(4) * inc_mean) - ...
    (x_temp * beta_temp) * gam_temp(4) ) / ...
    exp(z_temp * gam_temp) ) * ...
  normcdf( (x_temp * beta_temp) / exp(z_temp * gam_temp) );

less_than_130_inc_index = data_mat(:, 4) >= mean(data_mat(:, 4))*1.3;
beta_temp = b0(1:8);
x_temp = mean(data_mat(less_than_130_inc_index , 2:9));
z_temp = mean(data_mat(less_than_130_inc_index , 10:end));
gam_temp = b0(9:end);
inc_mean = mean(x_temp(: , 4));

inc_elast_het_130 = normpdf( (x_temp * beta_temp) / exp(z_temp * gam_temp) ) * ...
  (( (beta_temp(3) + beta_temp(4) * inc_mean) - ...
    (x_temp * beta_temp) * gam_temp(4) ) / ...
    exp(z_temp * gam_temp) ) * ...
  normcdf( (x_temp * beta_temp) / exp(z_temp * gam_temp) );
  
ret = inc_elast_het_70 - inc_elast_het_130;
  
end