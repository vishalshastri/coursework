function [ret] = inc_elast_credit_75_125_fn(b0, data_mat)

  inc_elast_data_input = mean(data_mat(:, 2:end));
  inc_elast_data_input(4) = inc_elast_data_input(4) * 0.75;

  inc_elast_credit_75 = logitcdf_fn(inc_elast_data_input * b0) * ...
    ( 1 - logitcdf_fn(inc_elast_data_input * b0) ) * ...
    b0(4)  * ...
    inc_elast_data_input(4) / logitcdf_fn(inc_elast_data_input * b0);
  
  inc_elast_data_input = mean(data_mat(:, 2:end));
  inc_elast_data_input(4) = inc_elast_data_input(4) * 1.75;

  inc_elast_credit_125 = logitcdf_fn(inc_elast_data_input * b0) * ...
    ( 1 - logitcdf_fn(inc_elast_data_input * b0) ) * ...
    b0(4)  * ...
    inc_elast_data_input(4) / logitcdf_fn(inc_elast_data_input * b0);

  ret = inc_elast_credit_75 - inc_elast_credit_125;
  
end