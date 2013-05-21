function [llf_vec] = sample_select_yes_rho_llf(b0, data_mat)

  visit = data_mat(:, 1);
  insur = data_mat(:, 2);
  rhsvar1 = data_mat(:, 3:8);
  rhsvar2 = data_mat(:, [3 4 7 9]);
  beta1 = b0(1:6);
  beta2 = b0(7:(end-1));
  
  argue1 =  (rhsvar1 * beta1);
  argue2 =  (rhsvar2 * beta2);
  
  rho = tanh(b0(end));
  
  llf_part1 = abs(1-insur) .* log( (1-normcdf(argue2)) );
  
  llf_part2 = abs(1-visit) .* insur .* log( mvncdf([-argue1 argue2], [0 0], [1 -rho; -rho 1]) );
  
  llf_part3 = visit .* insur .* log( mvncdf([argue1 argue2], [0 0], [1 rho; rho 1]) );
  
  % by p. 921 of Greene 7th edition
  
  llf_vec = llf_part1 + llf_part2 + llf_part3;
  

end