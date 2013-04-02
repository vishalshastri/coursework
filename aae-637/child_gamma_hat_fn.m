function ret = child_gamma_hat_fn(betas, data_mat)
  
mean_ch_2 = mean(data_mat(:, 2:end));
mean_ch_2(2) = 2;
mean_ch_2(3) = mean_ch_2(3) * 0.5;
mean_ch_2(4) = mean(data_mat(:, 4)) * 2 * 0.5;
eff_ch_2 = normcdf(mean_ch_2*betas);

mean_ch_3 = mean(data_mat(:, 2:end));
mean_ch_3(2) = 3;
mean_ch_3(3) = mean_ch_3(3) * 0.5;
mean_ch_3(4) = mean(data_mat(:, 4)) * 3 * 0.5;
eff_ch_3 = normcdf(mean_ch_3*betas);

child_marg_eff_50 = eff_ch_2 - eff_ch_3;


mean_ch_2 = mean(data_mat(:, 2:end));
mean_ch_2(2) = 2;
mean_ch_2(3) = mean_ch_2(3) * 1.5;
mean_ch_2(4) = mean(data_mat(:, 4)) * 2 * 1.5;
eff_ch_2 = normcdf(mean_ch_2*betas);

mean_ch_3 = mean(data_mat(:, 2:end));
mean_ch_3(2) = 3;
mean_ch_3(3) = mean_ch_3(3) * 1.5;
mean_ch_3(4) = mean(data_mat(:, 4)) * 3 * 1.5;
eff_ch_3 = normcdf(mean_ch_3*betas);

child_marg_eff_150 = eff_ch_2 - eff_ch_3;

ret = child_marg_eff_50 - child_marg_eff_150;

end