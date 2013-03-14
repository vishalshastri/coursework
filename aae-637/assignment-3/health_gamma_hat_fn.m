function ret = health_gamma_hat_fn(betas, data_mat)

  marg_input_good = mean(data_mat(:, 3:size(data_mat,2)));
  marg_input_good(15:17) = [1 0 0];

  marg_effect_good = exp(marg_input_good * betas_rand_full) * betas_rand_full(2);

  marg_input_poor = mean(data_mat(:, 3:size(data_mat,2)));
  marg_input_poor(15:17) = [0 0 1];

  marg_effect_poor = exp(marg_input_poor * betas_rand_full) * betas_rand_full(2);
 
  ret = marg_effect_good - marg_effect_poor;

end