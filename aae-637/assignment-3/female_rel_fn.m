function ret = female_rel_fn(betas, data_mat)

  marg_input_female = mean(data_mat(:, 3:size(data_mat,2)));
  marg_input_female(8) = 1;

  marg_input_male = mean(data_mat(:, 3:size(data_mat,2)));
  marg_input_male(8) = 0;

  ret = exp( marg_input_female * betas ) - exp( marg_input_male * betas );

end