function ret = rand_nls_model(betas, x_mat)

  ret = exp(x_mat * betas);
  
end