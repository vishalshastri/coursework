function ret = gamma_hat_fn(betas, x_mat)
 
  ret = exp(x_mat * betas) .* betas;
%  ret = poisspdf(exp(x_mat * betas), exp(x_mat * betas)) .* betas;

end