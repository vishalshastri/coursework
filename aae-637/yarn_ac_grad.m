function ret = yarn_ac_grad( betas, x)
  ret = betas(1) .* x .^ (betas(2));
end