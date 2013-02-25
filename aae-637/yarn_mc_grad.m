function ret = yarn_mc_grad( betas, x)
  ret = betas(1) .* (betas(2)+1) .* x .^ (betas(2));
end