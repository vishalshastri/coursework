function ret = yarn_model_fn( betas, x)
  ret = betas(1) .* x(:, 5) .^ (betas(2)+1);
end
