function ret = consump_ar1_model_fn( betas, x)
  ret = betas(1) + betas(2) .* x(:, 2) + ...
    betas(3) .* ( x(:, 3) - betas(1) -  betas(2) .* x(:, 4) ) ;
end
