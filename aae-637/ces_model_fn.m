function ret = ces_model_fn( betas, x)
    ret = betas(1) .* ...
      ( betas(2) .* x(:, 2).^(-betas(3)) + (1-betas(2)) .* x(:, 8).^(-betas(3)) ).^(-betas(4)/betas(3)) ;
end