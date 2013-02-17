function ret = ces_model_fn( betas, x)
    ret = betas(1) .* ...
      ( betas(2) .* x(:, 2).^(-beta(3)) + (1-beta(2)) .* x(:, 8).^(-beta(3)) ).^(-beta(4)/beta(3)) ;
end
