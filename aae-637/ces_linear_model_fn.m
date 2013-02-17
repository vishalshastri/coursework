function ret = ces_linear_model_fn( betas, x)
    ret = log(betas(1)) - (beta(4)/beta(3)) .* ...
      ( betas(2) .* x(:, 2).^(-beta(3)) + (1-beta(2)) .* x(:, 8).^(-beta(3)) ) ;
end
