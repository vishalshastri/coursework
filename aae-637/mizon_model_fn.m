function ret = mizon_model_fn(x, betas)
	 ret = betas(1) .* x(:, 1).^betas(2) .* x(:, 2).^betas(3);
end