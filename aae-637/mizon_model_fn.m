function ret = mizon_model_fn(capital, labor, betas)
	 ret = betas(1) .* capital.^betas(2) .* labor.^betas(3);
end