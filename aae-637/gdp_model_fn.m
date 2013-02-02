function ret = gdp_model_fn(consump, unemp, betas)
	 ret = (consump.^betas(1)) .* unemp.^betas(2);
end

