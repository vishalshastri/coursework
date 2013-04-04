function [ret] = logitcdf_fn(x)
  % Takes a scalar as argument
  if length(x)~=1 
    error('ERROR: Argument must be scalar')
  end
  
  ret = exp(x) / (1+exp(x));

end