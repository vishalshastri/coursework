function ret = ces_mrts( betas, x)
  ret = ( (1-betas(2))/betas(2)) * (x(2)/x(1))^(1+betas(3))
end