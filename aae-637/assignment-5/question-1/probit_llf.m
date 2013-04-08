function [tot_llf] = probit_llf(b0)
   global rhsvar depend;
   cdf_prob = normcdf(rhsvar*b0);
   llf = depend.*log(cdf_prob) + (1-depend).*log(1-cdf_prob);
   tot_llf=sum(llf);    
end

