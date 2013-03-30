function [tot_llf] = probit_llf(b0, data_mat)
   depend = data_mat(:, 1);
   rhsvar = data_mat(:, 2:size(data_mat, 2));   
   cdf_prob = normcdf(rhsvar*b0);
   llf = depend.*log(cdf_prob) + (1-depend).*log(1-cdf_prob);
   tot_llf=sum(llf);    
end

