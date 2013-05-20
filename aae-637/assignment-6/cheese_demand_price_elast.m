function [ret] = cheese_demand_price_elast(b0, data_mat)

   rhsvar=data_mat(:, 2:13);

   k=13;
   betas=b0(1:(k-1)); 
   sigmasq=b0(k);                       %*** Error Variance ***
   sigma=sqrt(sigmasq);                 %*** Error Std. Dev ***
   
   ret = (betas(2) /sigma) * normpdf( ( mean(rhsvar) * betas)/sigma) * ...
     mean(rhsvar(:,2)) / normcdf( (mean(rhsvar) * betas)/sigma);
     
end