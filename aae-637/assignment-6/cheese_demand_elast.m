function [ret] = cheese_demand_elast(b0, data_mat)

   rhsvar=data_mat(:, 2:13);

   k=13;
   betas=b0(1:(k-1)); 
   sigmasq=b0(k);                       %*** Error Variance ***
   sigma=sqrt(sigmasq);                 %*** Error Std. Dev ***
   
   ret = (betas(3) /sigma) * normpdf( ( mean(rhsvar) * betas)/sigma) * ...
     mean(rhsvar(:,3)) / normcdf( (mean(rhsvar) * betas)/sigma);
     
end