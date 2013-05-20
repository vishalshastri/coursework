function [ret] = cheese_purchase_price_elast(b0, data_mat)   
   
   rhsvar=data_mat(:, 2:13);
   z = mean(data_mat(:, [2 14 4 5 9 6 15 16 7 8 10 ]));
   depend=data_mat(:, 1);
   dum_above_lim = depend > 0;
   k=13;
   betas=b0(1:(k-1));                   %*** Pull Out Betas ***
   sigmasq=b0(k);                       %*** Error Variance ***
   gam = b0((k+1):end);
   sigma=sqrt(sigmasq);                 %*** Error Std. Dev ***
   sigma_imag = imag(sigma);
   sigma = real(sigma);

   argue=mean(rhsvar)*betas;
   
   
   marg = (betas(2)/sigma) * normcdf(z*gam) * normpdf(argue) + ...
     0 * normcdf(argue) * normpdf(z*gam);
     
   ret = marg * mean(rhsvar(:, 2)) / ...
     (normcdf(z*gam) * normcdf(argue/sigma));
     
end