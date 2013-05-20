function [ret] = cheese_uncon_elast_dif(b0, data_mat)  

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


marg = betas(3) * ...
  (1 - ...
    (argue/sigma) * (normpdf(argue/sigma)/normcdf(argue/sigma)) - ...
    ( (normpdf(argue/sigma)^2) / (normcdf(argue/sigma)^2)) ...
  );


 part_1 =  marg * mean(rhsvar(:,3)) * ...
      (argue + sigma*(normpdf(argue/sigma) / (1-normcdf(argue/sigma))));

% from p. 7 and 16 of censored reg ppt

ret = part_1;

end