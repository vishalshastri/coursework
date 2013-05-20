function [ elas_cdf ] = post_est_prob(b0, mean_rhs)
%   global numparm mean_rhs elasvar; 
   elasvar = round(b0(end));
   b0 = b0(1:(end-1));
   numparm = length(b0);
   sigma=sqrt(b0(numparm));                   %*** Std. Dev of Error term ***
   betas=b0(1:(numparm-1));                   %*** Beta Coefficients ***
   z = (mean_rhs'*betas)./sigma;                   %*** Standardized Value ***
   part_cdf=normpdf(z)*betas(elasvar)./sigma; %*** Partial of CDF wrt beta ***
   elas_cdf=part_cdf.*mean_rhs(elasvar)./normcdf(z);%*** Elasticity of CDF ***
   %fprintf('This is CDF Elas:    %5.4', mean_rhs(elasvar));
end  
