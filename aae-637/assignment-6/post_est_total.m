function [total] = post_est_total(b0, mean_rhs)
%   global numparm mean_rhs elasvar
   elasvar = round(b0(end));
   b0 = b0(1:(end-1));
   numparm = length(b0);

   sigma=sqrt(b0(numparm));                %*** Std. Dev of Error term ***
   betas=b0(1:(numparm-1));                %*** Beta Coefficients ***/
   z = (mean_rhs'*betas)./sigma;           %*** Standardized Value ***/
   ystr_est=mean_rhs'*betas+sigma*normpdf(z)./normcdf(z);
                        %*** Conditional Expected Value ***/
   m = betas(elasvar).*(1 - z*normpdf(z)/normcdf(z) ...        
            -(normpdf(z)^2)/(normcdf(z)^2));
                        %*** Marginal of Conditional (19.3.10c) *** 
   elas_ystr=m.*mean_rhs(elasvar)./ystr_est;
                        %*** Elasticity of Conditional Expected Value***
   part_cdf=normpdf(z)*betas(elasvar)./sigma;  
                        %***Partial of CDF wrt beta***
   elas_cdf=part_cdf.*mean_rhs(elasvar)./normcdf(z); %***CDF Elasticity***
   total=elas_ystr+elas_cdf; %*** Total Elasticity=Prob+E(Y)***    
end

