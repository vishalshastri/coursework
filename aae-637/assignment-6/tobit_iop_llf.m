function [llf_vec] = tobit_iop_llf(b0, data_mat)
%   global rhsvar depend bhhh dum_above_lim ;
   rhsvar=data_mat(:, 2:13);
   z = data_mat(:, [2 14 4 5 9 6 15 16 7 8 10 ]);
   depend=data_mat(:, 1);
   dum_above_lim = depend > 0;
   k=13;
   betas=b0(1:(k-1));                   %*** Pull Out Betas ***
   sigmasq=b0(k);                       %*** Error Variance ***
   gam = b0((k+1):end);
   sigma=sqrt(sigmasq);                 %*** Error Std. Dev ***
   sigma_imag = imag(sigma);
   sigma = real(sigma);
   if sigma==0
     sigma=abs(.00001 + sigma_imag);
   end
   argue=rhsvar*betas;
   error=(depend-argue);           %*** Error Term ***
%   const=.5*(log(2*pi)+log(sigmasq));
   
   llf_part1 = dum_above_lim .* (-log(sigma) + log(normpdf( ...
     (normcdf(z*gam) .* depend -  argue) / sigma ...
     )) + 2.*log(normcdf(z*gam))) ;
     
   llf_part2 = abs((1-dum_above_lim)) .* ...
     log( 1 - normcdf(z*gam) .* normcdf(argue ./ sigma) );
   
%   llf_part1=(-const-((error.^2)./(2.*sigmasq))).*dum_above_lim;
%   llf_part2=log(1 - normcdf(argue./sigma)).*(1-dum_above_lim);
   llf_tmp=llf_part1+llf_part2;
%   if bhhh==1;
        llf_vec=llf_tmp;
%        else llf_vec=sum(llf_tmp);
%   end;
end