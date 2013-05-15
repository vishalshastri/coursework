function [llf_vec] = tobit_llf(b0, data_mat)
%   global rhsvar depend bhhh dum_above_lim ;
   rhsvar=data_mat(:, 2:end);
   depend=data_mat(:, 1);
   dum_above_lim = depend > 0;
   k=length(b0);
   betas=b0(1:(k-1));                   %*** Pull Out Betas ***
   sigmasq=b0(k);                       %*** Error Variance ***
   sigma=sqrt(sigmasq);                 %*** Error Std. Dev ***
   argue=rhsvar*betas;
   error=(depend-argue);           %*** Error Term ***
   const=.5*(log(2*pi)+log(sigmasq));
   llf_part1=(-const-((error.^2)./(2.*sigmasq))).*dum_above_lim;
   llf_part2=log(1 - normcdf(argue./sigma)).*(1-dum_above_lim);
   llf_tmp=llf_part1+llf_part2;
%   if bhhh==1;
        llf_vec=llf_tmp;
%        else llf_vec=sum(llf_tmp);
%   end;
end