function [llf_vec] = het_tobit_llf(b0, data_mat)
%   global rhsvar num_betas depend bhhh dum_above_lim hetvar;
   %   global rhsvar depend bhhh dum_above_lim ;
   
   rhsvar=data_mat(:, 2:end);
   depend=data_mat(:, 1);
   dum_above_lim = depend > 0;
   hetvar = data_mat(:, [ 2 4 9 11 12 13]);
   num_betas = 12;
   betas=b0(1:num_betas);         %*** Pull Out Betas ***
   alphas=b0(num_betas+1:end);      %*** Pull Out Alphas ***
   argue=rhsvar*betas;
   error=(depend-argue);          %*** Error Term ***
   %********************************************************
   %***The following is how I define the hetero component **
   %********************************************************
   sigmasq=exp(hetvar*alphas);    %*** Sigma Squared ***
   sigma = sqrt(sigmasq);           %*** Sigma ***
   const=.5*(log(2*pi)+log(sigmasq));
   llf_part1=(-const-((error.^2)./(2.*sigmasq))).*dum_above_lim;
   llf_part2=log(1 - normcdf(argue./sigma)).*(1-dum_above_lim);
   llf_tmp=llf_part1+llf_part2;
%   if bhhh==1;
        llf_vec=llf_tmp;
%        else llf_vec=sum(llf_tmp);
%   end;
end