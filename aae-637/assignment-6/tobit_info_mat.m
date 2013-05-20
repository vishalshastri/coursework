function [ beta_cov ] = tobit_info_mat(b0, data_mat)
%  global numparm rhsvar ;
  numparm = length(b0);
  rhsvar = data_mat ;
  betas = b0(1:(numparm-1));
  sigmasq = b0(numparm);     
  sigma=sqrt(sigmasq);
  zz = (rhsvar*betas)./sigma;      %*** Standardize ***/
  pdf = normpdf(zz);                   %*** Std. Normal PDF at ZZ ***/
  cdf = normcdf(zz);                   %*** Std. Normal CDF at ZZ ***/
  %***** The Following is based on equation 19.3.8 of JHGLL ******/
  %****  write a, b, c as column vectors   ******
  a1 = -(zz.*pdf-(pdf.^2)./(1-cdf)-cdf)./(sigmasq);
  b1 = (((zz.^2).*pdf) + pdf -(zz.*(pdf.^2))./(1-cdf))./(2*sigma^3);
  c1 = -((zz.^3 .* pdf)+zz.*pdf-((zz.^2).*pdf.^2)./(1-cdf)-2.*cdf) ...
            ./(4*sigma^4);
  a1_2=repmat(a1,1,numparm-1); 
  a2 = (rhsvar.*(a1_2))'*rhsvar;
  b1_2=repmat(b1,1,numparm-1);
  b2 = (sum(rhsvar.*b1_2))';
  c2 = sum(c1);
  beta_cov=inv(vertcat(horzcat(a2,b2),horzcat(b2',c2)));
end
