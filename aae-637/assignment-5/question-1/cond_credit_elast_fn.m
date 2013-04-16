function [ret] = cond_credit_elast_fn(b0)
  % Grad(bp_bi(totparm),func_name,1)
  % declare globals
  global critic_limit iter_limit rhsvar numobs do_step func_name dh ...
    parname depend numc2 numc_1 numc_2 iter rhsvar1 rhsvar2 q1 q2 combined_rhs
    
    gamma_1 = b0(1:6);
    gamma_2 = b0(7:12);
    bp_bi  = b0(13:22);
    rho = b0(23);
  
	b1_bi=bp_bi(1:numc_1,:);
	b2_bi=bp_bi((numc_1+1):(numc_1+numc_2),:);
	gamma_1=vertcat(bp_bi(1:4),0,0);  %**gamma_1 of full dimension **
	gamma_2=vertcat(bp_bi(5:7),0,bp_bi(8:9)); %** gamma_2 of full dimension **
	q1_mn=1;                          %** used to define g_1 eq. 17.52 p. 740 ***
	q2_mn=1;                          %** used to define g_2 eq. 17.52 p. 740 ***
	w1_mn=q1_mn.*(combined_rhs*gamma_1); %** Defined on p. 739, Greene ***
	w2_mn=q2_mn.*(combined_rhs*gamma_2); %** Defined on p. 739, Greene ***
	rho_star_mn=q1_mn.*q2_mn.*rho;        %** Defined on p.740, Greene ***
	numer_1=w2_mn-rho_star_mn.*w1_mn;     %** Numerator of 17.52, p.740 ***
	numer_2=w1_mn-rho_star_mn.*w2_mn;     %** Numerator of 17.52, p.740 ***
	denom=sqrt(1-rho_star_mn^2);          %** Denomenator of 17.52, p.740 ***
	g_1=normpdf(w1_mn).*normcdf(numer_1./denom);%** Defined by 17.52, p.740 ***
	g_2=normpdf(w2_mn).*normcdf(numer_2./denom);%** Defined by 17.52, p.740 ***
    bi_cdf_marg=g_1.*gamma_1'+g_2.*gamma_2'; %**Partial of Bivariate CDF, p.743***
    
    omega=[1 rho_star_mn;rho_star_mn 1];
    mu=[0 0];
    lim_bi=[w1_mn w2_mn];
	part_2_2=(g_2-mvncdf(lim_bi,mu,omega).*normpdf(w2_mn)./normcdf(w2_mn)).*gamma_2;
	part_y1_y2_1=(1./normcdf(w2_mn)).*(g_1*gamma_1+part_2_2);
	
	
	rho_mat = [1 rho; rho 1];
  % ELASTICITY BELOW:
  elast_y1_y2_1 = part_y1_y2_1' .* ( combined_rhs / ...
      (mvncdf([combined_rhs*gamma_1 combined_rhs*gamma_2], 0, rho_mat) / ...
        normcdf(combined_rhs*gamma_2) ) );
  ret = elast_y1_y2_1(3);

end   


