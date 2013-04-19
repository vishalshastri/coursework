%{
**********************************************************************/
*** Program Designed to Estimate the Bi-Variate Probit Model     *****/
*** Univariate Probits are Estimated via NR and Analytical Hessians **/
*** Bivariate Probits are Estimated via BHHH with numerical gradients*/
**********************************************************************/
%}
clear;					         % Command to clear memory 
clc;                             % Command to clear command window
global critic_limit iter_limit rhsvar numobs do_step func_name dh ...
    parname depend numc2 numc_1 numc_2 iter rhsvar1 rhsvar2 q1 q2 combined_rhs;

urlwrite('http://www.aae.wisc.edu/aae637/data/matlab/greene_credit_v4.xls','temp.xls');
[base_data,varnames,raw]=xlsread('temp.xls');
[numobs,numc]=size(base_data);  % Determine size of full data matrix

%************* Read in all the data ***************
base_var1={'Age','Income','Avgexp'};
temp1=pull_data(varnames,base_var1,base_data);
rhsvar1=horzcat(ones(numobs,1),temp1);  %***RHS for Derogatory Report ***
base_var2={'Age','Income','Ownrent','Selfempl'};
temp2=pull_data(varnames,base_var2,base_data);
rhsvar2=horzcat(ones(numobs,1),temp2);  %***RHS for Credit Card Acceptance***
[numobs,numc_1]=size(rhsvar1);
[numobs,numc_2]=size(rhsvar2);
combined_rhs=mean(horzcat(rhsvar1,temp2(:,3:4))); %Combined X Matrix means ***/
depend1=base_data(:,2) > 0;      %*** LHS for Derogatory Report *** 
depend2=base_data(:,1);          %*** LHS for Credit Card Acceptance ***
parname_1={'Int_1','WifeAge1','Income1','Avg_Exp1'};  %** Derogatory Report Names **
parname_2={'Int_2','WifeAge2','Income2','OwnRent2','SlfEmpl2'}; %**Credit Card Names **
parname_combined={'Int_1','WifeAge1','Income1','Avg_Exp1', ...
        'Int_2','WifeAge2','Income2','OwnRent2','SlfEmpl2','Tanh_arg'};
parname_marginal={'WifeAge','Income','Avg_Exp','OwnRent','SelfEmpl'};
critic_limit=1e-6;           % Critical % change in parameters
iter_limit=250;              % Maximum number of iterations
do_step=1;                   % =1 variable step length, 0 fixed 
func_name =('probit_llf');   % Identify LLF function
alpha=.05;                   % Type I Error Probability 
dh = 0.000001;
do_marginal=1;
%*********************************************************
%**  Obtain the OLS estimates using all the data and   ***
%**  ignoring the discrete nature of the dep. variable ***
%*********************************************************
df1 = numobs-numc_1;   df2 = numobs-numc_2;
bls1 = (rhsvar1'*rhsvar1)\(rhsvar1'*depend1); %**CRM Starting Values Univariate Probit #1*/
bls2 = (rhsvar2'*rhsvar2)\(rhsvar2'*depend2); %**CRM Starting Values Univariate Probit #2*/
ehat1 = depend1 - rhsvar1*bls1;  ehat2 = depend2 - rhsvar2*bls2;
sse1 = ehat1'*ehat1;  sse2 = ehat2'*ehat2;
sighat1 = sse1./df1;  sighat2 = sse2./df2;
covb1 = sighat1.*inv(rhsvar1'*rhsvar1); covb2 = sighat2.*inv(rhsvar2'*rhsvar2);
stbls1 = sqrt(diag(covb1));   stbls2 = sqrt(diag(covb2));
tvalue1=bls1./stbls1;         tvalue2=bls2./stbls2;
   %*** These are Defined following Greene, p. 739 ***
q1=2.*depend1-1;    %*** q1 used to make LLF for Bivariate Probit simple ***
q2=2.*depend2-1;    %*** q2 used to make LLF for Bivariate Probit simple ***

disp('Maximum Likelihood Univariate Probit for Y1');
disp('  ')
depend=depend1;
rhsvar=rhsvar1;
numc2=numc_1;
parname=parname_1;
[bprob1,covbprob1,totllf1]=probit_bwg(bls1);
disp('Maximum Likelihood Univariate Probit for Y2');
disp('  ')
depend=depend2;
rhsvar=rhsvar2;
numc2=numc_2;
parname=parname_2;
[bprob2,covbprob2,totllf2]=probit_bwg(bls2);
%****************** Bi-Variate Probit Model Section  **********************      
disp('Now the Maximum Likelihood Iterations Begin for Bivariate Probit');
func_name =('bi_probit_llf');   % Identify LLF function
%b0=vertcat(bprob1,bprob2,.5);   % **Bivariate Starting Values**
parname=parname_combined;

b0=[-1.28286;   0.01056;   0.00585;   0.00033;   0.69987;  -0.00911;
   0.07406;   0.34923;  -0.26076;  -0.89373];

[bp_bi,covbp_bi,bi_llf_vec] = max_bhhh(b0,parname_combined);% ** Call Out the Bi-Probit **
%******** I Calculate Rho from TANH ********/   
func_name='tanh';
disp('***** Summary of Rho Est. and S.E. *****');
totparm=length(parname_combined);
rho_grad=Grad(bp_bi(totparm),func_name,1);  %**** Call Out Tanh procedure ****/
var_beta_rho=covbp_bi(totparm,totparm);	%** Variance of Tanh paramater **/
var_rho=(rho_grad.^2).*var_beta_rho;    %** Variance of Resulting Rho  **/
rho=tanh(bp_bi(totparm));
fprintf('Est. Rho:  %6.4f', rho) %** Print Est. Rho **/
disp('  ');
fprintf('Std. Err. of Est. Rho:  %6.4f', sqrt(var_rho)); %** S.E.of Rho**/
disp('  ');
disp('***** Testing H0: Bivariate H1: Univariate *****');
disp('********* 3 Tests for Bivariate vs. Univariate Probit **********');
%**** Lagrange Multiplier Test from P.742 in Greene ******
w1=q1.*(rhsvar1*bprob1);
w2=q2.*(rhsvar2*bprob2);
pdfprod=normpdf(w1).*normpdf(w2);
cdfprod=normcdf(w1).*normcdf(w2);
ratio=pdfprod./cdfprod;
numer=q1.*q2.*ratio;
numer_2=sum(numer).^2;
crossprod=normcdf(w1).*normcdf(-w1).*normcdf(w2).*normcdf(-w2);
denom=(pdfprod.^2)./crossprod;
denom_2=sum(denom);
LM_bi=numer_2./denom_2;
fprintf('LM Bivariate:  %8.4f', LM_bi);  disp('  ');
%********* Likelihood Ratio Test Statistic ********/
LR_bi=2.*(sum(bi_llf_vec)-(totllf1+totllf2));
fprintf('log-Likelihood Ratio:  %8.4f',LR_bi);
disp('  ');
%*********** Wald *************
Wald_bi=(tanh(bp_bi(totparm)))^2./var_rho;
fprintf('Wald Statistic:  %8.4f', Wald_bi);
disp('  ');
%****    Bivariate Probit Marginal Effects   ******/
%****This follows from Geene, p. 739-740 ****/
if do_marginal == 1;
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
	disp('This is bivariate probability marginal effects of both=1:');
	table_bwg(parname_marginal,bi_cdf_marg(2:length(bi_cdf_marg))',3);
    disp('  ');
    omega=[1 rho_star_mn;rho_star_mn 1];
    mu=[0 0];
    lim_bi=[w1_mn w2_mn];
	part_2_2=(g_2-mvncdf(lim_bi,mu,omega).*normpdf(w2_mn)./normcdf(w2_mn)).*gamma_2;
	part_y1_y2_1=(1./normcdf(w2_mn)).*(g_1*gamma_1+part_2_2);
	disp('This is partial of E(y1|y2=1):');
	table_bwg(parname_marginal,part_y1_y2_1(2:length(bi_cdf_marg)),3);		
end   


rho_mat = [1 rho; rho 1]
% ELASTICITY BELOW:
bi_cdf_elast = bi_cdf_marg .* (combined_rhs / ...
  mvncdf([combined_rhs*gamma_1 combined_rhs*gamma_2], 0, rho_mat) );
% by page 783 of 7th ed. Greene

disp('This is bivariate probability elasticity of both=1:');
table_bwg(parname_marginal,bi_cdf_elast(2:length(bi_cdf_elast))',3);


elast_y1_y2_1 = part_y1_y2_1' .* ( combined_rhs / ...
      (mvncdf([combined_rhs*gamma_1 combined_rhs*gamma_2], 0, rho_mat) ) )
     % / ...
      %  normcdf(combined_rhs*gamma_2) ) );
% by page 783 of 7th ed. Greene

disp('This is elasticity of E(y1|y2=1):');
table_bwg(parname_marginal,elast_y1_y2_1(2:length(elast_y1_y2_1)),3);	



elast_val = bi_cdf_elast(3)
  
F_hat_deriv = Grad([gamma_1 ; gamma_2 ; bp_bi; rho],'joint_credit_elast_fn',1);
F_hat_deriv = F_hat_deriv(13:22);

st_error_elast = F_hat_deriv * covbp_bi * F_hat_deriv';

t_stat_elast = elast_val / sqrt(  st_error_elast  );

% two-sided test
p_val = 1 - tcdf(t_stat_elast, size(base_data, 1) - length(F_hat_deriv));
fprintf('\nT-Stat. Income elasticity is zero (under joint):   %10.4f \n',t_stat_elast);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05/2
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end




elast_val = elast_y1_y2_1(3)
  
F_hat_deriv = Grad([gamma_1 ; gamma_2 ; bp_bi; rho],'cond_credit_elast_fn',1);
F_hat_deriv = F_hat_deriv(13:22);

st_error_elast = F_hat_deriv * covbp_bi * F_hat_deriv';

t_stat_elast = elast_val / sqrt(  st_error_elast  );

% two-sided test
p_val = 1 - tcdf(t_stat_elast, size(base_data, 1) - length(F_hat_deriv));
fprintf('\nT-Stat. Income elasticity is zero (under conditional):   %10.4f \n',t_stat_elast);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05/2
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end








