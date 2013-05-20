
%% QUESTION 1

%% Q. 1.a

urlwrite('http://www.aae.wisc.edu/aae637/data/matlab/cheese_only_data.xls','temp.xls');
[full_data,varnames,raw]=xlsread('temp.xls');

full_data(:, end+1) = repmat(1, size(full_data, 1), 1);

varnames(end+1)={'intercept'};

cheese_data = pull_data(varnames, ...
{'pc_tchzq' 'intercept' 'P_CHZ' 'incomet' 'refrig' 'perfafh' 'sm_city' 'city' 'hhsize' 'regdf' 'perlt6' 'per6_11' 'perge66'}, full_data);

cheese_data(:, 1) = pull_data(varnames, {'tchzq'}, full_data) ./ pull_data(varnames, {'hhsize'}, full_data);




start_vals = inv(cheese_data(:, 2:end)'*cheese_data(:, 2:end))*cheese_data(:, 2:end)'*cheese_data(:, 1);

df = size(cheese_data, 1) - length(start_vals);
ehat = cheese_data(:, 1) - cheese_data(:, 2:end)*start_vals;
sse = ehat'*ehat;
sighat = sse/df;

% start_vals = [start_vals; sighat]

start_vals = [  -0.0565; -0.0022;  0.0467;  0.0536; -0.1666;  0.0327;  0.0241; ...  
 -0.0147;  0.0605; -0.0204;  0.0236;  0.0198;  0.0712];

[cheese_betas, cheese_cov, cheese_llf_vec]  = max_bhhh(start_vals, ...
  {'intercept' 'P_CHZ' 'incomet' 'refrig' 'perfafh' 'sm_city' 'city' 'hhsize' 'regdf' 'perlt6' 'per6_11' 'perge66' 'sigma'}, ...
  size(cheese_data, 1), ...
  500, 1e-6, @tobit_llf, 10, 0.0001, ...
  cheese_data, 0, 0);
  
parname = {'intercept' 'P_CHZ' 'incomet' 'refrig' 'perfafh' 'sm_city' 'city' 'hhsize' 'regdf' 'perlt6' 'per6_11' 'perge66' 'sigma'}
  
cov_analytic=tobit_info_mat(cheese_betas, cheese_data(:, 2:end)); %**Call Out Infor. Mat-Based Cov ***
std_anal=sqrt(diag(cov_analytic));    %*** Analytic Std. Err. ***
mlzval=cheese_betas./std_anal;             %*** Analytic Z-Ratio ***
stdapp=sqrt(diag(cheese_cov));        %*** Approximate Std. Err. ***
mlz_orig=cheese_betas./stdapp;             %*** Approximate Z-Ratio ***
p_anal=2*(1-normcdf(abs(mlzval)));    %*** Analytic P-Value ***
p_appr=2*(1-normcdf(abs(mlz_orig)));  %*** Approximate P-Value ***
results2=horzcat(cheese_betas,std_anal,mlzval,p_anal,stdapp,mlz_orig,p_appr);
disp('*******Compare Analytical and Numerical Param. Std. Errors*******');
table_bwg(parname,results2,10); %*** Stdapp=numerical Hessian S.E. ***




start_vals = inv(cheese_data(:, 2)'*cheese_data(:, 2))*cheese_data(:, 2)'*cheese_data(:, 1);

df = size(cheese_data, 1) - length(start_vals);
ehat = cheese_data(:, 1) - cheese_data(:, 2)*start_vals;
sse = ehat'*ehat;
sighat = sse/df;

start_vals = [start_vals; sighat]

%start_vals = [  -0.0565; -0.0022;  0.0467;  0.0536; -0.1666;  0.0327;  0.0241; ...  
% -0.0147;  0.0605; -0.0204;  0.0236;  0.0198;  0.0712];

start_vals =  [ -0.0920;  0.0768];

[cheese_null_betas, cheese_null_cov, cheese_null_llf_vec]  = max_bhhh(start_vals, ...
  {'intercept' 'sigma'}, ...
  size(cheese_data, 1), ...
  500, 1e-6, @tobit_llf, 10, 0.0001, ...
  cheese_data(:, 1:2), 0, 0);


lr_test_output = 2 * ( sum(cheese_llf_vec) - sum(cheese_null_llf_vec));

lr_test_p_val = 1 - chi2cdf(lr_test_output, 3);
fprintf('LR Stat. (H_0: Model has explanatory power): %10.4f \n', lr_test_output);
fprintf('Prob Assum. H_0:            %10.4f \n', lr_test_p_val);
if lr_test_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end

% TODO: Correlation coef








%% Q. 1.b



prob_elas=post_est_cond([cheese_betas; 2], mean(cheese_data(:, 2:end))');
cond_elas=post_est_prob([cheese_betas; 2], mean(cheese_data(:, 2:end))');
total_elas=post_est_total([cheese_betas; 2], mean(cheese_data(:, 2:end))');

grad_prob=Grad([cheese_betas; 2], @post_est_prob, 1, .00001, mean(cheese_data(:, 2:end))');
grad_cond=Grad([cheese_betas; 2], @post_est_cond, 1, .00001, mean(cheese_data(:, 2:end))');
grad_total=Grad([cheese_betas; 2], @post_est_total, 1, .00001, mean(cheese_data(:, 2:end))');
grad_prob=grad_prob(1:(end-1));
grad_cond=grad_cond(1:(end-1));
grad_total=grad_total(1:(end-1));


%****************** Delta Method for Elasticity Variance ***************
disp('The 3 Income Elasticities as shown in McDonald and Moffitt');
var_probability=grad_prob*cheese_cov*grad_prob'; %***Prob Elas Var***
var_conditional=grad_cond*cheese_cov*grad_cond'; %***Cond E(Y) Elas Var***
var_total=grad_total*cheese_cov*grad_total';     %**Uncond E(Y)Elas Var**
all_elas=vertcat(prob_elas,cond_elas,total_elas);%*** Stack Elas. ***
all_elas_se=sqrt(vertcat(var_probability,var_conditional,var_total));
                                                 %*** SE of All Elas.***    
z_elas=all_elas./all_elas_se;                    %*** Z-Value All Elas ***     
pvalue_elas=2*(1-normcdf(abs(z_elas)));          %*** All Elas P-Value *** 
results_elas=horzcat(all_elas,all_elas_se,z_elas,pvalue_elas);
elas_name={'Prob-price','Cond-price','Total-price'};
table_bwg(elas_name,results_elas,9);



total_elas=post_est_total([cheese_betas; 3], mean(cheese_data(:, 2:end))');

grad_total=Grad([cheese_betas; 3], @post_est_total, 1, .00001, mean(cheese_data(:, 2:end))');
grad_total=grad_total(1:(end-1));


%****************** Delta Method for Elasticity Variance ***************

var_total=grad_total*cheese_cov*grad_total';     %**Uncond E(Y)Elas Var**
all_elas=vertcat(total_elas);%*** Stack Elas. ***
all_elas_se=sqrt(vertcat(var_total));
                                                 %*** SE of All Elas.***    
z_elas=all_elas./all_elas_se;                    %*** Z-Value All Elas ***     
pvalue_elas=2*(1-normcdf(abs(z_elas)));          %*** All Elas P-Value *** 
results_elas=horzcat(all_elas,all_elas_se,z_elas,pvalue_elas);
elas_name={'Total-Inc'};
table_bwg(elas_name,results_elas,9);






%% Q. 1.c

num_het = 6
num_betas = 12

b0_hetero=vertcat(cheese_betas(1:(num_betas)),log(cheese_betas(num_betas+1)), ...
(ones(num_het-1,1).* .0001))

start_vals=  [ -0.0702; -0.0015;  0.0229;  0.0350; -0.1000;  0.0122;  0.0147;  0.0043;  0.0352;  0.0249;  0.0480; -0.2138; -2.0420;  0.1520;  
 -0.3250; -0.7427; -0.6814;  1.9923];

[cheese_het_betas, cheese_het_cov, cheese_het_llf_vec]  = max_bhhh( ...
  start_vals, ...
  {'intercept' 'P_CHZ' 'incomet' 'refrig' 'perfafh' 'sm_city' 'city' 'hhsize' 'regdf' 'perlt6' 'per6_11' 'perge66' 'sigma' 'a_incomet' 'a_hhsize' 'a_perlt6' 'a_per6_11' 'a_perge66'}, ...
  size(cheese_data, 1), ...
  500, 1e-5, @het_tobit_llf, 3, .00001, ...
  cheese_data, 0, 0);


lr_test_output = 2 * ( sum(cheese_het_llf_vec) - sum(cheese_llf_vec));

lr_test_p_val = 1 - chi2cdf(lr_test_output, 3);
fprintf('LR Stat. (H_0: True model is homoskedastic): %10.4f \n', lr_test_output);
fprintf('Prob Assum. H_0:            %10.4f \n', lr_test_p_val);
if lr_test_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end




%% QUESTION 2

%% Q. 2.a

cheese_iop_data = [cheese_data pull_data(varnames, {'mp_age' 'mp_ltgs' 'mp_high'}, full_data)];

%cheese_iop_data(:,3)  = cheese_iop_data(:,3) ./ 100;
%cheese_iop_data(:,14)  = cheese_iop_data(:,14) ./ 10;

parnames = {'intercept' 'P_CHZ' 'incomet' 'refrig' 'perfafh' 'sm_city' 'city' 'hhsize' 'regdf' 'perlt6' 'per6_11' 'perge66' 'sigma' 'p_intercept' 'p_mp_age' 'p_incomet' 'p_refrig' 'p_hhsize' 'p_perfafh' 'p_mp_ltgs' 'p_mp_high' 'p_sm_city' 'p_city' 'p_regdf'};

%start_vals = [cheese_betas; .0001; .0001; .0001; .0001; .0001; .0001; .0001; .0001; .0001; .0001; .0001];

start_vals =  [  0.0690 ;-0.0014  ;0.0296 ; 0.0304; -0.0694;  0.0099  ;0.0107 ;-0.0151 ; 0.0269 ;-0.0346; -0.0068 ; 0.0195 ; 0.0160; -0.4056; -0.0129; -0.0486; -0.0680 ; 0.4120; -0.4297;  0.3009 ; 0.0222  ;
 -0.2503 ; 0.2741;  0.5775] ;


[cheese_iop_betas, cheese_iop_cov, cheese_iop_llf_vec]  = max_bhhh( ...
  start_vals, ...
  parnames, ...
  size(cheese_data, 1), ...
  500, 1e-4, @tobit_iop_llf, 1, .00001, ...
  cheese_iop_data, 0, 0);



lr_test_output = 2 * ( sum(cheese_iop_llf_vec) - sum(cheese_llf_vec));

lr_test_p_val = 1 - chi2cdf(lr_test_output, 3);
fprintf('LR Stat. (H_0: IOP model explains more than simple model): %10.4f \n', lr_test_output);
fprintf('Prob Assum. H_0:            %10.4f \n', lr_test_p_val);
if lr_test_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end

%TODO: Something is wrong with the LR test stat in both het and IOP models. Am I missing a constant term?


%% Q. 2.b



cheese_demand_elast_val = cheese_demand_elast(cheese_iop_betas, cheese_iop_data)

cheese_purchase_elast_val = cheese_purchase_elast(cheese_iop_betas, cheese_iop_data)



F_hat_deriv = Grad(cheese_iop_betas, @cheese_demand_elast, 1, .00001, cheese_iop_data);

% Grad(x0,func,num_row, dh, x_mat)
%F_hat_deriv = F_hat_deriv(1:6);

st_error_elast = F_hat_deriv * cheese_iop_cov * F_hat_deriv';

t_stat_elast = cheese_demand_elast_val  / sqrt(  st_error_elast  );

% two-sided test
p_val = 1 - tcdf(t_stat_elast, size(med_data, 1) - length(F_hat_deriv));
fprintf('\nT-Stat. Inc elast is 0 for demand :   %10.4f \n',t_stat_elast);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05/2
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end




F_hat_deriv = Grad(cheese_iop_betas, @cheese_purchase_elast, 1, .00001, cheese_iop_data);

% Grad(x0,func,num_row, dh, x_mat)
%F_hat_deriv = F_hat_deriv(1:6);

st_error_elast = F_hat_deriv * cheese_iop_cov * F_hat_deriv';

t_stat_elast = cheese_purchase_elast_val / sqrt(  st_error_elast  );

% two-sided test
p_val = 1 - tcdf(t_stat_elast, size(med_data, 1) - length(F_hat_deriv));
fprintf('\nT-Stat. Inc elast is 0 for purchases :   %10.4f \n',t_stat_elast);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05/2
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end





cheese_demand_price_elast_val = cheese_demand_price_elast(cheese_iop_betas, cheese_iop_data)

cheese_purchase_price_elast_val = cheese_purchase_price_elast(cheese_iop_betas, cheese_iop_data)



F_hat_deriv = Grad(cheese_iop_betas, @cheese_demand_price_elast, 1, .00001, cheese_iop_data);

% Grad(x0,func,num_row, dh, x_mat)
%F_hat_deriv = F_hat_deriv(1:6);

st_error_elast = F_hat_deriv * cheese_iop_cov * F_hat_deriv';

t_stat_elast = abs(cheese_demand_price_elast_val  / sqrt(  st_error_elast  ));

% two-sided test
p_val = 1 - tcdf(t_stat_elast, size(med_data, 1) - length(F_hat_deriv));
fprintf('\nT-Stat. Inc elast is 0 for demand :   %10.4f \n',t_stat_elast);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05/2
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end




F_hat_deriv = Grad(cheese_iop_betas, @cheese_purchase_price_elast, 1, .00001, cheese_iop_data);

% Grad(x0,func,num_row, dh, x_mat)
%F_hat_deriv = F_hat_deriv(1:6);

st_error_elast = F_hat_deriv * cheese_iop_cov * F_hat_deriv';

t_stat_elast = abs(cheese_purchase_price_elast_val / sqrt(  st_error_elast  ));

% two-sided test
p_val = 1 - tcdf(t_stat_elast, size(med_data, 1) - length(F_hat_deriv));
fprintf('\nT-Stat. Inc elast is 0 for purchases :   %10.4f \n',t_stat_elast);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05/2
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end






%% Q. 2.c


cheese_uncon_elast_val = cheese_uncon_elast(cheese_iop_betas, cheese_iop_data)



F_hat_deriv = Grad(cheese_iop_betas, @cheese_uncon_elast, 1, .00001, cheese_iop_data);

% Grad(x0,func,num_row, dh, x_mat)
%F_hat_deriv = F_hat_deriv(1:6);

st_error_elast = F_hat_deriv * cheese_iop_cov * F_hat_deriv';

t_stat_elast = abs(cheese_uncon_elast_val  / sqrt(  st_error_elast  ));

% two-sided test
p_val = 1 - tcdf(t_stat_elast, size(med_data, 1) - length(F_hat_deriv));
fprintf('\nT-Stat. Inc elast is 0 for demand :   %10.4f \n',t_stat_elast);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05/2
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end



cheese_uncon_elast_dif_val = cheese_uncon_elast_dif(cheese_iop_betas, cheese_iop_data)



F_hat_deriv = Grad(cheese_iop_betas, @cheese_uncon_elast_dif, 1, .00001, cheese_iop_data);

% Grad(x0,func,num_row, dh, x_mat)
%F_hat_deriv = F_hat_deriv(1:6);

st_error_elast = F_hat_deriv * cheese_iop_cov * F_hat_deriv';

t_stat_elast = abs(cheese_uncon_elast_dif_val  / sqrt(  st_error_elast  ));

% two-sided test
p_val = 1 - tcdf(t_stat_elast, size(med_data, 1) - length(F_hat_deriv));
fprintf('\nT-Stat. Inc elast is 0 for demand :   %10.4f \n',t_stat_elast);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05/2
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end













%% QUESTION 3

%% Q. 3.a

urlwrite('http://www.aae.wisc.edu/aae637/data/matlab/german_health_1988.xlsx','temp.xls');
[full_data,varnames,raw]=xlsread('temp.xls');

full_data(:, end+1) = repmat(1, size(full_data, 1), 1);

varnames(end+1)={'intercept'};

med_data = pull_data(varnames, ...
{'docvis' 'public' 'intercept' 'age' 'hhninc' 'hhkids' 'educ' 'married' 'female'}, full_data);

med_data(:, 1) = med_data(:, 1)>0;
med_data(:, 5) = med_data(:, 5) ./ 10000;



start_vals = inv(med_data(:, 3:8)'*med_data(:, 3:8))*med_data(:, 3:8)'*med_data(:, 1);

start_vals = [start_vals ; inv(med_data(:, [3 4 7 9])'*med_data(:, [3 4 7 9]))*med_data(:, [3 4 7 9])'*med_data(:, 2)];

parnames={'intercept' 'age' 'hhninc' 'hhkids' 'educ' 'married' 'intercept2' 'age2' 'educ2' 'female2'};


[med_no_rho_betas, med_no_rho_cov, med_no_rho_llf_vec]  = max_bhhh( ...
  start_vals, ...
  parnames, ...
  size(med_data, 1), ...
  500, 1e-4, @sample_select_no_rho_llf, 1, .000001, ...
  med_data, 0, 0);
  

parnames={'intercept' 'age' 'hhninc' 'hhkids' 'educ' 'married' 'intercept2' 'age2' 'educ2' 'female2', 'rho'};

start vals should be:    -0.5227
    0.0115
   -0.4314
   -0.1134
    0.0566
    0.1349
    3.4549
   -0.0013
   -0.1938
    0.1524
   -1.6273

[med_yes_rho_betas, med_yes_rho_cov, med_yes_rho_llf_vec]  = max_bhhh( ...
  [med_no_rho_betas; 0], ...
  parnames, ...
  size(med_data, 1), ...
  500, 1e-4, @sample_select_yes_rho_llf, 1, .00001, ...
  med_data, 0, 0);

sample_select_no_rho_llf(start_vals, med_data)


lr_test_output = 2 * ( sum(med_yes_rho_llf_vec) - sum(med_no_rho_llf_vec));

lr_test_p_val = 1 - chi2cdf(lr_test_output, 3);
fprintf('LR Stat. (H_0: There is no endogenous selection): %10.4f \n', lr_test_output);
fprintf('Prob Assum. H_0:            %10.4f \n', lr_test_p_val);
if lr_test_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end



%% Q. 3.b

parname_1={'Int_1','WifeAge1','Income1','Avg_Exp1'};  %** Derogatory Report Names **
parname_2={'Int_2','WifeAge2','Income2','OwnRent2','SlfEmpl2'}; %**Credit Card Names **
parname_combined={'Int_1','WifeAge1','Income1','Avg_Exp1', ...
        'Int_2','WifeAge2','Income2','OwnRent2','SlfEmpl2','Tanh_arg'};
parname_marginal={'WifeAge','Income','Avg_Exp','OwnRent','SelfEmpl'};




%	b1_bi=bp_bi(1:numc_1,:);
%	b2_bi=bp_bi((numc_1+1):(numc_1+numc_2),:);
	TODO:
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





b1_bi=med_yes_rho_betas(1:6);
b2_bi=med_yes_rho_betas(7:10);

gamma_1=vertcat(b1_bi,0);  %**gamma_1 of full dimension **
gamma_2=vertcat(b2_bi(1:2),0,0, b2_bi(3), 0, b2_bi(4)); %** gamma_2 of full dimension **

q1_mn=1;
q2_mn=1;
w1_mn=q1_mn.*(mean(med_data(:,3:9))*gamma_1); 
w2_mn=q2_mn.*(mean(med_data(:,3:9))*gamma_2); 
rho = tanh(med_yes_rho_betas(11));

rho_star_mn=q1_mn.*q2_mn.*rho;        %** Defined on p.740, Greene ***
numer_1=w2_mn-rho_star_mn.*w1_mn;     %** Numerator of 17.52, p.740 ***
numer_2=w1_mn-rho_star_mn.*w2_mn;     %** Numerator of 17.52, p.740 ***
denom=sqrt(1-rho_star_mn^2);          %** Denomenator of 17.52, p.740 ***
g_1=normpdf(w1_mn).*normcdf(numer_1./denom);%** Defined by 17.52, p.740 ***
g_2=normpdf(w2_mn).*normcdf(numer_2./denom);%** Defined by 17.52, p.740 ***
%bi_cdf_marg=g_1.*gamma_1+g_2.*gamma_2; %**Partial of Bivariate CDF, p.743***
bi_cdf_marg=g_1.*gamma_1'+g_2.*gamma_2';

med_inc_elast_val = normpdf(mean(med_data(:, 3:8))*med_yes_rho_betas(1:6)) * med_yes_rho_betas(2) * ...
   mean(med_data(:, 4))/ normcdf(mean(med_data(:,3:8))*med_yes_rho_betas(1:6))


  
F_hat_deriv = Grad(med_yes_rho_betas, @med_inc_elast, 1, .00001, med_data);
% Oops, mis-named this function

% Grad(x0,func,num_row, dh, x_mat)
F_hat_deriv = F_hat_deriv(1:6);

st_error_elast = F_hat_deriv * med_yes_rho_cov(1:6,1:6) * F_hat_deriv';

t_stat_elast = med_inc_elast_val / sqrt(  st_error_elast  );

% two-sided test
p_val = 1 - tcdf(t_stat_elast, size(med_data, 1) - length(F_hat_deriv));
fprintf('\nT-Stat. Age elasticity is zero :   %10.4f \n',t_stat_elast);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05/2
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end




omega=[1 rho_star_mn;rho_star_mn 1];
mu=[0 0];
lim_bi=[w1_mn w2_mn];
part_2_2=(g_2-mvncdf(lim_bi,mu,omega).*normpdf(w2_mn)./normcdf(w2_mn)).*gamma_2;
part_y1_y2_1=(1./normcdf(w2_mn)).*(g_1*gamma_1+part_2_2);
rho_mat = [1 rho; rho 1]

elast_y1_y2_1 = part_y1_y2_1' .* ( mean(med_data(:,3:9)) / ...
      (mvncdf([mean(med_data(:,3:9))*gamma_1 mean(med_data(:,3:9))*gamma_2], 0, rho_mat) ) )


elast_val = elast_y1_y2_1(2)

F_hat_deriv = Grad(med_yes_rho_betas, @med_cond_prob, 1, .00001, med_data);


% Grad(x0,func,num_row, dh, x_mat)
%F_hat_deriv = F_hat_deriv(1:6);

st_error_elast = F_hat_deriv * med_yes_rho_cov * F_hat_deriv';

t_stat_elast = elast_val / sqrt(  st_error_elast  );

% two-sided test
p_val = 1 - tcdf(t_stat_elast, size(med_data, 1) - length(F_hat_deriv));
fprintf('\nT-Stat. Age elasticity is zero (conditional prob) :   %10.4f \n',t_stat_elast);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05/2
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end




% TODO: DELETE BELOW:

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



 med_inc_elast



parnames={'intercept' 'age' 'hhninc' 'hhkids' 'educ' 'married' 
        'intercept2' 'age2' 'educ2' 'female2', 'rho'};


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











