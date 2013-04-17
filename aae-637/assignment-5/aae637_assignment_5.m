%% QUESTION 2

%% Q. 2.a


urlwrite('http://www.aae.wisc.edu/aae637/data/matlab/New_fish_file.xls','temp.xls');
[full_data,varnames,raw]=xlsread('temp.xls');

fish_data = horzcat( ...
  full_data(:, 2), ...
  full_data(:, 3:5), ...
  (full_data(:, 2)==3) .* full_data(:, 6), ...
  full_data(:, 2)==2, ...
  full_data(:, 2)==3, ...
  full_data(:, 2)==4 ...
);

% This is how you find the inc mean for the three modes of fishing
mean(full_data(full_data(:,2)==1 & full_data(:,3)==1, 6))
mean(full_data(full_data(:,2)==2 & full_data(:,3)==1, 6))
mean(full_data(full_data(:,2)==3 & full_data(:,3)==1, 6))
mean(full_data(full_data(:,2)==4 & full_data(:,3)==1, 6))

% full_data(:, 3:6), 

start_vals = inv(fish_data(:, 3:end)'*fish_data(:, 3:end))*fish_data(:, 3:end)'*fish_data(:, 2);

[fish_betas, fish_cov, fish_llf_vec]  = max_bhhh(start_vals, ...
  {'cost' 'c_rate' 'inc_x_priv' 'pier' 'priv_boat' 'chart_boat'}, ...
  size(fish_data, 1)/size(unique(fish_data(:, 1)), 1), ...
  250, 1e-6, @cond_logit_llf, 1, 0.000001, ...
  fish_data, 0, 0);


hessian_cov=inv(-ml_hess(@tot_cond_logit_llf, fish_betas, fish_data));  
                                 %** Hessian Based Cov. Matrix **
se_hess=sqrt(diag(hessian_cov));
tvalue2=fish_betas./se_hess;
df2= size(fish_data, 1)/size(unique(fish_data(:, 1)), 1)-length(fish_betas);               % Degrees of freedom for t-value
pvalue2=2*(1-tcdf(abs(tvalue2),df2));  % Column vector of param p-values 
results2=horzcat(fish_betas,se_hess,tvalue2,pvalue2);
disp('  ');
disp('*****Conditional Logit Results:  Hessian Based Cov *****');
table_bwg({'cost' 'c_rate' 'inc_x_priv' 'pier' 'priv_boat' 'chart_boat'},results2,1);


[fish_no_inc_betas, fish_no_inc_cov, fish_no_inc_llf_vec]  = max_bhhh(start_vals([1 2 4:6]), ...
  {'cost' 'c_rate' 'pier' 'priv_boat' 'chart_boat'}, ...
  size(fish_data, 1)/size(unique(fish_data(:, 1)), 1), ...
  250, 1e-6, @cond_logit_llf, 1, 0.000001, ...
  fish_data(:, [1:4 6:8]), 0, 0);


lr_test_output = 2 * ( sum(fish_llf_vec) - sum(fish_no_inc_llf_vec));

lr_test_p_val = 1 - chi2cdf(lr_test_output, 1);
fprintf('LR Stat. (H_0: Income has no impact on fishing mode choice): %10.4f \n', lr_test_output);
fprintf('Prob Wald Stat. Assum. H_0:            %10.4f \n', lr_test_p_val);
if lr_test_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end






%% Q. 2.b


fish_data_omit = fish_data( fish_data(:, end)==0, 1:(end-1));

% full_data(:, 3:6), 

start_vals = inv(fish_data_omit(:, 3:end)'*fish_data_omit(:, 3:end))*fish_data_omit(:, 3:end)'*fish_data_omit(:, 2);

[fish_omit_betas, fish_omit_cov, fish_omit_llf_vec]  = max_bhhh(start_vals, ...
  {'cost' 'c_rate' 'inc_x_priv' 'pier' 'priv_boat'}, ...
  size(fish_data_omit, 1)/size(unique(fish_data_omit(:, 1)), 1), ...
  250, 1e-6, @cond_logit_llf, 1, 0.000001, ...
  fish_data_omit, 0, 0);

hessian_cov_omit=inv(-ml_hess(@tot_cond_logit_llf, fish_omit_betas, fish_data_omit));  



beta_dif = fish_betas(1:(end-1)) - fish_omit_betas;

chi_square = beta_dif' * ...
  inv( hessian_cov_omit - hessian_cov(1:(end-1), 1:(end-1)) ) * ...
  beta_dif;

       %***** Chi-square Stat, Greene, p.724 ********
fprintf('This is the IIA Chi_square for charter boat: %6.4f',chi_square); 
disp('  ');
fprintf('Degrees of Freedom:%4.0f', length(fish_betas)); disp('  ');
chi_sq_critical=chi_bwg(0.05,length(fish_betas),chi_square);


%% Q. 2.c


%******************Est.Elasticity Effects *******************

nalts = size(unique(fish_data(:, 1)), 1);
mode_id = fish_data(:, 1);
rhsvar = fish_data(:, 3:end);
bp_multi = fish_betas;
numvar= length(fish_data(1, 3:end));
mode_name={'Beach','Pier','Priv Boat','Ch boat'};

for j=1:nalts; %****Mode Specific Means ****
  mode_var=rhsvar(mode_id == j,:);
  if j == 1;
     mean_mode=mean(mode_var)';
  else
     mean_mode=horzcat(mean_mode,mean(mode_var)');
  end;
end;
denom=0;
for i=1:nalts;
   rhsvar_i=rhsvar(mode_id == i,:);   
   mean_rhs=mean(rhsvar_i);
   denom=denom+exp(mean_rhs*bp_multi);
   if i == 1;
      mean_mat=mean_rhs;
   else
      mean_mat=vertcat(mean_mat,mean_rhs);
   end;
end;
for i=1:nalts;
   if i == 1;
      est_prob=exp(mean_mat(i,:)*bp_multi)./denom;
   else
      est_prob=vertcat(est_prob,(exp(mean_mat(i,:)*bp_multi)./denom));
   end; 	
end;  
  
   for j=1:nalts;
     for k=1:nalts;
      for i=1:numvar;
      	if (j == k);
            elas_effects=mean_mode(i,k).*(1-est_prob(k)).*bp_multi(i);                                                  %****Greene p. 846****/
        else
            elas_effects=mean_mode(i,k).*(-est_prob(k)).*bp_multi(i);
        end;
        if i == 1;
            elas_effects1(k,j)=elas_effects;
        elseif i == 2;
            elas_effects2(k,j)=elas_effects;
        elseif i == 3;
            elas_effects3(k,j)=elas_effects;
        end;                
      end;      
     end;
   end;

disp('This is the elas. effect of Mode Cost on Mode Choice Prob.');
disp('******************************************************');
table_bwg(mode_name,elas_effects1,5);
disp('******************************************************'); 
disp('This is the elas. effect of Term. Time on Mode Choice Prob.');
disp('******************************************************');
table_bwg(mode_name,elas_effects2,5);
disp('******************************************************'); 


F_hat_deriv = Grad(fish_betas, @beach_cost_elast_fn, 1, .00001, fish_data);
st_error_inc_elast = F_hat_deriv * hessian_cov * F_hat_deriv';
t_stat_inc_elast = abs(elas_effects1(1,1)) / sqrt(  st_error_inc_elast );

% two-sided test
p_val = 1 - tcdf(t_stat_inc_elast, size(fish_data, 1) - size(fish_data, 1)/size(unique(fish_data(:, 1)),1));
fprintf('\nT-Stat. H0: Beach cost elasticity is zero:   %10.4f \n',t_stat_inc_elast);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05/2
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end




F_hat_deriv = Grad(fish_betas, @priv_cost_elast_fn, 1, .00001, fish_data);
st_error_inc_elast = F_hat_deriv * hessian_cov * F_hat_deriv';
t_stat_inc_elast = abs(elas_effects1(3,3)) / sqrt(  st_error_inc_elast );

% two-sided test
p_val = 1 - tcdf(t_stat_inc_elast, size(fish_data, 1) - size(fish_data, 1)/size(unique(fish_data(:, 1)),1));
fprintf('\nT-Stat. H0: Private boat cost elasticity is zero:   %10.4f \n',t_stat_inc_elast);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05/2
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end



F_hat_deriv = Grad(fish_betas, @chart_cost_elast_fn, 1, .00001, fish_data);
st_error_inc_elast = F_hat_deriv * hessian_cov * F_hat_deriv';
t_stat_inc_elast = abs(elas_effects1(3,3)) / sqrt(  st_error_inc_elast );

% two-sided test
p_val = 1 - tcdf(t_stat_inc_elast, size(fish_data, 1) - size(fish_data, 1)/size(unique(fish_data(:, 1)),1));
fprintf('\nT-Stat. H0: Charter boat cost elasticity is zero:   %10.4f \n',t_stat_inc_elast);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05/2
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end




F_hat_deriv = Grad(fish_betas, @pier_chart_dif_elast_fn, 1, .00001, fish_data);
st_error_inc_elast = F_hat_deriv * hessian_cov * F_hat_deriv';
t_stat_inc_elast = abs(elas_effects1(2,2) - elas_effects1(4,4)) / sqrt(  st_error_inc_elast );

% two-sided test
p_val = 1 - tcdf(t_stat_inc_elast, size(fish_data, 1) - size(fish_data, 1)/size(unique(fish_data(:, 1)),1));
fprintf('\nT-Stat. H0: Pier & Charter cost elasticity are equal:   %10.4f \n',t_stat_inc_elast);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05/2
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end




%% Q. 2.d

disp('This is the elas. effect of Priv_D*Inc on Mode Choice Prob.');
disp('******************************************************');
table_bwg(mode_name,elas_effects3,5);
disp('******************************************************');




%% QUESTION 3

%% Q. 3.a

clear
clc



urlwrite('http://www.aae.wisc.edu/aae637/data/matlab/fish_data_v2.xlsx','temp.xls');
[full_data,varnames,raw]=xlsread('temp.xls');


ord_fish_data = horzcat(repmat(1, size(full_data,1), 1), full_data(:, 2:end));

rhsvar = ord_fish_data(:, [1 2 3 16]);

start_vals = inv(rhsvar'*rhsvar)*rhsvar'*full_data(:, 1);
start_vals=vertcat(start_vals,0.1,1.1);  

[fish_betas, fish_cov, fish_llf_vec]  = max_bhhh(start_vals, ...
  {'const' 'cost' 'c_rate' 'income' 'mu_pier' 'mu_priv' }, ...
  size(ord_fish_data, 1), ...
  250, 1e-6, @fish_ordered_llf, 1, 0.000001, ...
  ord_fish_data, 0, 0);

%% Q. 3.b

mean_rhsvar = mean(rhsvar(:, 1:4));
x_beta = mean_rhsvar*fish_betas(1:4);
mu_1 = fish_betas(5);
mu_2 = fish_betas(6);

day_cost_beach_elast = -normpdf(-x_beta) * fish_betas(2) * ...
  (mean_rhsvar(2) / normcdf(-x_beta) )
  
day_cost_pier_elast = ( normpdf(-x_beta) - normpdf(mu_1 - x_beta ) ) * fish_betas(2) * ...
  (mean_rhsvar(2) / (normcdf(mu_1 - x_beta) - normcdf(-x_beta) ) )

day_cost_priv_elast = ( normpdf(mu_1-x_beta) - normpdf(mu_2 - x_beta ) ) * fish_betas(2) * ...
  (mean_rhsvar(2) / (normcdf(mu_2 - x_beta) - normcdf(mu_1-x_beta) ) )

day_cost_charter_elast = ( normpdf(mu_2 - x_beta ) ) * fish_betas(2) * ...
  (mean_rhsvar(2) / (1-normcdf(mu_2 - x_beta)) )



day_cost_beach_elast = fish_elast_fn([fish_betas; 2; 1], rhsvar)
day_cost_pier_elast = fish_elast_fn([fish_betas; 2; 2], rhsvar)
day_cost_priv_elast = fish_elast_fn([fish_betas; 2; 3], rhsvar)
day_cost_charter_elast = fish_elast_fn([fish_betas; 2; 4], rhsvar)


inc_beach_elast = fish_elast_fn([fish_betas; 4; 1], rhsvar)
inc_pier_elast = fish_elast_fn([fish_betas; 4; 2], rhsvar)
inc_priv_elast = fish_elast_fn([fish_betas; 4; 3], rhsvar)
inc_charter_elast = fish_elast_fn([fish_betas; 4; 4], rhsvar)

elast_args = cell(2, 8);

elast_args{1,1} = [fish_betas; 2; 1];
elast_args{1,2} = [fish_betas; 2; 2];
elast_args{1,3} = [fish_betas; 2; 3];
elast_args{1,4} = [fish_betas; 2; 4];

elast_args{2,1} = '\nT-Stat. H0: Elasticity impact of cost on beach is zero:   %10.4f \n';
elast_args{2,2} = '\nT-Stat. H0: Elasticity impact of cost on pier is zero:   %10.4f \n';
elast_args{2,3} = '\nT-Stat. H0: Elasticity impact of cost on private is zero:   %10.4f \n';
elast_args{2,4} = '\nT-Stat. H0: Elasticity impact of cost on charter is zero:   %10.4f \n';

elast_args{1,5} = [fish_betas; 4; 1];
elast_args{1,6} = [fish_betas; 4; 2];
elast_args{1,7} = [fish_betas; 4; 3];
elast_args{1,8} = [fish_betas; 4; 4];

elast_args{2,5} = '\nT-Stat. H0: Elasticity impact of inc on beach is zero:   %10.4f \n';
elast_args{2,6} = '\nT-Stat. H0: Elasticity impact of inc on pier is zero:   %10.4f \n';
elast_args{2,7} = '\nT-Stat. H0: Elasticity impact of inc on private is zero:   %10.4f \n';
elast_args{2,8} = '\nT-Stat. H0: Elasticity impact of inc on charter is zero:   %10.4f \n';

for i=1:8

  F_hat_deriv = Grad(elast_args{1,i}, @fish_elast_fn, 1, .00001, rhsvar);
  F_hat_deriv = F_hat_deriv(1:6);
  st_error_inc_elast = F_hat_deriv * fish_cov * F_hat_deriv';
  t_stat_inc_elast = abs(fish_elast_fn(elast_args{1,i}, rhsvar)) / sqrt(  st_error_inc_elast );

  % two-sided test
  p_val = 1 - tcdf(t_stat_inc_elast, size(ord_fish_data, 1) - size(fish_cov,1) );
  fprintf(elast_args{2,i},t_stat_inc_elast);
  fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
  if p_val < .05/2
      disp('    There is, therefore, enough evidence to reject H_0');
  else
      disp('    There is, therefore, not enough evidence to reject H_0');
  end

end







%% Q. 3.c

start_vals=vertcat(fish_betas, 0);  

[fish_het_betas, fish_het_cov, fish_het_llf_vec]  = max_bhhh(start_vals, ...
  {'const' 'cost' 'c_rate' 'income' 'mu_pier' 'mu_priv' 'sigma'}, ...
  size(ord_fish_data, 1), ...
  250, 1e-6, @fish_het_ordered_llf, 1, 0.000001, ...
  ord_fish_data, 0, 0);
  

lr_test_output = 2 * ( sum(fish_het_llf_vec) - sum(fish_llf_vec));

lr_test_p_val = 1 - chi2cdf(lr_test_output, 1);
fprintf('LR Stat. (H_0: hlthg = hlthf = hlthp = 0): %10.4f \n', lr_test_output);
fprintf('Prob Wald Stat. Assum. H_0:            %10.4f \n', lr_test_p_val);
if lr_test_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end


% TODO: Elasticities



x_beta = mean(ord_fish_data(:, [1 2 3 16])) * fish_het_betas(1:4);

sigma_est = exp( mean(ord_fish_data(:,16)) * fish_het_betas(end));

mu_2 = fish_het_betas(6);

inc_chart_elast_het =  normpdf(  (mu_2 - x_beta)./sigma_est ) * ...
  ( ( fish_het_betas(4) - (mu_2 - x_beta)*fish_het_betas(end) ) / sigma_est ) * ...
  mean(ord_fish_data(:,16)) * normcdf(  (mu_2 - x_beta)./sigma_est )

%p. 754 of Greene 7th ed

F_hat_deriv = Grad(fish_het_betas, @fish_elast_het_fn, 1, .00001, ord_fish_data);
%  F_hat_deriv = F_hat_deriv;
st_error_inc_elast = F_hat_deriv * fish_het_cov * F_hat_deriv';
t_stat_inc_elast = abs(inc_chart_elast_het) / sqrt(  st_error_inc_elast );

% two-sided test
p_val = 1 - tcdf(t_stat_inc_elast, size(ord_fish_data, 1) - size(fish_het_cov,1) );
fprintf('\nT-Stat. H0: Elasticity impact of income on charter is zero:   %10.4f \n',t_stat_inc_elast);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05/2
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end

















sigma_est = exp(z_mat*gamm);


log(1-normcdf((mu(j-1)-argue)./sigma_est))


day_cost_charter_elast = ( normpdf(mu_2 - x_beta ) ) * fish_betas(2) * ...
  (mean_rhsvar(2) / (1-normcdf(mu_2 - x_beta)) )






marginal:
nrom









LLF = -1343.4994
LLF = -1344.7452


fish_ordered_llf(start_vals, ord_fish_data)


inv(full_data'*full_data)


test = Grad(start_vals,@fish_het_ordered_llf,size(ord_fish_data, 1), 0.000001, ord_fish_data);

inv(test'*test)


dummy_mat = ord_fish_data(:, 5:7);
test = abs(sum(dummy_mat, 2)-1)



RHS is Day_cost and catch_rate and intercept and Mnth_Inc
{must add intercept}
OK, instructions say that we need intercept, but colineraity
dummy_mat is Beach_D through Chart_D

num_val is length(unique(depend))
num_mu = num_val - 1

Choice Var is useful to get OLS starting vals

  Columns 1 through 5

    'Choice'    'Day_Cost'    'CtchRate'    'Beach_D'    'Pier_D'

  Columns 6 through 10

    'Boat_D'    'Chart_D'    'Beach_V'    'Pier_V'    'Boat_V'

  Columns 11 through 14

    'Chart_V'    'ctch_bch'    'ctch_pie'    'ctch_bt'

  Columns 15 through 16

    'ctch_cht'    'Mnth_Inc'










test = cond_logit_llf(start_vals, fish_data)


    'HHID'    'MODE_ID'    'CHOICE'    'COST'    'C_RATE'

  Column 6

    'INCOME'






clear;					         % Command to clear memory 
clc;                             % Command to clear command window
global critic_limit iter_limit rhsvar numobs do_step func_name dh ...
    parname numc nalts num_ind mode_id mode mean_mode mode_name ...
    numvar mean_mat weight;
urlwrite('http://www.aae.wisc.edu/aae637/data/matlab/table_f_21_2.xls','temp.xls');
[base_data,varnames,raw]=xlsread('temp.xls');
[numobs,numc]=size(base_data);  % Determine size of full data matrix
%{
****************** Full data set  *********************************
******* Note the Number of Obs. is nalts*number of orig. obs. *****
*******************************************************************
%}
base_var={'GC','T_TIME'};
mode_var={'MODE'};	               %*** Identify Mode Actually Used ***
modeid_var={'MODE_ID'};            %*** Identify Mode Option ***
hhinc_var={'HH_INC'};
tempvar=pull_data(varnames,base_var,base_data);
mode=pull_data(varnames,mode_var,base_data);
mode_id=pull_data(varnames,modeid_var,base_data);
hh_inc=pull_data(varnames,hhinc_var,base_data);
dum_air=mode_id == 1;                 %*** Dummy Variable for Air ***
dum_train=mode_id == 2;               %*** Dummy Variable for Train ***
dum_bus=mode_id == 3;                 %*** Dummy variable for Bus ***
air_hhinc=dum_air.*hh_inc;     %*** Interaction of Air Dummy and HHINC ***
rhsvar=horzcat(tempvar,air_hhinc,dum_air,dum_train,dum_bus);
mode_name={'Air','Train','Bus','Auto'};
parname={'beta_g','beta_t','gamma_H','alpha_air','alp_train','alpha_bus'};
                                %***** Define Parameter Names *****
critic_limit=1e-6;              % Critical % change in parameters
iter_limit=250;                 % Maximum number of iterations
do_step=1;                      % =1 variable step length, 0 fixed 
func_name =('cond_logit_llf');  % Identify LLF function
alpha=.05;                      % Type I Error Probability 
dh = 0.000001;
actual_p=vertcat(0.14,0.13,0.09,0.64);	%*** Pop. choice probability ***
numvar=3;                       %*** Number of Variables to est elas.***
b0=vertcat(.45,1,.01,1,1,.1);     %*** Starting Values ***
%************************************************************************
[nalts,nc]=size(unique(mode_id));    %*** Number of Alternatives ***
num_ind=numobs./nalts;               %*** Number of Individuals ***
selected=mode_id(mode==1,:);
for i=1:nalts;                        %*** Sample choice prob. ***
   temp=selected(selected==i,:);
   [numindtemp,nc]=size(temp);
   if i == 1;
      sample_p=numindtemp./num_ind;
   else
      sample_p=vertcat(sample_p,(numindtemp./num_ind));
   end;
end;
weight=actual_p./sample_p;       %*** Weights used in choice prob. ***

%*************************************************************************  
disp('Now the Maximum Likelihood Iterations Begin for Conditonal Logit');
numobs=num_ind;
[bp_multi,covbp_multi,llf]=max_bhhh(b0,parname); 