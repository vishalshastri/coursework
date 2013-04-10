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

%TODO: is there an impact?? second-guessing - just the p value or the mean of data or ??

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
disp('This is the elas. effect of Air_D*Inc on Mode Choice Prob.');
disp('******************************************************');
table_bwg(mode_name,elas_effects3,5);
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

%% TODO



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
  {'const' 'cost' 'c_rate' 'income' 'mu_pier' 'mu_priv' 'mu_chart'}, ...
  size(ord_fish_data, 1), ...
  250, 1e-6, @fish_ordered_llf, 1, 0.000001, ...
  ord_fish_data, 0, 0);




fish_ordered_llf(start_vals, ord_fish_data)


inv(full_data'*full_data)


test = Grad(start_vals,@fish_ordered_llf,size(ord_fish_data, 1), 0.000001, ord_fish_data);

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