%% QUESTION 1

%% Q. 1.a

path(path,'C:\Users\tdmcarthur\Documents\MATLAB\') ;

urlwrite('http://www.aae.wisc.edu/aae637/data/matlab/fluid_probit_data.xls','temp.xls');
[full_data,varnames,raw]=xlsread('temp.xls');


mod_data = horzcat( ...
    full_data(:, strcmp(varnames,'fluidx')) > 0, ...
    repmat(1, size(full_data, 1), 1), ...
    full_data(:, strcmp(varnames,'num_yung')), ...
    full_data(:, strcmp(varnames,'incomet')), ...
    full_data(:, strcmp(varnames,'num_yung')) .* full_data(:, strcmp(varnames,'incomet')), ...
    full_data(:, strcmp(varnames,'sm_city')), ...
    full_data(:, strcmp(varnames,'city')), ...
    full_data(:, strcmp(varnames,'refrig')), ...
    full_data(:, strcmp(varnames,'perfafh')) ...
    );

betas_start=(mod_data(:, 2:end)'*mod_data(:, 2:end))\(mod_data(:, 2:end)'*mod_data(:, 1));  

[fluid_betas,fluid_cov,fluid_LLF_vec] = probit_bwg(betas_start, mod_data, ...
  {'const', 'num_yung', 'incomet', 'yung_x_inc', 'sm_city', 'city', 'refrig', 'perfafh'}, ...
  @probit_llf, 1, 250, 1e-6);

fluid_wald_stat = fluid_betas(2:end)' * inv( [zeros(7,1) eye(7)] * ...
   fluid_cov * [zeros(7,1) eye(7)]' ) * fluid_betas(2:end);

chi_bwg(0.05,7,fluid_wald_stat);

%% Q. 1.b


resticted_data = horzcat( ...
    full_data(:, strcmp(varnames,'fluidx')) > 0, ...
    repmat(1, size(full_data, 1), 1), ...
    full_data(:, strcmp(varnames,'incomet')), ...
    full_data(:, strcmp(varnames,'sm_city')), ...
    full_data(:, strcmp(varnames,'city')), ...
    full_data(:, strcmp(varnames,'refrig')), ...
    full_data(:, strcmp(varnames,'perfafh')) ...
    );

betas_start=(resticted_data(:, 2:end)' * resticted_data(:, 2:end))\(resticted_data(:, 2:end)'*resticted_data(:, 1));  

[rest_fluid_betas,rest_fluid_cov,rest_fluid_LLF_vec] = probit_bwg(betas_start, resticted_data, ...
  {'const', 'incomet', 'sm_city', 'city', 'refrig', 'perfafh'}, ...
  @probit_llf, 1, 250, 1e-6);




lr_test_output = 2 * ( sum(fluid_LLF_vec) - sum(rest_fluid_LLF_vec));

lr_test_p_val = 1 - chi2cdf(lr_test_output, 3);
fprintf('LR Stat. (H_0: num_yung = yung_x_inc = 0): %10.4f \n', lr_test_output);
fprintf('Prob LR Stat. Assum. H_0:            %10.4f \n', lr_test_p_val);
if lr_test_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end

%% Q. 1.c



fluid_inc_elast_val = normpdf(mean(mod_data(:, 2:end))*fluid_betas) * ...
  (fluid_betas(2) + fluid_betas(3)*mean(mod_data(:, 4)) ) * ...
  (mean(mod_data(:, 3)) / normcdf(mean(mod_data(:, 2:end))*fluid_betas))
  
  
F_hat_deriv = Grad(fluid_betas, @fluid_inc_elast, 1, .00001, mod_data);

st_error_fluid_inc_elast = F_hat_deriv * fluid_cov * F_hat_deriv';

t_stat_fluid_inc_elast = fluid_inc_elast_val / sqrt(  st_error_fluid_inc_elast  );

% One-sided test
p_val = 1 - tcdf(t_stat_fluid_inc_elast, size(mod_data, 1) - length(fluid_betas));
fprintf('\nT-Stat. Income elasticity is positive:   %10.4f \n',t_stat_fluid_inc_elast);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end




%% Q. 1.d
%Just marg effect w/ binary:

mean_refrig = mean(mod_data(:, 2:end));
mean_refrig(7) = 1;
eff_refrig = normcdf(mean_refrig*fluid_betas);

mean_no_refrig = mean(mod_data(:, 2:end));
mean_no_refrig(7) = 0;
eff_no_refrig = normcdf(mean_no_refrig*fluid_betas);

refrig_marg_eff = eff_refrig - eff_no_refrig;


delta_f_beta = normpdf(mean_refrig) - normpdf(mean_no_refrig);
% delta_f_beta = normpdf(mean_refrig*fluid_betas) - normpdf(mean_no_refrig*fluid_betas);

st_error_refrig = delta_f_beta * fluid_cov * delta_f_beta'
% from p. 734 7th ed. of Greene

t_stat_refrig = refrig_marg_eff / sqrt(  st_error_refrig  )

% One-sided test
p_val = 1 - tcdf(t_stat_refrig, size(mod_data, 1) - length(fluid_betas));
fprintf('\n T-Stat. Marg effect of refig is positive:   %10.4f \n',t_stat_refrig);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end

%% Q. 1.e


mean_ch_2 = mean(mod_data(:, 2:end));
mean_ch_2(2) = 2;
mean_ch_2(4) = mean(mod_data(:, 4)) * 2;
eff_ch_2 = normcdf(mean_ch_2*fluid_betas);

mean_ch_3 = mean(mod_data(:, 2:end));
mean_ch_3(2) = 3;
mean_ch_3(4) = mean(mod_data(:, 4)) * 3;
eff_ch_3 = normcdf(mean_ch_3*fluid_betas);

child_marg_eff = eff_ch_2 - eff_ch_3



mean_ch_2 = mean(mod_data(:, 2:end));
mean_ch_2(2) = 2;
mean_ch_2(3) = mean_ch_2(3) * 0.5;
mean_ch_2(4) = mean(mod_data(:, 4)) * 2 * 0.5;
eff_ch_2 = normcdf(mean_ch_2*fluid_betas);

mean_ch_3 = mean(mod_data(:, 2:end));
mean_ch_3(2) = 3;
mean_ch_3(3) = mean_ch_3(3) * 0.5;
mean_ch_3(4) = mean(mod_data(:, 4)) * 3 * 0.5;
eff_ch_3 = normcdf(mean_ch_3*fluid_betas);

child_marg_eff_50 = eff_ch_2 - eff_ch_3



mean_ch_2 = mean(mod_data(:, 2:end));
mean_ch_2(2) = 2;
mean_ch_2(3) = mean_ch_2(3) * 1.5;
mean_ch_2(4) = mean(mod_data(:, 4)) * 2 * 1.5;
eff_ch_2 = normcdf(mean_ch_2*fluid_betas);

mean_ch_3 = mean(mod_data(:, 2:end));
mean_ch_3(2) = 3;
mean_ch_3(3) = mean_ch_3(3) * 1.5;
mean_ch_3(4) = mean(mod_data(:, 4)) * 3 * 1.5;
eff_ch_3 = normcdf(mean_ch_3*fluid_betas);

child_marg_eff_150 = eff_ch_2 - eff_ch_3


F_hat_deriv = Grad(fluid_betas, @child_gamma_hat_fn, 1, .00001, mod_data);


st_error_child = F_hat_deriv * fluid_cov * F_hat_deriv';

t_stat_child = (child_marg_eff_50 - child_marg_eff_150) / sqrt(  st_error_child  );

p_val = 1 - tcdf(t_stat_child, size(mod_data, 1) - length(fluid_betas));
fprintf('\nT-Stat. ME of 2->3 children w/50%%\n and 150%% of mean income are equal:   %10.4f \n',t_stat_child);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05/2
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end
  




%% QUESTION 2

%% Q. 2.a

hetero_data = horzcat(  mod_data, ...
    full_data(:, strcmp(varnames,'perge66')), ...
    full_data(:, strcmp(varnames,'refrig')), ...
    full_data(:, strcmp(varnames,'regdf')), ...
    full_data(:, strcmp(varnames,'incomet')), ...
    full_data(:, strcmp(varnames,'perfafh')) ...
    );

% TODO: delete this:
quick_sol = [ -1.4276 ; -0.0828;  0.5650;  0.0598;  0.3265;  0.9737;  0.7086  ;
 -0.7051;  0.1177;  0.1132;  0.2193;  0.2573;  0.0506  ]
% [fluid_betas; repmat(0, 5, 1)]

[hetero_fluid_betas, hetero_fluid_cov, hetero_fluid_llf_vec]  = max_bhhh([fluid_betas; repmat(0, 5, 1)], ...
  {'const', 'num_yung', 'incomet', 'yung_x_inc', 'sm_city', 'city', 'refrig', 'perfafh' ...
  'het_perg' 'het_ref' 'het_reg' 'het_inc' 'het_perf'}, ...
  size(hetero_data, 1), 250, 1e-6, @probit_llf_hetero, 1, 0.000001, ...
  hetero_data, 0, 8);




lr_test_output = 2 * ( sum(hetero_fluid_llf_vec) - sum(fluid_LLF_vec));

lr_test_p_val = 1 - chi2cdf(lr_test_output, 3);
fprintf('LR Stat. (H_0: Relationship is heteroskedastic): %10.4f \n', lr_test_output);
fprintf('Prob LR Stat. Assum. H_0:            %10.4f \n', lr_test_p_val);
if lr_test_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end

%% Q. 2.b

beta_temp = hetero_fluid_betas(1:8);
x_temp = mean(hetero_data(:, 2:9));
x_temp(3:4) = x_temp(3:4)*0.70;
z_temp = mean(hetero_data(:, 10:end));
z_temp(4) = z_temp(4)*0.70;
gam_temp = hetero_fluid_betas(9:end);
inc_mean = mean(hetero_data(:, 4)) * 0.7;

inc_elast_het_70 = normpdf( (x_temp * beta_temp) / exp(z_temp * gam_temp) ) * ...
  (( (beta_temp(2) + beta_temp(3) * inc_mean) - ...
    (x_temp * beta_temp) * gam_temp(4) ) / ...
    exp(z_temp * gam_temp) ) * ...
  normcdf( ( x_temp * beta_temp) / exp(z_temp * gam_temp) )
    
beta_temp = hetero_fluid_betas(1:8);
x_temp = mean(hetero_data(:, 2:9));
x_temp(3:4) = x_temp(3:4) * 1.30;
z_temp = mean(hetero_data(:, 10:end));
z_temp(4) = z_temp(4) * 1.30;
gam_temp = hetero_fluid_betas(9:end);
inc_mean = mean(hetero_data(:, 4)) * 1.3;

inc_elast_het_130 = normpdf( ( x_temp * beta_temp) / exp(z_temp * gam_temp) ) * ...
  (( (beta_temp(2) + beta_temp(3) * inc_mean) - ...
    (x_temp * beta_temp) * gam_temp(4) ) / ...
    exp(z_temp * gam_temp) ) * ...
  normcdf( (x_temp * beta_temp) / exp(z_temp * gam_temp) )



F_hat_deriv = Grad(hetero_fluid_betas, @inc_elast_het_70_130_fn, 1, .00001, hetero_data);


st_error_inc_het = F_hat_deriv * hetero_fluid_cov * F_hat_deriv';

t_stat_inc_het = (inc_elast_het_70 - inc_elast_het_130) / sqrt(  st_error_child  );

p_val = 1 - tcdf(t_stat_inc_het, size(mod_data, 1) - length(fluid_betas));
fprintf('\nT-Stat. Inc. elast. w/70%%\n and 130%% of mean income are equal:   %10.4f \n',t_stat_inc_het);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05/2
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end







%% QUESTION 3

%% Q. 3.a

clc;
clear;

urlwrite('http://www.aae.wisc.edu/aae637/data/matlab/greene_credit_v4.xls','temp.xls');
[full_data,varnames,raw]=xlsread('temp.xls');

full_data = full_data(full_data(:, strcmp(varnames,'Active'))~=0, :);

mod_data = horzcat( ...
    full_data(:, strcmp(varnames,'Majordrg')) > 0, ...
    repmat(1, size(full_data, 1), 1), ...
    full_data(:, strcmp(varnames,'Age'))/100, ...
   (full_data(:, strcmp(varnames,'Age'))/100).^2, ...
    full_data(:, strcmp(varnames,'Inc_per'))/10, ...
    full_data(:, strcmp(varnames,'Avgexp'))/1000, ...
    full_data(:, strcmp(varnames,'Ownrent')) ...
    );

betas_start=(mod_data(:, 2:end)'*mod_data(:, 2:end))\(mod_data(:, 2:end)'*mod_data(:, 1));  


[credit_betas, credit_cov, credit_llf_vec]  = max_bhhh(betas_start, ...
  {'const', 'Age', 'Age_sq', 'Inc_Per', 'Avgexp', 'Ownrent'}, ...
  size(mod_data, 1), 250, 1e-4, @logit_llf, 1, 0.000001, ...
  mod_data, 0, 0);
  

%% Q. 3.b

naive_data = horzcat( ...
    full_data(:, strcmp(varnames,'Majordrg')) > 0, ...
    repmat(1, size(full_data, 1), 1) ...
    );

betas_start=mean(naive_data(:,1));  

[credit_naive_betas, credit_naive_cov, credit_naive_llf_vec]  = max_bhhh(betas_start, ...
  {'const'}, ...
  size(naive_data, 1), 250, 1e-4, @logit_llf, 1, 0.000001, ...
  naive_data, 0, 0);
  

lr_test_output = 2 * ( sum(credit_llf_vec) - sum(credit_naive_llf_vec));

lr_test_p_val = 1 - chi2cdf(lr_test_output, 5);
fprintf('LR Stat. (H_0: Our model does better than naive model): %10.4f \n', lr_test_output);
fprintf('Prob LR Stat. Assum. H_0:            %10.4f \n', lr_test_p_val);
if lr_test_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end

%% Q. 3.c

age_elast_credit = normpdf(mean(mod_data(:, 2:end)) * credit_betas) * ...
  (credit_betas(2) + 2*credit_betas(3)*mean(mod_data(:, 3)) ) * ...
  mean(mod_data(:, 3)) / normcdf(mean(mod_data(:, 2:end)) * credit_betas)

inc_elast_credit = normpdf(mean(mod_data(:, 2:end)) * credit_betas) * ...
  credit_betas(4)  * ...
  mean(mod_data(:, 5)) / normcdf(mean(mod_data(:, 2:end)) * credit_betas)
  
  
  

F_hat_deriv = Grad(credit_betas, @age_elast_credit_fn, 1, .00001, mod_data);


st_error_age = F_hat_deriv * credit_cov * F_hat_deriv';

t_stat_age = age_elast_credit / sqrt(  st_error_age  );

p_val = 1 - tcdf(t_stat_age, size(mod_data, 1) - length(credit_betas));
fprintf('\nT-Stat. Elasticity of age:   %10.4f \n',t_stat_age);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05/2
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end




F_hat_deriv = Grad(credit_betas, @inc_elast_credit_fn, 1, .00001, mod_data);


st_error_inc = F_hat_deriv * credit_cov * F_hat_deriv';

t_stat_inc = inc_elast_credit / sqrt(  st_error_inc  );

p_val = 1 - tcdf(t_stat_inc, size(mod_data, 1) - length(credit_betas));
fprintf('\nT-Stat. Elasticity of income:   %10.4f \n',t_stat_inc);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05/2
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end

%% Q. 3.d

inc_elast_data_input = mean(mod_data(:, 2:end));
inc_elast_data_input(4) = inc_elast_data_input(4) * 0.75;

inc_elast_credit_75 = normpdf(inc_elast_data_input * credit_betas) * ...
  credit_betas(4)  * ...
  inc_elast_data_input(4) / normcdf(inc_elast_data_input * credit_betas)
  
inc_elast_data_input = mean(mod_data(:, 2:end));
inc_elast_data_input(4) = inc_elast_data_input(4) * 1.75;

inc_elast_credit_125 = normpdf(inc_elast_data_input * credit_betas) * ...
  credit_betas(4)  * ...
  inc_elast_data_input(4) / normcdf(inc_elast_data_input * credit_betas)



F_hat_deriv = Grad(credit_betas, @inc_elast_credit_75_125_fn, 1, .00001, mod_data);


st_error_inc = F_hat_deriv * credit_cov * F_hat_deriv';

t_stat_inc = abs( inc_elast_credit_75-inc_elast_credit_125 / sqrt(  st_error_inc  ) );

p_val = 1 - tcdf(t_stat_inc, size(mod_data, 1) - length(credit_betas));
fprintf('\nT-Stat. Elast. of inc. equal at 75%% and 125%% of mean inc:   %10.4f \n',t_stat_inc);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05/2
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end








git add .
git commit -m 'Almost done with AAE637 assignment 4'
git push origin master


parname={'ONE','W_AGE','W_AGESQ','W_EDU','FAMINC'}; 
critic_limit=1e-6;              % Critical % change in parameters
iter_limit=250;                 % Maximum number of iterations
do_step=1;                      % =1 variable step length, 0 fixed 
func_name =('probit_llf');      % Identify LLF function
alpha=.05;                      % Type I Error Probability 
dh = 0.000001;
