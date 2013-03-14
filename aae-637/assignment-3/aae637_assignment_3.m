
path(path,'C:\Users\tdmcarthur\Documents\MATLAB\') ;

%% Question 1

%% Q. 1.a

clear

urlwrite('http://www.aae.wisc.edu/aae637/data/matlab/randdata.xls','temp.xls');
[full_data,varnames,raw]=xlsread('temp.xls');


orig_obs = size(full_data,1);

full_data = horzcat(full_data(:, strcmp(varnames,'mdvis')), ...
    repmat(0, orig_obs, 1), ...
    repmat(1, orig_obs, 1), ...
    full_data(:, ~strcmp(varnames,'mdvis')) ...
    );
varnames = ['mdvis'  'mdvislnfact' 'const' varnames(~strcmp(varnames,'mdvis')) ];

for i=1:size(full_data, 1) 
  if full_data(i, 1) ~= 0
    full_data(i, 2) = sum(log(1:full_data(i, 1)));
  end
end

targ_vars = {'mdvis' 'mdvislnfact' 'const' 'logc' 'idp' 'fmde' 'linc' 'lnum' 'xage' 'female' ...
  'child' 'fchild' 'black' 'educdec' 'physlm' 'disea' 'hlthg' 'hlthf' 'hlthp'};

finaldata=pull_data(varnames,targ_vars,full_data);

vars_rescaled = targ_vars(max(finaldata)>=10);
vars_rescaled = vars_rescaled(~strcmp(vars_rescaled,'mdvis') & ~strcmp(vars_rescaled,'mdvislnfact') & ~strcmp(vars_rescaled,'linc'));

for i=vars_rescaled
  finaldata(:, strcmp(targ_vars, i)) = finaldata(:, strcmp(targ_vars, i)) ./ 10;
end

x_vars = finaldata(:, 3:size(finaldata,2));

rand_start_vals = inv(x_vars' * x_vars) * x_vars' * log( 1 + finaldata(:, 1) )
% OLS starting values actually mess things up, must use log of Y

count_likelihood_fn_pass = @count_likelihood_fn

[betas_rand_full, cov_rand_full, llf_vec_rand_full] = max_bhhh( ...
  rand_start_vals , targ_vars(3:length(targ_vars))  ,  ...
  orig_obs, 250, 1e-5, count_likelihood_fn_pass, 1, .000001, finaldata, 0, 0);
% repmat(0.2, length(targ_vars)-2, 1) 
% rand_start_vals

targ_vars = {'mdvis' 'mdvislnfact' 'const' 'logc' 'idp' 'fmde' 'linc' 'lnum' 'xage' 'female' ...
  'child' 'fchild' 'black' 'educdec' 'physlm' 'disea'};

mdvis_hat = exp( finaldata(:, 3:size(finaldata,2)) * betas_rand_full);
mdvis_actual = finaldata(:, strcmp(targ_vars, 'mdvis'));

r_2_p = 1 - sum( ( ( mdvis_actual - mdvis_hat ) ./ sqrt(mdvis_hat) ).^2 ) / ...
sum( ( ( mdvis_actual - mean(mdvis_actual) ) ./ sqrt(mean(mdvis_actual)) ).^2 ) ;
% p. 844 of Greene 7th Edition

fprintf('Standardized residual R-Sq_p = %2.4f', r_2_p)
disp('  ')



%% Q. 1.b

restricted_data=pull_data(varnames,targ_vars,full_data);

vars_rescaled = targ_vars(max(restricted_data)>=10);
vars_rescaled = vars_rescaled(~strcmp(vars_rescaled,'mdvis') & ~strcmp(vars_rescaled,'mdvislnfact'));

for i=vars_rescaled
  restricted_data(:, strcmp(targ_vars, i)) = restricted_data(:, strcmp(targ_vars, i)) ./ 10;
end


[betas_rand_restricted, cov_rand_restricted, llf_vec_rand_restricted] = max_bhhh( ...
  betas_rand_full(1:(length(betas_rand_full)-3)), targ_vars(3:length(targ_vars)), ...
  orig_obs, 250, 1e-5, count_likelihood_fn_pass, 1, .000001, restricted_data, 0);
  
  
lr_test_output = 2 * ( sum(llf_vec_rand_full) - sum(llf_vec_rand_restricted));

lr_test_p_val = 1 - chi2cdf(lr_test_output, 3);
fprintf('LR Stat. (H_0: hlthg = hlthf = hlthp = 0): %10.4f \n', lr_test_output);
fprintf('Prob Wald Stat. Assum. H_0:            %10.4f \n', lr_test_p_val);
if lr_test_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end


%% Q. 1.c

rand_nls_model_pass = @rand_nls_model;

marg_input_good = mean(finaldata(:, 3:size(finaldata,2)));
marg_input_good(15:17) = [1 0 0];

marg_effect_good = exp(marg_input_good * betas_rand_full) * betas_rand_full(2);


marg_input_poor = mean(finaldata(:, 3:size(finaldata,2)));
marg_input_poor(15:17) = [0 0 1];

marg_effect_poor = exp(marg_input_poor * betas_rand_full) * betas_rand_full(2);


display(marg_effect_good)
display(marg_effect_poor)

gamma_hat_fn_pass = @gamma_hat_fn

health_gamma_hat_fn_pass = @health_gamma_hat_fn

F_hat_deriv = Grad(betas_rand_full, health_gamma_hat_fn_pass,1 , .00001, finaldata);
% By p. 733 of Greene 7th edition

  
%Now, 2-tailed T-test:

st_error_health = F_hat_deriv * cov_rand_full * F_hat_deriv'

t_stat_health = (marg_effect_good - marg_effect_poor) / sqrt(  st_error_health  )

p_val = 1 - tcdf(t_stat_health, orig_obs - length(cov_rand_full));
fprintf('\n T-Stat. ME of ch in LOGC for good and poor health are equal:   %10.4f \n',t_stat_health);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05/2
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end
  

%% Q. 1.d
  

income_elast = betas_rand_full(5);
% due to http://cameron.econ.ucdavis.edu/racd/simplepoisson.pdf
% and http://mpra.ub.uni-muenchen.de/19895/1/MPRA_paper_19895.pdf

display(income_elast)

t_stat = (1 - income_elast ) ./  sqrt(cov_rand_full(5,5))

p_val = 1 - tcdf(t_stat, orig_obs - length(cov_rand_full));
fprintf('\n T-Stat. Elasticity of income on visits ==1 :   %10.4f \n',t_stat);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05/2
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end
  


%% Q. 1.e

marg_input_female = mean(finaldata(:, 3:size(finaldata,2)));
marg_input_female(8) = 1;

marg_input_male = mean(finaldata(:, 3:size(finaldata,2)));
marg_input_male(8) = 0;

female_rel_effect = exp( marg_input_female * betas_rand_full ) - ...
  exp( marg_input_male * betas_rand_full );
  
display(female_rel_effect)
  
% Use delta method

female_rel_fn_pass = @female_rel_fn

female_delta_vec = Grad(betas_rand_full, female_rel_fn_pass, 1, .00001, finaldata);
  
female_marg_effect_variance = female_delta_vec * cov_rand_full * female_delta_vec';

display(  female_marg_effect_variance)
  
  


%% Q. 1.f

rand_nls_model_pass = @rand_nls_model

[betas_rand_nls, cov_rand_nls, r2_rand_nls] = nls(betas_rand_full, finaldata(:,1), ...
  {'const' 'logc' 'idp' 'fmde' 'linc' 'lnum' 'xage' 'female' ...
  'child' 'fchild' 'black' 'educdec' 'physlm' 'disea' 'hlthg' 'hlthf' 'hlthp'},  ...
  1e-6, 250, ...
  size(full_data,1), 1, rand_nls_model_pass, finaldata(:, 3:size(finaldata,2)), .000001, 0) ;

rand_unrestr_likelihood = count_likelihood_fn(betas_rand_full, finaldata);

rand_restr_likelihood = count_likelihood_fn(betas_rand_nls, finaldata);

lr_test_output = 2 * ( sum(rand_unrestr_likelihood) - sum(rand_restr_likelihood));

lr_test_p_val = 1 - chi2cdf(lr_test_output, 3);
fprintf('LR Stat. (Unrestricted and restricted results are equal): %10.4f \n', lr_test_output);
fprintf('Prob Wald Stat. Assum. H_0:            %10.4f \n', lr_test_p_val);
if lr_test_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end


%% Question 2


%% Q. 2.a

clear

urlwrite('http://www.aae.wisc.edu/aae637/data/matlab/US_gas_use.xlsx','temp.xls');
[full_data,varnames,raw]=xlsread('temp.xls');

full_data(:,2) = full_data(:,strcmp(varnames,'Pop'))/1000000;
full_data(:,3) = full_data(:,strcmp(varnames,'GasP'))/10;
full_data(:,5) = full_data(:,strcmp(varnames,'PC_Inc'))/1000;
full_data(:,6) = full_data(:,strcmp(varnames,'PNC'))/10;
full_data(:,7) = full_data(:,strcmp(varnames,'PUC'))/10;

orig_obs = size(full_data,1);

%vars_rescaled = varnames(max(full_data)>=10);

%while length(vars_rescaled)>0
%  for i=vars_rescaled
%    full_data(:, strcmp(varnames, i)) = full_data(:, strcmp(varnames, i)) ./ 10;
%  end
%vars_rescaled = varnames(max(full_data)>=10);
%end





transf_data = horzcat( ...
    log( full_data(2:orig_obs, strcmp(varnames,'Q_Gas')) ./  ...
      full_data(2:orig_obs, strcmp(varnames,'Pop')) ), ...
    repmat(1, orig_obs-1, 1), ...
    log( full_data(2:orig_obs, strcmp(varnames,'GasP')) ), ...
    log( full_data(2:orig_obs, strcmp(varnames,'PC_Inc')) ), ...
    log( full_data(2:orig_obs, strcmp(varnames,'PNC')) ), ...
    log( full_data(2:orig_obs, strcmp(varnames,'PUC')) ), ...
    full_data(2:orig_obs, strcmp(varnames,'Shock73')), ...
    full_data(2:orig_obs, strcmp(varnames,'Shock79')), ...
    full_data(2:orig_obs, strcmp(varnames,'Recess')), ...
    log( full_data(1:(orig_obs-1), strcmp(varnames,'Q_Gas')) ./ ...
      full_data(1:(orig_obs-1), strcmp(varnames,'Pop')) ), ...
    repmat(1, orig_obs-1, 1), ...
    full_data(2:orig_obs, strcmp(varnames,'PC_Inc')) , ...
    full_data(2:orig_obs, strcmp(varnames,'Shock73')), ...
    full_data(2:orig_obs, strcmp(varnames,'Shock79')), ...
    full_data(2:orig_obs, strcmp(varnames,'Recess')) ...
    );



transf_data_x = transf_data(:, 2:10);
start_vals = (transf_data_x' * transf_data_x)^-1 * transf_data_x' * transf_data(:, 1);

gas_likelihood_fn_pass = @gas_likelihood_fn

beta_names = {'const', 'LnGasP', 'LnPC_Inc', 'LnPNC', 'LnPUC', 'Shock73', 'Shock79', 'Recess', ...
'QgasPoplag', 'het_const', 'het_PC_Inc', 'het_Sh73', 'het_Sh79' 'het_Recess' }

[betas_gas, cov_gas, llf_vec_gas] = max_bhhh( ...
  [start_vals; repmat(.02, 5, 1)], beta_names, ...
  size(transf_data, 1), 250, 1e-4, gas_likelihood_fn_pass, 1, .0000001, transf_data, 1, 9);





%% Wald test

gas_wald_stat = betas_gas(11:14)' * inv( 2 .* [zeros(4,1) eye(4)] * ...
   inv(  transf_data(:, 11:15)' *  transf_data(:, (11:15))) * ...
    [zeros(4,1) eye(4)]' ) * betas_gas(11:14)
% eq. 12.3.93 of JHGLL

    
chi_bwg(0.05,4,gas_wald_stat);

%% LR test

%[betas_gas_restricted, cov_gas_restricted, llf_vec_gas_restricted] = max_bhhh( ...
%  start_vals, beta_names(1:9), size(transf_data, 1), ...
%  250, 1e-6, gas_likelihood_fn_pass, 1, .0000001, transf_data(:, 1:10), 1, 0);

ols_est_gas = inv(transf_data(:, 2:10)' * transf_data(:, 2:10)) * ...
  transf_data(:, 2:10)' * transf_data(:, 1);
  
resid_gas = transf_data(:, 1) - transf_data(:, 2:10) * ols_est_gas;

gas_lr_stat = size(transf_data, 1) * log(resid_gas' * resid_gas ./ (size(transf_data, 1) )) - ...
  sum( transf_data(:, 11:15) * betas_gas(10:14))
% by eq. 12.3.102 of JHGLL


  
chi_bwg(0.05,4,gas_lr_stat);


%% LM test

q_vec_gas = resid_gas .^ 2 - ( resid_gas' * resid_gas ./ size(transf_data, 1)  );


gas_lm_stat = q_vec_gas' * transf_data(:, 11:15) * ...
  inv( transf_data(:, 11:15)' *  transf_data(:, (11:15)) ) * ...
  transf_data(:, 11:15)' * q_vec_gas ...
  ./ (2 * ( resid_gas' * resid_gas ./ size(transf_data, 1) )^2 )
% by JHGLL, eq. 12.3.99

chi_bwg(0.05,4,gas_lm_stat);


%% Q. 2.b


% Null that ksi_P =< -1

w_test_mat = [0 1/(1-betas_gas(9)) 0 repmat(0,1,5) betas_gas(2)/(1-betas_gas(9))^2 repmat(0,1,5)];

eval_mat = betas_gas(2)/(1-betas_gas(9));

w_test_output = (eval_mat + [1])' * inv(w_test_mat * cov_gas * w_test_mat') * ...
  (eval_mat + [1]);
  
display(w_test_output)
  
chi_bwg(0.05,1,w_test_output);



% Null that ksi_I=1

w_test_mat = [0 0 1/(1-betas_gas(9)) repmat(0,1,5) betas_gas(3)/(1-betas_gas(9))^2 repmat(0,1,5)];

eval_mat = betas_gas(3)/(1-betas_gas(9));

w_test_output = (eval_mat - [1])' * inv(w_test_mat * cov_gas * w_test_mat') * ...
  (eval_mat - [1]);
  
display(w_test_output)
  
chi_bwg(0.05/2,1,w_test_output);

% Null that ksi_I = 1 & ksi_P =< -1
  

w_test_mat = [0 1/(1-betas_gas(9)) 0 repmat(0,1,5) betas_gas(2)/(1-betas_gas(9))^2 repmat(0,1,5) ;
              0 0 1/(1-betas_gas(9)) repmat(0,1,5) betas_gas(3)/(1-betas_gas(9))^2 repmat(0,1,5)];

eval_mat = [betas_gas(2)/(1-betas_gas(9));
            betas_gas(3)/(1-betas_gas(9))];
            
w_test_output = (eval_mat - [-1; 1])' * inv(w_test_mat * cov_gas * w_test_mat') * ...
  (eval_mat - [-1; 1]);
  
display(w_test_output)
  
chi_bwg(0.05/2,1,w_test_output);

% Question 3

%% Q. 3.a


clear

urlwrite('http://www.aae.wisc.edu/aae637/data/matlab/US_gas_use.xlsx','temp.xls');
[full_data,varnames,raw]=xlsread('temp.xls');

orig_obs = size(full_data,1);

vars_rescaled = varnames(max(full_data)>=10);

while length(vars_rescaled)>0
  for i=vars_rescaled
    full_data(:, strcmp(varnames, i)) = full_data(:, strcmp(varnames, i)) ./ 10;
  end
vars_rescaled = varnames(max(full_data)>=10);
end

% full_data(:, strcmp(varnames,'Pop')) = full_data(:, strcmp(varnames,'Pop'))/1000


transf_data = horzcat( ...
    log( full_data(:, strcmp(varnames,'Q_Gas')) ./  ...
      full_data(:, strcmp(varnames,'Pop'))), ...
    repmat(1, orig_obs, 1), ...
    log( full_data(:, strcmp(varnames,'GasP')) ), ...
    log( full_data(:, strcmp(varnames,'PC_Inc')) ), ...
    log( full_data(:, strcmp(varnames,'PNC')) ), ...
    log( full_data(:, strcmp(varnames,'PUC')) ), ...
    full_data(:, strcmp(varnames,'Shock73')), ...
    full_data(:, strcmp(varnames,'Shock79')), ...
    full_data(:, strcmp(varnames,'Recess')) ...
    );
    
beta_names = {'const', 'LnGasP', 'LnPC_Inc', 'LnPNC', 'LnPUC', 'Shock73', 'Shock79', 'Recess'};


[betas_crm_gas, covb_crm_gas, rho_crm_gas]=crm(transf_data(:, 2:9), transf_data(:, 1), beta_names, 0);


%% Q. 3.b

[betas_fgls_gas, covb_fgls_gas, rho_fgls_gas]= fgls_2nd_step(transf_data(:, 2:9), transf_data(:, 1), beta_names,rho_crm_gas, 0) ;

%% Q. 3.c

gas_rho_likelihood_fn_pass = @gas_rho_likelihood_fn;

[betas_gas_ml_auto, cov_gas_ml_auto, llf_vec_gas_ml_auto] = max_bhhh( ...
  [betas_fgls_gas; atanh(rho_crm_gas)], [beta_names, 'rho'], ...
  orig_obs, 250, 1e-3, gas_rho_likelihood_fn_pass, 0, .00001, transf_data, 0, 0);


%% Q. 3.d.

gas_rho_likelihood_fn_pass = @gas_rho_likelihood_fn;

[betas_b_m, rho_b_m, cov_betas_b_m] = beach_mackinnon_alg(betas_fgls_gas, rho_fgls_gas, transf_data, gas_rho_likelihood_fn_pass,[beta_names, 'rho'],  1e-6, 250);


%% Q. 3.e.



estimates = horzcat([betas_crm_gas; rho_crm_gas], ...
  [betas_fgls_gas; rho_fgls_gas],...
  betas_gas_ml_auto, ...
  [betas_b_m; rho_b_m], ...
  sqrt([diag(covb_crm_gas); (1-rho_crm_gas^2)/orig_obs] ), ...
  sqrt([diag(covb_fgls_gas); (1-rho_fgls_gas^2)/orig_obs] ), ...
  sqrt(diag(cov_gas_ml_auto)), ...
  sqrt(diag(cov_betas_b_m)) );
  


estimates = estimates';

[f.numpars,f.numest] = size(estimates);

parnames = {'CRM_v' 'FGLSv' 'ML__v' 'B&M_v' 'CRM_se' 'FGLSse' 'B&M_se' 'ML__se' };

header = {'Mod', 'const', 'GasP', 'PCInc', 'PNC', 'PUC', 'Sh73', 'Sh79', 'Rec','Rho'};
fprintf('--------------------------------------------------------------------- \n');
fprintf('%5s %6s %6s %6s %6s %6s %6s %6s %6s %6s %80s', header{1}, header{2}, ...
          header{3}, header{4}, header{5}, header{6}, header{7}, header{8}, header{9}, header{10})
fprintf('\n -------------------------------------------------------------------- \n');
for i = 1:f.numpars
    fprintf('%6s %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f \n', ...
        parnames{i}, estimates(i,:));
end
disp('  ');     



z_stats = horzcat([betas_crm_gas; rho_crm_gas] ./ sqrt([diag(covb_crm_gas); (1-rho_crm_gas^2)/orig_obs] ), ...
  [betas_fgls_gas; rho_fgls_gas] ./ sqrt([diag(covb_fgls_gas); (1-rho_fgls_gas^2)/orig_obs] ),...
  betas_gas_ml_auto ./ sqrt(diag(cov_gas_ml_auto)), ...
  [betas_b_m; rho_b_m] ./ sqrt(diag(cov_betas_b_m)) ...
   );

z_stats = z_stats'

[f.numpars,f.numest] = size(z_stats);

parnames = {'CRM' 'FGLS' 'ML' 'B&M' };

header = {'Mod', 'const', 'GasP', 'PCInc', 'PNC', 'PUC', 'Sh73', 'Sh79', 'Rec','Rho'};
fprintf('-----------------------*********Z-STATS*********--------------------- \n');
fprintf('--------------------------------------------------------------------- \n');
fprintf('%5s %7s %6s %6s %6s %6s %6s %6s %6s %6s %80s', header{1}, header{2}, ...
          header{3}, header{4}, header{5}, header{6}, header{7}, header{8}, header{9}, header{10})
fprintf('\n -------------------------------------------------------------------- \n');
for i = 1:f.numpars
    fprintf('%6s %7.3f %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f %6.3f \n', ...
        parnames{i}, z_stats(i,:));
end
disp('  ');     


%% Q. 3.f.

header = {'const', 'GasP', 'PCInc', 'PNC', 'PUC', 'Sh73', 'Sh79', 'Rec','Rho'};


coef_mat = horzcat([betas_crm_gas; rho_crm_gas] , ...
  [betas_fgls_gas; rho_fgls_gas] ,...
  betas_gas_ml_auto , ...
  [betas_b_m; rho_b_m]  ...
   );


cov_mats = cell(1, 4);

cov_mats{1} = covb_crm_gas(1:5,1:5);
cov_mats{2} = covb_fgls_gas(1:5,1:5);
cov_mats{3} = cov_gas_ml_auto(1:5,1:5);
cov_mats{4} = cov_betas_b_m(1:5,1:5);

model_names = {'CRM', 'FGLS', 'ML', 'B&M'};


for i=1:4

  R_mat = [0 0 1 -1  0;
           0 0 1  0 -1];

  w_test_output = (R_mat * coef_mat(1:5, i) - [0])' * ...
    inv(R_mat * cov_mats{i} * R_mat') * ...
    (R_mat * coef_mat(1:5, i) - [0]);
  
  w_test_p_val = 1 - chi2cdf(real(w_test_output), size(R_mat, 1));
  fprintf('Wald Stat. (B_2 = B_3 = B_4) for %s method: %10.4f \n', model_names{i}, w_test_output);
  fprintf('Prob Wald Stat. Assum. H_0 for %s method:  %10.4f \n', model_names{i}, w_test_p_val);
  if w_test_p_val < 0.05
      disp('    There is, therefore, enough evidence to reject H_0');
  else
      disp('    There is, therefore, not enough evidence to reject H_0');
  end
  disp('    ');

end


  