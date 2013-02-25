%% QUESTION 1

%% Q. 1.a

path(path,'C:\Users\tdmcarthur\Documents\MATLAB\') ;

urlwrite('http://www.aae.wisc.edu/aae637/data/matlab/mizon_1977.xlsx','temp.xls');
[full_data,varnames,raw]=xlsread('temp.xls');


full_data(:, 8) = full_data(:,strcmp(varnames,'Hour')) .* ...
  (full_data(:,strcmp(varnames,'Labor')) - ...
  full_data(:,strcmp(varnames,'Unemploy'))) ./ 100 ;
  
varnames = [varnames, 'LH'] ;


[grid_array1, grid_array2, grid_array3] = ndgrid(-1:0.1:3, -1:0.1:3, -1:0.1:3) ;

search_vec = ones(prod( size(grid_array1)), 1 );
% Create the matrix to fill with SSE values

for m=1:size(search_vec,1) 

  model_temp = mizon_model_fn( ...
    [grid_array1(m); grid_array2(m); grid_array3(m)], ...
    horzcat( ...
    full_data(:, strcmp(varnames,'Capital')), ...
    full_data(:, strcmp(varnames,'LH')) ) ) ;
  % Calling our model function that produces the y-hats
  search_vec(m,1) = ssefn( ...
    full_data(:, strcmp(varnames,'Quant')), model_temp ...
  ) ;
  % Assigning the results of the sse function to the appropriate
  % place in the matrix

end

min_grid_sse = min(search_vec);
% This will give us the min

min_grid_sol = [ 1 ; 1; 1];
% Simply a placeholder 
min_grid_sol(1) = grid_array1(find(min_grid_sse==search_vec));
min_grid_sol(2) = grid_array2(find(min_grid_sse==search_vec));
min_grid_sol(3) = grid_array3(find(min_grid_sse==search_vec));
% Now assigning the actual value of the parameters from the grid vector


mizon_model_fn_pass = @mizon_model_fn

x_mat_input = horzcat(  full_data(:, strcmp(varnames,'Capital')), ...
    full_data(:, strcmp(varnames,'LH')) ) ;

[gn_sol, cov_model1, r2_1] = nls(min_grid_sol, full_data(:, strcmp(varnames,'Quant')), ...
  {'gamma', 'beta', 'alpha'}, 1e-6, 250, ...
  size(full_data,1), 1, mizon_model_fn_pass, x_mat_input, .000001, 1) ;


[nr_sol, cov_model2] = nr_alg(gn_sol ,full_data(:, strcmp(varnames,'Quant')), ...
  {'gamma', 'beta', 'alpha'}, 1e-6, 250, 1, mizon_model_fn_pass, x_mat_input);
% Now we use the parameter values from the GN method to start the NR method


%% Q. 1.b

test_hessian = model_hess(mizon_model_fn_pass, nr_sol, x_mat_input, full_data(:, strcmp(varnames,'Quant')));
[throwaway,definiteness] = chol(test_hessian);
if definiteness==0 
  display('******* AT LOCAL MINIMUM ********')
else
  error('****  NOT AT LOCAL MINIMUM *****')
end




%% QUESTION 2

%% Q. 2.b 

urlwrite('http://www.aae.wisc.edu/aae637/data/matlab/nl_cons_v2.xls','temp.xls');
[full_data,varnames,raw]=xlsread('temp.xls');

orig_obs = size(full_data,1);

full_data = horzcat( ...
    full_data(3:orig_obs, strcmp(varnames,'CONSUME')), ...
    full_data(3:orig_obs, strcmp(varnames,'INC')), ...
    full_data(2:(orig_obs-1), strcmp(varnames,'CONSUME')), ...
    full_data(2:(orig_obs-1), strcmp(varnames,'INC')), ...
    full_data(1:(orig_obs-2), strcmp(varnames,'CONSUME')), ...
    full_data(1:(orig_obs-2), strcmp(varnames,'INC')) ...
    );

varnames = [varnames, 'CONSUME_L1', 'INC_L1', 'CONSUME_L2', 'INC_L2'];

consump_model_fn_pass = @consump_model_fn;

[ar2_coef, ar2_cov, ar2_r2] = nls([1; 1; 1; 1], full_data(:, strcmp(varnames,'CONSUME')), ...
  {'beta1', 'beta2', 'theta1', 'theta2'}, 1e-7, 250, ...
  size(full_data,1), 1, consump_model_fn_pass, full_data, .0000001, 0) ;



%% Q. 2.c 

consump_ar1_model_fn_pass = @consump_ar1_model_fn;

[ar1_coef, ar1_cov, ar1_r2] = nls([1; 1; 1], full_data(:, strcmp(varnames,'CONSUME')), ...
  {'beta1', 'beta2', 'beta3'}, 1e-6, 250, ...
  size(full_data,1), 1, consump_ar1_model_fn_pass, full_data, .000001, 0) ;


ar_f = ( (ar2_r2-ar1_r2)/(4-3) ) / ...
   ((1-ar2_r2)/(size(full_data,1) - 4) ); 
% Doing an F-test on the two R-squareds.

ar_p_val = 1 - fcdf(ar_f, 4-3, size(full_data,1) - 4);


fprintf('F Stat. (H_0: ar1_r2 = ar2_r2): %10.4f \n', ar_f);
fprintf('Prob Wald Stat. Assum. H_0:            %10.4f \n', ar_p_val);
if ar_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end

%% Q. 2.d

w_test_mat=[zeros(2,2) eye(2)];

w_test_output = (w_test_mat * ar2_coef - [0; 0])' * inv(w_test_mat * ar2_cov * w_test_mat') * ...
  (w_test_mat * ar2_coef - [0; 0]);
  
w_test_p_val = 1 - chi2cdf(w_test_output, size(w_test_mat, 1));
fprintf('Wald Stat. (H_0: theta_1 = theta_2 = 0): %10.4f \n', w_test_output);
fprintf('Prob Wald Stat. Assum. H_0:            %10.4f \n', w_test_p_val);
if w_test_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end








%% QUESTION 3 

%% Q. 3.a

urlwrite('http://www.aae.wisc.edu/aae637/data/matlab/yarn-industry.xls','temp.xls');
[full_data,varnames,raw]=xlsread('temp.xls');



full_data(:, 4) =  cumsum(full_data(:,strcmp(varnames,'Time')))  ./ ...
  cumsum(full_data(:,strcmp(varnames,'Frames'))) ; 
 
full_data(:, 5) = cumsum( full_data(:,strcmp(varnames,'Frames')) ) ;
 
varnames = [varnames, 'Ave_Time', 'Cumul_Frames'];

yarn_model_fn_pass = @yarn_model_fn;

[yarn_coef, yarn_cov] = nls([1; 1], full_data(:, strcmp(varnames,'Ave_Time')), ...
  {'gamma', 'delta'}, 1e-6, 250, ...
  size(full_data,1), 1, yarn_model_fn_pass, full_data, .000001, 0) ;


t_stat = (yarn_coef(2)) ./  sqrt([ 0 1] * yarn_cov * [ 0 1]');

p_val = 1 - tcdf(abs(t_stat), size(full_data,1) - 1);
fprintf('\n T-Stat. (H_0: delta > 0):            %10.4f \n',t_stat);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end




%% Q. 3.b

yarn_ac_grad_pass = @yarn_ac_grad

for i=[50 400 800]
  display(yarn_coef(1) * i^yarn_coef(2))
    
  w_test_mat=Grad(yarn_coef, yarn_ac_grad_pass, 1, 1e-6, i);
   
  eval_mat = yarn_coef(1) * i^yarn_coef(2);

  w_test_output = (eval_mat - [11.5])' * inv(w_test_mat * yarn_cov * w_test_mat') * ...
    (eval_mat - [11.5]);
  
  w_test_p_val = 1 - chi2cdf(w_test_output, size(w_test_mat, 1));
  fprintf('Wald Stat. (H_0: Ave Cost at %3.0f  = 11.5): %10.4f \n', i, w_test_output);
  fprintf('Prob Wald Stat. Assum. H_0:            %10.4f \n', w_test_p_val);
  if w_test_p_val < 0.05
      disp('    There is, therefore, enough evidence to reject H_0');
  else
      disp('    There is, therefore, not enough evidence to reject H_0');
  end

end



%% Q. 3.c


yarn_mc_grad_pass = @yarn_mc_grad

for i=[50 400 800]
  display(yarn_coef(1) .* (yarn_coef(2)+1) .* i .^ (yarn_coef(2)))
  
  
   w_test_mat=Grad(yarn_coef, yarn_mc_grad_pass, 1, 1e-6, i);
   
   eval_mat = yarn_coef(1) .* (yarn_coef(2)+1) .* i .^ (yarn_coef(2));


w_test_output = (eval_mat - [11.5])' * inv(w_test_mat * yarn_cov * w_test_mat') * ...
  (eval_mat - [11.5]);
  
w_test_p_val = 1 - chi2cdf(w_test_output, size(w_test_mat, 1));
fprintf('Wald Stat. (H_0: Marginal Cost at %3.0f  = 11.5): %10.4f \n', i, w_test_output);
fprintf('Prob Wald Stat. Assum. H_0:            %10.4f \n', w_test_p_val);
if w_test_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end

end



%% Q. 3.d


yarn_mc_m_ac_grad_pass = @yarn_mc_m_ac_grad

for i=[50 400 800]
  
   w_test_mat=Grad(yarn_coef, yarn_mc_m_ac_grad_pass, 1, 1e-6, i);
   
   eval_mat = yarn_mc_m_ac_grad(yarn_coef, i);
   
   display(eval_mat);


w_test_output = (eval_mat - [3])' * inv(w_test_mat * yarn_cov * w_test_mat') * ...
  (eval_mat - [3]);
  
w_test_p_val = 1 - chi2cdf(w_test_output, size(w_test_mat, 1));
fprintf('Wald Stat. (H_0: MC-AC at %3.0f  = 3): %10.4f \n', i, w_test_output);
fprintf('Prob Wald Stat. Assum. H_0:            %10.4f \n', w_test_p_val);
if w_test_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end

end



for i=[50 400 800]
  
  
   w_test_mat=Grad(yarn_coef, yarn_mc_m_ac_grad_pass, 1, 1e-6, i);
   
   eval_mat = yarn_mc_m_ac_grad(yarn_coef, i);
   
   display(eval_mat);


w_test_output = (eval_mat + [3])' * inv(w_test_mat * yarn_cov * w_test_mat') * ...
  (eval_mat + [3]);
  
w_test_p_val = 1 - chi2cdf(w_test_output, size(w_test_mat, 1));
fprintf('Wald Stat. (H_0: AC-MC at %3.0f  = 3): %10.4f \n', i, w_test_output);
fprintf('Prob Wald Stat. Assum. H_0:            %10.4f \n', w_test_p_val);
if w_test_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end

end






%% QUESTION 4

%% Q. 4.a

urlwrite('http://www.aae.wisc.edu/aae637/data/matlab/mizon_1977.xlsx','temp.xls');
[full_data,varnames,raw]=xlsread('temp.xls');


full_data(:, 8) = full_data(:,strcmp(varnames,'Hour')) .* ...
  (full_data(:,strcmp(varnames,'Labor')) - ...
  full_data(:,strcmp(varnames,'Unemploy'))) ./ 100 ;
varnames = [varnames, 'LH'] ;

full_data(:, 9) = log(full_data(:,strcmp(varnames,'Quant'))) ;
varnames = [varnames, 'Ln_Quant'] ;



ces_model_fn_pass = @ces_model_fn


[ces_coef, ces_cov] = nls([230; 0.1; 1.5; 0.1], full_data(:, strcmp(varnames,'Quant')), ...
  {'alpha', 'delta', 'rho', 'eta'}, 1e-6, 250, ...
  size(full_data,1), 1, ces_model_fn_pass, full_data, .000000000001, 0) ;
  

test_hessian = model_hess(ces_model_fn_pass, ces_coef, full_data, full_data(:, strcmp(varnames,'Quant')));
[throwaway,definiteness] = chol(test_hessian);
if definiteness==0 
  display('******* AT LOCAL MINIMUM ********')
else
  error('****  NOT AT LOCAL MINIMUM *****')
end

  
ces_linear_model_fn_pass = @ces_linear_model_fn

  
[ces_lin_coef, ces_lin_cov, ces_lin_r2] = nls(ces_coef, full_data(:, strcmp(varnames,'Ln_Quant')), ...
  {'alpha', 'delta', 'rho', 'eta'}, 1e-6, 1000, ...
  size(full_data,1), 1, ces_linear_model_fn_pass, full_data, .000000000001, 0) ;


model_temp = ces_linear_model_fn(  real(ces_lin_coef), full_data) ;
display(ssefn(full_data(:, strcmp(varnames,'Ln_Quant')), model_temp ) )


test_hessian = model_hess(ces_linear_model_fn_pass, real(ces_lin_coef), full_data, full_data(:, strcmp(varnames,'Ln_Quant')));
[throwaway,definiteness] = chol(test_hessian);
if definiteness==0 
  display('******* AT LOCAL MINIMUM ********')
else
  error('****  NOT AT LOCAL MINIMUM *****')
end


corrcoef( [ces_model_fn( ces_coef, full_data)...
  full_data(:, strcmp(varnames,'Quant')) ] )

corrcoef( [ces_model_fn( ces_lin_coef, full_data)...
  full_data(:, strcmp(varnames,'Quant')) ] )
  

  

%% Q. 4.d

w_test_mat=[ 0 0 0 1];

w_test_output = (w_test_mat * ces_lin_coef - [1])' * inv(w_test_mat * ces_lin_cov * w_test_mat') * ...
  (w_test_mat * ces_lin_coef - [1]);
  
w_test_p_val = 1 - chi2cdf(w_test_output, size(w_test_mat, 1));
fprintf('Wald Stat. (H_0: eta = 1): %10.4f \n', w_test_output);
fprintf('Prob Wald Stat. Assum. H_0:            %10.4f \n', w_test_p_val);
if w_test_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end

%% Q. 4.e


mean_labor = mean(full_data(:,strcmp(varnames,'LH')))
mean_capital = mean(full_data(:,strcmp(varnames,'Capital')))


ces_mp_k_val = ces_mp_k( ces_lin_coef, [mean_capital mean_labor])
ces_mp_l_val = ces_mp_l( ces_lin_coef, [mean_labor, mean_capital])

ces_mp_k_pass = @ces_mp_k
ces_mp_l_pass = @ces_mp_l

grad_mp_k = Grad(ces_lin_coef,ces_mp_k_pass, 1, 1e-6, [mean_capital mean_labor])
grad_mp_l = Grad(ces_lin_coef,ces_mp_l_pass, 1, 1e-6, [mean_labor mean_capital])



z = Grad(ces_lin_coef, ces_model_fn_pass, size(full_data, 1), 1e-6, full_data);
sighat2 = ssefn(full_data(:,strcmp(varnames,'Quant')), ces_model_fn(ces_lin_coef, full_data)) / ...
  (size(full_data,1) - length(ces_lin_coef));    
covb = inv(z'*z).*sighat2


t_stat = (ces_mp_k_val) ./  sqrt(grad_mp_k * covb * grad_mp_k')

p_val = 1 - tcdf(t_stat, size(full_data,1) - length(ces_lin_coef));
fprintf('\n T-Stat. (H_0: MP of Capital < 0):            %10.4f \n',t_stat);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end



t_stat = (ces_mp_l_val) ./  sqrt(grad_mp_l * covb * grad_mp_l')

p_val = 1 - tcdf(t_stat, size(full_data,1) - length(ces_lin_coef));
fprintf('\n T-Stat. (H_0: MP of Labor < 0):            %10.4f \n',t_stat);
fprintf('Prob T-Stat. Assum. H_0:               %10.4f \n',p_val);
if p_val < .05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end


%% Q. 4.f


ces_mp_all_pass = @ces_mp_all
ces_mrts_pass = @ces_mrts

w_test_mat = Grad(ces_lin_coef, ces_mrts_pass, 1, 1e-6, [mean_capital mean_labor])

eval_mat = ces_mrts(ces_lin_coef, [mean_capital mean_labor])

w_test_output = (eval_mat - [1])' * inv(w_test_mat * covb * w_test_mat') * ...
  (eval_mat - [1]);
  
w_test_p_val = 1 - chi2cdf(w_test_output, size(w_test_mat, 1));
fprintf('Wald Stat. (H_0: MP_Capital/MP_Labor==1): %10.4f \n', w_test_output);
fprintf('Prob Wald Stat. Assum. H_0:            %10.4f \n', w_test_p_val);
if w_test_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end

%% Q. 4.g

w_test_mat=[ 0 0 -(ces_lin_coef(3)+1)^-2 0 ] ;
% p. 543 of Judge et al.

eval_mat = (ces_lin_coef(3)+1)^-1

w_test_output = (eval_mat - [1])' * inv(w_test_mat * covb * w_test_mat') * ...
  (eval_mat - [1]);
  
w_test_p_val = 1 - chi2cdf(w_test_output, size(w_test_mat, 1));
fprintf('Wald Stat. (H_0: Elast. of Subst.==1): %10.4f \n', w_test_output);
fprintf('Prob Wald Stat. Assum. H_0:            %10.4f \n', w_test_p_val);
if w_test_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end

%% Q. 4.h


full_data(:, 10) = 0;
full_data(full_data(:,strcmp(varnames,'Year'))==1957, 10) = 1;

full_data(:, 11) = 0;
full_data(full_data(:,strcmp(varnames,'Year'))==1960, 11) = 1;

ces_linear_model_dummy_fn_pass = @ces_linear_model_dummy_fn

[ces_lin_dum_coef, ces_lin_dum_cov, ces_lin_dum_r2] = nls([ces_coef; 1.1; 1.1], full_data(:, strcmp(varnames,'Ln_Quant')), ...
  {'alpha', 'delta', 'rho', 'eta', '57beta', '60beta'}, 1e-6, 1000, ...
  size(full_data,1), 1, ces_linear_model_dummy_fn_pass, full_data, .000000000001, 0) ;


ces_dum_f = ( (ces_lin_dum_r2 - ces_lin_r2)/(6-4) ) / ...
   ((1-ces_lin_dum_r2)/(size(full_data,1) - 6) ); 

ces_dum_p_val = 1 - fcdf(ces_dum_f, 6-4, size(full_data,1) - 6);


fprintf('F Stat. (H_0: R_2 of two models are equal): %10.4f \n', ces_dum_f);
fprintf('Prob Wald Stat. Assum. H_0:            %10.4f \n', ces_dum_p_val);
if ces_dum_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end





