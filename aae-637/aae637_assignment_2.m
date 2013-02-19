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
%[grid_array1, grid_array2, grid_array3] = ndgrid(-10:2:10, -10:2:10, -10:2:10) ;
% We will iterate over each value -2 to 2, in steps of 0.01, for each 
% parameter
% When we did -100:1:100, we got optimums of: 2, 0, 1
% When we did -5:0.1:5, we got: 1.8 0.2 0.8

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
%    1.8000
%    0.2000
%    0.8000

mizon_model_fn_pass = @mizon_model_fn

x_mat_input = horzcat(  full_data(:, strcmp(varnames,'Capital')), ...
    full_data(:, strcmp(varnames,'LH')) ) ;

[gn_sol, cov_model1, r2_1] = nls(min_grid_sol, full_data(:, strcmp(varnames,'Quant')), ...
  {'beta1', 'beta2', 'beta3'}, 1e-6, 250, ...
  size(full_data,1), 1, mizon_model_fn_pass, x_mat_input, .000001, 1) ;

%  values before wrote code to break out of function when reached convex section:
%    1.3040
%    0.2215
%    0.8288

[nr_sol, cov_model2] = nr_alg(test1 ,full_data(:, strcmp(varnames,'Quant')), ...
  {'beta1', 'beta2', 'beta3'}, 1e-6, 250, 1, mizon_model_fn_pass, x_mat_input);
%b =   1.3040
%b =   0.2215
%b =   0.8288


%% Q. 1.b

test_hessian = model_hess(mizon_model_fn_pass, nr_sol, x_mat_input, full_data(:, strcmp(varnames,'Quant')));
[throwaway,definiteness] = chol(test_hessian);
if definiteness==0 
  display('******* AT LOCAL MINIMUM ********')
else
  error('****  NOT AT LOCAL MINIMUM *****')
end




%% QUESTION 2

%% Q. 2.a (reformulation of the model is in yarn_model_fn.m

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
  {'beta1', 'beta2', 'beta3', 'beta4'}, 1e-6, 250, ...
  size(full_data,1), 1, consump_model_fn_pass, full_data, .000001, 0) ;

consump_ar1_model_fn_pass = @consump_ar1_model_fn;

%% Q. 2.b 

[ar1_coef, ar1_cov, ar1_r2] = nls([1; 1; 1], full_data(:, strcmp(varnames,'CONSUME')), ...
  {'beta1', 'beta2', 'beta3'}, 1e-6, 250, ...
  size(full_data,1), 1, consump_ar1_model_fn_pass, full_data, .000001, 0) ;


ar_f = ( (ar2_r2-ar1_r2)/(4-3) ) / ...
   ((1-ar2_r2)/(size(full_data,1) - 4) ); 
% by p. 18 of http://faculty.chicagobooth.edu/matt.taddy/teaching/class3.pdf

ar_p_val = 1 - fcdf(ar_f, 4-3, size(full_data,1) - 4);


fprintf('F Stat. (H_0: ar1_r2 = ar1_r1): %10.4f \n', ar_f);
fprintf('Prob Wald Stat. Assum. H_0:            %10.4f \n', ar_p_val);
if ar_p_val < 0.05
    disp('    There is, therefore, enough evidence to reject H_0');
else
    disp('    There is, therefore, not enough evidence to reject H_0');
end

%% Q. 2.c

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

% 'Lot'    'Time'    'Frames'
 
full_data(:, 4) = cumsum( full_data(:,strcmp(varnames,'Time')) ) ./ ...
  cumsum( full_data(:,strcmp(varnames,'Frames')) ); % ./ (1:size(full_data,1))';

%full_data(:, 4) = cumsum( full_data(:,strcmp(varnames,'Time')) ) ;
 
full_data(:, 5) = cumsum( full_data(:,strcmp(varnames,'Frames')) ) ;
 
varnames = [varnames, 'Ave_Time', 'Cumul_Frames'];

yarn_model_fn_pass = @yarn_model_fn;

[yarn_coef, yarn_cov] = nls([1; 1], full_data(:, strcmp(varnames,'Ave_Time')), ...
  {'beta1', 'beta2'}, 1e-6, 250, ...
  size(full_data,1), 1, yarn_model_fn_pass, full_data, .000001, 0) ;

%yarn_model_fn( yarn_coef, [1 1 1 1 50])
%yarn_model_fn( yarn_coef, [1 1 1 1 400])
%yarn_model_fn( yarn_coef, [1 1 1 1 800])

for i=[50 400 800]

ind_1 = max(find(full_data(:,strcmp(varnames,'Cumul_Frames')) < i));
ind_2 = min(find(full_data(:,strcmp(varnames,'Cumul_Frames')) > i));

slope = ( full_data(ind_1, strcmp(varnames,'Ave_Time')) - ...
  full_data(ind_2, strcmp(varnames,'Ave_Time')) ) / ...
  ( full_data(ind_1, strcmp(varnames,'Cumul_Frames')) - ...
  full_data(ind_2, strcmp(varnames,'Cumul_Frames')) ) ;
  
time_est = full_data(ind_1, strcmp(varnames,'Ave_Time')) + ...
   (i-full_data(ind_1, strcmp(varnames,'Cumul_Frames'))) * slope;
   
sd_est = (mean( (full_data(1:ind_1, strcmp(varnames,'Ave_Time')) - ... 
  mean(full_data(1:ind_1, strcmp(varnames,'Ave_Time'))) ).^2 ))^.5;

t_stat = (time_est-11.5)/(sd_est/(ind_1^.5));

p_val = 1 - tcdf(t_stat, ind_1);
display(p_val)

end

full_data(:, 6) = cumsum( full_data(:,strcmp(varnames,'Time')) ) ;
varnames = [varnames, 'Cumul_Time'];

full_data(:, 7) = 0;

full_data(2:size(full_data,1), 7) = ...
  ( full_data(1:(size(full_data,1)-1), 6) - full_data(2:size(full_data,1), 6) ) ./ ...
  ( full_data(1:(size(full_data,1)-1), 5) - full_data(2:size(full_data,1), 5) );
varnames = [varnames, 'Marg_Cost'];


for i=[50 400 800]

ind_1 = max(find(full_data(:,strcmp(varnames,'Cumul_Frames')) < i));
ind_2 = min(find(full_data(:,strcmp(varnames,'Cumul_Frames')) > i));

slope = ( full_data(ind_1, strcmp(varnames,'Marg_Cost')) - ...
  full_data(ind_2, strcmp(varnames,'Marg_Cost')) ) / ...
  ( full_data(ind_1, strcmp(varnames,'Cumul_Frames')) - ...
  full_data(ind_2, strcmp(varnames,'Cumul_Frames')) ) ;
  
time_est = full_data(ind_1, strcmp(varnames,'Marg_Cost')) + ...
   (i-full_data(ind_1, strcmp(varnames,'Cumul_Frames'))) * slope;
   
sd_est = (mean( (full_data(2:ind_1, strcmp(varnames,'Marg_Cost')) - ... 
  mean(full_data(2:ind_1, strcmp(varnames,'Marg_Cost'))) ).^2 ))^.5;

t_stat = (time_est-11.5)/(sd_est/((ind_1-1)^.5));

p_val = 1 - tcdf(t_stat, ind_1-1);
display(p_val)

end


full_data(:, 8) = full_data(:, strcmp(varnames,'Marg_Cost')) - ...
  full_data(:, strcmp(varnames,'Ave_Time')) ;

varnames = [varnames, 'Marg_Cost_m_Ave_Time'];

for i=[50 400 800]

ind_1 = max(find(full_data(:,strcmp(varnames,'Cumul_Frames')) < i));
ind_2 = min(find(full_data(:,strcmp(varnames,'Cumul_Frames')) > i));

slope = ( full_data(ind_1, strcmp(varnames,'Marg_Cost_m_Ave_Time')) - ...
  full_data(ind_2, strcmp(varnames,'Marg_Cost_m_Ave_Time')) ) / ...
  ( full_data(ind_1, strcmp(varnames,'Cumul_Frames')) - ...
  full_data(ind_2, strcmp(varnames,'Cumul_Frames')) ) ;
  
time_est = full_data(ind_1, strcmp(varnames,'Marg_Cost_m_Ave_Time')) + ...
   (i-full_data(ind_1, strcmp(varnames,'Cumul_Frames'))) * slope;
   
sd_est = (mean( (full_data(2:ind_1, strcmp(varnames,'Marg_Cost_m_Ave_Time')) - ... 
  mean(full_data(2:ind_1, strcmp(varnames,'Marg_Cost_m_Ave_Time'))) ).^2 ))^.5;

t_stat = (time_est-3)/(sd_est/((ind_1-1)^.5));

p_val = 1 - tcdf(t_stat, ind_1-1);
display(p_val)

end

%% QUESTION 4


urlwrite('http://www.aae.wisc.edu/aae637/data/matlab/mizon_1977.xlsx','temp.xls');
[full_data,varnames,raw]=xlsread('temp.xls');


full_data(:, 8) = full_data(:,strcmp(varnames,'Hour')) .* ...
  (full_data(:,strcmp(varnames,'Labor')) - ...
  full_data(:,strcmp(varnames,'Unemploy'))) ./ 100 ;
varnames = [varnames, 'LH'] ;

full_data(:, 9) = log(full_data(:,strcmp(varnames,'Quant'))) ;
varnames = [varnames, 'Ln_Quant'] ;

  
  
[grid_array1, grid_array2, grid_array3, grid_array4] = ndgrid(200:400, .1:.1:.9, .1:.1:2, .1:.1:2) ;
% ndgrid(34:.1:37, -39:0.1:-35, .8:.01:.99, .8:.01:.99)
%  1.2228e+07 37:1:45, -30:1:-20, .1:.1:.9, .1:.1:.9

search_vec = ones(prod( size(grid_array1)), 1 );
for m=1:size(search_vec,1) 
  model_temp = ces_model_fn(  [grid_array1(m); grid_array2(m); grid_array3(m); grid_array4(m)], full_data) ;
  search_vec(m,1) = ssefn(full_data(:, strcmp(varnames,'Quant')), model_temp ) ;
end

min_grid_sse = min(search_vec);
min_ind = find(min_grid_sse==search_vec);
min_grid_sol = [ grid_array1(min_ind) ; grid_array2(min_ind); grid_array3(min_ind); grid_array4(min_ind)];

ces_model_fn_pass = @ces_model_fn


[ces_coef, ces_cov] = nls(min_grid_sol, full_data(:, strcmp(varnames,'Quant')), ...
  {'alpha', 'delta', 'rho', 'eta'}, 1e-6, 250, ...
  size(full_data,1), 1, ces_model_fn_pass, full_data, .000000000001, 0) ;
%      1.2361
%    0.2018
%   -0.1618
%    1.0576
  

test_hessian = model_hess(ces_model_fn_pass, ces_coef, full_data, full_data(:, strcmp(varnames,'Quant')));
[throwaway,definiteness] = chol(test_hessian);
if definiteness==0 
  display('******* AT LOCAL MINIMUM ********')
else
  error('****  NOT AT LOCAL MINIMUM *****')
end



%[grid_array1, grid_array2, grid_array3, grid_array4] = ndgrid(1:2:50, .1:.05:.9, -.9:.05:5, .5:.5:5) ;
% ndgrid(34:.1:37, -39:0.1:-35, .8:.01:.99, .8:.01:.99)
%  1.2228e+07 37:1:45, -30:1:-20, .1:.1:.9, .1:.1:.9

%search_vec = ones(prod( size(grid_array1)), 1 );
%for m=1:size(search_vec,1) 
%  model_temp = ces_linear_model_fn(  [grid_array1(m); grid_array2(m); grid_array3(m); %grid_array4(m)], full_data) ;
%  search_vec(m,1) = ssefn(full_data(:, strcmp(varnames,'Ln_Quant')), model_temp ) ;
%end

%min_grid_sse = min(search_vec);
%min_ind = find(min_grid_sse==search_vec);
%min_grid_sol = [ grid_array1(min_ind) ; grid_array2(min_ind); grid_array3(min_ind); %grid_array4(min_ind)];
%min_grid_sol
%min_grid_sse

  
ces_linear_model_fn_pass = @ces_linear_model_fn

  
[ces_lin_coef, ces_lin_cov] = nls(ces_coef, full_data(:, strcmp(varnames,'Ln_Quant')), ...
  {'alpha', 'delta', 'rho', 'eta'}, 1e-6, 1000, ...
  size(full_data,1), 1, ces_linear_model_fn_pass, full_data, .000000000001, 0) ;
% SSE: 3.2027
%  7.65918155722338e-08
%  0.218956891635365
% -0.0459031788106847
%  0.798882076847029



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
  


ces_linear_restricted_model_fn_pass = @ces_linear_restricted_model_fn

[ces_lin_res_coef, ces_lin_cov] = nls(ces_lin_coef(1:3), full_data(:, strcmp(varnames,'Ln_Quant')), ...
  {'alpha', 'delta', 'rho'}, 1e-3, 3000, ...
  size(full_data,1), 1, ces_linear_restricted_model_fn_pass, full_data, .000000001, 0) ;
  

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






