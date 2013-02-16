%% QUESTION 1

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

[gn_sol, cov_model1] = nls(min_grid_sol, full_data(:, strcmp(varnames,'Quant')), ...
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


test_hessian = model_hess(mizon_model_fn_pass, nr_sol, x_mat_input, full_data(:, strcmp(varnames,'Quant')));
[throwaway,definiteness] = chol(test_hessian);
if definiteness==0 
  display('******* AT LOCAL MINIMUM ********')
else
  error('****  NOT AT LOCAL MINIMUM *****')
end




%%%%% QUESTION 2


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

varnames = [varnames, 'CONSUME_L1', 'INC_L1', 'CONSUME_L2', 'INC_L2']

consump_model_fn_pass = @consump_model_fn

[ar2_coef, ar2_cov] = nls([1; 1; 1; 1], full_data(:, strcmp(varnames,'CONSUME')), ...
  {'beta1', 'beta2', 'beta3', 'beta4'}, 1e-6, 250, ...
  size(full_data,1), 1, consump_model_fn_pass, full_data, .000001, 0) ;


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

urlwrite('http://www.aae.wisc.edu/aae637/data/matlab/yarn-industry.xls','temp.xls');
[full_data,varnames,raw]=xlsread('temp.xls');

% 'Lot'    'Time'    'Frames'
 
full_data(:, 4) = cumsum( full_data(:,strcmp(varnames,'Time')) ) ./ 1:size(full_data,1)
 
full_data(:, 5) = cumsum( full_data(:,strcmp(varnames,'Frames')) ) 
 
varnames = [varnames, 'Ave_Time', 'Cumul_Frames']

yarn_model_fn_pass = @yarn_model_fn

[yarn_coef, yarn_cov] = nls([1; 1], full_data(:, strcmp(varnames,'Ave_Time')), ...
  {'beta1', 'beta2'}, 1e-6, 250, ...
  size(full_data,1), 1, yarn_model_fn_pass, full_data, .000001, 0) ;









