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

%search_vec_sav=search_vec;
%search_vec = search_vec_sav;

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

[test1, test2] = nls(min_grid_sol, full_data(:, strcmp(varnames,'Quant')), ...
  {'beta1', 'beta2', 'beta3'}, 1e-6, 250, ...
  size(full_data,1), 1, mizon_model_fn_pass, x_mat_input, .000001) ;

%  values before wrote code to break out of function when reached convex section:
%    1.3040
%    0.2215
%    0.8288

test5 = nr_alg(test1 ,full_data(:, strcmp(varnames,'Quant')), ...
  {'beta1', 'beta2', 'beta3'}, 1e-6, 250, 1, mizon_model_fn_pass, x_mat_input);
%b =   1.3040
%b =   0.2215
%b =   0.8288



nls(betas,y,names, critic_limit, iter_limit, numobs, do_step, func_name, x_mat, dh)


