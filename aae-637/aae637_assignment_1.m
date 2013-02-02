%%path(path,'C:\Users\tdmcarthur\Documents\MATLAB\') urlwrite('http://www.aae.wisc.edu/aae637/data/matlab/c_50_00q_v2.xls','temp.xls')[full_data,varnames,raw]=xlsread('temp.xls');orig_obs = size(full_data,1)lagged_data = horzcat(full_data(5:orig_obs, strcmp(varnames,'Year')), ...    full_data(5:orig_obs, strcmp(varnames,'QTR')), ...    full_data(5:orig_obs, strcmp(varnames,'REALGDP')), ...    full_data(1:(orig_obs-4), strcmp(varnames,'CONS')), ...    full_data(4:(orig_obs-1), strcmp(varnames,'UNEMP')) ...    );test = gdp_model_fn(lagged_data(:, strcmp(varnames,'CONS')), ...    lagged_data(:, strcmp(varnames,'UNEMP')), ...    [grid_vec(m); grid_vec(n)] )grid_vec = -100:1:100 ;grid_vec = -1.7:0.01:1.7 ;search_mat = ones(size(grid_vec, 2), size(grid_vec, 2) );for m=1:size(grid_vec,2)     for n=1:size(grid_vec,2)        model_temp =gdp_model_fn(lagged_data(:, strcmp(varnames,'CONS')), ...          lagged_data(:, strcmp(varnames,'UNEMP')), ...          [grid_vec(m); grid_vec(n)] ) ;        search_mat(m,n) = ssefn( ...          lagged_data(:, strcmp(varnames,'REALGDP')), model_temp ...        ) ;    endendimagesc(log(search_mat))min_grid_sse = min(min(search_mat));min_grid_sol = [ 1 ; 1];[min_grid_sol(1), min_grid_sol(2)] = find(min_grid_sse==search_mat);min_grid_sol(1) = grid_vec(min_grid_sol(1));min_grid_sol(2) = grid_vec(min_grid_sol(2));step_length = 0.00001model_temp = gdp_model_fn(lagged_data(:, strcmp(varnames,'CONS')), ...  lagged_data(:, strcmp(varnames,'UNEMP')), ...    [min_grid_sol(1)+step_length; min_grid_sol(2)] ) ;b_c_plus = ssefn( ...    lagged_data(:, strcmp(varnames,'REALGDP')), model_temp ...  ) ;model_temp = gdp_model_fn(lagged_data(:, strcmp(varnames,'CONS')), ...  lagged_data(:, strcmp(varnames,'UNEMP')), ...    [min_grid_sol(1)-step_length; min_grid_sol(2)] ) ;b_c_minus = ssefn( ...    lagged_data(:, strcmp(varnames,'REALGDP')), model_temp ...  ) ;  model_temp = gdp_model_fn(lagged_data(:, strcmp(varnames,'CONS')), ...  lagged_data(:, strcmp(varnames,'UNEMP')), ...    [min_grid_sol(1); min_grid_sol(2)+step_length] ) ;b_u_plus = ssefn( ...    lagged_data(:, strcmp(varnames,'REALGDP')), model_temp ...  ) ;model_temp = gdp_model_fn(lagged_data(:, strcmp(varnames,'CONS')), ...  lagged_data(:, strcmp(varnames,'UNEMP')), ...    [min_grid_sol(1); min_grid_sol(2)-step_length] ) ;b_u_minus = ssefn( ...    lagged_data(:, strcmp(varnames,'REALGDP')), model_temp ...  ) ;  if b_c_plus < b_c_minus  b_c_factor = 1 ;else  b_c_factor = -1 ;endif b_u_plus < b_u_minus  b_u_factor = 1 ;else  b_u_factor = -1 ;endmin_gs_sol = min_grid_sol ;min_gs_sse_both_prev = min_grid_sse ;min_gs_sse_b_c_prev = min_grid_sse ;min_gs_sse_b_u_prev = min_grid_sse ;switch_direction = 0 ;step_length = 0.00001 ;trend = 0switch_tracker = 0%% while {}for i=1:1000  model_temp = gdp_model_fn(lagged_data(:, strcmp(varnames,'CONS')), ...    lagged_data(:, strcmp(varnames,'UNEMP')), ...      [min_gs_sol(1)+step_length*b_c_factor; min_gs_sol(2)] ) ;  min_gs_sse_b_c_current = ssefn( ...      lagged_data(:, strcmp(varnames,'REALGDP')), model_temp ...    ) ;    if min_gs_sse_b_c_current < min_gs_sse_b_c_prev    min_gs_sol(1) = min_gs_sol(1)+step_length*b_c_factor ;    min_gs_sse_b_c_prev = min_gs_sse_b_c_current ;  else    min_gs_sse_b_c_current = min_gs_sse_b_c_prev ;    b_c_factor = b_c_factor*(-0.8) ;    switch_direction = 1 ;  end    model_temp = gdp_model_fn(lagged_data(:, strcmp(varnames,'CONS')), ...    lagged_data(:, strcmp(varnames,'UNEMP')), ...      [min_gs_sol(1); min_gs_sol(2)+step_length*b_u_factor] ) ;  min_gs_sse_b_u_current = ssefn( ...      lagged_data(:, strcmp(varnames,'REALGDP')), model_temp ...    ) ;    if min_gs_sse_b_u_current < min_gs_sse_b_u_prev    min_gs_sol(2) = min_gs_sol(2)+step_length*b_u_factor ;  else    min_gs_sse_b_u_current = min_gs_sse_b_u_prev ;    b_u_factor = b_u_factor*(-0.8) ;    switch_direction = 1 ;  end    if switch_direction == 0    min_gs_sse_both_current = min_gs_sse_b_u_current ;    sse_improvement = min_gs_sse_both_prev-min_gs_sse_b_u_current ;    min_gs_sse_both_prev = min_gs_sse_both_current   ;    min_gs_sse_b_c_prev = min_gs_sse_both_current   ;    min_gs_sse_b_u_prev = min_gs_sse_both_current   ;  end    disp( [switch_direction ; sse_improvement])  trend = [trend ; sse_improvement] ;  switch_tracker = [switch_tracker ; switch_direction] ;  switch_direction = 0 ; end  plot(log(trend)); hold on;plot(switch_tracker, ':r') min_grid_sol min_grid_sse  min_gs_sse_both_current min_gs_solmin_grid_sol = ''combnk(1:5,2)for i=1:sizelagged_data = [full_data(5:orig_obs, strcmp(varnames,'Year')) full_data(5:orig_obs, strcmp(varnames,'QTR'))function[ret] = sse_fn(y, x, beta)  ret = (y - x*beta)'(y - x*beta)end  ret = y - x*beta;  ret = ret' * ret;