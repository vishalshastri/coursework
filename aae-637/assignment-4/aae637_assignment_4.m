%% QUESTION 1

%% Q. 1.a

path(path,'C:\Users\tdmcarthur\Documents\MATLAB\') ;

urlwrite('http://www.aae.wisc.edu/aae637/data/matlab/fluid_probit_data.xls','temp.xls');
[full_data,varnames,raw]=xlsread('temp.xls');


mod_data = horzcat( 
    full_data(:, strcmp(varnames,'fluidx')) > 0, ...
    repmat(1, size(full_data, 1), 1), ...
    full_data(:, strcmp(varnames,'num_yung')), ...
    full_data(:, strcmp(varnames,'incomet')), ...
    full_data(:, strcmp(varnames,'num_yung')) *. full_data(:, strcmp(varnames,'incomet')), ...
    full_data(:, strcmp(varnames,'sm_city')), ...
    full_data(:, strcmp(varnames,'city')), ...
    full_data(:, strcmp(varnames,'refrig')), ...
    full_data(:, strcmp(varnames,'perfafh')) ...
    );

varnames = [varnames, 'CONSUME_L1', 'INC_L1', 'CONSUME_L2', 'INC_L2'];

max(full_data)

betas_start=(mod_data(:, 2:end)'*mod_data(:, 2:end))\(mod_data(:, 2:end)'*mod_data(:, 1));  

[fluid_betas,fluid_cov,fluid_LLF_vec] = probit_bwg(betas_start, mod_data, ...
  {'const', 'num_yung', 'incomet', 'yung_x_inc', 'sm_city', 'city', 'refrig', 'perfafh'}, ...
  probit_llf, 1, 250, 1e-6)






parname={'ONE','W_AGE','W_AGESQ','W_EDU','FAMINC'}; 
critic_limit=1e-6;              % Critical % change in parameters
iter_limit=250;                 % Maximum number of iterations
do_step=1;                      % =1 variable step length, 0 fixed 
func_name =('probit_llf');      % Identify LLF function
alpha=.05;                      % Type I Error Probability 
dh = 0.000001;
