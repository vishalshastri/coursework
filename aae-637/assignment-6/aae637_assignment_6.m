


urlwrite('http://www.aae.wisc.edu/aae637/data/matlab/cheese_only_data.xls','temp.xls');
[full_data,varnames,raw]=xlsread('temp.xls');



start_vals = inv(fish_data(:, 3:end)'*fish_data(:, 3:end))*fish_data(:, 3:end)'*fish_data(:, 2);

[fish_betas, fish_cov, fish_llf_vec]  = max_bhhh(start_vals, ...
  {'cost' 'c_rate' 'inc_x_pier' 'inc_x_priv' 'inc_x_chart' 'pier' 'priv_boat' 'chart_boat'}, ...
  size(fish_data, 1)/size(unique(fish_data(:, 1)), 1), ...
  250, 1e-6, @cond_logit_llf, 1, 0.000001, ...
  fish_data, 0, 0);



[b_tobit,covb_tobit,tobit_llf_vec] =  ...       
    max_bhhh(b0,parname);
end;
cov_analytic=tobit_info_mat(b_tobit); %**Call Out Infor. Mat-Based Cov ***
std_anal=sqrt(diag(cov_analytic));    %*** Analytic Std. Err. ***
mlzval=b_tobit./std_anal;             %*** Analytic Z-Ratio ***
stdapp=sqrt(diag(covb_tobit));        %*** Approximate Std. Err. ***
mlz_orig=b_tobit./stdapp;             %*** Approximate Z-Ratio ***
p_anal=2*(1-normcdf(abs(mlzval)));    %*** Analytic P-Value ***
p_appr=2*(1-normcdf(abs(mlz_orig)));  %*** Approximate P-Value ***
results2=horzcat(b_tobit,std_anal,mlzval,p_anal,stdapp,mlz_orig,p_appr);
disp('*******Compare Analytical and Numerical Param. Std. Errors*******');
table_bwg(parname,results2,10); %*** Stdapp=numerical Hessian S.E. ***
%******* Evaluate Value and Std. Error of Various Elasticities ********
mean_rhs = mean(rhsvar)';             %*** Point of Evalaution ***
%*** Create Elasticities ***
prob_elas=post_est_prob(b_tobit);    %*** Est. Probability ***
cond_elas=post_est_cond(b_tobit);    %*** Exp. Cond. E(Y-value) ***
total_elas=post_est_total(b_tobit);  %*** Unconditional E(Y-value) ***
%********** Evaluate Parameter Gradients ***************************
grad_prob=Grad(b_tobit,'post_est_prob',1); %***Param. Prob. Grad. ***
grad_cond=Grad(b_tobit,'post_est_cond',1); %***Param. Cond. E(Y) Grad **
grad_total=Grad(b_tobit,'post_est_total',1);%*Param. Uncond E(Y) Grad **
%****************** Delta Method for Elasticity Variance ***************
disp('The 3 Income Elasticities as shown in McDonald and Moffitt');
var_probability=grad_prob*covb_tobit*grad_prob'; %***Prob Elas Var***
var_conditional=grad_cond*covb_tobit*grad_cond'; %***Cond E(Y) Elas Var***
var_total=grad_total*covb_tobit*grad_total';     %**Uncond E(Y)Elas Var**
all_elas=vertcat(prob_elas,cond_elas,total_elas);%*** Stack Elas. ***
all_elas_se=sqrt(vertcat(var_probability,var_conditional,var_total));
                                                 %*** SE of All Elas.***    
z_elas=all_elas./all_elas_se;                    %*** Z-Value All Elas ***     
pvalue_elas=2*(1-normcdf(abs(z_elas)));          %*** All Elas P-Value *** 
results_elas=horzcat(all_elas,all_elas_se,z_elas,pvalue_elas);
elas_name={'Prob-Inc','Cond-Inc','Total-Inc'};
table_bwg(elas_name,results_elas,9);



