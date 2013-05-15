%{
********************************************************************
********************************************************************
**  This is a maximum likelihood program to estimate a tobit model 
*********************************************************************
*********************************************************************
%}
clc;                             % Command to clear command window
global critic_limit iter_limit rhsvar numobs do_step func_name dh ...
    parname depend mean_rhs num_above numparm bhhh ... 
    dum_above_lim elasvar;
delete('w:\canada_tobit.out');       % Delete the previous output
[base_data,varnames,raw] = xlsread('w:\canada_v4'); % Load excel data 
diary('w:\canada_tobit.out');
[numobs,numc]=size(base_data);  % Determine size of full data matrix
%************* Read in all the data ***************
base_var={'totinc','nokids','fulltm','atlantic','quebec','bc'};
rhsvar=horzcat(ones(numobs,1),pull_data(varnames,base_var,base_data));
dep_var={'tot_rest'};
depend=pull_data(varnames,dep_var,base_data)/100;   % Identify dep. var 
parname={'int','totinc','nokids','fulltm','atlantic','quebec', ...
    'bc','sigmasq'};          %*** Col. vector of param. names ***
critic_limit=1e-6;            % Critical % change in parameters
iter_limit=250;               % Maximum number of iterations
do_step=1;                    % =1 variable step length, 0 fixed 
func_name =('tobit_llf');     % Identify LLF function
alpha=.05;                    % Type I Error Probability 
dh = 0.000001;
bhhh=0;
dum_above_lim=depend > 0; 
index=find(dum_above_lim);
num_above=sum(dum_above_lim);  %*** Number of Noncensored Obs. ***
%*********** Calculate Elasticities for these variables ************
elasvar=1;                     %*** Pick Variable for Marginal ***
elasvar=elasvar+1;             %*** Add 1 due to constant term ***
%*****************************************************
%*  Obtain the OLS estimates using all the data and **
%*  ignoring the truncated nature of the dependent  **
%*  variable.                                       **
%*****************************************************
bls = (rhsvar'*rhsvar)\(rhsvar')*depend;
df = numobs - length(bls);
ehat = depend - rhsvar*bls;
sse = ehat'*ehat;
sighat = sse/df;
covb = sighat.*inv(rhsvar'*rhsvar);
stbls = sqrt(diag(covb));
fprintf('The Percent of OBS with Zeros:  %5.4f', ...
                            (1-(num_above/numobs))*100);
disp('  ');
disp('These are CRM Paremeter Estimates');
disp('*************************************************');
results=horzcat(bls,stbls);
name_2=parname(1:(length(parname)-1));
table_bwg(name_2,results,2);
disp('  ');    
disp('Now the Tobit Iterations Start');  
disp('  ');   
b0=vertcat(bls,sighat);    %***Use CRM Results for Starting Values ***
numparm=length(b0);        %*** No. of Est. Coef. including sigmasq***
if bhhh ==0;
   [b_tobit,covb_tobit,tot_tobit_llf] =  ...  
           max_NR(b0,parname);  %*** Call Out the NR-Tobit ***
else [b_tobit,covb_tobit,tobit_llf_vec] =  ...       
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
disp('  ');
diary off;
close all;