%{
********************************************************************
********************************************************************
**  This is a maximum likelihood program to estimate a tobit model 
*********************************************************************
*********************************************************************
%}
clc;                             % Command to clear command window
global critic_limit iter_limit rhsvar numobs do_step func_name dh ...
    parname depend mean_rhs mean_het num_above numparm bhhh ... 
    dum_above_lim elasvar hetvar num_het num_betas;
delete('w:\canada_tobit_hetero.out'); % Delete the previous output
[base_data,varnames,raw] = xlsread('w:\canada_v4'); % Load excel data 
diary('w:\canada_tobit_hetero.out');
[numobs,numc]=size(base_data);        % Determine size of full data 
%************* Read in all the data ***************
base_var={'totinc','nokids','fulltm','atlantic','quebec','bc'};
rhsvar=horzcat(ones(numobs,1),pull_data(varnames,base_var,base_data));
dep_var={'tot_rest'};
depend=pull_data(varnames,dep_var,base_data)/100; % Identify dep. var 
hetname={'totinc','fulltm','quebec'}; %*** Variables in Hetero. Fcn ***
hetvar=horzcat(ones(numobs,1),pull_data(varnames,hetname,base_data));
hetname={'Het_int','Totinc','Fulltm','Quebec'};
parname={'int','totinc','nokids','fulltm','atlantic','quebec', ...
    'bc','sigmasq'}; 
num_betas=numel(parname)-1;   %*** Number of Betas ***
num_het=numel(hetname);       %*** Number of Alphas ***                              
critic_limit=1e-6;            % Critical % change in parameters
iter_limit=250;               % Maximum number of iterations
do_step=1;                    % =1 variable step length, 0 fixed 
func_name =('tobit_llf');     % Identify LLF function (Homo.)
alpha=.05;                    % Type I Error Probability 
dh = 0.000001;
bhhh=1;
dum_above_lim=depend > 0; 
num_above=sum(dum_above_lim);  %*** Number of Noncensored Obs. ***
%*********** Calculate Elasticities for these variables **********
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
table_bwg(name_2,results,3);
disp('  ');    
disp('Now the Tobit Iterations Start');  
disp('  ');   
b0=vertcat(bls,sighat);    %***Use CRM Results for Starting Values ***
numparm=length(b0);        %*** No. of Est. Coef. including sigmasq***
if bhhh ==0;
   [homopar,covb_homo,homo_llf_tot] =  ...  
           max_NR(b0,parname);  %*** Call Out the NR-Tobit ***
else [homopar,covb_homo,homo_llf_vec] =  ...       
    max_bhhh(b0,parname);
end;
parname={'int','totinc','nokids','fulltm','atlantic','quebec', ...
    'bc','het_int','het-inc','het-full','het-QB'}; 
func_name =('het_tobit_llf'); % Identify LLF function (Hetero)
b0_hetero=vertcat(homopar(1:(num_betas)),log(homopar(num_betas+1)), ...
(ones(num_het-1,1).* .0001));
numparm=numel(b0_hetero);     %*** No. of Est. Coef. including alphas***
disp('Now the Heteroscedastic Tobit Iterations Start');  
disp('  '); 
if bhhh ==0;
   [heteropar,covb_hetero,tot_hetero_tot] =  ...  
           max_NR(b0_hetero,parname);  %*** Call Out the NR-Tobit ***
else [heteropar,covb_hetero,hetero_llf_vec] =  ...       
    max_bhhh(b0_hetero,parname);
end;
mean_rhs = mean(rhsvar)';             %*** Point of Evalaution ***
mean_het = mean(hetvar)';             %*** Point of Evalaution ***
fprintf('Total Sample Homoscedastic Log-Lik. Value:  %8.2f', ...
                        sum(homo_llf_vec));
disp('  ')
fprintf('Total Sample Heteroscedastic Log-Lik. Value:  %8.2f', ...
                        sum(hetero_llf_vec));
disp('  ');
chisq_stat=(2*(sum(hetero_llf_vec)-sum(homo_llf_vec)));
fprintf('Likelihood Ratio Test of Heteroscedasticity:  %5.2f', ...
                        chisq_stat);
disp('  ')
vvv=chi_bwg(alpha,(num_het-1),chisq_stat);
%*** Create Elasticities ***
prob_elas_hetero=post_est_prob_hetero(heteropar); 
                                %*** Est. Probability ***
cond_elas_hetero=post_est_cond_hetero(heteropar);  
                                %*** Exp. Cond. E(Y-value) ***
total_elas_hetero=post_est_total_hetero(heteropar);  
                                %*** Unconditional E(Y-value) ***
%********** Evaluate Parameter Gradients ***************************
grad_prob_hetero=Grad(heteropar,'post_est_prob_hetero',1); 
                                %***Param. Prob. Grad. ***
grad_cond_hetero=Grad(heteropar,'post_est_cond_hetero',1); 
                                %***Param. Cond. E(Y) Grad ***
grad_total_hetero=Grad(heteropar,'post_est_total_hetero',1);
                                %*Param. Uncond E(Y) Grad ***
%**************** Delta Method for Elasticity Variance *************
disp('  ');
disp('The 3 Income Elasticities as shown in McDonald and Moffitt');
var_probability_hetero=grad_prob_hetero*covb_hetero*grad_prob_hetero';
                                %***Prob Elas Var***
var_conditional_hetero=grad_cond_hetero*covb_hetero*grad_cond_hetero'; 
                                %***Cond E(Y) Elas Var***
var_total_hetero=grad_total_hetero*covb_hetero*grad_total_hetero';     
                                %**Uncond E(Y)Elas Var**
all_elas=vertcat(prob_elas_hetero,cond_elas_hetero,total_elas_hetero);
                                %*** Stack Elas. ***
all_elas_se=sqrt(vertcat(var_probability_hetero, ...
    var_conditional_hetero,var_total_hetero)); %*** SE of All Elas.***    
z_elas=all_elas./all_elas_se;                  %*** Z-Value All Elas ***     
pvalue_elas=2*(1-normcdf(abs(z_elas)));        %*** All Elas P-Value *** 
results_elas=horzcat(all_elas,all_elas_se,z_elas,pvalue_elas);
elas_name={'Prob-Inc','Cond-Inc','Total-Inc'};
table_bwg(elas_name,results_elas,9);
disp('  ');
diary off;
close all;