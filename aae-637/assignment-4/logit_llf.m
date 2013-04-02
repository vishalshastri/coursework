function [tot_llf] = logit_llf(b0, data_mat)
   depend = data_mat(:, 1);
   rhsvar = data_mat(:, 2:size(data_mat, 2));   
   logistic_prob = 1./(1+exp(-rhsvar*b0));
   llf = depend.*log(logistic_prob) + (1-depend).*log(1-logistic_prob);
   % P. 731 of 7th ed. Greene
   tot_llf=llf;    
   % Actually, this gives us a vector
end

