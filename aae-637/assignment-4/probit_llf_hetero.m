function [tot_llf] = probit_llf_hetero(b0, data_mat)
   num_betas = 8; 
   depend = data_mat(:, 1);
   rhsvar = data_mat(:, 2:(num_betas+1)); 
   z_mat = data_mat(:, (num_betas+2):end);
   gammas =  b0((num_betas+1):end);
   b0 = b0(1:num_betas);
   cdf_prob = normcdf(rhsvar*b0 ./ exp(z_mat*gammas));
   llf = depend.*log(cdf_prob) + (1-depend).*log(1-cdf_prob);
   tot_llf=sum(llf);    
end

