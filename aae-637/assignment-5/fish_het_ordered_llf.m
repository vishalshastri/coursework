function [lli] = fish_het_ordered_llf(b0, data_mat)
      rhsvar = data_mat(:, [1 2 3 16]);
      z_mat = data_mat(:, 16);
      dummy_mat = data_mat(:, 5:7);
      num_val = 4;
      % num_val would be 4 if we didnt have a constant term in our regression
      num_mu = num_val - 2;
      % num_mu = num_val - 1;  if we didnt have a constant
      %k=length(b0)-num_mu;
      betas=b0(1:size(rhsvar, 2));                         %*** Grab Betas ***
      gamm = b0(end);
      argue=rhsvar*betas;                %*** X*Beta     ***
      %mu=b0(k+1:end);     %*** Grab Mu's  ***
      mu=b0(5:6);     %*** Grab Mu's  ***
      sigma_est = exp(z_mat*gamm);
      for j=0:3;
        if j == 0
           lli = abs(sum(dummy_mat, 2)-1).*log(normcdf(-argue./sigma_est));
           elseif j == 1;
           lli=lli+dummy_mat(:,j).*log(normcdf((mu(j)-argue)./sigma_est) - normcdf(-argue./sigma_est)); 
                                        %*** First Choice ***
           elseif (and((j >1) ,(j < 3)))  %*** Intermed. Choices ***
           lli=lli+dummy_mat(:,j).*log(normcdf((mu(j)-argue)./sigma_est)- ...
                                   normcdf((mu(j-1)-argue)./sigma_est));
           elseif (j == 3)             %*** Final Choice ***
           lli=lli+dummy_mat(:,j).*log(1-normcdf((mu(j-1)-argue)./sigma_est));
        end;
      end;   
end


