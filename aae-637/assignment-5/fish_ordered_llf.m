function [lli] = fish_ordered_llf(b0, data_mat)
      rhsvar = data_mat(:, [1 2 3 16]);
      dummy_mat = data_mat(:, 5:7);
      num_val = 4;
      % num_val would be 4 if we didnt have a constant term in our regression
      num_mu = num_val - 2;
      % num_mu = num_val - 1;  if we didnt have a constant
      %k=length(b0)-num_mu;
      betas=b0(1:size(rhsvar, 2));                         %*** Grab Betas ***
      argue=rhsvar*betas;                %*** X*Beta     ***
      %mu=b0(k+1:end);     %*** Grab Mu's  ***
      mu=b0(5:6);     %*** Grab Mu's  ***
      for j=0:3;
        if j == 0
           lli = abs(sum(dummy_mat, 2)-1).*log(normcdf(-argue));
           elseif j == 1;
           lli=lli+dummy_mat(:,j).*log(normcdf(mu(j)-argue) - normcdf(-argue)); 
                                        %*** First Choice ***
           elseif (and((j >1) ,(j < 3)))  %*** Intermed. Choices ***
           lli=lli+dummy_mat(:,j).*log(normcdf(mu(j)-argue)- ...
                                   normcdf(mu(j-1)-argue));
           elseif (j == 3)             %*** Final Choice ***
           lli=lli+dummy_mat(:,j).*log(1-normcdf(mu(j-1)-argue));
        end;
      end;   
end


