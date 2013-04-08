function [bi_totllf] = bi_probit_llf(b)
      global rhsvar1 rhsvar2 numc_1 numc_2 q1 q2 numobs ...
          w_combined;
      bi_totllf=zeros(numobs,1);
      b1 = b(1:numc_1);
      b2 = b((numc_1+1):(numc_1 + numc_2));
      rho = tanh(b(numc_1 + numc_2 + 1));   %**** Use Tanh for |rhos|<1 ***/
      w1 =q1.*(rhsvar1*b1);                 %*** See Page 739 in Greene ***/
      w2 =q2.*(rhsvar2*b2);                 %*** See Page 739 in Greene ***/
      w_combined=[w1(:) w2(:)];
      rho_star = q1.*q2.*rho;               %*** Modify Rho due to sign change ***/
      for i=1:numobs
        est_sigma = vertcat(horzcat(1,rho_star(i)),horzcat(rho_star(i),1));
        bi_totllf(i) = log(mvncdf(w_combined(i,:),0,est_sigma)); 
                                %*** Bivariate Stanadard Normal CDF ***/
      end
end

