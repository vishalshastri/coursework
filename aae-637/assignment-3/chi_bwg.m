function [crit_val_chi] = chi_bwg(alph,df,chistat)
  crit_val_chi=chi2inv(1-alph,df);
  fprintf('This is the Prob. of Type I error:  %3.2f\r', alph);
  disp('  ')
  fprintf('This is the critical Chi-Sq. Stat:  %6.3f\r', crit_val_chi);
    if chistat >= crit_val_chi;
        fprintf('-->Reject H0 \r');
        else
        fprintf('-->Do Not Rejct H0 \r');
    end
 end     

