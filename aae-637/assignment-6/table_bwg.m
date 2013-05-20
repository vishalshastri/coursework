% Define parameter names like this:  parnames = {'alpha' 'beta1' 'beta2' 'beta3'}';

function f = table_bwg(parnames,estimates,option)
  [f.numpars,f.numest] = size(estimates);
  fprintf('----------------------------------------------------------------- \n');
  if option==1; 
       header = {'Variables' 'Value' 'Std.Err' 'T-Value' 'P-Value'};
       fprintf('----------------------------------------------------------------- \n');
       fprintf('%10s %10s %10s %10s %10s  \n', header{1}, header{2}, ...
                                        header{3}, header{4}, header{5});
       fprintf('----------------------------------------------------------------- \n');
       for i = 1:f.numpars
           fprintf('%10s %10.5f %10.5f %10.5f %10.5f  \n', parnames{i}, estimates(i,:));
       end
       disp('  ');
  elseif option==2;
       header = {'Variables' 'Value' 'Std.Err' 'T-Value'};
       fprintf('----------------------------------------------------------------- \n');
       fprintf('%10s %10s %10s %10s \n', header{1}, header{2}, ...
                                        header{3}, header{4});
       fprintf('----------------------------------------------------------------- \n');
       for i = 1:f.numpars
           fprintf('%10s %10.5f %10.5f %10.5f  \n', parnames{i}, estimates(i,:));
          
       end
       disp('  ');
  elseif option==3;
       header = {'Variables' 'Estimate'};
       fprintf('----------------------------------------------------------------- \n');
       fprintf('%10s %10s \n', header{1}, header{2});
       fprintf('----------------------------------------------------------------- \n');
       for i = 1:f.numpars
           fprintf('%10s %10.5f \n', parnames{i}, estimates(i,:));
       end
       disp('  '); 
  elseif option==4; 
       header = {'Variable' 'Coeff' 'STD-Hess' 'T-HESS' 'STD-E(H)' 'T-E(H)' 'STD-BHHH' 'T-BHHH'};
       fprintf('----------------------------------------------------------------- \n');
       fprintf('%8s %8s %8s %8s %8s %8s %8s %80s  \n', header{1}, header{2}, ...
                        header{3}, header{4}, header{5}, header{6}, header{7}, header{8})
       fprintf('----------------------------------------------------------------- \n');
       for i = 1:f.numpars
           fprintf('%8s %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f %8.3f  \n', ...
               parnames{i}, estimates(i,:));
       end
       disp('  ');       
  elseif option==5;
       header = {'Attribute' 'PR_Beach' 'PR_Pier' 'PR_Pr_boat' 'PR_Ch_boat'};
       fprintf('----------------------------------------------------------------- \n');
       fprintf('%10s %10s %10s %10s %10s \n', header{1}, header{2}, ...
           header{3}, header{4}, header{5});
       fprintf('----------------------------------------------------------------- \n');
       for i = 1:f.numpars
           fprintf('%10s %10.5f %10.5f %10.5f %10.5f \n', parnames{i}, estimates(i,:));
       end
       disp('  '); 
  elseif option==6;
       header = {'Variable' 'Coeff' 'Hess-SE' 'HESS-T' 'Rob-SE' 'ROBUST-T'};
       fprintf('----------------------------------------------------------------- \n');
       fprintf('%10s %10s %10s %10s %10s %10s \n', header{1}, header{2}, ...
           header{3}, header{4}, header{5}, header{6});
       fprintf('----------------------------------------------------------------- \n');
       for i = 1:f.numpars
         fprintf('%10s %10.5f %10.5f %10.5f %10.5f %10.5f \n', parnames{i}, estimates(i,:));
       end
       disp('  ');       
 elseif option==7;
       header = {'Variable' ' Pr_SD ' ' Pr_D ' ' Pr_A ' ' Pr_SA '};
       fprintf('----------------------------------------------------------------- \n');
       fprintf('%10s %10s %10s %10s %10s \n', header{1}, header{2}, ...
           header{3}, header{4}, header{5});
       fprintf('----------------------------------------------------------------- \n');
       for i = 1:f.numpars
         fprintf('%10s %10.5f %10.5f %10.5f %10.5f  \n', parnames{i}, estimates(i,:));
       end
       disp('  ');               
 elseif option==8;
       header = {'Variable' ' Est.Coef.' 'Est.S.E.' 'Est.Coef.','Est.S.E.' };
       fprintf('----------------------------------------------------------------- \n');
       fprintf('%10s %10s %10s %10s \n', header{1}, header{2},header{3},header{4});
       fprintf('----------------------------------------------------------------- \n');
       for i = 1:f.numpars
         fprintf('%10s %10.5f %10.5f %10.5f %10.5f  \n', parnames{i}, estimates(i,:));
       end
       disp('  ');  
  elseif option==9; 
       header = {'Variables' 'Value' 'Std.Err' 'Z-Value' 'P-Value'};
       fprintf('----------------------------------------------------------------- \n');
       fprintf('%10s %10s %10s %10s %10s  \n', header{1}, header{2}, ...
                                        header{3}, header{4}, header{5});
       fprintf('----------------------------------------------------------------- \n');
       for i = 1:f.numpars
           fprintf('%10s %10.5f %10.5f %10.5f %10.5f  \n', parnames{i}, estimates(i,:));
       end
       disp('  ');       
  elseif option==10; 
       header = {'Variable' 'Value' 'An_S.E' 'Z-Anal' 'PVal_An' ...
              'App.S.E' 'Z-Appr' 'PVal_Ap'};
       fprintf('----------------------------------------------------------------------- \n');
       fprintf('%10s %10s %7s %7s %7s %7s %7s %7s\n', header{1}, header{2}, ...
                    header{3}, header{4}, header{5}, header{6}, header{7}, header{8});
       fprintf('----------------------------------------------------------------------- \n');
       for i = 1:f.numpars
           fprintf('%10s %10.5f %7.4f %7.4f %7.4f %7.4f %7.4f %7.4f  \n', ...
                            parnames{i}, estimates(i,:));
       end
       disp('  ');                
end     
 disp('-----------------------------------------------------------------');    
end

