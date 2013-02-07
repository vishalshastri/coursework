% PARTABLE   Takes econometric estimates, along with their parameter names
% and a descriptive column header, and combines them in a nicely formatted
% table.
%
% EXAMPLE 1: 4 Parameters, 3 Estimate Categories
% parnames = {'alpha' 'beta1' 'beta2' 'beta3'}';
% estimates = [5.63 2.3 2.45; 10 5 2; 6 2 3; 20.5 10.25 2];
% header = {'PARNAMES' 'EST' 'STERR' 'TVALUE'};
% print = partable(parnames,estimates, header);
%
% EXAMPLE 2: 1 Parameter, 4 Estimate Categories
% parnames = {'alpha'}';
% estimates = [5.65 1005.67 10.78 0.98];
% header = {'PARNAME' 'AVG' 'SUM' 'MAX' 'MIN'};
% print = partable(parnames,estimates, header);
%
% EXAMPLE 3: Too Many Estimate Categories
% parnames = {'alpha'}';
% estimates = [1 2 3 4 5 6 7 8];
% header = {'PARNAME' 'C1' 'C2' 'C3' 'C4' 'C5' 'C6' 'C7' 'C8'};
% print = partable(parnames,estimates, header);
%
% EXAMPLE 4: 2nd Column Header is Too Long
% parnames = {'alpha'}';
% estimates = [5.65 1005.67 10.78 0.98];
% header = {'PARNAME' 'WAYWAYTOOLONG' 'SUM' 'MAX' 'MIN'};
% print = partable(parnames,estimates, header)
%
% EXAMPLE 5: Parameter Name is Too Long
% parnames = {'waywaytoolong'}';
% estimates = [5.65 1005.67 10.78 0.98];
% header = {'PARNAME' 'WAYWAYTOOLONG' 'SUM' 'MAX' 'MIN'};
% print = partable(parnames,estimates, header)
%
% EXAMPLE 6: 2 Parameters, 7 Estimate Categories
% parnames = {'alpha' 'beta1'}';
% estimates = [1 2 3 4 5 6 7; 1.1 2.2 3.3 4.4 5.5 6.6 7.7];
% header = {'PARNAMES' 'C1' 'C2' 'C3' 'C4' 'C5' 'C6' 'C7'};
% print = partable(parnames,estimates, header);

function f = partable(parnames,estimates,header)

   [f.numpars,f.numest] = size(estimates);

   if f.numest > 7
       error('Too many estimate categories! Max is 7!');
   end
   
   for i = 1:f.numest
       if length(header{i}) > 10
           error('One or more column headers is too long! Max length is 10 chars.!')
       end
   end
   
   for i = 1:f.numpars
       if length(parnames{i}) > 10
           error('One or more par. names is too long! Max length is 10 chars.!')
       end
   end

   if f.numest == 1
       fprintf('--------------------- \n')
       fprintf('%10s %10s \n', header{1}, header{2})
       fprintf('--------------------- \n')
       for i = 1:f.numpars
           fprintf('%10s %10.5f \n', parnames{i}, estimates(i,:))
       end
       fprintf('--------------------- \n')
   end

   if f.numest == 2
       fprintf('-------------------------------- \n')
       fprintf('%10s %10s %10s \n', header{1}, header{2}, header{3})
       fprintf('-------------------------------- \n')
       for i = 1:f.numpars
           fprintf('%10s %10.5f %10.5f \n', parnames{i}, estimates(i,:))
       end
       fprintf('-------------------------------- \n')
   end

   if f.numest == 3
       fprintf('------------------------------------------- \n')
       fprintf('%10s %10s %10s %10s \n', header{1}, header{2}, header{3}, header{4})
       fprintf('------------------------------------------- \n')
       for i = 1:f.numpars
           fprintf('%10s %10.5f %10.5f %10.5f \n', parnames{i}, estimates(i,:))
       end
       fprintf('------------------------------------------- \n')
   end

   if f.numest == 4
       fprintf('------------------------------------------------------ \n')
       fprintf('%10s %10s %10s %10s %10s \n', header{1}, header{2}, header{3}, header{4}, header{5})
       fprintf('------------------------------------------------------ \n')
       for i = 1:f.numpars
           fprintf('%10s %10.5f %10.5f %10.5f %10.5f \n', parnames{i}, estimates(i,:))
       end
       fprintf('------------------------------------------------------ \n')
   end
   
   if f.numest == 5
       fprintf('----------------------------------------------------------------- \n')
       fprintf('%10s %10s %10s %10s %10s %10s \n', header{1}, header{2}, header{3}, header{4}, header{5}, header{6})
       fprintf('----------------------------------------------------------------- \n')
       for i = 1:f.numpars
           fprintf('%10s %10.5f %10.5f %10.5f %10.5f %10.5f \n', parnames{i}, estimates(i,:))
       end
       fprintf('----------------------------------------------------------------- \n')
   end

   if f.numest == 6
       fprintf('---------------------------------------------------------------------------- \n')
       fprintf('%10s %10s %10s %10s %10s %10s %10s \n', header{1}, header{2}, header{3}, header{4}, header{5}, header{6}, header{7})
       fprintf('---------------------------------------------------------------------------- \n')
       for i = 1:f.numpars
           fprintf('%10s %10.5f %10.5f %10.5f %10.5f %10.5f %10.5f \n', parnames{i}, estimates(i,:))
       end
       fprintf('---------------------------------------------------------------------------- \n')
   end
   
   if f.numest == 7
       fprintf('--------------------------------------------------------------------------------------- \n')
       fprintf('%10s %10s %10s %10s %10s %10s %10s %10s \n', header{1}, header{2}, header{3}, header{4}, header{5}, header{6}, header{7}, header{8})
       fprintf('--------------------------------------------------------------------------------------- \n')
       for i = 1:f.numpars
           fprintf('%10s %10.5f %10.5f %10.5f %10.5f %10.5f %10.5f %10.5f \n', parnames{i}, estimates(i,:))
       end
       fprintf('--------------------------------------------------------------------------------------- \n')
   end
end
   
