%{
This is how you use this function

[base_data,varnames,raw] = xlsread('w:\greene_credit_v3'); 
wanted={'Age', 'Ownrent'};  Note:  These are the names as they appear in the spreadsheet.
finaldata=pull_data(varnames,wanted,base_data);

%}

function [fin_data] = pull_data(vnames,varsOfInterest,dat)
    [numobs,numc]=size(dat);
    fin_data=zeros(numobs,length(varsOfInterest));
    for j = 1: length(vnames);
       for i = 1:length(varsOfInterest)
           location = strcmp(varsOfInterest(i),vnames(j));
           if location == 1;
               fin_data(:,i)= dat(:,j);                   
           end
        end  
    end
end
