function [llf_vec] = cond_logit_llf(b0, data_mat)

   rhsvar = data_mat(:, 3:end);
   nalts = size(unique(data_mat(:, 1)), 1);
   mode_id = data_mat(:, 1);
   mode = data_mat(:, 2);
   part_2=0;
   inner_1=0;
   for j=1:nalts;                             %*** No. of Alternatives ***
      inner_2=0;
      for i=1:nalts;           %*** Create LLF denom. ***         
         rhsvar_i=rhsvar(mode_id==i,:);         
         inner_2=inner_2+exp(rhsvar_i*b0);    %*** (T x 1) ***     
      end;
      dummy_j=mode(mode_id == j,:); %**Dummy var.,jth model chosen **
      part_2=part_2+dummy_j.*log(inner_2);
      rhsvar_j=rhsvar(mode_id == j,:);       %*** XB for jth alternative ***
   	  inner_1=inner_1+dummy_j.*(rhsvar_j*b0); %***Sum if j consumed (T x 1) ***
   end;  
   llf_vec = inner_1-part_2;
   
end

