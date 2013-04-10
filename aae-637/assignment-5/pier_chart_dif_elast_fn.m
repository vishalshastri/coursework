function [ret] = pier_chart_dif_elast_fn(b0, data_mat)

nalts = size(unique(data_mat(:, 1)), 1);
mode_id = data_mat(:, 1);
rhsvar = data_mat(:, 3:end);
bp_multi = b0;
numvar= length(data_mat(1, 3:end));
mode_name={'Beach','Pier','Priv Boat','Ch boat'};

for j=1:nalts; %****Mode Specific Means ****
  mode_var=rhsvar(mode_id == j,:);
  if j == 1;
     mean_mode=mean(mode_var)';
  else
     mean_mode=horzcat(mean_mode,mean(mode_var)');
  end;
end;
denom=0;
for i=1:nalts;
   rhsvar_i=rhsvar(mode_id == i,:);   
   mean_rhs=mean(rhsvar_i);
   denom=denom+exp(mean_rhs*bp_multi);
   if i == 1;
      mean_mat=mean_rhs;
   else
      mean_mat=vertcat(mean_mat,mean_rhs);
   end;
end;
for i=1:nalts;
   if i == 1;
      est_prob=exp(mean_mat(i,:)*bp_multi)./denom;
   else
      est_prob=vertcat(est_prob,(exp(mean_mat(i,:)*bp_multi)./denom));
   end; 	
end;  
  
   for j=1:nalts;
     for k=1:nalts;
      for i=1:numvar;
      	if (j == k);
            elas_effects=mean_mode(i,k).*(1-est_prob(k)).*bp_multi(i);                                                  %****Greene p. 846****/
        else
            elas_effects=mean_mode(i,k).*(-est_prob(k)).*bp_multi(i);
        end;
        if i == 1;
            elas_effects1(k,j)=elas_effects;
        elseif i == 2;
            elas_effects2(k,j)=elas_effects;
        elseif i == 3;
            elas_effects3(k,j)=elas_effects;
        end;                
      end;      
     end;
   end;

ret = elas_effects1(2,2) - elas_effects1(4,4);

end