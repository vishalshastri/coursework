function [ret] = med_cond_prob(b0, data_mat)


b1_bi=b0(1:6);
b2_bi=b0(7:10);

gamma_1=vertcat(b1_bi,0);  %**gamma_1 of full dimension **
gamma_2=vertcat(b2_bi(1:2),0,0, b2_bi(3), 0, b2_bi(4)); %** gamma_2 of full dimension **

q1_mn=1;
q2_mn=1;
w1_mn=q1_mn.*(mean(data_mat(:,3:9))*gamma_1); 
w2_mn=q2_mn.*(mean(data_mat(:,3:9))*gamma_2); 
rho = tanh(b0(11));

rho_star_mn=q1_mn.*q2_mn.*rho;        %** Defined on p.740, Greene ***
numer_1=w2_mn-rho_star_mn.*w1_mn;     %** Numerator of 17.52, p.740 ***
numer_2=w1_mn-rho_star_mn.*w2_mn;     %** Numerator of 17.52, p.740 ***
denom=sqrt(1-rho_star_mn^2);          %** Denomenator of 17.52, p.740 ***
g_1=normpdf(w1_mn).*normcdf(numer_1./denom);%** Defined by 17.52, p.740 ***
g_2=normpdf(w2_mn).*normcdf(numer_2./denom);%** Defined by 17.52, p.740 ***


omega=[1 rho_star_mn;rho_star_mn 1];
mu=[0 0];
lim_bi=[w1_mn w2_mn];
part_2_2=(g_2-mvncdf(lim_bi,mu,omega).*normpdf(w2_mn)./normcdf(w2_mn)).*gamma_2;
part_y1_y2_1=(1./normcdf(w2_mn)).*(g_1*gamma_1+part_2_2);
rho_mat = [1 rho; rho 1];

ret  = part_y1_y2_1' .* ( mean(data_mat(:,3:9)) / ...
      (mvncdf([mean(data_mat(:,3:9))*gamma_1 mean(data_mat(:,3:9))*gamma_2], 0, rho_mat) ) );
  
ret = ret(2);
      
end
