function H=ml_hess(f, betas, data_mat)

%function H=FHESS(f,x,varargin)
% FHESS Computes numerical Hessian of a function
% USAGE:
% H=FHESS(f,x)
% INPUTS
% f : a string name of a function
% x : a vector at which to evaluate the Hessian of the function
 k = size(betas,1);
 fx = sum(f(betas, data_mat));
 h = eps.^(1/3)*abs(betas);%,1e-2)%;
 xh = betas+h;
 h = xh-betas;
 ee = sparse(1:k,1:k,h,k,k);
 g = zeros(k,1);
 for i=1:k
   g(i) = sum(f(betas+ee(:,i), data_mat));
 end
 H=h*h';
 for i=1:k
   for j=i:k
     H(i,j) = (sum(f(betas+ee(:,i)+ee(:,j), data_mat))-g(i)-g(j)+fx) ...
                  / H(i,j);
     H(j,i)=H(i,j);
   end
 end 

