function H=model_hess(f, betas, x_mat)

%function H=FHESS(f,x,varargin)
% FHESS Computes numerical Hessian of a function
% USAGE:
% H=FHESS(f,x)
% INPUTS
% f : a string name of a function
% x : a vector at which to evaluate the Hessian of the function
 k = size(betas,1);
 fx = f(betas, x_mat);
 h = eps.^(1/3)*max(abs(betas),1e-2);
 xh = betas+h;
 h = xh-betas;
 ee = sparse(1:k,1:k,h,k,k);
 g = zeros(k,1);
 for i=1:k
   g(i) = f(betas+ee(:,i), x_mat);
 end
 H=h*h';
 for i=1:k
   for j=i:k
     H(i,j) = f(betas+ee(:,i)+ee(:,j))-g(i)-g(j)+fx), x_mat) ...
                  / H(i,j);
     H(j,i)=H(i,j);
   end
 end 

