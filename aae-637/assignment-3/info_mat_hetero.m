function ret = info_mat_hetero(betas, data_mat, num_betas)
  % Betas must be a k x 1 vector
%  BETA  = vertcat(betas(1:9), repmat(0, 5, 1));
  ALPHA = betas((num_betas+1):(size(data_mat,2)-1)) ;
  
  A = data_mat(:, 2:(num_betas+1))' * ...    
    diag(exp( - data_mat(:, (num_betas+2):size(data_mat,2)) * ALPHA)) * data_mat(:, 2:(num_betas+1));
  
  B = .5 .* data_mat(:, (num_betas+2):size(data_mat,2))' * ...
    data_mat(:, (num_betas+2):size(data_mat,2));
  
  ret =  blkdiag(A, B)  ;
  
end