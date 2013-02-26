function ret = count_likelihood_fn(betas, data_mat)
  % Betas must be a k x 1 vector
  Z = data_mat[:, 3:size(data_mat, 2)]
  ret = sum( -exp(Z * betas) + data_mat[:, 1] .* Z * betas - data_mat[:, 2])
end