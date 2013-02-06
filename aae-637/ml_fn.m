function ret = ml_fn(y_input, model_input, deg_freedom)
  sse_output = ssefn( y_input, model_input) ;
  resids = y_input - model_input;
  ret = .5*-sum(log(2*pi)+log(sse_output/deg_freedom)- ...
       (((sse_output/deg_freedom)^-1)*resids.*resids));
end

