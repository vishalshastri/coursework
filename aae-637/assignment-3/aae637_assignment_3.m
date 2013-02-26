

for ( i in size(full_data, 1) )
  if full_data(i, 1) != 0
    = log(1:full_data(i, 1))
  end
end
