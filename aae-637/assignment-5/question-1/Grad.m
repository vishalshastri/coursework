function g = Grad(x0,func,num_row)
    global  dh ;
    delta = abs(x0).*dh;   %Delta(i) defined relative to |x0(i)|
    g=zeros(num_row,length(x0)); %Initialize (numrow x k) gradient matrix
    for i = 1 : length(x0)
        if x0(i) == 0      % **Loop to avoid delta(i) = 0       
            delta(i)=dh;
        end
        u = x0;  %  ** Reset the U vector                    
        u(i) = x0(i) + delta(i);
        f1 = feval (func,u);     % **Evaluate the function with + delta(i)
        u(i) = x0(i) - delta(i);
        f2 = feval( func, u );  % **Evaluate the function with - delta(i)  
        g(1:num_row,i) = (f1 - f2) / (2 * delta(i)); 
											%(ith param.(numobs x 1)
                                            %gradient vector                                                               
    end
end  

