# Question 2(c)

# n=5, store sample values in vector
sampleVal = c(21.72,14.65,50.42,28.78,11.23)

# Probability density function
# of the continuous random variable defined in the function
funcTheta = function(t,x){
  return (t/x^(t+1))
}

# Negative of log-likelihood function
neg.loglik.fun = function(par,dat){
  logSum = sum(log(funcTheta(par,dat)))
  return (-logSum)
}

# Minimize -log (L), i.e., maximize log (L)
# obtain parameter estimate
ml.est = optim(par=1, fn = neg.loglik.fun, method = "BFGS", hessian = TRUE, dat = sampleVal)
ml.est$par

# Question 2(d)
# Standard error of the MLE
(result = sqrt(diag(solve(ml.est$hessian))))

# Calculate 95% Confidence Interval of estimate
ConfInter = ml.est$par + c(-1,1)*qnorm(0.975)*result
ConfInter
