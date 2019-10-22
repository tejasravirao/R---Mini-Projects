# Question 1

# given values of n 
n = c(1,2,3,5,10,30)

# given values of theta
theta = c(1,5,50,100)

set.seed(123)

# Question 1(b)
# function to obtain Maximum Likelihood Estimator
# and Method of Moments estimator

MC_simulation = function(n, theta){
  
  result = runif(n, min = 0, max = theta)
  
  # obtain thetaOne
  thetaOne = max(result)
  
  # obtain thetaTwo
  thetaTwo = 2 * (mean(result))
  
  return (c(thetaOne,thetaTwo))
}

# replicate the simulation 1000 times
MSE_method = function(n,theta){
  
  thetaEstm = replicate(1000, MC_simulation(n, theta))
  
  # compute MSE value which is for MLE and MOME 
  # for particular n, theta
  return (rowMeans((thetaEstm - theta) ^ 2))
  
}

# get the length of n and theta
nLen = length(n)
thetaLen = length(theta)


#Question 1(c)
# create two matrices of dimensions (n x theta)
# these two matrices will store MSE of MLE and MSE of MOME in
# the respective matrices for every combination of n, theta

MSE_MLE = matrix(nrow = nLen, ncol = thetaLen)
MSE_MOME  = matrix(nrow = nLen, ncol = thetaLen)

for(i in 1:nLen){
  for(j in 1:thetaLen){
    finalResult = MSE_method(n[i], theta[j])
    MSE_MLE[i,j] = finalResult[1]
    MSE_MOME[i,j] = finalResult[2]
  }
}

# display the final matrices of MLE and MOME
MSE_MLE
MSE_MOME

# graph plots for MSE of MLE and MSE of MOME for each value of theta

# for theta = 1
# solid line for MSE of MLE
plot(n, MSE_MLE[,1],main = "theta = 1",ylab = "MSE", type = "l", lty = "solid")

# dotted line for MSE of MOME
lines(n,MSE_MOME[,1], lty = "dotted")


# for theta = 5
# solid line for MSE of MLE
plot(n, MSE_MLE[,2],main = "theta = 5",ylab = "MSE", type = "l", lty = "solid")

# dotted line for MSE of MOME
lines(n,MSE_MOME[,2], lty = "dotted")


# for theta = 50
# solid line for MSE of MLE
plot(n, MSE_MLE[,3],main = "theta = 50",ylab = "MSE", type = "l", lty = "solid")

# dotted line for MSE of MOME
lines(n,MSE_MOME[,3], lty = "dotted")


# for theta = 100
# solid line for MSE of MLE
plot(n, MSE_MLE[,4],main = "theta = 100",ylab = "MSE", type = "l", lty = "solid")

# dotted line for MSE of MOME
lines(n,MSE_MOME[,4], lty = "dotted")

