# Random number generator seed 
# Delivers uniform random variables 
set.seed(11)

# Q1 b(i)

# Lifetime of Block A
# Simulate one draw of Xa
BlockA = rexp(1, rate = 0.1)

# Lifetime of Block B
# Simulate one draw of Xb
BlockB = rexp(1, rate = 0.1)

# Simulate one draw of satellite lifetime T
# Using max as SatelliteT lasts until both fail 
# so longer lifetime considered
SatelliteT = max(BlockA, BlockB)

# Q1 b(ii)
# perform step 10000 times to give 
# 10000 distributions of T
 drawsT = replicate(10000, max(rexp(1,rate = 0.1), rexp(1,rate = 0.1)))

# Q1 b(iii)
# histogram of draws of T 
hist(drawsT, freq = FALSE)

# superimpose density function
curve((0.2*exp(-0.1*x)-0.2*exp(-0.2*x)), add = TRUE, xlab = "drawsT", ylab = "density value")

# Q1 b(iv)
# find Expected value E(T)
mean(drawsT)

# Q1 b(v)
# find probability that satellite lasts more than 15 years
mean(15 < abs(drawsT))

# utility method to repeat above tasks using replicate
# reps is the no. of Monte Carlo repetitions
parametersHelper = function(reps){
  drawsT = replicate(reps, max(rexp(1,rate = 0.1), rexp(1,rate = 0.1)))
  ExpectedVal = mean(drawsT)
  Prob = mean(15 < abs(drawsT))
  print(c(ExpectedVal,Prob))
}

# Q1 b(vi) 
# replicate the operation 4 more times
replicate(4, parametersHelper(10000))

# Q1 c
# replicate the procedure 5 more times 
# but using 1000 Monte Carlo replications
replicate(5, parametersHelper(1000))

# Q1 c
# replicate the procedure 5 more times 
# but using 100000 Monte Carlo replications
replicate(5, parametersHelper(100000))

