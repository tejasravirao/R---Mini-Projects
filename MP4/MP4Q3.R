# Question 3
library(boot)
set.seed(123)

# read "VAPOR.csv" file
vapdata = read.csv("D:/Users/rao29/Documents/Sem 5/Stats for Data Science/MP4/VAPOR.csv", header = TRUE, sep = ",")

# histogram for theoretical and experimental values
hist(vapdata$theoretical, main = "Histogram of Theoretical Values", xlab = "Theoretical Values")
hist(vapdata$experimental, main = "Histogram of Experimental Values", xlab = "Experimental Values")

# QQ plot for theoretical values
qqnorm(vapdata$theoretical,main = "Theoretical Values")
qqline(vapdata$theoretical)

# QQ plot for experimental values
qqnorm(vapdata$experimental, main = "Experimental Values")
qqline(vapdata$experimental)

# side by side boxplots for theoretical and experimental values
boxplot(vapdata$theoretical,vapdata$experimental, main ="Vapor pressure readings", names = c("Theoretical", "Experimental"), ylab = "Vapor Pressure")

# Summary statistics of theoretical values
summary(vapdata$theoretical)

# Summary statistics of experimental values
summary(vapdata$experimental)

# obtain difference between Theoretical values and Experimental values
diff = vapdata$theoretical - vapdata$experimental

# Histogram and QQ plot for difference
hist(diff, main = "Difference of two values")
qqnorm(diff, main = "Difference of two values")
qqline(diff)

# construct a confidence interval using non-parametric bootstrap
# define function to calculate mean
mean.npar = function(x,indices){
  result = mean(x[indices])
  return(result)
}

(mean.npar.boot = boot(data = diff, mean.npar, R = 999, sim = "ordinary", stype = "i"))

# calculate 95% confidence interval using percentile bootstrap
boot.ci(mean.npar.boot,conf = 0.95, type = "perc")

