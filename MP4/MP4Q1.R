# Question 1

library(boot)

# read "gpa.csv" file 
gpacsv = read.csv("D:/Users/rao29/Documents/Sem 5/Stats for Data Science/MP4/gpa.csv", header = TRUE, sep = ',')

# plot scatter plot of gpa against act
plot(gpacsv$gpa, gpacsv$act, main = "Scatterplot of GPA vs ACT", xlab = "GPA", ylab = "ACT", pch=20)

# point estimate of population correlation(rho) between GPA and ACT
rhop = cor(gpacsv$gpa,gpacsv$act)
print(paste("Point estimate of correlation between GPA and ACT = ", rhop))

# define function to find correlation between GPA and ACT
corr.npar = function(x,indices){
  result = cor(gpacsv$gpa[indices], gpacsv$act[indices])
  return(result)
}

# calculate point estimate, bias and standard error values
(corr.npar.boot = boot(data = gpacsv, corr.npar, R = 999, sim = "ordinary", stype = "i"))

# calculate 95% confidence interval using percentile bootstrap
boot.ci(corr.npar.boot,conf = 0.95, type = "perc")

