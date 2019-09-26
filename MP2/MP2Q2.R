
# Question 2
# read csv file
acc = read.csv("D:/Users/rao29/Documents/Sem 5/Stats for Data Science/MP2/motorcycle.csv")

# summary of accident data
summary(acc)

# total number of accidents
acc.total = acc$Fatal.Motorcycle.Accidents

# boxplot of number of accidents
boxplot(acc.total, main = "Boxplot number of accidents")

# statistics on number of accidents
(acc.total.stats = summary(acc.total))

# mean - number of accidents
(acc.total.mean = mean(acc.total))

# standard deviation - number of accidents
(acc.total.sd = sd(acc.total))

# median - number of accidents
(acc.total.median = median(acc.total))

# Q1 and Q3 values
(acc.total.Q1 = acc.total.stats[2])
(acc.total.Q3 = acc.total.stats[5])

# interquartile range - number of accidents
(acc.total.iqr = IQR(acc.total))

# obtain the outliers
(acc.total.outliers = subset(acc, ((acc.total < (acc.total.Q1 - 1.5*acc.total.iqr))|(acc.total > (acc.total.Q3 + 1.5*acc.total.iqr)))))
