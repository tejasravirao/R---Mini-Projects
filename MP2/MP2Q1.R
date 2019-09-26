
# Read csv file from path
raceData = read.csv("D:/Users/rao29/Documents/Sem 5/Stats for Data Science/MP2/roadrace.csv", na.strings = "*")

# Store Maine column from csv into MaineVar
MaineVar = raceData$Maine

# Question 1(a)

# Use barplot to show bar graph of Maine variable
barplot(table(MaineVar), main = "Question 1(a)", xlab = "Maine or Away", ylab = "Num of Runners", ylim = c(0,5000))

# Summary of Maine or Away runners
summary(MaineVar)


# Question 1(b)

# Using subset() function 
# to get data for Maine group and Away group separately
Mgroup = subset(raceData, raceData$Maine == "Maine")
Agroup = subset(raceData, raceData$Maine == "Away")

# For each of the groups
# get their respective times(in minutes)
Mgroup.time = Mgroup$Time..minutes.
Agroup.time = Agroup$Time..minutes.

# Histograms of the runner's times (in minutes)
# for both groups
hist(Mgroup.time, main = "Runners from Maine", xlim = c(0,140), ylim = c(0,2000), xlab = "Running Time (minutes)" )
box()
hist(Agroup.time, main = "Runners from Away", xlim = c(0,140), ylim = c(0,2000), xlab = "Running Time (minutes)" )
box()

# Provide Statistics

# Summary for Maine group
# Use () to show the stat directly
(Mgroup.time.stats = summary(Mgroup.time))

# Mean - Maine group
(Mgroup.time.mean = mean(Mgroup.time))

# Standard Deviation - Maine group
(Mgroup.time.sd = sd(Mgroup.time))

# range - Maine group
(Mgroup.time.range = range(Mgroup.time))

# median - Maine group
(Mgroup.time.median = median(Mgroup.time))

# interquartile range - Maine group
(Mgroup.time.iqr = IQR(Mgroup.time))

# obtain Q1 and Q3 values for Maine group
(Mgroup.time.Q1 = Mgroup.time.stats[2])
(Mgroup.time.Q3 = Mgroup.time.stats[5])

# lower and upper bounds to detect outliers for Maine group
(Mgroup.time.lower = max((Mgroup.time.Q1 - (1.5*Mgroup.time.iqr)),min(Mgroup.time)))
(Mgroup.time.upper = min((Mgroup.time.Q3 + (1.5*Mgroup.time.iqr)),max(Mgroup.time)))


# Summary for Away group
# Use () to show the stat directly
(Agroup.time.stats = summary(Agroup.time))

# Mean - Away group
(Agroup.time.mean = mean(Agroup.time))

# Standard Deviation - Away group
(Agroup.time.sd = sd(Agroup.time))

# range - Away group
(Agroup.time.range = range(Agroup.time))

# median - Away group
(Agroup.time.median = median(Agroup.time))

# interquartile range - Away group
(Agroup.time.iqr = IQR(Agroup.time))

# obtain Q1 and Q3 values for Away group
(Agroup.time.Q1 = Agroup.time.stats[2])
(Agroup.time.Q3 = Agroup.time.stats[5])

# lower and upper bounds to detect outliers for Away group
(Agroup.time.lower = max((Agroup.time.Q1 - (1.5*Agroup.time.iqr)),min(Agroup.time)))
(Agroup.time.upper = min((Agroup.time.Q3 + (1.5*Agroup.time.iqr)),max(Agroup.time)))


# Question 1(c)

# side by side box plots
var = c("Maine Runners", "Away Runners")
boxplot(Mgroup.time, Agroup.time, names = var, main = "Side by Side Boxplots", ylab = "Time")

# outliers - Maine group
(Mgroup.time.outliers = subset(Mgroup, (Mgroup$Time..minutes. < (Mgroup.time.Q1 - 1.5*Mgroup.time.iqr))|(Mgroup$Time..minutes. > (Mgroup.time.Q3 + 1.5*Mgroup.time.iqr))))

# outliers - Away group
(Agroup.time.outliers = subset (Agroup, (Agroup$Time..minutes. < (Agroup.time.Q1 - 1.5*Agroup.time.iqr)) | (Agroup$Time..minutes. > (Agroup.time.Q3 +1.5*Agroup.time.iqr))))


# Question 1(d)

# Obtain Male and Female Data
genMale = subset(raceData, raceData$Sex == "M")
genFemale = subset(raceData, raceData$Sex == "F")

# create side by side boxplots
var2 = c("Male Runners", "Female Runners")
boxplot(genMale$Age, genFemale$Age, names = var2, main = "Boxplots for age of runners", ylab = "Age")

# Summary for male age
(genMale.age.stats = summary(genMale$Age))

# stats for Male Age
# mean - Male Age
(genMale.age.mean = mean(genMale$Age))

# standard deviation - Male Age
(genMale.age.sd = sd(genMale$Age))

# range - Male Age
(genMale.age.range = range(genMale$Age))

# median - Male Age
(genMale.age.median = median(genMale$Age))

# interquartile range - Male Age
(genMale.age.iqr = IQR(genMale$Age))

# Q1 and Q3 values - Male Age
(genMale.age.Q1 = genMale.age.stats[2])
(genMale.age.Q3 = genMale.age.stats[5])


# summary for female age
(genFemale.age.stats = summary(genFemale$Age))

# stats for female age
# mean - Female age
(genFemale.age.mean = mean(genFemale$Age))

# standard deviation - Female Age
(genFemale.age.sd = sd(genFemale$Age))

# range - Female Age
(genFemale.age.range = range(genFemale$Age))

# median - Female Age
(genFemale.age.median = median(genFemale$Age))

# interquartile range - Female Age
(genFemale.age.iqr = IQR(genFemale$Age))

# Q1 and Q3 values for Female Age
(genFemale.age.Q1 = genFemale.age.stats[2])
(genFemale.age.Q3 = genFemale.age.stats[5])
