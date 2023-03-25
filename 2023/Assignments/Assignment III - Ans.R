# Preparation work
conf.rt = 
     read.csv("https://lngproc.hss.nthu.edu.tw/statisticsR/Week4-5/confRT.csv") 
head(conf.rt)

#1
# The number of values in RT is equal to the number of rows in a data frame.
nrow(conf.rt)	#4117
# You get the same number by measuring the length of the variable as a vector.
length(conf.rt$RT)

# 2
# Extract the RT variable as a vector and pass it to mean()
conf.mean = mean(conf.rt$RT)	#826.5089

# 3
# Get the difference between each value and the mean
diffs = conf.rt$RT - mean(conf.rt$RT)
# Get Mean Sum of Squares
ss = sum(diffs ^ 2) / (nrow(conf.rt) - 1)
# Get the square root of Mean SS for SD
conf.sd = sqrt(ss)	# 2738.88
sd(conf.rt$RT)	# same number

# 4
set.seed(95)

# 5
# Sample 4117 data points from a normal distribution with the same mean/sd.
conf.rt.norm = rnorm(n = nrow(conf.rt), mean = conf.mean, sd = conf.sd)

# 6
# Extract a subset of conf.rt.norm in which values are equal to or 
# higher than mean + sd * 1.5
conf.rt.norm.sub = conf.rt.norm[conf.rt.norm >= conf.mean + conf.sd * 1.5]

# 7
# Calculate the probability of observing a value in conf.rt.norm.sub from
# conf.rt.norm
length(conf.rt.norm.sub)/length(conf.rt.norm)	# 0.06825358

# 8
# Extract a subset of conf.rt in which values in RT are equal to or
# higher than mean + sd * 1.5
conf.rt.sub = subset(conf.rt, RT >= conf.mean + conf.sd * 1.5)

# 9
# Calculate the probability of observing a value in RT in conf.rt.sub from
# conf.rt
nrow(conf.rt.sub)/nrow(conf.rt)	# 0.004615011

# 10
# The probability of observing a value equal to or higher than mean + sd * 1.5
# is much smaller in our sample distribution than in the normal distribution.
# Thus, our sample distribution deviates substaintialy from the normal distribution.
