# Load the course script
source("https://lngproc.hss.nthu.edu.tw/statisticsR/courseUtil.R")

# Get the sample dataset from Myers (2015) again for the F-test
Myers.clean = loadCourseCSV(week = "Week7-9", file = "MyersClean.csv")

# Get RTs from the two different sessions in the "wordlikeness"
# experiment, so we can compare the variances in the two samples
# using a F-test.
s1.rt = Myers.clean[Myers.clean$Session == 1, ]$logRT
s2.rt = Myers.clean[Myers.clean$Session == 2, ]$logRT

# Run F-test and get the full report.
var.test(s1.rt, s2.rt)

# Get the number of data points in each sample, which minus 1 is 
# the degree of freedom for each sample variance.
length(s1.rt)
length(s2.rt)

# Get the variance for each sample and calculate the ratio,
# which is also the F-value in the F-test.
var(s1.rt) / var(s2.rt)

# Manually retrieve a one-tailed p-value from an F-distribution
# with the ratio and two degrees of freedom. Since the ratio is
# above 1 (the assumed population mean), get the one-tailed p
# from the upper tail first.
pf(q = 1.160577, df1 = 6750, df2 = 7200, lower.tail = FALSE)
# Get the two-tailed p.
pf(q = 1.160577, df1 = 6750, df2 = 7200, lower.tail = FALSE) * 2

# A simulated dataset of Saito & Lyster's (2012) study of how form-
# focused teaching could help with the learning English r for Japanese
# speakers.
sl.sim = loadCourseCSV(week = "Week10-11", file = "SaitoLysterSim.csv")

# Convert the variable/column with characters into a "factor" using
# as.factor(), which helps divide our dataset into subsets based on
# the "unique labels", or formally known as "levels/categories".
# We can use levels() to check the levels of a factor.
levels(as.factor(sl.sim$Group))