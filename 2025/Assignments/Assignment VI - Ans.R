# Preparation work
source("https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/refs/heads/main/courseUtil.R")
Myers.sample = loadCourseCSV(2025, "3_Data", "Myers_2015_Sample.csv")

# Part I
# Task 1 - Probably needs no explanation
Myers.sub = subset(Myers.sample, RT > 200)

# Task 2
# Log-transform RT first
Myers.sub$logRT = log(Myers.sub$RT)

# Order the entire data frame by logRT in ascending order
logRT.ord = order(Myers.sub$logRT)
Myers.sub.ord = Myers.sub[logRT.ord,]

# Get the sample size (i.e., the total number of data points or observations)
Myers.n = nrow(Myers.sub.ord)
# Get the number of the ordered observation that stands for the boundary
# that separates the first quartile (25%) from the rest of the ordered data distribution.
# Round the number because the output should represent the "position" of the value
# in the entire data distribution
logRT.q1.pos = round(Myers.n / 4) # the 3545 value in the sample represent the boundary

logRT.q1 = Myers.sub.ord[logRT.q1.pos,]$logRT # The actual boundary is 6.54535

# You can also extract the entire logRT variable as a vector and do a similar
# calculation.

# Task 3
# Get the mean and the SD of logRT
logRT.mean = mean(Myers.sub.ord$logRT)
logRT.sd = sd(Myers.sub.ord$logRT)

# Set the random seed to #1111
set.seed(1111)

# Sample 1000 values from the corresponding normal distribution
norm.sample = rnorm(n = 1000, mean = logRT.mean, sd = logRT.sd)

# Check the proportion of values lower than the Q1 boundary
# Again, the condition in the numerator gives you a vector of TRUEs and FALSEs,
# and summing the entire vector gives you the number of TRUEs.
sum(norm.sample < logRT.q1) / length(norm.sample) # 0.265 or 26.5%

# Bonus
# In the lower tail of a normal distribution, the number of values below the
# Q1 boundary in the sample is proportionally close to 25%, which suggests that
# at least, the sample distribution is not skewed to the left.

# Part II
# Task 1
# Get the subset for the first session
Myers.s1 = subset(Myers.sub.ord, Session == 1)

# Use plot() to generate a SCATTERPLOT, in which the x-axis is TrialOrder
# and the y-axis is the raw RT.
plot(x = Myers.s1$TrialOrder, y = Myers.s1$RT,
     # Give an informative title and axis labels
     main = "Reaction Time Variation by Trial Order in Myers' Sample Data",
     xlab = "Trial Order (1-3000)", ylab = "Reaction Time (ms)")

# In the scatterplot, the distribution of reaction times becomes narrower
# as the trial order increases, and more reaction times gather in the
# bottom-right corner, which suggests that the reaction time increases
# with trial order. This might indicate a satiation effect - as nonwords
# became increasingly (un)acceptable, participants reacted faster to each
# nonword in the first session.

# Task 2
# Load the ggplot2 package
library(ggplot2)

# I want to have a more informative session label, because 1 & 2 are not clear
# on their own
Myers.sub.ord$Session_Lab = ifelse(Myers.sub.ord$Session == 1, 
                                   yes = "Session: 1",
                                   no = "Session: 2")

# Use ggplot2 to generate the scatterplot for both sessions
ggplot(data = Myers.sub.ord, mapping = aes(x = TrialOrder, y = RT)) +
  # Make individual points half-transparent to make each point more visible
  # especially when they overlap with each other.
  geom_point(alpha = .5) + 
  # Split the plot into two columns by Session, and set the "scales" parameter
  # as "free_x", so the x-axis only covers the trial order range that includes
  # data points.
  facet_grid(~ Session_Lab, scales = "free_x") +
  labs(title = "Reaction Time Variation by Trial Order in Myers' Sample Data",
       x = "Trial Order (1-3000)", y = "Reaction Time (ms)") 

# In both sessions, the Mandarin native speakers began with a wider distribution
# of reaction times, which then gradually decreased over time for latter trials.
# This similarity shows that the native speakers may have got more and more tired 
# of all the nonwords toward the end of each session and responded more quickly
# over time.