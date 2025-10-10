# Get ready by loading the sample data set
source("https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/refs/heads/main/courseUtil.R")
Myers.sample = loadCourseCSV(2025, "3_Data", "Myers_2015_Sample.csv")

# Part I
# Task I
# While I listed two conditions (RT != 0 and RT > 250), they simply represented
# just one condition (i.e., RT > 250), but I wouldn't mind if you completed this
# task in two separate statements
Myers.sub = subset(Myers.sample, RT > 250)

# Task II
# Just use log() for log-transformation
Myers.sub$logRT = log(Myers.sub$RT)
# Get the mean and SD for logRT to convert all logRTs into SDs
logRT.mean = mean(Myers.sub$logRT)
logRT.sd = sd(Myers.sub$logRT)
# Convert all logRTs into SDs and store the output to logRT.sd
Myers.sub$logRT.sd = (Myers.sub$logRT - logRT.mean) / logRT.sd

# Task III
# You can use & to combine two conditions in subset() as well.
# Again, I don't mind if this task is done in two separate statements.
Myers.sd2 = subset(Myers.sub, logRT.sd <= 2 & logRT.sd >= -2)

# Task IV
# A function that receives two trial order ranges (as a vector)
trialRangeMeanRT = function(range1, range2) {
  # Get two subsets based on whether the values of TrialOrder are included in
  # each range vector
  Myers.range1 = subset(Myers.sd2, TrialOrder %in% range1)
  Myers.range2 = subset(Myers.sd2, TrialOrder %in% range2)
  # Get the two RT means from each subset
  range1.rt.mean = mean(Myers.range1$RT)
  range2.rt.mean = mean(Myers.range2$RT)
  # Print out the message: You can combine more than just two values into a 
  # long string in paste(). I set "sep" to be " " so every two values are
  # separated with a space.
  rt.msg = paste("The mean RT of the two trial order ranges is", range1.rt.mean, 
                 "milliseconds and", range2.rt.mean, "milliseconds", sep = " ")
  print(rt.msg)
}

# Task V
# Submit two ranges to trialRangeMeanRT to make two comparisons
trialRangeMeanRT(200:300, 400:500)
trialRangeMeanRT(900:1000, 2900:3000)
# From the 200-300th trials to the 400-500th trials, the average reaction time
# drops from 1696 to 1567 milliseconds, a decrease of about 130 milliseconds.
# This might suggest that the participants had started to lose their intuition
# of whether a nonword was acceptable, so they began to speed up their responses.
# Then, during the 900-1000th trials, the mean reaction time further decreases
# to 1209 millisecond, which remained similar toward the end of the experiment
# during the 2900-3000 trials. So, the satiation effect led to a reaction time
# that hit the floor in the second half of the experiment.

# Task VI (Bonus)
# The proportion of the area below -1.5 SD in a normal distribution is about
# 6.7%.
pnorm(q = -1.5) # 0.0668072

# When you check whether each value in logRT.sd is lower than -1.5 
# (i.e., -1.5SD from the mean), you get a bunch of TRUEs and FALSEs. And as
# explained in the instructions, TRUEs are 1s, so when you sum all the TRUEs
# and FALSEs together, you get the number of TRUEs. Dividing the number of
# TRUEs by the total number of observations (i.e., the number of rows of the
# entire data set), you get the probability (or proportion) of values below
# -1.5, which is 0.0407034, or about 4.1%.
sum(Myers.sd2$logRT.sd < -1.5) / nrow(Myers.sd2) # 0.0407034

# Since the proportion of the area below -1.5SD from the mean in the normal
# distribution is slightly bigger than in the actual sample, the lower/left
# tail in the sample is likely shorter than in the normal distribution. That
# said, the difference is not huge, so we cannot say that the sample distribution
# is not normal just based on the difference.