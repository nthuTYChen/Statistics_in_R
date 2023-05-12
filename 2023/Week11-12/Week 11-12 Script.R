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

# Run the one-way independent-measure ANOVA to explain rF3 with
# Group
sl.sim.aov = aov(formula = rF3 ~ Group, data = sl.sim)
# Get the summary of the ANOVA results.
summary(sl.sim.aov)

# You get the same F-value in the above analysis by dividing the
# Mean Sq of Group (the interesting variance) by the Mean Sq of 
# Residuals (i.e.the boring, unexplained within-group variance).
5345500 / 107131

# Get to the bottom of the math in one-way independent-measure
# ANOVA
# Calculate the individual group means and the grand mean (i.e.,
# the average of all rF3s)
mean.grand = mean(sl.sim$rF3)
mean.control = mean(sl.sim[sl.sim$Group == "Control",]$rF3)
mean.ffi = mean(sl.sim[sl.sim$Group == "FFI",]$rF3)
mean.fficf = mean(sl.sim[sl.sim$Group == "FFI+CF",]$rF3)

# Calculate the squared difference between each group mean
# and the grand mean
ss.control = (mean.control - mean.grand) ^ 2
ss.ffi = (mean.ffi - mean.grand) ^ 2
ss.fficf = (mean.fficf - mean.grand) ^ 2

# Calculate the number of data points in each group
control.n = nrow(subset(sl.sim, Group == "Control"))
ffi.n = nrow(subset(sl.sim, Group == "FFI"))
fficf.n = nrow(subset(sl.sim, Group == "FFI+CF"))

# Multiply each sqaured difference with the number of data points
# and sum everything together, you get Sum Sq in the above
# ANOVA result, which is the total between-group variance.
ss.between = control.n * ss.control + ffi.n * ss.ffi + fficf.n * ss.fficf

# There are three levels in the variable Group, so our degree of 
# freedom is 3 - 1 = 2, which is the number you see in the ANOVA
# result. And if you divide ss.between by the df, you get the same
# Mean Sq for Group - the mean between-group variance.
ss.between / (3 - 1)

# Now, moving on to the calculation of within-group variance
# (i.e., the boring variance)
# Get the rF3 for each of the three groups
control.rF3 = sl.sim[sl.sim$Group == "Control",]$rF3
ffi.rF3 = sl.sim[sl.sim$Group == "FFI",]$rF3
fficf.rF3 = sl.sim[sl.sim$Group == "FFI+CF",]$rF3

# Calculate squared differences between individual rF3 and 
# the mean within each group
ss.control.within = sum((control.rF3 - mean.control) ^ 2)
ss.ffi.within = sum((ffi.rF3 - mean.ffi) ^ 2)
ss.fficf.within = sum((fficf.rF3 - mean.fficf) ^ 2)

# Sum all within-group variances and you get the total within-
# group variance (i.e., Sum Sq for Residuals in the ANOVA results)
ss.within = sum(ss.control.within, ss.ffi.within, ss.fficf.within)

# The number of df for ss.within is the number of data points
# minus the number of levels in Group, which is 640 - 3 = 637,
# the same number you see for Residuals in the ANOVA result.
# And if you divide ss.within by the df, you get the same Mean
# Sq for Residuals
ss.within / (nrow(sl.sim) - 3)

# Compare an one-way independent-measure ANOVA to a two-sample
# t-test assuming an equal variance
# Exclude the FFI+CF group, so the comparison is between Control
# and FFI
sl.sim.sub = subset(sl.sim, Group != "FFI+CF")
# Run the same one-way indepdent-measure ANOVA, except that
# Group now only has two levels Control and FFI
sl.sim.sub.aov = aov(formula = rF3 ~ Group, data = sl.sim.sub)
summary(sl.sim.sub.aov)

# Run the two-sample unpaired t-test assuming an equal variance
# and you get the same df and p-value. If you square the t-value
# you also get the same F-value.
t.test(formula = rF3 ~ Group, data = sl.sim.sub, var = TRUE)

# In the previous full ANOVA, we only know that Group has a 
# significant main effect, but we don't which two groups have
# a significantly different rF3, so we need to do some post-hoc
# pairwise comparison for this information. Check the logic and
# the technical details of Tukey's HSD test in the handout.
TukeyHSD(sl.sim.aov)

# Load another dataset simulating a within-subject design
# based on Saito & Lyster's (2015) study. That is, instead of
# have three independent groups of learners, now we have the
# same group of learners undergoing three different learning
# conditions (i.e., Control, FFI, and FFI+CF)
sl.rep.sim = loadCourseCSV(week = "Week10-11", 
                           file = "SaitoLysterRepSim.csv")
head(sl.rep.sim)

# Since every subject produces ten tokens within each condition,
# we first average rF3 over the ten tokens for each subject
# within each condition to help simplify our analysis. But in 
# theory, you shouldn't do this because you're losing information
# regarding the variance across the ten items.
sl.rep.avg = aggregate(rF3 ~ Subject + Condition, FUN = mean, 
                       data = sl.rep.sim)

# Run one-way independent-measure ANOVA for comparison
sl.rep.aov = aov(formula = rF3 ~ Condition, data = sl.rep.avg)
summary(sl.rep.aov)

# Run one-way repeated-measure ANOVA, which is appropriate for our
# within-subject design. Check the handout for the full explanation
# of the codes as well as the comparison with the one-way independent-
# measure ANOVA.
sl.rep.avg$Subject = as.factor(sl.rep.avg$Subject)
sl.rep.aov.good = aov(formula = rF3 ~ Condition + 
                        Error(Subject / Condition), data = sl.rep.avg)
summary(sl.rep.aov.good)

# You get an error message if you want to extend Tukey's HSD test to
# a one-way repeated-measure ANOVA.
TukeyHSD(sl.rep.aov.good)

# So, we can do post-hoc pairwise comparisons with Bonforroni Correction.
# Our Bonforroni-corrected alpha (i.e., significance level) is .05 divided
# by the number of pairwise comparisons. Here we have three (i.e., 
# Control-FFI, Control-FFI+CF, FFI-FFI+CF), thus .05 / 3 = .0167.
alpha = .05 / 3

sl.cl.ffi = subset(sl.rep.avg, Condition != "FFI+CF")
t.test(formula = rF3 ~ Condition, paired = T, data = sl.cl.ffi)
# p > .0167

sl.cl.fficf = subset(sl.rep.avg, Condition != "FFI")
t.test(formula = rF3 ~ Condition, paired = T, data = sl.cl.fficf)
# p < .0167

sl.ffi.fficf = subset(sl.rep.avg, Condition != "Control")
t.test(formula = rF3 ~ Condition, paired = T, data = sl.ffi.fficf)
# p < .0167

# Load the sample dataset of Chen (2020); check the course handout
# for the explanation.
chen.sample = loadCourseCSV("Week10-11", "Chen2020Sample.csv")

# A two-way independent-measure ANOVA that explains the acceptance rate
# by learner group and the initial tone of a test item as well as their 
# interaction.
# Note once again that in this example, the dependent variable is a binary-coded
# acceptability of test items (0 = unacceptable, 1 = acceptable). Keep in mind
# that normally you should only use ANOVA for the analysis of a continuous 
# dependent variable.
chen.aov = aov(formula = Accept ~ Group * InitialTone, 
               data = chen.sample)

# Get the output of the two-way ANOVA; main effects and the interaction
# are all significant. Check the handout to link the effects to the visualized
# data in Figure 4.
summary(chen.aov)

# All the cross-overs between Group and Initial Tone has 1536 data points -
# a fully balanced dataset.
xtabs(~ Group + InitialTone, data = chen.sample)

# Calculate the average acceptance rate for each of the four cross-overs
# between the two independent variables. Try to link the numbers to the results
# of the two-way ANOVA above.
aggregate(Accept ~ Group + InitialTone, FUN = mean, data = chen.sample)

# Calculate the average acceptance rate for each of the two groups. 
# Try to link the numbers to the results of the two-way ANOVA above.
aggregate(Accept ~ Group, FUN = mean, data = chen.sample)
# Calculate the average acceptance rate for each of the two levels in 
# InitialTone.  Try to link the numbers to the results of the two-way 
# ANOVA above.
aggregate(Accept ~ InitialTone, FUN = mean, data = chen.sample)

# Calculate the variance for each cross-over following the formula in the
# handout
# Group = NonFinalH, InitialTone = H
1536 * (0.4602865 - 0.4947917 - 0.5361328 + 0.511566) ^ 2 +
  # Group = NonFinalR, InitialTone = H
  1536 * (0.6119792 - 0.5283203 - 0.5361328 + 0.511556) ^ 2 +
  # Group = NonFinalH, InitialTone = R
  1536 * (0.5292969 - 0.4947917 - 0.4869792 + 0.511556) ^ 2 +
  # Group = NonFinalR, InitialTone = R
  1536 * (0.4446615 - 0.5283203 - 0.4869792 + 0.511556) ^ 2
# When you sum all these variances together, you get the Sum Sq for the
# two-way interaction in the two-way ANOVA

# With a fully balanced dataset, the order of independent variables does not
# influence the F statistics.
chen.aov.2 = aov(Accept ~ InitialTone * Group, data = chen.sample)
summary(chen.aov.2)

# Remove the first row from the original dataset.
chen.sample.2 = chen.sample[-1, ]
# Missing one data point from one of the four cross-overs - it is no longer
# a perfectly balanced dataset.
xtabs(~ Group + InitialTone, data = chen.sample.2)

# Now the order of the two independent variables matters as there are some
# minor changes in the F statistics. Check the handout for a detailed 
# explanation of why this happens, and make sure to keep this in mind if you
# need to run any ANOVA in the future.
chen.aov.3 = aov(Accept ~ Group * InitialTone, data = chen.sample.2)
chen.aov.4 = aov(Accept ~ InitialTone * Group, data = chen.sample.2)