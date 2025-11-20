# Get ready
library(languageR)
head(english)

# Part I
# Task 1
# Convert log naming RT back to the raw RT
english$RTnamingRaw = exp(english$RTnaming)

# Use a boxplot to visualize the change in raw naming RT by target word length
# in letter. Boxplot is appropriate here according to the description of the
# task - it shows DATA DISTRIBUTION for each target word length, so it is
# easier to see the change of y along the x-axis. If you choose to generate
# a scatterplot, you can't see the data distribution information, and all
# individual dots would be clustered as a single line for each (discrete) target
# word length, making it difficult to see the trend.
boxplot(english$RTnamingRaw ~ english$LengthInLetter,
        main = "Raw naming RT variation by Target Word Length (English)",
        xlab = "Target word length in letter", ylab = "Naming reaction time (ms)",
        # I choose to expand the y-axis scale a bit, because with some space
        # above and below each box, the plot is more readable.
        ylim = c(300, 900))

# There are two things that support the "positive correlation" between raw
# reaction time and target word length. First of all, the position of the lines
# representing the naming RT median is higher as target word length increases
# (at least from 2 to 5). Then, the boxes representing the 25-75% range of a
# data distribution also gradually moves up with an increased target word length.

# Task 2
library(ggplot2)

# A boxplot is still the most appropriate option here, since it shows the
# distribution of raw naming RT in each crossover of the two levels from both
# variables (i.e., voiced+old, voiceless+old, etc.).

# Map reaction time to y since we want to observe its variation based on other
# variables. One main variable Voice is mapped to x.
ggplot(data = english, mapping = aes(x = AgeSubject, y = RTnamingRaw)) +
  # Make the boxes narrower
  geom_boxplot(width = .5) +
  # Again, expand the scale of y a bit to increase the readibility
  scale_y_continuous(limits = c(300, 900)) +
  # Divide the plot based on AgeSubject into columns, as requested by the task
  facet_grid(~ Voice) +
  labs(title = "Raw naming RT variation by Subject Age and Initial Voicing (English",
       x = "Age of Subjects", y = "Naming Reaction Time (ms)") +
  theme_bw()

# It is very obvious that older subjects are substantially slower than young
# subjects, regardless of initial segment voicing, as the two boxes representing
# older subjects are much higher. The impact of initial segment voicing also
# seems crucial, as the medians/boxes  of the "voiceless" category are also
# higher.

# Task 3
# The explanation might be on the technical side of measuring RTs in a production
# task. In this task, participants are considered to begin their production of
# a target item when there are some phonetic signals that can be detected by
# the experimental software. For initial voiceless segments, the signals are
# detectable when the air comes out of the mouth after, for example, the closure
# of your lips when trying to produce [p]. By contrast, for initial voiced
# segments, vocal cords may start to vibrate to produce voicing even before
# the closure ends (e.g. when you try to produce [m]), so the signals could be
# detected earlier. So, unfortunately, the difference here might not bear any
# deep theoretical implications.

# Part II
# Task 4

# Treat RTnaming as the population, so its mean is the population mean,
# and its SD is the population SD.
pop.mean = mean(english$RTnaming) # 6.322505...
pop.sd = sd(english$RTnaming) # 0.178481478...

# Sample mean/SD/size is the mean/SD/size of the "old" subset
english.old = subset(english, AgeSubject == "old")
sample.mean = mean(english.old$RTnaming) # 6.4934999...
sample.sd = sd(english.old$RTnaming) # 0.05568...
sample.n = nrow(english.old) # 2284

# calculate z-value manually
english.old.z = (sample.mean - pop.mean) / (pop.sd / sqrt(sample.n)) # 45.786...

# The hypothesis is directional, since the sample mean is assumed to
# be HIGHER (i.e., a slower RT) than the population mean. So, check the 
# probability of observing the z-value or higher in the UPPER tail.
english.old.p = pnorm(english.old.z, lower.tail = F)
english.old.p 
# 0 (it is actually not zero, but an extremely small value close to zero)

# We compared our sample mean (M = 6.49) to an assumed population mean of 6.32
# with an assumed population SD of 0.179 in a one-sample z-test, 
# which suggests a significantly higher sample mean (z = 45.79, 
# one-tailed p < .001).

# Task 5
# We need the additional parameter DF for the one-sample t-test, which is sample
# size minus 1
sample.df = sample.n - 1 # 2283

# Use all the relevant numbers to calculate the t statistics
english.old.t = (sample.mean - pop.mean) / (sample.sd / sqrt(sample.n)) # 144.18

# Get the upper tail p-value from a corresponding t distribution
english.old.p = pt(english.old.t, df = sample.df, lower.tail = F)
english.old.p
# 0 (again, it is actually not zero, but an extremely small value close to zero)

# If the results are reported following the APA format, it looks like this:
# We compared our sample mean (M = 6.49, sd = 0.057) to an assumed population 
# mean of 6.32 in a one-sample t-test,  which suggests a significantly higher 
# sample mean (t(2283) = 144.18, one-tailed p < .001).

# Task 6
# Just run a two-sample t-test using t.test(), and use the y ~ x to explain
# how RTnaming varies as a function of Voice.

# But in this case, you need to first check if the two samples have a comparable
# variance in their RTnaming.

# Get the two subsets representing each sample first.
english.vcd = subset(english, Voice == "voiced")
english.vcl = subset(english, Voice == "voiceless")

var.test(english.vcd$RTnaming, english.vcl$RTnaming)
# The test results suggest a non-significant difference in RTnaming variance.

# Run a two-sample test assuming EQUAL variance, based on the results of the
# variance test.
t.test(RTnaming ~ Voice, data = english, var = T)

# If the results are reported following the APA format, it looks like this:
# We tested the difference in log naming RT arising from initial segment voicing
# in a two-sample t-test assuming equal variance, and the results suggest a 
# a significant difference (t(4566) = -4.8946, two-tailed p < .001).