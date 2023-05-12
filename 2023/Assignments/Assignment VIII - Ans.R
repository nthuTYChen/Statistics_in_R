library(languageR)
head(lexdec)

# Part I
# Task I
# The task should be familiar to you already, but the trick here is that
# you can combine all three conditions together using "&" to say that
# each row must have "correct" as the value of the variable "Correct".
rt.mean = mean(lexdec$RT)
rt.sd = sd(lexdec$RT)
lexdec.sub = subset(lexdec, RT > rt.mean - 2 * rt.sd & 
                      RT < rt.mean + 2 * rt.sd &
                      Correct == "correct")

# Task II
# Again, shouldn't be too difficult to understand the math
(nrow(lexdec) - nrow(lexdec.sub)) / nrow(lexdec)
nrow(lexdec.sub)
# About 7.9% of observations were dropped from lexdec

# Task III
# Divide log-RTs into two samples based on whether the previous test item
# is a word or a nonword.
rt.prevWord = lexdec.sub[lexdec.sub$PrevType == "word",]$RT
rt.prevNonword = lexdec.sub[lexdec.sub$PrevType == "nonword",]$RT

# Run the test of variance first to decide which two-sample t-test
# to apply in our comparison.
var.test(rt.prevNonword, rt.prevWord) # p = .627; equal variance

t.test(rt.prevWord, rt.prevNonword, var.equal = TRUE)
# t(1526) = -5.74, two-tailed p = < .001
# We have compared mean log-RTs by PrevType in an unpaired two-sample t-test 
# assuming an equal variance, which suggests a significant difference (t(1526) 
# = -5.74, two-tailed p < .001).

# Task IV
# Divide log-RTs into two samples based on whether a test item is morphologically
# simplex or complex.
rt.simplex = lexdec.sub[lexdec.sub$Complex == "simplex", ]$RT
rt.complex = lexdec.sub[lexdec.sub$Complex == "complex", ]$RT

# Run the test of variance first to decide which two-sample t-test
# to apply in our comparison.
var.test(rt.simplex, rt.complex) # p = .845; equal variance

t.test(rt.simplex, rt.complex, var.equal = TRUE)
# t(1526) = -1.03, two-tailed p = .3
# We have compared mean log-RTs by Complex in an unpaired two-sample t-test 
# assuming an equal variance, which suggests a nonsignificant difference 
# (t(1526) = -1.03, two-tailed p = .3).

# Task V
# Only the difference by PrevType is significant, so only calculate its effect
# size, namely the absolute difference between the two means over the SD of 
# all log-RTs.
cohend.prev = abs(6.327679 - 6.383964) / sd(lexdec.sub$RT)
# 0.2 < 0.291 < 0.6; medium effect size

# Part II
# Task VI
# You need to convert log-RTs back to raw RTs using exp() before calculating
# the differences since log transformation is not linear; a difference of, say,
# 0.1 on a log scale, means very different things between large numbers and 
# between small numbers. Try abs(exp(0.1) - exp(0.2)) and abs(exp(5.1) - exp(5.2)).
abs(exp(6.327679) - exp(6.383964))
# 32.4 ms for the difference caused by PrevType
abs(exp(6.354679) - exp(6.370252))
# 9 ms for the difference caused by Complex

# Task VII
library(ggplot2)

# Convert all log-RTs back to raw RTs and store them into a variable call RT.raw
lexdec.sub$RT.raw = exp(lexdec.sub$RT)

# Generate a boxplot to show the distribution of raw RTs based on Complex.
ggplot(mapping = aes(x = Complex, y = RT.raw), data = lexdec.sub) +
  geom_boxplot() + scale_y_continuous(limits = c(250, 1100)) +
  labs(title = "Distribution of Lexical Judgment raw RT by morphological structure",
       subtitle = "correct responses and real words only",
       # Show the statistical test and its result with the "caption" parameter
       caption = "Unparied two-sample t-test assuming an equal variance\nt(1526) = -1.032, two-tailed p = .3",
       x = "Morphological Structure", y = "Reaction time in seconds") +
  theme_bw()

# Bonus
source(
  "https://lngproc.hss.nthu.edu.tw/statisticsR/Week10-11/AssignmentVIII.R")
head(sim.diff)

# Task VIII
# Get all necessary distributional information for the calculation of 
# 95% confidence interval
sim.diff.mean = mean(sim.diff)
sim.diff.df = length(sim.diff) - 1
sim.diff.se = sd(sim.diff) / sqrt(length(sim.diff))

# Get the critical t-value that marks an area representing a probability of 
# 2.5% in the upper tail of a t-distribution based on a specific degree of 
# freedom (i.e., 100 - 1 = 99).  The critical t-value that marks an area 
# representing the same probability is thus the t-value in the upper tail
# multiplied by -1. The range between 1.984 and -1.984 in the t-distribution
# thus marks a range representing a probability of 95%.
sim.diff.crit.t = qt(p = 0.025, df = sim.diff.df, lower.tail = F) # 1.984

# Calculate the upper and lower bound of the 95% confidence interval
sim.diff.ci.upper = sim.diff.mean + sim.diff.crit.t * sim.diff.se # 29.07
sim.diff.ci.lower = sim.diff.mean - sim.diff.crit.t * sim.diff.se # 10.12

# Task IX
# The 95% CI suggests that if I re-run the same study another 99 times and 
# collect 100 differences between paired RTs, my study will be one of 95 studies
# whose 95% CI covers the true population mean. Since my 95% CI ranges from
# 10.07 to 29.11, which does not cover zero, I could confidently say that the
# true population mean is NOT zero (and there's only 5% of chance that I could
# be wrong). Put differently, the mean difference between paired RTs in my sample
# is very unlikely to be observed due to total randomness, and it is significantly 
# different from zero.
