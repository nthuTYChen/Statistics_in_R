# Preparation work
library(languageR)
head(lexdec)

# Part I
# Task 1
# Just use aggregate() to average RT by both Subject and PrevType
lexdec.subj = aggregate(RT ~ Subject + PrevType, FUN = mean, data = lexdec)
head(lexdec.subj)
# Subject labels are ordered properly for each PrevType level, so RTs are
# perfectly aligned for running paired t-test manually.

# Task 2
# Get the paired differences based on PrevType
prevtype.diff = lexdec.subj[lexdec.subj$PrevType == "word",]$RT -
  lexdec.subj[lexdec.subj$PrevType == "nonword",]$RT
# Get the mean/SD of the paired differences and the sample size
diff.mean = mean(prevtype.diff) # -0.06...
diff.sd = sd(prevtype.diff) # 0.0539205...
diff.n = length(prevtype.diff) # 21
# Get the SE for the t statistics
diff.se = diff.sd / sqrt(diff.n) # 0.0117664...
# Get the t value
diff.t = diff.mean / diff.se # -5.099
# Get the one-tailed p based on the absolute t value.
# Using an absolute t value so you always get the upper tail p first regardless
# of whether you have a positive or negative t value.
diff.p = pt(abs(diff.t), df = diff.n - 1, lower.tail = F) # < .001
# Two-tailed p
diff.p * 2 # < .001

# Task 3
# Cohen'd for the paired t-test; Cohen'd is always positive, so make sure that
# you get the absolute value.
abs(diff.mean / diff.sd) # 1.14 = Huge effect size

# Get the absolute critical value for calculating the 95% CI
diff.crit = abs(qt(p = 0.025, df = diff.n-1)) # -2.08596344..
# Upper boundary
diff.upper = diff.mean + diff.crit * diff.se # -0.0354567...
diff.lower = diff.mean - diff.crit * diff.se # -0.0845456...

# To test if lexical decision reaction times vary by the wordness of the 
# previous test item within subjects, we ran a paired t-test, which suggests
# a significant difference in reaction times (t(20) = -5.099, two-tailed 
# p < .001, 95% CI [-0.036, -0.085]). Responses to real words preceded also by 
# real words shorten significantly.

# Task 4
# Set the random seed to #100
set.seed(100)

# A for loop that runs 100 times
for(i in 1:100) {
  # Random sampling of n (= 21) from a normal distribution with the 
  # mean/SD of the paired differences in each loop.
  diff.sample = rnorm(n = diff.n, mean = diff.mean, sd = diff.sd)
  # Get the mean/SD/SE of each random sample for calculating 95% CI
  diff.sample.mean = mean(diff.sample)
  diff.sample.sd = sd(diff.sample)
  # n does not change
  diff.sample.se = diff.sample.sd / diff.n
  # Because n is the same, df is also the same, and the critical value is
  # also the same.
  # 95% CI upper bound
  diff.sample.upper = diff.sample.mean + diff.crit * diff.sample.se
  # 95% CI lower bound
  diff.sample.lower = diff.sample.mean - diff.crit * diff.sample.se
  # Print out the result of checking whether the 95% CI covers zero
  # If the lower bound is higher than 0 or the upper bound is lower than zero,
  # the entire CI does not cover zero.
  print(diff.sample.lower > 0 | diff.sample.upper < 0)
}

# None of the 100 95% CIs calculated in the for loop covers zero (it's always
# TRUE), which means that with the mean and the SD of the paired differences,
# it is very unlikely to randomly sample 21 values with a 95% CI covers zero.
# Since the original sample 95% CI does not cover zero, and among 100 replications
# it is one of the 95 95% CIs that does cover the true population mean, it is
# safe to say that the true population mean is not zero. Put differently, the
# mean difference is significantly different from zero.

# Bonus
# The ANOVA equivalent to the paired t-test in the above tasks is a one-way
# repeated-measure ANOVA.
diff.aov = aov(RT ~ PrevType + Error(Subject/PrevType), data = lexdec.subj)
summary(diff.aov)
# To test if RT varies significantly by the wordness of the previous test items,
# we ran a one-way repeated-measures ANOVA in which RT was the dependent variable
# and PrevType was the only within-subject factor with two levels (word vs.
# nonword). The analysis suggested a significant effect of PrevType (F(1,20) =
# 26, two-tailed p < .001).

# Note that the two-tailed p here is identical to the two-tailed p in the paired
# t-test, and the t square is identical to F.

# A boxplot is the best choice to visualize data distribution across the two
# levels of PrevType. This is a demo using ggplot2.
library(ggplot2)
ggplot(data = lexdec.subj, mapping = aes(x = PrevType, y = RT)) +
  geom_boxplot() +
  scale_y_continuous(limits = c(6, 7)) +
  labs(title = "Reaction time variation in the English lexical decision task",
       subtitle = "Only responses to real words",
       x = "The 'wordness' of the previous test item", 
       y = "Reaction Time (log-milliseconds)") +
  theme_bw()