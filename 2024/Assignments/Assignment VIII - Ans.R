# Preparation works
source("https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/main/courseUtil.R")
chen.sample = loadCourseCSV(2024, "5_ANOVA", "Chen2020Sample.csv") 

# Part I
# Task I
# You need to convert raw reaction time into natural logs before excluding
# outliers. I also convert natural logs into z-scores, so I could exclude
# outliers faster.
chen.sample$rt.log.z = scale(log(chen.sample$answerKeysFormal.rt))

# Exclude outliers based on the required thresholds
chen.sample.clean = subset(chen.sample, answerKeysFormal.rt > 0.1 &
                             rt.log.z >= -2 & rt.log.z <= 2)

# Calculate the proportion of excluded judgments
(1 - nrow(chen.sample.clean) / nrow(chen.sample)) * 100 # 4.8%

# Task II
# Calculate by-subject mean acceptance rates within each crossing of the
# two levels in Group and Initial Tone
chen.sample.acc = aggregate(Accept ~ participant + Group + InitialTone,
                            FUN = mean, data = chen.sample.clean)

# Task III
library(ggplot2)

# The crucial part in the mapping is to establish the link between InitialTone
# and the colors used to fill the boxes, so the data is divided within each
# group by different levels in InitialTone.
ggplot(data = chen.sample.acc, 
       mapping = aes(x = Group, y = Accept, fill = InitialTone)) +
  geom_boxplot() +
  # Add the horizontal line by setting yintercept. It is also made transparent
  # to avoid blocking any key information in the boxes.
  geom_hline(yintercept = 0.5, linewidth = 1.5,  alpha = 0.3) +
  # There is a clear upper and lower bounds since the acceptance rate must
  # vary between 0 and 1.
  scale_y_continuous(limits = c(0, 1)) +
  labs(title = "By-subject acceptance rates in Chen's (2020) sample data set",
       y = "Mean Acceptance Rates", 
       caption = "The grey line represents chance performance (0.5)") +
  theme_bw()

# Task IV
# The most appropriate test would be a one-sample t-test because you are asked
# to compare four sample means to an assumed population mean of 0.5.

# Group = *NonFinalR, Initial Tone = H
t.test(chen.sample.acc[chen.sample.acc$Group == "NonFinalR" & 
                         chen.sample.acc$InitialTone == "H",]$Accept,
       mu = 0.5)
# "To test if the *NonFinalR group has a mean acceptance rate significantly
# different from 0.5 when the initial tone of test items is H, we ran a 
# one-sample t-test, which suggested a significant difference between the 
# sample mean and the population mean (t(23) = 3.8, p < .001).

# The mean acceptance rate of test items with an initial H is significantly 
# above the 0.5 chance.

# Group = *NonFinalR, Initial Tone = R
t.test(chen.sample.acc[chen.sample.acc$Group == "NonFinalR" & 
                         chen.sample.acc$InitialTone == "R",]$Accept,
       mu = 0.5)
# "To test if the *NonFinalR group has a mean acceptance rate significantly
# different from 0.5 when the initial tone of test items is R, we ran a 
# one-sample t-test, which suggested a significant difference between the 
# sample mean and the population mean (t(23) = -2.14, p < .05).

# The mean acceptance rate of test items with an initial R is significantly 
# below the 0.5 chance.

# The *NonFinalR group seems to have learned the hidden regularities.

# Group = *NonFinalH, Initial Tone = H
t.test(chen.sample.acc[chen.sample.acc$Group == "NonFinalH" & 
                         chen.sample.acc$InitialTone == "H",]$Accept,
       mu = 0.5)
# "To test if the *NonFinalH group has a mean acceptance rate significantly
# different from 0.5 when the initial tone of test items is H, we ran a 
# one-sample t-test, which suggested a non-significant difference between the 
# sample mean and the population mean (t(23) = -1.01, p = .323).

# The mean acceptance rate of test items with an initial H is NOT significantly 
# below the 0.5 chance.

# Group = *NonFinalH, Initial Tone = R
t.test(chen.sample.acc[chen.sample.acc$Group == "NonFinalH" & 
                         chen.sample.acc$InitialTone == "R",]$Accept,
       mu = 0.5)
# "To test if the *NonFinalH group has a mean acceptance rate significantly
# different from 0.5 when the initial tone of test items is R, we ran a 
# one-sample t-test, which suggested a non-significant difference between the 
# sample mean and the population mean (t(23) = 1.03, p = .313).

# The mean acceptance rate of test items with an initial R is NOT significantly 
# above the 0.5 chance.

# The *NonFinalH group seems to have failed to learn the hidden regularities.

# Part II
library(languageR)
head(lexdec)

# Task I
# One-way repeated-measures ANOVA is the most appropriate statistical tests
# because every participant saw the same set of test items; whether a test item
# was preceded by a real word or not is a WITHIN-SUBJECT design factor.

# Task II
# We "don't care about incorrect responses", so extract the proper subset for
# the analysis.
lexdec.cor = subset(lexdec, Correct == "correct")
# Run one-way repeated-measures ANOVA
lexdec.cor.aov = aov(RT ~ PrevType + Error(Subject / PrevType), data =lexdec.cor)
summary(lexdec.cor.aov)

# To test if the lexicality (i.e., real word or not) of the previous test item
# has any significant impact on the RT of the current test item, we ran an
# one-way repeated-measures ANOVA with log-RT as the dependent variable and
# PrevType as the only within-subject independent variable. The results suggested
# a significant effect of PrevType (F(1, 20) = 27.54, p < .001).

# Task III
# The corresponding t-test is a paired t-test, but the two samples divided by
# PrevType do not have the same sample size, so it is not possible to run a
# paired t-test. We can only run an unpaired t-test ASSUMING AN EQUAL VARIANCE
# (which is also the fundamental assumption of ANOVA).
t.test(RT ~ PrevType, var.equal = T, data = lexdec.cor)

# According to the t-test, when test items (which are correctly judged real words)
# were preceded by a real word, reaction times were significantly faster, so
# the answer to the research question is Yes.