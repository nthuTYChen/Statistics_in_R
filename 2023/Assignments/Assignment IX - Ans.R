library(languageR)
head(english)

# Task I - The thing to be explained is probably how to limit the range of
# LengthInLetters to 3-5 with two joint conditions LengthInLetters > 2 and 
# LengthInLetters < 6. LengthInLetters >=3 and LengthInLetters <= 5 would
# do the job, too.
english.sub = subset(english, LengthInLetters > 2 & LengthInLetters < 6 
                     & AgeSubject == "young")

# Task II - as.factor() converts LengthInLetters as a factor Len.fac even
# though LengthInLetters originally includes only numeric values.
english.sub$Len.fac = as.factor(english.sub$LengthInLetters)
# Three levels in the factor: "3", "4", "5". Note they are enclosed in
# double quotation marks, which also shows that they are no longer "numbers".
levels(english.sub$Len.fac)

# Task III - Generate a density plot, and although the distribution is close
# to a normal one, its upper tail is still longer than the lower tail, so 
# there's still room for improvement.
plot(density(english.sub$RTnaming))


# Task IV - Following the above conclusion, we need to exclude outliers by
# setting the upper/lower bound to mean+-2SD. 2SD is the bottom line since
# values that it is not usual to observe anything that go beyond +-2SD by
# pure chance. 2.5SD also works, if you want to exclude only super-extreme
# observations.
english.clean = subset(english.sub, 
                       RTnaming >= mean(RTnaming) - sd(RTnaming) * 2 &
                         RTnaming <= mean(RTnaming) + sd(RTnaming) * 2)

# Task V - Manually calculating the key statistics in an one-way
# independent-measure ANOVA.

# Take out naming RTs from words as long as 3, 4, and 5 letters respectively
# since they would be used repeatedly in my calculation below.
english.len3 = subset(english.clean, Len.fac == 3)
english.len4 = subset(english.clean, Len.fac == 4)
english.len5 = subset(english.clean, Len.fac == 5)

# Starting with SS-between: The sum of all squared differences between each
# group mean and the grand mean multiplied by the number of observations in each
# group, which is divided by the number of groups minus 1
grand.mean = mean(english.clean$RTnaming)
len.3.mean = mean(english.len3$RTnaming)
len.4.mean = mean(english.len4$RTnaming)
len.5.mean = mean(english.len5$RTnaming)

ss.between = (nrow(english.len3) * (len.3.mean - grand.mean) ^ 2 + 
                nrow(english.len4) * (len.4.mean - grand.mean) ^ 2 +
                nrow(english.len5) * (len.5.mean - grand.mean) ^ 2) / (3 - 1)

# SS-within: Sum the squared differences between every observation in a group
# and the group mean for each of the three groups, and then sum all these
# squared differences across all three groups. Divide this sum by the number
# of total observations minus the number of groups.
ss.within = (sum((english.len3$RTnaming - len.3.mean) ^ 2) + 
               sum((english.len4$RTnaming - len.4.mean) ^ 2) +
               sum((english.len5$RTnaming - len.5.mean) ^ 2)) / 
                                              (nrow(english.clean) - 3)

# F-value: the interesting between-group variance divided by the boring 
# within group variance.
naming.f = ss.between / ss.within

# Task VI - Validate your calculation with aov(). You should find the same
# ss.between, ss.within, and F-value.
naming.aov = aov(formula = RTnaming ~ Len.fac, data = english.clean)
summary(naming.aov)
# To test the main effect of word length in letters on naming latency, we
# ran a one-way independent-measure ANOVA, in which naming latency is the
# dependent variable and word length is the only independent variable, which
# suggests a significant main effect of word length (F(2, 2021) = 119.4,
# p < .001)

# Task VII
TukeyHSD(naming.aov)
# Naming latency is significantly slower for words with four letters
# than for words with three letters. It is also significantly slowers for
# words with five letters than for words with three letters. Finally, the
# latency is significantly slower for words with five letters than for words
# with four letters.

# Task VIII - Two-way interaction between Len.fac and CV
naming.twoway = aov(formula = RTnaming ~ Len.fac * CV, data = english.clean)
summary(naming.twoway)
# To test the main effect of word length in letters and initial segment 
# (C vs. V) on naming latency, we ran a two-way independent-measure ANOVA,
# in which naming latency was the dependent variable and word length and
# initial segment were the independent variables. The interaction between
# word length and initial segment was also included. The results suggest
# a significant main effect of word length (F(2, 2018) = 119.29, p < .001)
# but the effect of initial segment was not significant (F(1, 2018) = 0.002,
# p = .967). There was neither a significant interaction between the two
# independent variables (F(2, 2018) = 0.38, p = .684). Therefore, the main
# effect of word length is constant for words with an initial consonant/vowel.

# Note that your numbers might be different in Task V-VIII depending on
# whether you have excluded outliers and your choice of the outlier boundaries.

# Task IX
library(ggplot2)

# Prepare for more informative labels for the CV variable in the boxplot 
english.clean$CV.label = ifelse(english.clean$CV == "C", "Initial Segment: C",
                                "Initial Segment: V")

ggplot(mapping = aes(x = Len.fac, y = RTnaming), data = english.clean) +
  # Use the more informative CV labels in facet_grid()
  geom_boxplot() + facet_grid(~ CV.label) + scale_y_continuous(limits = c(6, 6.3)) +
  # Try to incorporate all ANOVA statistics in the caption
  labs(title = "Naming latency by Length in Letters and Initial Segment",
       x = "Number of Letters", y = "log-transformed Naming Latency",
       caption = "Letter Number: F(2, 2018) = 119.29, two-tailed p < .001\n
       CV: F(1, 2018) = 0, two-tailed p = .967\n
       Letter Number x CV: F(2, 2018) = 0.38, two-tailed p = .684") +
  theme_bw()

# Task X
# We have three post-hoc pairwise comparison, so own Bonforroni-corrected
# significance level (alpha) is .05 / 3 = .0167
correct.p = .05 / 3 # .0167
# Remember to set "var" to TRUE for a two-sample unpaired t-test assuming
# an equal variance
t.test(english.len3$RTnaming, english.len4$RTnaming, var = TRUE) 
# p = .0159; still a significant difference between 3-letter and 4-letter words
t.test(english.len4$RTnaming, english.len5$RTnaming, var = TRUE) 
# p < .001; still a significant difference between 4-letter and 5-letter words
t.test(english.len3$RTnaming, english.len5$RTnaming, var = TRUE) 
# p < .001; still a significant difference between 3-letter and 5-letter words