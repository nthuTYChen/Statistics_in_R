# Preparation works

library(languageR)
head(durationsOnt)

# Part I
# Task I
# Get the median of YearOfBirth
yob.m = median(durationsOnt$YearOfBirth)
# Get the subset whose YearOfBirth has a value above or equal to the median
durationsOnt.yg = subset(durationsOnt, YearOfBirth >= yob.m)

# Task II
# Calculate the mean/SD of DurationOfPrefix of the subset
dur.yg.mean = mean(durationsOnt.yg$DurationOfPrefix)
dur.yg.sd = sd(durationsOnt.yg$DurationOfPrefix)

# Use subset() to include the observations in durationsOnt.yg whose DurationOfPrefix
# is within mean+-2.5SD; that is, the observations whose DurationOfPrefix is 
# beyond the range are excluded as outliers.
durationsOnt.yg.norm = subset(durationsOnt.yg, 
                              DurationOfPrefix < dur.yg.mean + 2.5 * dur.yg.sd &
                              DurationOfPrefix > dur.yg.mean - 2.5 * dur.yg.sd)

# Calculate the difference in the number of observations between the two subsets
# to calculate the proportion of outliers in durationsOnt.yg
(nrow(durationsOnt.yg) - nrow(durationsOnt.yg.norm)) / nrow(durationsOnt.yg)

# Task III
# The "another thing" to be done is to check if DurationOfPrefix is normally
# distributed, so the sample distribution matches the fundamental assumption
# in a parametric analysis. Based on the density plot (or Q-Q Plot if you prefer),
# the shape of the distribution is generally symmetrical (although there's a 
# small dent right in the middle), and the mean and the median are almost the 
# same, so the two vertical lines completely overlap in the density plot. Thus,
# the distribution of DurationOfPrefix seems normal.
plot(density(durationsOnt.yg.norm$DurationOfPrefix), "The Distribution of ont- Prefix Duration")
abline(v = mean(durationsOnt.yg.norm$DurationOfPrefix), col = "blue")
abline(v = median(durationsOnt.yg.norm$DurationOfPrefix), col = "red")

# Save the mean of DurationOfPrefix for the later use
dur.yg.norm.mean = mean(durationsOnt.yg.norm$DurationOfPrefix)
# Just to show how close the mean and the median are.
median(durationsOnt.yg.norm$DurationOfPrefix)

# Task IV
# The assumed mean and SD of the population
pop.mean = 0.174
pop.sd = 0.13
# The number of observations in our sample
obs.n = nrow(durationsOnt.yg.norm)
# The formula of the one-sample z-test: z = -1.8916645...
z = (dur.yg.norm.mean - pop.mean) / (pop.sd / sqrt(obs.n))

# Task V
# Get the lower-tail probability based on the z-value obtained in Task IV.
# One crucial point here is whether you should derive a one-tailed or a two-tailed
# p-value. Our research hypothesis is DIRECTIONAL, because it specifically
# assumes that the prefix duration would be significantly shorter for younger
# speakers. So one-tailed (lower-tail) p-value is enough. The p-value is lower
# than .05, so the finding suggests falsifying the null hypothesis. That is,
# the research hypothesis is probably right.
dur.yg.p = pnorm(q = z)

# Part II
# Task I
# Use ifelse() to return "Young"s and "Old"s based on whether each value in
# YearOfBirth is above/equal to the median (TRUE) or not (FALSE). The returned
# vector of "Young"s and "Old"s is saved to the Generation column of durationsOnt.
durationsOnt$Generation = ifelse(durationsOnt$YearOfBirth >= median(durationsOnt$YearOfBirth),
                                 yes = "Young", no = "Old")

# Task II
# A box plot is the best choice for visualizing the distribution of a "CONTINUOUS"
# variable, because it presents crucial information such as median, 1st/3rd quartiles,
# IQR, and outliers.

# Use the build-in boxplot() function. A proper title and y-axis label are
# necessary (x-axis needs no change since the name of the column is already
# self-explanatory). Expand the range of of the y-axis to optimize data presentation.
boxplot(DurationPrefixVowel ~ Generation, data = durationsOnt, 
        main = "The distribution of Prefix Vowel Duration by Generation",
        ylab = "Duration of Prefix Vowel (s)", ylim = c(0, 0.15))

# Load the ggplot2 package
library(ggplot2)
# Similar settings and logic; if you choose to add individual data points,
# remember to set "outliers = F" in geom_boxplot() to avoid repetition.
ggplot(data = durationsOnt, mapping = aes(x = Generation, y = DurationPrefixVowel)) +
  geom_boxplot() + scale_y_continuous(limits = c(0, 0.15)) +
  labs(title = "The distribution of Prefix Vowel Duration by Generation",
                        y = "Duration of Prefix Vowel (s)") +
  theme_bw()

# Task III
# If you have complete Task II in the correct way, you should only see one outlier
# dot in the box plot in the Old subset, which goes beyond Q3 (the upper edge of
# the box) + IQR * 1.5. Thus, we can use this logic to identify this sole outlier
# in the Old subset.

# Get the Old subset and store it separately.
durationsOnt.old = subset(durationsOnt, Generation == "Old")
# Use the summary() function to get the numbers representing the Q3 boundary.
# You can also do it in the old-school way in Assignment III - Check the number
# of observations, sort the data set based on the values of DurationPrefixVowel
# in increasing order, and get the value that represents the boundary that
# separates Q3 from Q4 in the distribution.
summary(durationsOnt.old$DurationPrefixVowel)
# Get the IQR of the distribution using the IQR function.
dur.old.iqr = IQR(durationsOnt.old$DurationPrefixVowel)
# Set Q3
q3 = 0.07736
# Use subset() to find the only observation in durationsOnt.old whose
# DurationPrefixVowel goes beyond Q3 + 1.5 * IQR (0.1196349), and the output
# indeed only has one observation, whose DurationPrefixVowel is 0.123218.
subset(durationsOnt.old, DurationPrefixVowel > q3 + 1.5 * dur.old.iqr)