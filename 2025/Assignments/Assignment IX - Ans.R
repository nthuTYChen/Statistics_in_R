# Part I
# Call it "compareDist" because it is for comparing data distributions.
# It takes a vector of numeric values, so call the within-function local
# variable "vec".
compareDist = function(vec) {
  # Set a 2 x 2 layout
  par(mfrow=c(2, 2))
  # Add-one smoothing
  vec.nozero = vec + 1
  # Convert the subset using natural log transformationhttp://127.0.0.1:23917/graphics/plot_zoom_png?width=1110&height=900
  vec.log = log(vec.nozero)
  # Generate the Q-Q plot for the original data distribution
  qqnorm(vec.nozero, main = "Distribution before log Transformation")
  qqline(vec.nozero, col = "red")
  # Generate the boxplot for the original data distribution
  boxplot(vec.nozero, main = "Distribution before log Transformation")
  # Do the same things for the log-transformed data distribution
  qqnorm(vec.log, main = "Distribution after log Transformation")
  qqline(vec.log, col = "red")
  boxplot(vec.log, main = "Distribution after log Transformation")
  # Don't forget to reset the layout at the end
  par(mfrow=c(1, 1))
}

# a. Test the function with the Raw RT data from Myers.sample
source("https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/refs/heads/main/courseUtil.R")
Myers.clean = loadCourseCSV(2025, "5_ANOVA", "MyersClean.csv")

compareDist(Myers.clean$RT)

# b. Test the function with the Raw RT data from "english" of the "languageR"
# package
library(languageR)
head(english)

compareDist(english$NounFrequency)

# Part II
# Task 1
# Since the research hypotheses require the dependent variable "naming RT"
# to be explained by two independent variables (age and familiarity) and their
# interaction, a two-way independent-measures ANOVA is the most appropriate
# statistical test in addition to regression.

# Task 2
# ifelse() does the job easily for you. Divide the familiarity ratings
# based on their median
english$Fam.cat = ifelse(english$Familiarity > median(english$Familiarity),
                         "high", "low")

# Task 3
# aggregate() is your old friend that can complete this task in milliseconds.
aggregate(RTnaming ~ AgeSubject + Fam.cat, FUN = mean, data = english)

# Task 4
# Two-way independent-measures ANOVA with interaction
english.aov = aov(RTnaming ~ AgeSubject * Fam.cat, data = english)
summary(english.aov)
# To test if naming reaction times are influenced by the age of the subjects
# and the familiarity of the target words, we ran a two-way ANOVA including
# the two-way interaction between the independent variables AgeSubject and
# Familiarity. The results suggest a significant effect of AgeSubject 
# (F(1, 4564) = 133.56, p < .001) and Familiarity (F(1, 4564) = 0.85, p < .001).
# The interaction is also significant (F(1, 4564) = 0.05, p < .001).

# Task 5
# Eta-square
# AgeSubject
133.56 / 11.02 # 12.11978 > 0.14; HUGE effect
# Fam.cat
0.85 / 11.02 # 0.01 < 0.07713249 < 0.14; medium effect
# AgeSubject x Fam.cat
0.05 / 11.02 # 0.004537... < 0.01; extremely small effect

# Task 6
# ggplot2 is more convenient
library(ggplot2)
# Since AgeSubject has the largest effect size, it can be see more easily in
# the figure. More subtle differences caused by familiarity should be placed
# together so the visual comparison can be done at ease. Accordingly, I map
# familiarity to the x-axis and divide my plot based on subject age.
ggplot(data = english, mapping = aes(x = Fam.cat, y = RTnaming)) +
  # Boxplot is the best for showing the distribution of continuous values.
  geom_boxplot() + facet_grid(~ AgeSubject) +
  labs(x = "Target Word Familiarity", y = "Naming Reaction Time (log-ms)",
       caption = "Whiskers = IQR * 1.5 from 1st/3rd quartile or minimal/maximal values") +
  theme_bw()
# a. According the boxplot, older speakers did name target words more slowly 
# then younger speakers, and the difference is significant according to ANOVA.
# So, the first hypothesis is supported.

# b. In the same boxplot, medians are lower when target words have a higher
# familiarity for both old and young speakers. Since the familiarity effect is
# also significant in ANOVA, the second hypothesis is supported.

# c. For young speakers, the two RT medians for high/low-familiarity words
# are closer than for old speakers, so the effect of familiarity is dependent
# on subject age, and this interaction is significant. However, this interaction
# is opposite to our hypothesis that the difference would be smaller for OLDER
# speakers. So, the third hypothesis is not supported by the results. Also,
# the effect size is extremely small, so maybe the effect of familiarity is
# just similar for both groups of speakers.