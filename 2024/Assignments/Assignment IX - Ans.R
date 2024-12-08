# Preparation work

library(languageR)
head(verbs)

# Part I

# Task 1
# My own speculation is that if the receiver is INANIMATE, then it is not
# possible to "give" too many things to that receiver, and the theme could be
# in general shorter. For example, you can say something like "I give a toy
# and a book to that person", where the theme has five words. In contrast,
# in sentences like "I give some money to that organization," the theme is
# more specific and thus shorter. Again, this is just some random guess. You
# should check the reference study listed in the official document of "verbs"
# to know more about the theoretical basis.

# Task 2
# Extract the 'give' subset 
verbs.give = subset(verbs, Verb == "give")

# Check the distributional normality of LengthOfTheme using a Q-Q Plot
qqnorm(verbs.give$LengthOfTheme)
qqline(verbs.give$LengthOfTheme, col = "red", lwd = 2)

# According to the Q-Q Plot, there are some lengths that are lower than
# expected in the upper tail, but the individual data points do not seem
# too far-off the line representing the perfect overlap between the sample
# distribution and its corresponding normal distribution. Thus, it may be
# OK to not exclude outliers.

# Task 3
# One-way independent-measures ANOVA: Dependent variable = LengthOfTheme
# Independent variable = AnimacyOfRec
verbs.give.aov = aov(LengthOfTheme ~ AnimacyOfRec, data = verbs.give)
summary(verbs.give.aov)

# Validate SS, Mean Sq, and F-values
# Divide the data set into two samples by AnimacyOfRec
verbs.give.ani = subset(verbs.give, AnimacyOfRec == "animate")
verbs.give.inani = subset(verbs.give, AnimacyOfRec == "inanimate")
# Global theme length mean
length.m = mean(verbs.give$LengthOfTheme)
# Theme length mean when receivers are animate/inanimate
animate.m = mean(verbs.give.ani$LengthOfTheme)
inanimate.m = mean(verbs.give.inani$LengthOfTheme)
# SS-between; 20.10522..., which is identical to the number in the ANOVA
ss.between = nrow(verbs.give.ani) * (animate.m - length.m) ^ 2 +
  nrow(verbs.give.inani) * (inanimate.m - length.m) ^ 2

# MS-between; there are only two levels in AnimacyOfRec, so df-between is
# 2 - 1 = 1
verbs.give.ms = ss.between / 1

# SS-within; 285.38259..., which is identical to the number in the ANOVA
ss.within = sum((verbs.give.ani$LengthOfTheme - animate.m) ^ 2) +
  sum((verbs.give.inani$LengthOfTheme - inanimate.m) ^ 2)

# Residual Mean Sq (or MSE); there are 403 data points in the entire subset, and in the
# comparison between "two samples", the degree of freedom for residuals is
# 403 - 2 = 401
verbs.give.mse = ss.within / (nrow(verbs.give) - 2)

# F = MS / MSE = 28.25
verbs.give.f = verbs.give.ms / verbs.give.mse

# Upper-tail p = 1.773e-07
pf(q = verbs.give.f, df1 = 1, df2 = nrow(verbs.give) - 2, lower.tail = F)

# Report the result in the APA format
# To test the effect of the animacy of receivers on the length of themes of a
# dative verb, we ran a one-way independent-measures ANOVA, in which the length
# of themes was the dependent variable and the animacy of receivers was the only
# independent variable. The result suggested a significant effect of the independent
# variable (F(1, 401) = 28.25, p < .001).

# Task 4
# We're running an independent-measures ANOVA, so it's OK to run Tukey's HSD
# post-hoc comparison.
TukeyHSD(verbs.give.aov)

# According to the post-hoc pairwise comparison, inanimate receivers are indeed
# paired with shorter themes than animate receivers, which corroborates with
# my speculation. Of course, it doesn't mean my "theory" is absolutely correct.

# Task 5
# Calculate the eta-square of the significant effect of AnimacyOfRec
verbs.give.eta = ss.between / (ss.between + ss.within) # 0.0658
# The eta-square sits between 0.01 and 0.14, so I've got a medium effect size.

# Task 6: Optional
library(ggplot2)

# Use aggregate() to get a data frame that contains the average of each sample
# under comparison
verbs.give.avg = aggregate(LengthOfTheme ~ AnimacyOfRec, FUN = mean, data = verbs.give)

ggplot(data = verbs.give.avg, mapping = aes(x = AnimacyOfRec, y = LengthOfTheme)) +
  # Set "stat" as "identity", so the means in verbs.give.avg can be set as the
  # bar heights.
  geom_bar(stat = "identity", width = 0.2) + 
  scale_y_continuous(limits = c(0, 2)) +
  labs(title = "The average length of themes by receiver animacy", 
       subtitle = "dative verb = give", x = "Receiver Animacy",
       y = "Mean Length of Themes (Words)") +
  theme_bw()

# Part II
# Check the data set
head(lexdec)

# Task I
# Build the simple linear regression model
wt.sz.lm = lm(formula = meanWeight ~ meanSize, data = lexdec)
summary(wt.sz.lm)

# Report the result following the APA format
# To test if mean weight ratings of noun objects in English can be predicted by
# their mean size ratings, we regressed the former against the later in a linear
# regression model, which suggested a significant and positive effect of mean
# size ratings (B = 1.078, SE = 0.001, t = 1100.7, p < .001). The model has a 
# strong effect size of r2 = .999.

# Task 2
# Create a numeric vector of the four mean size ratings
new.sizes = c(1, 1.3, 2.7, 4)
# Use the intercept and slope numbers in the regression model to calculate the
# predicted mean weight ratings in a multinomial formula
-0.5643014 + 1.0778207 * new.sizes

# Task 3
ggplot(data = lexdec, mapping = aes(x = meanSize, y = meanWeight)) +
  geom_point(alpha = .5) + 
  # If you need to add a regression line, just use geom_smooth() and set the
  # "method" parameter as "lm", which, of course, stands for linear regression.
  geom_smooth(method = "lm", color = "red") +
  labs(title = "The correlation between weight and size of English nouns",
       x = "Mean Size Rating", y = "Mean Weight Rating") + 
  theme_bw()

# Task 4
# Just map "label" to the "Word" column in ggplot() and replace geom_point() 
# with geom_text()
ggplot(data = lexdec, mapping = aes(x = meanSize, y = meanWeight, label = Word)) +
  geom_text(size = 4) + 
  geom_smooth(method = "lm", color = "red") +
  labs(title = "The correlation between weight and size of English nouns",
       x = "Mean Size Rating", y = "Mean Weight Rating") + 
  theme_bw()