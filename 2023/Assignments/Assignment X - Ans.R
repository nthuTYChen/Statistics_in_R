library(languageR)
head(lexdec)

# Part I
# Task I
# No need to explain the following four lines, right...?
rt.mean = mean(lexdec$RT)
rt.sd = sd(lexdec$RT)
freq.mean = mean(lexdec$Frequency)
freq.sd = sd(lexdec$Frequency)

# As explained in the instructions, the outliers must be excluded from the two
# variables at the same time. So, if you have conditions that limit the selected
# values within a specific range in each of the two variables, just joint all of 
# them together with the AND operator &.
lexdec.sub = subset(lexdec, RT >= rt.mean - 2 * rt.sd & RT <= rt.mean + 2 * rt.sd &
                      Frequency >= freq.mean - 2 * freq.sd & Frequency <= freq.mean + 2 * freq.sd)

# Task IIa.
plot(density(lexdec.sub$RT), main = "log-RT distribution in the lexdec dataset")
# It's not a perfect bell curve, but it is more or less symmetrical.
plot(density(lexdec.sub$Frequency), main = "log-Frequency distribution in the lexdec dataset")
# This one looks like a mountain rather than a bell with "fatter" slopes on both sides,
# but at least it is not severely skewed.

# Task IIb.
plot(RT ~ Frequency, data = lexdec.sub, main = "The RT-Frequency correlation in the lexdec dataset",
     xlab = "log Word Frequency", ylab = "log RT", ylim = c(5.8, 7))
# The range of log RT does not seem to vary significantly by log Word Frequency,
# except on the very right side of the x-axis where there's a smaller range,
# so the variation in log RT is constant and homoscedasticity is not severely 
# violated.

# Task III
lexdec.lm = lm(RT ~ Frequency, data = lexdec.sub)
summary(lexdec.lm)
# To test if RT varies significantly with word frequency, we regressed log RT 
# against log word frequency in simple linear regression and found a significant
# main effect of word frequency (B = -0.035, SE = 0.004, t(1498) = -7.937, 
# p < .001). The effect size is small (r2 = .04).

# Task IV
# In other words, when log word frequency increases by 1, log RT decreases 
# significantly by 0.035. This supports our hypothesis that high-frequency
# words are retrieved faster for judgments with a shorter RT.

# Task V
# Just follow the linear equation y = b0 + b1x to calculate the predicted values
# and then use exp() to convert the predicted log RT into a raw RT.
exp(6.523106 + -0.035142 * 4.3) # 585.2244
exp(6.523106 + -0.035142 * 5) # 571.0039
exp(6.523106 + -0.035142 * 7.2) # 528.5126

# Part II
# Task VI
lexdec.lm2 = lm(RT ~ Frequency + FamilySize, data = lexdec.sub)
summary(lexdec.lm2)
# To test if RT varies significantly with word frequency and word family size, 
# we regressed log RT against two independent variables log word frequency and 
# log family size in multiple linear regression. The main effect of log word
# frequency was found significant (B = -0.029, SE = 0.006, t(1497) = -4.979, 
# p < .001) whereas the main effect of log family size was not (B = -0.013, 
# SE = 0.008, t(1497) = -1.580, p = .114). The effect size is small (r2 = .042).

# Compared to lexdec.lm, lexdec.lm incorporating FamilySize as an additional
# predictor only explains 0.2% more variance (i.e., .042 - .04).

# Task VII
lexdec.lm3 = lm(RT ~ Frequency * FamilySize, data = lexdec.sub)
summary(lexdec.lm3)

# To test if RT varies significantly with word frequency and word family size, 
# we regressed log RT against two independent variables log word frequency and 
# log family size as well as their two-way interaction in multiple linear 
# regression. The main effect of log word frequency was found significant 
# (B = -0.027, SE = 0.006, t(1496) = -4.264, p < .001) whereas the main effect 
# of log family size was not (B = 0.022, SE = 0.038, t(1496) = -0.582, 
# p = .561). The two-way interaction was not significant either (B = -0.006, 
# SE = 0.007, t(1496) = -0.956, p = .339). The effect size is small (r2 = .043).

# Compared to lexdec.lm2, lexdec.lm3 with the two-way interaction only explains
# 0.1% more variance.

# According to the interaction coefficient, the negative effect of Frequency on RT
# decreases only by 0.006 (since the cofficient is negative) when FamilySize 
# increases by 1. And with the large p-value, we can conclude that the significant
# effect of Frequency is not dependent on FamilySize.

# Task VIII
# Just follow the linear equation y = b0 + b1x1 + b2x2 + b3x1x2. The rest is the
# same as Task V
exp(6.492405 + - 0.026865 * 3.5 + 0.021836 * 1.3 + -0.006208 * 3.5 * 1.3) 
# 600.9537
exp(6.492405 + - 0.026865 * 2.8 + 0.021836 * 2.4 + -0.006208 * 2.8 * 2.4) 
# 618.8554

# Bonus
# Task IX
library(ggplot2)
ggplot(mapping = aes(x = Frequency, y = RT), data = lexdec.sub) +
  geom_point(alpha = 0.5, color = "darkgrey", size = 3) +
  # This is how you add the trend line generated with simple linear regression
  # fitted to the dataset lexdec.sub in a ggplot2.
  geom_smooth(method = "lm", lwd = 1.5, color = "red") +
  labs(title = "RT variation by Word Frequency in Lexical Decision", 
       x = "log Word Frequency", y = "log Reaction Time", 
       # To be even more informative, include the statistics of simple linear
       # regression.
       caption = "Frequency: B = -0.035, SE = 0.004, t(1498) = -7.937, p < .001") +
  theme_bw()

# Task X
# Create a data frame using data.frame(), which contains the two same variables
# incorporated in our multiple linear regression separated by a comma. For each
# variable, use seq() to create a number sequence in which every two values are
# separated by a specific interval. In our case, the range of the two variables
# is 1 and it is divided into 11 values with an interval of 0.1.
new.values = data.frame(Frequency = seq(from = 2, to = 3, by = 0.1),
                        FamilySize = seq(from = 0.5, to = 1.5, by = 0.1))
# Put the data frame with the values of the two predictors and the simple linear
# regression model into predict(), which returns 11 predicted RTs calculated 
# based on each pair of Frequency and FamilySize values.
predict(object = lexdec.lm, newdata = new.values)
#       1        2        3        4        5        6        7        8        9       10       11 
#6.452822 6.449308 6.445794 6.442280 6.438765 6.435251 6.431737 6.428223 6.424709 6.421195 6.417680