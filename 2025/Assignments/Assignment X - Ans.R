library(languageR)
head(lexdec)

# Part I
# Task 1
# Extract the subset that includes only correct judgments
lexdec.corr = subset(lexdec, Correct == "correct")

# Remove outlier observations based on RT and Frequency, which are the two crucial continuous
# variables in our hypothesis testing process.

# Convert both RT and Frequency into z-scores, just for the convenience of the
# screening process.
lexdec.corr$RT.z = scale(lexdec.corr$RT)
lexdec.corr$Freq.z = scale(lexdec.corr$Frequency)

# I adopted two different thresholds here, since I'm not sure whether you would
# use SD+-2 or SD+-2.5 as yours.
lexdec.sub.2 = subset(lexdec.corr, RT.z >= -2 & RT.z <= 2 & 
                        Freq.z >= -2 & Freq.z <= 2)

lexdec.sub.2.5 = subset(lexdec.corr, RT.z >= -2.5 & RT.z <= 2.5 & 
                        Freq.z >= -2.5 & Freq.z <= 2.5)
# Why removing all outlier observations at the same time? If you start by 
# removing observations based on outlier values in a variable, you also change 
# the distribution of another variable, and the outlier values in another
# variable would be different, too. Let me give you an example. If you have
# four observations with two sets of pair variables (1, 3, 5, 11) and 
# (2, 3, 7, 8). In the first variable, the four value 11 may be an outlier,
# so you remove the fourth observation. Now, the two variables are (1, 3, 5)
# and (2, 3, 7). Then you check the second variable, and you might also find
# that 7 is an outlier, so you also decide to remove the third observation.
# But 7 might not be an outlier in its ORIGINAL distribution. So, without
# considering all outlier values at the same time, you would wrongly exclude
# too many data points.

# Task 2
# Q-Q Plot for RT and Freq would be just enough. Since I have two 'clean'
# subsets, let me demonstrate the distributional normality of the two 
# continuous variables in the same plot.
par(mfrow = c(2, 2)) # A 2x2 layout
qqnorm(lexdec.sub.2$RT, main = "log Reaction Time (within SD+-2)")
qqline(lexdec.sub.2$RT, col = "red")
qqnorm(lexdec.sub.2$Frequency, main = "log Lemma Frequency (within SD+-2)")
qqline(lexdec.sub.2$Frequency, col = "red")
qqnorm(lexdec.sub.2.5$RT, main = "log Reaction Time (within SD+-2.5)")
qqline(lexdec.sub.2.5$RT, col = "red")
qqnorm(lexdec.sub.2.5$Frequency, main = "log Lemma Frequency (within SD+-2.5)")
qqline(lexdec.sub.2.5$Frequency, col = "red")
par(mfrow = c(1, 1)) # Reset the layout

# Task 3
# Sum-code Complex first
lexdec.sub.2$Complex.sum = as.factor(lexdec.sub.2$Complex)
contrasts(lexdec.sub.2$Complex.sum) = contr.sum(2)

lexdec.sub.2.5$Complex.sum = as.factor(lexdec.sub.2.5$Complex)
contrasts(lexdec.sub.2.5$Complex) = contr.sum(2)

# 1: Complex = complex, -1: Complex = simplex
contrasts(lexdec.sub.2.5$Complex) 

# Model fitting: RT x Complex with a two-way interaction
lexdec.lm.2 = lm(RT ~ Frequency * Complex.sum, data = lexdec.sub.2)
summary(lexdec.lm.2)

#                        Estimate Std. Error t value Pr(>|t|)    
#(Intercept)             6.443734   0.038204 168.665  < 2e-16 ***
#Frequency              -0.016956   0.009216  -1.840  0.06597 .  
#Complex.sum1           -0.105164   0.038204  -2.753  0.00598 ** 
#Frequency:Complex.sum1  0.022937   0.009216   2.489  0.01292 * 

lexdec.lm.2.5 = lm(RT ~ Frequency * Complex.sum, data = lexdec.sub.2.5)
summary(lexdec.lm.2.5)

#                        Estimate Std. Error t value Pr(>|t|)    
#(Intercept)             6.476551   0.029746 217.730  < 2e-16 ***
#Frequency              -0.021637   0.007419  -2.917  0.00359 ** 
#Complex.sum1           -0.083342   0.029746  -2.802  0.00514 ** 
#Frequency:Complex.sum1  0.018348   0.007419   2.473  0.01349 *

# Before explaining the beta coefficients, it is important to note a 
# substaintial difference in the effect of Frequency between my two models
# above. SD+-2.5 might be too conservative so that many outliers are kept in
# the dataset to inflat the effect of Frequency. That's an increase
# in the chance of Type I Error (a false positive).

# The beta coeffiencient of the intercept suggests that correct responses are
# on average as long as 6.44/6.39 log-ms when the frequency is zero, regardless
# of whether the target is morphologically complex or not (Complex is sum-coded
# so the intercepts represent a predicted RT when Complex is zero - that is,
# when Complex is neither simplex = 1 or complex = -1). The coefficients of
# Complex mean that when log lemma frequency is hold constant, log RT on average
# differs by |-0.105/-0.083| x 2 = 0.21/0.166 log ms between morphologically
# simple and complex words.

# Task 4
# ggplot2 makes the task easier in this case.
library(ggplot2)
# First of all, you need a scatter plot to show how RT varies by Frequency
# with different morphological structures.

# The SD+-2 version.
# You can map Complex to a visual parameter or divide the plot into separate
# columns. Here I demonstrate the first method.
ggplot(data = lexdec.sub.2, mapping = aes(x = Frequency, y = RT, 
                                          color = Complex)) +
  geom_point(size = 2.5, alpha = .3) +
  # If you want to have the regression line, geom_smooth() is what you need.
  geom_smooth(method = "lm", se = FALSE) +
  # Expand the y-axis scale a bit to increase the readibility.
  scale_y_continuous(limits = c(5.7, 7)) +
  labs(title = "English lexical decision time by Lemma Frequency and Morphology",
       subtitle = "Correct Responses Only",
       x = "log Lemma Frequency", y = "log Reaction Time", color = "Morphology") +
  theme_bw()

# The SD+-2.5 version.
ggplot(data = lexdec.sub.2.5, mapping = aes(x = Frequency, y = RT, 
                                          color = Complex)) +
  geom_point(size = 2.5, alpha = .3) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(limits = c(5.7, 7)) +
  labs(title = "English lexical decision time by Lemma Frequency and Morphology",
       subtitle = "Correct Responses Only",
       x = "log Lemma Frequency", y = "log Reaction Time", color = "Morphology") +
  theme_bw()

# Task 5

# Based only on the SD+-2 version.

# To test the research hypothesis, we regressed log reaction time against 
# lemma frequency and the sum-coded factor morphological complexity. The two-way
# interaction between the two predictors was also included. The results suggest
# a significant main effect of morphological complexity (B = -0.017, SE = 0.038, 
# t = -2.753, p = .005) and a significant two-way interaction between the 
# predictors (B = 0.023, SE = 0.009, t = 2.489, p = .013).

# The model output does not support the hypothesis regarding the frequency effect,
# since overall, lemma frequency does not significantly affect the latency of
# correct responses. However, morphological complexity significantly impacts
# correct response RT, as participants responded to morphologically simple words
# faster than to morphologically complex words, with a mean difference of
# 0.21 log ms (see my explanation in Task 3). The hypothesis that the effect of
# frequency varies by morphological complexity is supported by the significant
# two-way interaction. The frequency effect is weaker for morphologically complex
# words (or, alternatively, stronger for morphologically simple words). That is,
# morphologically complex words are just more difficult to process, regardless of
# their lemma frequency.

# Task 6

# Based only on the SD+-2 version
# Note that simplex = -1 and complex = 1 in the sum-coded factor Complex
6.443734 + -0.016956 * 6.512 + -0.105164 * -1 + 6.512 * -1 * 0.022937 # 6.289115
6.443734 + -0.016956 * 4.783 + -0.105164 * -1 + 4.783 * -1 * 0.022937 # 6.35809
6.443734 + -0.016956 * 5.175 + -0.105164 * -1 + 5.175 * -1 * 0.022937 # 6.342452

# Task 7

# Based only on the SD+-2 version

# Create a data frame as the "new" data with the same variable names incorporated
# in the regression model and "new" values in each variable
lexdec.new = data.frame(Frequency = c(6.512, 4.783, 5.175),
                        Complex.sum = c("simplex", "simplex", "complex"))

# Minor differences exist because of the numbers taken from the model summary
# are rounded.
predict(lexdec.lm.2, lexdec.new)

# Task 8 (Bonus)

# Based only on the SD+-2 version
# Get the first 10 rows
lexdec.sub.10 = lexdec.sub.2[1:10,]

# Round the actual values
lexdec.sub.10$RT.short = round(lexdec.sub.10$RT, 3)

# Convert Complex.sum to 1 and -1 (sum coding) for calculating predicted values
lexdec.sub.10$Complex.num = ifelse(lexdec.sub.10$Complex.sum == "complex", 1, -1)

# Predicted values based on the beta coefficients
lexdec.sub.10$Predict = 
  6.443734 + # Intercept
  -0.016956 * lexdec.sub.10$Frequency + # Frequency effect
  -0.105164 * lexdec.sub.10$Complex.num + # Complex effect
  lexdec.sub.10$Frequency * lexdec.sub.10$Complex.num * 0.022937 # Frequency * Complex
# Rounding
lexdec.sub.10$Predict.short = round(lexdec.sub.10$Predict, 3)

# First 10 residual values
# The output of lexdec.lm.2$residuals[1:10] is a table, so let me convert it
# to a vector so I can put the values to the variable in my data frame
lexdec.sub.10$Residual = as.vector(lexdec.lm.2$residuals[1:10])
# Rounding
lexdec.sub.10$Residual.short = round(lexdec.sub.10$Residual, 3)

# If the difference between actual and predicted values are residuals, the
# calculation below should give us numbers close to zeros, which is true.
lexdec.sub.10$RT.short - lexdec.sub.10$Predict.short - lexdec.sub.10$Residual.short

# A simpler way

# Get the predict values directly from the model using predict()
lexdec.sub.10$Predict.model = 
  predict(lexdec.lm.2, lexdec.sub.10[c("Frequency", "Complex.sum")])

# Rounding
lexdec.sub.10$Predict.model.short = round(lexdec.sub.10$Predict.model, 3)

# Again, all differences are very close to zeros.
lexdec.sub.10$RT.short - lexdec.sub.10$Predict.model.short - 
  lexdec.sub.10$Residual.short