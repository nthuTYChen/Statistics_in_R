library(languageR)
head(durationsOnt)

# Check the range of log word frequency in the dataset
range(durationsOnt$Frequency)

# Revisit the correlation between word frequency and prefix nasal duration
# in the dataset, which is not significant (p = .117)
cor.test(durationsOnt$Frequency, durationsOnt$DurationPrefixNasal)

# Run a simple linear regress to fit a model to the data, so we can make
# a prediction on a prefix nasal duration given a specific log word frequency
dur.lm = lm(formula = DurationPrefixNasal ~ Frequency, data = durationsOnt)
# Get the statistics in the model
summary(dur.lm)

# Get the predicted prefix nasal duration when the log word frequency is 4
# y = b0 + b1*x, where b0 is the intercept, b1 is the main effect coefficent
# (or slope), and x is the value of the main effect.
0.055275 + -0.002052 * 4

# Generate a data frame for a series of values in x (Frequency) for the 
# corresponding predict numbers in y (DurationPrefixNasal). The name of x
# has to be identical to the name of the independent variable in the model
# seq() = generate a series of numbers "from" a "to" b with an interval of 0.1
freq.val = data.frame(Frequency = seq(from = 0, to = 2, by = 0.1))
# Specify the model and send the new data to predict() to get the predicted
# values of y
dur.pred = predict(object = dur.lm, newdata = freq.val)

# Change the table of predicted values into a data frame.
dur.pred.df = as.data.frame(dur.pred)

# Merge the predicted values with the values in x for plotting the predicted
# trend line.
freq.pred = data.frame(Frequency = freq.val$Frequency, 
                          Dur.Pred = dur.pred.df$dur.pred)

# Generate a scatter plot showing the linear relation between prefix nasal 
# duration and log word frequency
plot(freq.pred$Frequency, freq.pred$Dur.Pred, 
     main = "Predict Prefix Nasal Duration by log Word Frequency", 
     xlab = "log Word Frequency", ylab = "Predicted Prefix Nasal Duration (s)",
     ylim = c(0, 0.1))

# Create a "null model" in which only intercept is included (i.e., the "1" part)
# In this model, the intercept is the mean of all data points, since mean is
# a number that is "at the center" of all data points.
dur.lm.int = lm(DurationPrefixNasal ~ 1, data = durationsOnt)
summary(dur.lm.int)

# You get the same number of the intercept by calculating the mean
mean(durationsOnt$DurationPrefixNasal)

# In a linear regression model, the unexplained variance is the differences
# between individual data points and their predicted values, which are stored
# in the "residuals" list of the model.
model.res = dur.lm$residuals
# Square the differences and sum them together, you get a sum of squared errors.
model.sse = sum(model.res ^ 2)
model.sse

# We can also get the differences between a mean and all individual data points
# to calculate the sum of squared errors in the null model.
null.res = dur.lm.int$residuals
null.sse = sum(null.res ^ 2)
null.sse

# r2, which is the effect size in a linear model, is calculated by comparing
# the SSE in our model incoporating an independent variable to the SSE in the
# null model to see if our model with the independent variable give rises to
# a smaller SSE, meaning more variance is explained.
model.sse / null.sse # About 97.6%
# r2 is 1 minus the proportion above, so more 2.4% variance in the null model
# is explained by our model incorporating frequency as a predictor. It's a 
# very small effect size.
1 - model.sse / null.sse

# In another attempt, we try to have a standardized beta coefficient in linear
# regression, which means we need to convert both of our dependent and 
# independent variables into z-scores. In this case, 0 on each scale is not
# "nothing" but the mean of the variable.
nas.dur = durationsOnt$DurationPrefixNasal
durationsOnt$DurPrefixN.z = (nas.dur - mean(nas.dur)) / sd(nas.dur)
freq = durationsOnt$Frequency
durationsOnt$Freq.z = (freq - mean(freq)) / sd(freq)

# The t-value and the p-value of the frequency effect is still the same,
# because covariance does not change with z-scores. (Do you still remember that
# in Pearson's correlation coeffient, two correlated continuous variables are
# also converted into z-scores first?)
dur.lm.z = lm(DurPrefixN.z ~ Freq.z, data = durationsOnt)
summary(dur.lm.z)

# Moving on to linear regression with a "fixed predictor", that is, a predictor
# that only has fixed number of levels. In "PlosivePresent", there are only two
# levels "yes" and "no", which means whether [t] in the ont- prefix is produced
# in an ont-word or not. See the handout for my explanation of the hypothesis
# to be tested here.
xtabs(~ PlosivePresent, data = durationsOnt)

# We need to convert PlosivePresent into a real factor first with as.factor()
# so the values in the variable could be treated as levels rather than strings.
durationsOnt$Pl.Pr.Fac = as.factor(durationsOnt$PlosivePresent)
# Check my handout to see how to interpret this contrast table.
contrasts(durationsOnt$Pl.Pr.Fac)

# Linear regression modeling with a fixed predictor. Check my handout for 
# detailed explanations.
dur.lm.plo = lm(DurationPrefixNasal ~ Pl.Pr.Fac, data = durationsOnt)
summary(dur.lm.plo)

# If you run a two-sample unpaired t-test assuming an equal variance, you get
# the same t-value, df, and the p-value. So, a t-test is just a special form
# of linear regression. For other details, see my handout.
t.test(DurationPrefixNasal ~ PlosivePresent, var = TRUE, 
       data = durationsOnt)