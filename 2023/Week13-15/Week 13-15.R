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

dur.lm.int = lm(DurationPrefixNasal ~ 1, data = durationsOnt)
summary(dur.lm.int)

mean(durationsOnt$DurationPrefixNasal)

model.res = dur.lm$residuals
model.sse = sum(model.res ^ 2)
model.sse

null.res = dur.lm.int$residuals
null.sse = sum(null.res ^ 2)
null.sse

model.sse / null.sse
1 - model.sse / null.sse

nas.dur = durationsOnt$DurationPrefixNasal
durationsOnt$DurPrefixN.z = (nas.dur - mean(nas.dur)) / sd(nas.dur)
freq = durationsOnt$Frequency
durationsOnt$Freq.z = (freq - mean(freq)) / sd(freq)

dur.lm.z = lm(DurPrefixN.z ~ Freq.z, data = durationsOnt)
summary(dur.lm.z)

xtabs(~ PlosivePresent, data = durationsOnt)

durationsOnt$Pl.Pr.Fac = as.factor(durationsOnt$PlosivePresent)
contrasts(durationsOnt$Pl.Pr.Fac)

dur.lm.plo = lm(DurationPrefixNasal ~ Pl.Pr.Fac, data = durationsOnt)
summary(dur.lm.plo)

t.test(DurationPrefixNasal ~ PlosivePresent, var = TRUE, 
       data = durationsOnt)