# Load a script from my server, so you can simply use loadCourseCSV()
# for the rest of this course to help load some datasets for this course.
# Note that this function is not a base function, so you always have to
# load this script in order to use it.
source("https://lngproc.hss.nthu.edu.tw/statisticsR/courseUtil.R")

# Load wordRT.csv from the Week4-5 folder from my course server,
# and save the data frame as wordRT
wordRT = loadCourseCSV(week = "Week4-5", file = "wordRT.csv")

# wordRT itself is not a proper data frame, because one row represents
# multiple observations from different speakers. Also see Week 4-5 handout
# for detailed explantion.

# Try to "reshape" the data frame into a data frame in which each row represents
# one observation.
# So we need to use some function from the "reshape" package.
library(reshape)

# The data frame wordRT is transformed using melt(); id.var represents the instances
# of our observations, that is every single word. The rest two columns Male and Female
# actually belongs to one variable "Gender", so in melt(), these column names will be
# placed in the same column named "Gender" as defined by variable.name. Finally, the
# original numeric values will also be placed in one single column, and we will call
# this column "RT", as defined in value.name.
wordRT.new = melt(wordRT, id.var = "Word", variable.name = "Gender", value.name = "RT")
# The function melt() from "reshape" actually does not set the name of the two columns
# representing the variable or the value, so we need to change the column names by
# ourselves.
# colnames() gives you a vector including all column names of a data frame, and then
# you can replace the vector with another vector including new column names.
colnames(wordRT.new) = c("Word", "Gender", "RT")

# A better melt() that helps set the column name is from the package "reshape2"
wordRT = loadCourseCSV(week = "Week4-5", file = "wordRT.csv")

library(reshape2)
wordRT.new = melt(wordRT, id.var = "Word", variable.name = "Gender", value.name = "RT")

# Finally, you can check the data frame itself, and you will see one observation of
# a word per row, in which a word is specified with the speaker who produces it and
# the time in milliseconds spent on producing it.

# Then, you can use nrow() to check how many rows/observation there is in the new data frame.
nrow(wordRT.new)

jabberwocky.wd = loadCourseCSV("Week4-5", "jabberwocky_words.txt")
nrow(jabberwocky.wd)

jabberwocky.table = table(jabberwocky.wd$Word)

jabberwocky.df = as.data.frame(jabberwocky.table)

colnames(jabberwocky.df) = c("Word", "Count")

nrow(jabberwocky.df)
sum(jabberwocky.df$Count)

order(jabberwocky.df$Count, decreasing = TRUE)

jabberwocky.df[71,]

count.des.ord = order(jabberwocky.df$Count, decreasing = TRUE)

jabberwocky.ord = jabberwocky.df[count.des.ord, ]

jabberwocky.wordCat = 
  loadCourseCSV("Week4-5", "jabberwocky_words_cat.csv")

pos.na = is.na(jabberwocky.wordCat$POS)
sum(pos.na)

jabberwocky.wordCat[pos.na, ]
jabberwocky.wordCat[!pos.na, ]

jabberwocky.all = 
  merge(jabberwocky.ord, jabberwocky.wordCat, by = "Word")

xtabs(formula = Count ~ Real, data = jabberwocky.all)

xtabs(formula = ~ Real, data = jabberwocky.all)

# 2 x 2 Contingency Table
jabberwocky.xtabs = 
  xtabs(formula = ~ Real + POS, data = jabberwocky.all)

jabberwocky.sum = sum(jabberwocky.xtabs)
jabberwocky.xtabs / 
  
jabberwocky.perc = 
  round(x = jabberwocky.xtabs / jabberwocky.sum * 100, digit = 1)

library(languageR)
head(durationsOnt)

summary(durationsOnt$DurationOfPrefix)

abs(median(durationsOnt$DurationOfPrefix) - 
      mean(durationsOnt$DurationOfPrefix))

abs(0.1167 - 0.1581)
abs(0.1581 - 0.1743)

par(mfrow = c(1, 2))
plot(density(durationsOnt$DurationOfPrefix),
     main = "Duration of ont- Prefix (Density)")
abline(v = median(durationsOnt$DurationOfPrefix), col = "blue",
       lwd = 1.5)
abline(v = mean(durationsOnt$DurationOfPrefix), col = "red", 
       lwd = 1.5)

qqnorm(durationsOnt$DurationOfPrefix, 
       main = "Duration of ont- Prefix (Q-Q Plot)")
qqline(durationsOnt$DurationOfPrefix, col = "red", lwd = 1.5)

par(mfrow = c(1, 1))

pnorm(q = -2.5)
pnorm(q = -2.5) * 2
pnorm(q = -3) * 2

# Mean +- SD * 2.5
sd.lower = mean(durationsOnt$DurationOfPrefix) -
  sd(durationsOnt$DurationOfPrefix) * 2.5

sd.upper = mean(durationsOnt$DurationOfPrefix) +
  sd(durationsOnt$DurationOfPrefix) * 2.5