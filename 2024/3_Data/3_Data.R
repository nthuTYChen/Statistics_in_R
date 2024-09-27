source("https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/main/courseUtil.R")

wordRT = loadCourseCSV(year = 2024, topic = "3_Data", file = "wordRT.csv")

library(reshape2)

wordRT.new = melt(data = wordRT, id.var = "Word", variable.name = "Gender",
                  value.name = "RT")

nrow(wordRT.new)

jabberwocky.wd = loadCourseCSV(2024, "3_Data", "jabberwocky_words.txt")

nrow(jabberwocky.wd)

jabberwocky.table = table(jabberwocky.wd$Word)
head(jabberwocky.table)

jabberwocky.df = as.data.frame(jabberwocky.table)
head(jabberwocky.df)

colnames(jabberwocky.df) = c("Word", "Count")

nrow(jabberwocky.df)
sum(jabberwocky.df$Count)

order(jabberwocky.df$Count, decreasing = T)
jabberwocky.df[71,]
jabberwocky.df[2,]

count.des.ord = order(jabberwocky.df$Count, decreasing = T)

jabberwocky.ord = jabberwocky.df[count.des.ord,]
head(jabberwocky.ord)

jabberwocky.wordCat = loadCourseCSV(2024, "3_Data", "jabberwocky_words_cat.csv")

cat.na = is.na(jabberwocky.wordCat$Cat)
sum(cat.na)

jabberwocky.wordCat[cat.na,]
head(jabberwocky.wordCat[!cat.na,])

jabberwocky.wordCat.noNA = jabberwocky.wordCat[!cat.na,]

jabberwocky.all = merge(jabberwocky.ord, jabberwocky.wordCat.noNA, by = "Word")

nrow(jabberwocky.all)
nrow(jabberwocky.ord)

xtabs(formula = Count ~ Real, data = jabberwocky.all)

xtabs(formula = ~ Real, data = jabberwocky.all)

jabberwocky.xtabs = xtabs(formula = ~ Real + Cat, data = jabberwocky.all)

jabberwocky.sum = sum(jabberwocky.xtabs)

jabberwocky.xtabs / jabberwocky.sum

jabberwocky.perc = jabberwocky.xtabs * 100 / jabberwocky.sum

round(x = jabberwocky.perc, digit = 1)

library(languageR)
head(durationsOnt[1:7])

summary(durationsOnt$DurationOfPrefix)

par(mfrow = c(1, 2))
plot(density(durationsOnt$DurationOfPrefix), main = "Duration of ont- Prefix (Density)")
abline(v = median(durationsOnt$DurationOfPrefix), col = "red", lwd = 1.5)
abline(v = mean(durationsOnt$DurationOfPrefix), col = "blue", lwd = 1.5)

qqnorm(durationsOnt$DurationOfPrefix, main = "Duration of ont- Prefix (Q-Q Plot)")
qqline(durationsOnt$DurationOfPrefix, col = "red", lwd = 1.5)
par(mfrow = c(1, 1))

pnorm(q = -2.5)
pnorm(q = -3)

sd.lower = mean(durationsOnt$DurationOfPrefix) - sd(durationsOnt$DurationOfPrefix) * 2.5
sd.upper = mean(durationsOnt$DurationOfPrefix) + sd(durationsOnt$DurationOfPrefix) * 2.5

# Logical Operators
outliers = 
  durationsOnt$DurationOfPrefix < sd.lower | durationsOnt$DurationOfPrefix > sd.upper

sum(outliers)

durationsOnt[outliers, 1:7]

durationsOnt$Freq.raw = exp(durationsOnt$Frequency)
head(durationsOnt$Freq.raw)