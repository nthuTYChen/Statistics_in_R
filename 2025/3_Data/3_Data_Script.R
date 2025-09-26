source("https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/refs/heads/main/courseUtil.R")

jabberwocky.wd = loadCourseCSV(2025, "3_Data", "jabberwocky_words.txt")

head(jabberwocky.wd)

nrow(jabberwocky.wd) # Token frequency

jabberwocky.table = table(jabberwocky.wd$Word)

head(jabberwocky.table)

jabberwocky.df = as.data.frame(jabberwocky.table)

head(jabberwocky.df)

colnames(jabberwocky.df) = c("Word", "Count")

head(jabberwocky.df)

nrow(jabberwocky.df)

sum(jabberwocky.df$Count)

order(jabberwocky.df$Count, decreasing = T)

jabberwocky.df[71,]

count.des.ord = order(jabberwocky.df$Count, decreasing = T)

jabberwocky.ord = jabberwocky.df[count.des.ord,]

head(jabberwocky.ord)
tail(jabberwocky.ord, 10)

jabberwocky.wordCat = loadCourseCSV(2025, "3_Data", "jabberwocky_words_cat.csv")

head(jabberwocky.wordCat)

# NA
cat.na = is.na(jabberwocky.wordCat$Cat)

head(cat.na)

sum(cat.na)

jabberwocky.wordCat[cat.na,]

head(!cat.na)

head(jabberwocky.wordCat[!cat.na,])

jabberwocky.all = merge(jabberwocky.ord, jabberwocky.wordCat, by = "Word")

head(jabberwocky.all)

xtabs(formula = Count ~ Real, data = jabberwocky.all)

xtabs(formula = ~ Real, data = jabberwocky.all)

xtabs(formula = ~ Real + Cat, data = jabberwocky.all)

jabberwocky.xtabs = xtabs(formula = ~ Real + Cat, data = jabberwocky.all)

jabberwocky.sum = sum(jabberwocky.xtabs)

jabberwocky.prob = jabberwocky.xtabs / jabberwocky.sum

jabberwocky.perc = jabberwocky.prob * 100

jabberwocky.perc = round(jabberwocky.prob * 100, digit = 1)

library(languageR)

head(durationsOnt)

hist(durationsOnt$YearOfBirth)

par(mfrow = c(1, 2))
plot(density(durationsOnt$DurationOfPrefix),
     main = "Duration of ont- Prefix (Density)")
qqnorm(durationsOnt$DurationOfPrefix,
       main = "Duration of ont- Prefix (Q-Q Plot)")
qqline(durationsOnt$DurationOfPrefix, col = "red", lwd = 1.5)
par(mfrow = c(1, 1))

pnorm(q = -2) * 2
pnorm(q = -2.5) * 2