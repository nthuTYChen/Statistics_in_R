resourcesURL = "https://lngproc.hss.nthu.edu.tw/statisticsR/Week1/"

df = read.csv(paste(resourcesURL, "dummyDataFrame.csv", sep = ""))

x = 1:2

df[x, ]

y = 2:3

df[x, y]

x = c(1, 3)

df[x, y]

simpleAdd = function() {
  x = 1 + 2
  x
}

simpleAdd()

complexSubtract = function(x, y) {
  z = x - y
  z
}

complexSubtract(x=2, y=4)
complexSubtract(x=9, y=12)

complexSubtract(y=12, x=9)

complexSubtract(9, 12)
complexSubtract(12, 9)

complexCalc = function(x=0, y=0) {
  x = x ^ 2
  y = 1 / y
  z = x + y
  return(z)
}

result = complexCalc(7, 5)

result = complexCalc(y=5)

str(df)

source("https://lngproc.hss.nthu.edu.tw/statisticsR/Week3/distExamples.R")

mean(dist.norm)
mean(dist.skewL)
mean(dist.skewR)

median(dist.norm)
median(dist.skewL)
median(dist.skewR)

mean(dist.norm) - median(dist.norm)
mean(dist.skewL) - median(dist.skewL)
mean(dist.skewR) - median(dist.skewR)

dist.norm.den = density(dist.norm)
plot(dist.norm.den, main = "(Near-)Normal Distribution")
abline(v = mean(dist.norm), col = "red", lwd = 1.5)
abline(v = median(dist.norm), col = "blue", lwd = 1.5)

dist.skewL.den = density(dist.skewL)
plot(dist.skewL.den, main = "Non-normal distrubtion; Left Skewness")
abline(v = mean(dist.skewL), col = "red", lwd = 1.5)
abline(v = median(dist.skewL), col = "blue", lwd = 1.5)

dist.skewR.den = density(dist.skewR)
plot(dist.skewR.den, main = "Non-normal distrubtion; Right Skewness")
abline(v = mean(dist.skewR), col = "red", lwd = 1.5)
abline(v = median(dist.skewR), col = "blue", lwd = 1.5)