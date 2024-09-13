nums = 1:100

sample(x = nums, size = 3)

for(n in 1:10) {
  print(n)
}

for(n in 1:10) {
  res = sample(x = nums, size = 3)
  print(res)
}

for(n in 1:50) {
  res = sample(x = nums, size = 3)
  print(res)
}

source("https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/main/2024/2_Concept/distExamples.R")

mean(dist.norm)
mean(dist.skewR)
mean(dist.skewL)

median(dist.norm)
median(dist.skewR)
median(dist.skewL)

dist.norm.den = density(dist.norm)
plot(dist.norm.den, main = "(Near-)Normal Distribution")
abline(v = mean(dist.norm), col = "red", lwd = 1.5)
abline(v = median(dist.norm), col = "blue", lwd = 1.5)