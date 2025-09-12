nums = 1:100

sample(x = nums, size = 3)

# For loop
for(n in 1:10) {
  res = sample(x = nums, size = 3)
  print(res)
}

for(n in 1:50) {
  res = sample(x = nums, size = 3)
  print(res)
}

source("https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/refs/heads/main/2025/2_Concept/distExamples.R")

mean(dist.norm)
mean(dist.skewL)
mean(dist.skewR)

median(dist.norm)

mean(dist.norm) - median(dist.norm)
mean(dist.skewL) - median(dist.skewL)
mean(dist.skewR) - median(dist.skewR)

dist.norm.den = density(dist.norm)
plot(dist.norm.den, main = "(Near-) Normal Distribution")

abline(v = mean(dist.norm), col = "red", lwd = 1.5)
abline(v = median(dist.norm), col = "blue", lwd = 1.5)

median(dist.norm)
range(dist.norm)

summary(dist.norm)

IQR(dist.norm)