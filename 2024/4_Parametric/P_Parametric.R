mean.pop = 300
sd.pop = 40
mean.sample = 320
n = 10

se = 40 / sqrt(10)
z = (mean.sample - mean.pop) / se

pnorm(q = z, lower.tail = F)

pnorm(q = z, lower.tail = F) * 2

set.seed(500)

for(n in 1:10) {
  sam = rnorm(n = 10, mean = 300, sd = 40)
  M = mean(sam)
  diff = abs(M - 300)
  print(diff)
}

n = 40
se = 40 / sqrt(n)
z = (320 - 300) / se

pnorm(q = z, lower.tail = F)

pnorm(q = z, lower.tail = F) * 2

set.seed(800)

for(n in 1:10) {
  sam = rnorm(n = 40, mean = 300, sd = 40)
  M = mean(sam)
  diff = abs(M - 300)
  print(diff)
}