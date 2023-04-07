mean.pop = 300
sd.pop = 40

(360 - mean.pop) / sd.pop # z-score for population
(275 - mean.pop) / sd.pop

pnorm(q = 1.5, lower.tail = FALSE)
pnorm(q = -0.625, lower.tail = TRUE)

set.seed(100)
sample = rnorm(n = 100, mean = 300, sd = 10)

(sample[1] - mean(sample)) / sd(sample)

sample.z = scale(sample)

z.10 = (325 - mean.pop) / (40 / sqrt(10))
pnorm(q = z.10, lower.tail = FALSE) * 2

z.50 = (305 - mean.pop) / (40 / sqrt(50))
pnorm(q = z.50, lower.tail = FALSE) * 2

t = (325 - mean.pop) / (40 / sqrt(10))
p.upper = pt(q = t, df = 10 - 1, lower.tail = FALSE)
p.upper * 2

set.seed(5)
group1 = rnorm(n = 10, mean = 325, sd = 40)

t.test(x = group1, mu = 300)

set.seed(9)
group2 = rnorm(n = 40, mean = 325, sd = 40)

t.test(x = group2, mu = 300)