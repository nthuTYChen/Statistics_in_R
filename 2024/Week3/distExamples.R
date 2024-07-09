set.seed(100)

dist.norm = rnorm(100, 50, 5)

dist.skewR = exp(dist.norm)

dist.skewL = 1 / exp(dist.norm)
