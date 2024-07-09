set.seed(100)

dist.norm = rnorm(100, 50, 5)

dist.skewR = dist.norm ^ 4

dist.skewL = 1 / dist.skewR ^ 4
