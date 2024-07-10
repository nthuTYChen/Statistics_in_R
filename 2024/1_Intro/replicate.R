set.seed(999)

rand.norms = rnorm(100, 0, 1)

plot(qqnorm(rand.norms))