library(ggplot2)

# Figure 1

binoms.df = data.frame(n = 0:10, d = dbinom(0:10, size = 10, prob = 0.5))

ggplot(data = binoms.df, mapping = aes(x = n, y = d)) +
  geom_point(stat = "identity", size = 3, color = "grey40") +
  scale_x_continuous(breaks = 0:10) +
  labs(title = "Figure 1. Binomial distribution for 10 binary wordlikeness judgment \ntrials",
       x = "hits", y = "density") +
  theme_bw()

# Figure 2

binoms.10 = data.frame(n = 0:10, d = dbinom(0:10, size = 10, prob = 0.5), SampleN = "Sample N = 10")
binoms.50 = data.frame(n = 0:50, d = dbinom(0:50, size = 50, prob = 0.5), SampleN = "Sample N = 50")

norms.10 = data.frame(n = seq(0, 10, length = 500), d = dnorm(seq(0, 10, length = 500), 5, sqrt(5 * 0.5)), SampleN = "Sample N = 10")
norms.50 = data.frame(n = seq(0, 50, length = 2500), d = dnorm(seq(0, 50, length = 2500), 25, sqrt(25 * 0.5)), SampleN = "Sample N = 50")

ggplot(mapping = aes(x = n, y = d)) +
  facet_grid(~ SampleN, scales = "free") +
  geom_line(data = rbind(norms.10, norms.50), stat = "identity", lwd = 1, color = "grey40") +
  geom_point(data = rbind(binoms.10, binoms.50), stat = "identity", size = 1.5, color = "red", alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 50, 5)) +
  labs(title = "Figure 2. Binomial vs. normal distribution for different sample Ns",
       caption = "Grey curve = Normal Distribution",
       x = "hits", y = "density") +
  theme_bw()

# Figure 3
binoms.05 = data.frame(n = 0:100, d = dbinom(0:100, size = 100, prob = 0.5), Probability = ".5")
binoms.03 = data.frame(n = 0:100, d = dbinom(0:100, size = 100, prob = 0.3), Probability = ".3")
binoms.015 = data.frame(n = 0:100, d = dbinom(0:100, size = 100, prob = 0.15), Probability = ".15")

binoms = rbind(binoms.05, binoms.03, binoms.015)
ggplot(data = binoms, mapping = aes(x = n, y = d, color = Probability)) +
  geom_line(stat = "identity", lwd = 1.5) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Figure 3. Binomial distribution for different probabilities of hits", x = "hits", y = "density") +
  theme_bw()

# Figure 4
set.seed(1)
cor.rand = data.frame(x = rnorm(10, 0.5, 0.2), y = rnorm(10, 0.5, 0.2))

cor.outliers = rbind(cor.rand, data.frame(x = 3, y = 3))

ggplot(mapping = aes(x = x, y = y)) +
  geom_point(data = cor.rand, size = 2, color = "grey40", alpha = .4) +
  geom_point(data = data.frame(x = 3, y = 3), size = 2, color = "red", alpha = .4) +
  labs(title = "Figure 4. An outlier in a correlation test", 
       caption = "Without the outlier: r = -0.38, p = .28\nWith the outlier: r = 0.93, p < .001; rho = 0.29, p = .39") +
  theme_bw()

# Figure 5
chisq.3 = data.frame(X = seq(0, 20, length = 100), y = dchisq(x = seq(0, 20, length = 100), df = 3), df = "3")
chisq.5 = data.frame(X = seq(0, 20, length = 100), y = dchisq(x = seq(0, 20, length = 100), df = 5), df = "5")
chisq.10 = data.frame(X = seq(0, 20, length = 100), y = dchisq(x = seq(0, 20, length = 100), df = 10), df = "10")

chisq.dist = rbind(chisq.3, chisq.5, chisq.10)

ggplot(data = chisq.dist, mapping = aes(x = X, y = y, color = df)) +
  geom_line(stat = "identity", lwd = 2) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Figure 5. Chi-squared distributions", x = "X2", y = "density") +
  theme_bw()