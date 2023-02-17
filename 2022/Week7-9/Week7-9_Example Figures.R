library(ggplot2)

# Figure 1
set.seed(1)

norm = data.frame(Values = dnorm(seq(100, 500, length = 401), 300, 40))
norm$Raw = seq(100, 500, length = 401)

set.seed(2)
sample.10 = data.frame(Raw = rnorm(10, 300, 40), Group = "10 three-yearolds")

set.seed(2)
sample.50 = data.frame(Raw = rnorm(50, 300, 40), Group = "50 three-year-olds")

sampled = rbind(sample.10, sample.50)

ggplot() +
  geom_smooth(data = norm, mapping = aes(x = Raw, y = Values), stat = "identity", lwd = 1.5, color = "grey40") +
  geom_density(data = sampled, mapping = aes(x = Raw), color = "red", alpha = 0.5) +
  facet_grid(. ~ Group) +
  labs(title = "Figure 1. A putative case of vocabulary size sampling", 
       x = "(Fake) Vocabulary Size", y = "Density",
       caption = "Grey curve = normal distribution (mean = 300; sd = 40)\nRed curve = Actual Sampling") +
  theme_bw()

# Figure 2

set.seed(1)
d = dnorm(seq(-3, 3, length = 150))

norms = data.frame(z = rep(seq(-3, 3, length = 150), 3), 
                   d = rep(d, 3),
                   scene = c(rep("one-tailed: lower", 150),
                             rep("one-tailed: upper", 150),
                             rep("two-tailed", 150)))

norms$tails = FALSE
norms[norms$scene == "one-tailed: lower" & norms$z <= -1.65,]$tails = TRUE
norms[norms$scene == "one-tailed: upper" & norms$z >= 1.65,]$tails = TRUE
norms[norms$scene == "two-tailed" & norms$z <= -1.96, ]$tails = TRUE

ggplot(data = norms, mapping = aes(x = z, y = d)) +
  geom_line(stat = "identity", lwd = 2, color = "grey40") +
  geom_area(data = subset(norms, tails == TRUE), stat = "identity",
            color = "grey40", fill = "orange") +
  facet_grid(. ~ scene) + 
  geom_area(data = subset(norms, scene == "two-tailed" & z >= 1.96),
                          color = "grey40", fill = "orange") +
  labs(title = "Figure 2. One-tailed vs. Two-tailed p-value",
       caption = "alpha = .05", x = "z-score", y = "density") +
  theme_bw()


# Figure 3
znorm.sd20 = data.frame(x = seq(220, 380, length = 500), d = dnorm(seq(220, 380, length = 500), 300, 20), SD = "= 20")
znorm.sd50 = data.frame(x = seq(220, 380, length = 500), d = dnorm(seq(220, 380, length = 500), 300, 50), SD = "= 50")

ggplot(data = rbind(znorm.sd20, znorm.sd50), mapping = aes(x = x, y = d, color = SD)) +
  geom_line(stat = "identity", lwd = 2, alpha = .4) +
  labs(title = "Figure 3. Same means, different SDs", caption = "Mean = 300", x = "x", y = "density") +
  theme_bw()

# Figure 4
seq.x = seq(-3, 3, length = 150)
znorm = data.frame(x = seq.x, d = dnorm(seq.x), Type = "Normal")
t.5 = data.frame(x = seq.x, d = dt(seq.x, 5), Type = "t(df = 5)")
t.10 = data.frame(x = seq.x, d = dt(seq.x, 10), Type = "t(df = 10)")
t.20 = data.frame(x = seq.x, d = dt(seq.x, 20), Type = "t(df = 20)")

ggplot(data = rbind(znorm, t.5, t.10, t.20), mapping = aes(x = x, y = d, color = Type)) +
  geom_line(stat = "identity", lwd = 2, alpha = .4) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Figure 4. Normal vs. t distributions", x = "x", y = "density") +
  theme_bw()

# Figure 5
set.seed(1)
cor.rand = data.frame(x = rnorm(10, 0.5, 0.2), y = rnorm(10, 0.5, 0.2))

cor.outliers = rbind(cor.rand, data.frame(x = 3, y = 3))

ggplot(mapping = aes(x = x, y = y)) +
  geom_point(data = cor.rand, size = 2, color = "grey40", alpha = .4) +
  geom_point(data = data.frame(x = 3, y = 3), size = 2, color = "red", alpha = .4) +
  labs(title = "Figure 5. An outlier in a correlation test", 
       caption = "Without the outlier: r = -0.38, p = .28\nWith the outlier: r = 0.93, p < .001") +
  theme_bw()

# Figure 6
poly.df = data.frame(x = seq(-5, 5, length = 50))
poly.df$y = poly.df$x^2 - 5

ggplot(data = poly.df, mapping = aes(x = x, y = y)) +
  geom_line(stat = "identity", lwd = 1.5, color = "grey40", alpha = .4) +
  geom_hline(yintercept = 0, color = "red") +
  geom_vline(xintercept = 0, color = "blue") +
  labs(title = "Figure 6. Nonlinear x-y correlation", caption = "Quadratic equation: y = x^2 - 5") +
  theme_bw()

# Figure 7
home.sced = data.frame(x = seq(-2, 2, length = 100), y = seq(-2, 2, length = 100))
home.sced[seq(1, 100, by = 2),]$y = home.sced[seq(1, 100, by = 2),]$y + seq(1, 100, by = 2) * 0.1

ggplot(data = home.sced, mapping = aes(x = x, y = y)) +
  geom_point(size = 2, color = "grey40", alpha = 0.4) +
  labs(title = "Figure 7. Violation of homoscedasticity") +
  theme_bw()

# Figure 8

set.seed(1)

norm = data.frame(Values=dnorm(seq(-3, 3, length=150), 0, 1))
norm$z = seq(-3, 3, length=150)

set.seed(15)
sample.10 = data.frame(z = rnorm(5), SampleN = "Sample N = 10", Speaker = rep("Native", 5))
sample.10 = rbind(sample.10, data.frame(z = rnorm(5), SampleN = "Sample N = 10", Speaker = rep("Nonnative", 5)))

set.seed(15)
sample.30 = data.frame(z = rnorm(15), SampleN = "Sample N = 30", Speaker = rep("Native", 15))
sample.30 = rbind(sample.30, data.frame(z = rnorm(15), SampleN = "Sample N = 30", Speaker = rep("Nonnative", 15)))

set.seed(15)
sample.80 = data.frame(z = rnorm(40), SampleN = "Sample N = 80", Speaker = rep("Native", 40))
sample.80 = rbind(sample.80, data.frame(z = rnorm(40), SampleN = "Sample N = 80", Speaker = rep("Nonnative", 40)))

norm.sampled = rbind(sample.10, sample.30, sample.80)

ggplot() +
  geom_line(data = norm, mapping = aes(x = z, y = Values), stat = "identity", lwd = 1.2, alpha = 0.5) +
  geom_density(data = norm.sampled, aes(x = z, color = Speaker), alpha = 0.5) +
  facet_grid(. ~ SampleN) +
  labs(title = "Figure 8. A putative case of vocabulary size sampling for native/nonnative speakers", 
       x = "Standardized (Fake) Vocabulary Size", y = "Density",
       caption = "Grey curve = normal distribution (mean = 0; sd = 1)") +
  theme_bw()

# Figure 9
set.seed(1)
n = 30
means = 0
se = 0
ci.upper = 0
ci.lower = 0
for(i in 1:50) {
  sample = rnorm(n)
  means[i] = mean(sample)
  se[i] = sd(sample) / sqrt(n)
  ci.upper[i] = mean[i] + 1.96 * se[i]
  ci.lower[i] = mean[i] - 1.96 * se[i]
}

ci.test = data.frame(sample.mean = means, sample.se = se, 
                     ci.upper = ci.upper, ci.lower = ci.lower, x = 1:50)

means.avg = mean(means)

ci.test$includeMu = ifelse(ci.test$ci.lower > means.avg | ci.test$ci.upper < means.avg, "No", "Yes")

ggplot(data = ci.test, mapping = aes(x = x, y = sample.mean, color = includeMu)) +
  geom_errorbar(ymin = ci.lower, ymax = ci.upper) +
  geom_hline(yintercept = mean(ci.test$sample.mean), color = "red") +
  scale_x_continuous(labels = NULL, breaks = NULL) +
  scale_y_continuous(limits = c(-1, 1)) +
  scale_color_manual(values = c("#252525", "#cccccc")) +
  coord_flip() + labs(title = "Figure 9. 95% CI of 50 Samples from Normal Distribution",
    subtitle = "mu = 0, sd = 1", x = "", y = "95% CI", caption = "red line = M") +
  guides(color = guide_legend(title = "Include mu")) +
  theme_bw()

