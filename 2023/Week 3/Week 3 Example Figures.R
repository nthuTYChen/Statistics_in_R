library(ggplot2)

set.seed(1)
rand.iq = rnorm(n=100, mean=99, sd=5)
rand.iq = round(x=rand.iq, digit=0)
set.seed(999)
rand.brain = rnorm(n=100, mean=1300, sd=150)
rand.brain = round(x=rand.brain, digit=0)

pop.male = data.frame(IQ=rand.iq, brainSize=rand.brain, Gender=rep(x="Male", times=100))

set.seed(2)
rand.iq = rnorm(n=100, mean=99, sd=5)
rand.iq = round(x=rand.iq, digit=0)
set.seed(998)
rand.brain = rnorm(n=100, mean=1300, sd=150)
rand.brain = round(x=rand.brain, digit=0)

pop.female = data.frame(IQ=rand.iq, brainSize=rand.brain, Gender=rep(x="Female", times=100))

pop.all = rbind(pop.male, pop.female)

# Figure 1
ggplot(data=pop.all, mapping = aes(x=IQ, y=brainSize, group=Gender, color=Gender)) +
  geom_point(size=2) + scale_color_brewer(palette = "Set1") + 
  labs(x="IQ Score", y="Brain Size (g)", title="Figure 1",
       subtitle="The distribution of 100 people by IQ and Brain Size") +
  theme_bw()

pop.all$Selected = ifelse(runif(n=100, min=0, max=1) > 0.5, "Yes", "No")

# Figure 2
ggplot(data=subset(pop.all, Selected == "No"), mapping = aes(x=IQ, y=brainSize)) +
  geom_point(color="grey", size=2) +
  geom_point(data=subset(pop.all, Selected == "Yes"), color="red", size=2)  + 
  labs(x="IQ Score", y="Brain Size (g)", title="Figure 2",
       subtitle="The distribution of 100 people by IQ and Brain Size",
       caption="Red = a random selection of 50 samples") +
  theme_bw()

mean.iq = mean(pop.all$IQ)
sd.iq = sd(pop.all$IQ)
mean.size = mean(pop.all$brainSize)
sd.size = sd(pop.all$brainSize)

pop.all$Typicality = ifelse(pop.all$IQ > mean.iq + (sd.iq*1.5) | pop.all$IQ < mean.iq - (sd.iq*1.5) |
                               pop.all$brainSize > mean.size + (sd.size*1.5) |
                               pop.all$brainSize < mean.size - (sd.size*1.5), "Extreme", "Typical")

# Figure 3
ggplot(data=pop.all, mapping = aes(x=IQ, y=brainSize, group=distFeature, color=distFeature)) +
  geom_point(size=2) + scale_color_brewer(palette = "Set2") + 
  labs(x="IQ Score", y="Brain Size (g)", title="Figure 3",
       subtitle="The distribution of 100 people by IQ and Brain Size",
       caption="Extreme = IQ/Brain Size > mean + sd x 1.5 or < mean - sd x 1.5") +
  guides(color = guide_legend(title = "Typicality")) +
  theme_bw()

pop.all$Subset = ifelse(pop.all$IQ > mean.iq & pop.all$brainSize < mean.size, "Biased", "Rest")

# Figure 4
ggplot(data=pop.all, mapping = aes(x=IQ, y=brainSize, group=Subset, color=Subset)) +
  geom_point(size=2) + scale_color_manual(values = c("#e34a33", "#fee8c8")) + 
  labs(x="IQ Score", y="Brain Size (g)", title="Figure 4",
       subtitle="The distribution of 100 people by IQ and Brain Size",
       caption="Biased = IQ > mean and Brain Size < mean") +
  theme_bw()

# Figure 5

set.seed(1)

norm = data.frame(Values=dnorm(seq(-3, 3, length=150), 0, 1))
norm$z = seq(-3, 3, length=150)

ggplot(data=norm, aes(x=z, y=Values)) + geom_line(stat="identity", lwd=1.5) +
  labs(title = "Figure 5", subtitle = "Normal Distribution", caption = "Mean = 0, SD = 1",
       x = "z", y = "Density") + theme_bw()

# Figure 6

ggplot(data=norm, aes(x=z, y=Values)) + 
  geom_area(data=subset(norm, z <= -1.96), stat = "identity", color="black", fill="orange") +
  geom_area(data=subset(norm, z >= 1.96), stat = "identity", color="black", fill="orange") +
  geom_line(stat="identity", lwd=1.5) +
  labs(title = "Figure 6", subtitle = "Normal Distribution", caption = "Mean = 0, SD = 1; Orange = 2.5%",
       x = "z", y = "Density") + theme_bw()

# Supplementary Materials 1

source("https://lngproc.hss.nthu.edu.tw/statisticsR/Week3/distExamples.R")

dist.skewR.norm = dnorm(seq(58000:300000), mean = 129000, sd = 39718.07)

dist.a = data.frame(x = density(dist.skewR)$x, y = density(dist.skewR)$y, Distribution = "Sample")
dist.b = data.frame(x = seq(58000:300000), y = dist.skewR.norm, Distribution = "Theoretical")
dist.skewR.comp = rbind(dist.a, dist.b)

dist.skewR.ord = dist.skewR[order(dist.skewR)]
dist.skewR.5perc = dist.skewR.ord[95]

dist.skewR.norm.5perc = qnorm(p = .95, mean = 129000, sd = 39716.07)

ggplot(dist.skewR.comp, aes(x = x, y = y, color = Distribution)) +
  geom_line(stat = "identity", lwd = 2) +
  geom_area(data = subset(dist.skewR.comp, Distribution == "Sample" & x >= dist.skewR.5perc), 
            fill = "red", alpha = 0.5) +
  geom_area(data = subset(dist.skewR.comp, Distribution == "Theoretical" & x >= dist.skewR.norm.5perc), 
            fill = "turquoise", alpha = 0.5) +
  labs(title = "Figure 1. Rightly skewed vs. Theoretical distribution", x = "Value", y = "Density",
       subtitle = "Mean = 129,000; SD = 39716.07", caption = "Shaded Area = 95%-100%") +
  theme_bw()

dist.skewL.norm = dnorm(seq(58000:300000) * -1, mean = -129000, sd = 39718.07)

dist.a = data.frame(x = density(dist.skewL)$x, y = density(dist.skewL)$y, Distribution = "Sample")
dist.b = data.frame(x = seq(58000:300000) * -1, y = dist.skewL.norm, Distribution = "Theoretical")
dist.skewL.comp = rbind(dist.a, dist.b)

dist.skewL.ord = dist.skewL[order(dist.skewL)]
dist.skewL.5perc = dist.skewL.ord[5]

dist.skewL.norm.5perc = qnorm(p = .05, mean = -129000, sd = 39716.07)

ggplot(dist.skewL.comp, aes(x = x, y = y, color = Distribution)) +
  geom_line(stat = "identity", lwd = 2) +
  geom_area(data = subset(dist.skewL.comp, Distribution == "Sample" & x <= dist.skewL.5perc), 
            fill = "red", alpha = 0.5) +
  geom_area(data = subset(dist.skewL.comp, Distribution == "Theoretical" & x <= dist.skewL.norm.5perc), 
            fill = "turquoise", alpha = 0.5) +
  labs(title = "Figure 2. Leftly skewed vs. Theoretical distribution", x = "Value", y = "Density",
       subtitle = "Mean = -129,000; SD = 39716.07", caption = "Shaded Area = 0%-5%") +
  theme_bw()
