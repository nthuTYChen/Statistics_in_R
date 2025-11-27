source("https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/refs/heads/main/courseUtil.R")

library(ggplot2)

# Figure 1
fdist.10.5 = data.frame(x = seq(0, 8, length = 100), 
                        y = df(seq(0, 8, length = 100), 10, 5),
                        df = "df1 = 10, df2 = 5")

fdist.100.50 = data.frame(x = seq(0, 8, length = 100), 
                          y = df(seq(0, 8, length = 100), 100, 50),
                          df = "df1 = 100, df2 = 50")

fdist.200.100 = data.frame(x = seq(0, 8, length = 100), 
                           y = df(seq(0, 8, length = 100), 200, 100),
                           df = "df1 = 200, df2 = 100")

fdist.all = rbind(fdist.10.5, fdist.100.50, fdist.200.100)

ggplot(data = fdist.all, mapping = aes(x = x, y = y, color = df)) +
  geom_line(stat = "identity", lwd = 2) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Figure 1. F distribution by different dfs", x = "F", y = "density") +
  theme_bw()

# Figure 2

sl.rep.sim = loadCourseCSV(2025, "5_ANOVA", "SaitoLysterRepSim.csv")
sl.rep.avg = aggregate(rF3 ~ Subject + Condition, FUN = mean, data = sl.rep.sim)

ggplot(data = sl.rep.sim, mapping = aes(x = Condition, y = rF3, fill = Condition)) +
  geom_boxplot(position = "dodge2") +
  scale_y_continuous(limits = c(1800, 2800)) +
  labs(title = "Figure 2. L2 English retroflex produced by L1 Japanese Speakers", x = "Pedagogical Condition", 
       y = "F3 in Hz (averaged within subjects", subtitle = "Simulated data") +
  theme_bw()

# Figure 3
set.seed(555)
sim.x.a = data.frame(Value = rnorm(50, 1, 1), VarX = "A", VarY = "A", Interaction = "Enhanced/Reduced")
sim.x.b = data.frame(Value = rnorm(50, 1.3, 1), VarX = "B", VarY = "A", Interaction = "Enhanced/Reduced")
sim.y.a = data.frame(Value = rnorm(50, 2, 1), VarX = "A", VarY = "B", Interaction = "Enhanced/Reduced")
sim.y.b = data.frame(Value = rnorm(50, 1.5, 1), VarX = "B", VarY = "B", Interaction = "Enhanced/Reduced")

sim.enhanced = rbind(sim.x.a, sim.x.b, sim.y.a, sim.y.b)

sim.x.a = data.frame(Value = rnorm(50, 1, 1), VarX = "A", VarY = "A", Interaction = "Opposite")
sim.x.b = data.frame(Value = rnorm(50, 2, 1), VarX = "B", VarY = "A", Interaction = "Opposite")
sim.y.a = data.frame(Value = rnorm(50, 2.2, 1), VarX = "A", VarY = "B", Interaction = "Opposite")
sim.y.b = data.frame(Value = rnorm(50, 1.3, 1), VarX = "B", VarY = "B", Interaction = "Opposite")

sim.opposite = rbind(sim.x.a, sim.x.b, sim.y.a, sim.y.b)

sim.x.a = data.frame(Value = rnorm(50, 1, 1), VarX = "A", VarY = "A", Interaction = "None")
sim.x.b = data.frame(Value = rnorm(50, 2, 1), VarX = "B", VarY = "A", Interaction = "None")
sim.y.a = data.frame(Value = rnorm(50, 1.5, 1), VarX = "A", VarY = "B", Interaction = "None")
sim.y.b = data.frame(Value = rnorm(50, 2.5, 1), VarX = "B", VarY = "B", Interaction = "None")

sim.none = rbind(sim.x.a, sim.x.b, sim.y.a, sim.y.b)

sim.all = rbind(sim.enhanced, sim.opposite, sim.none)

ggplot(sim.all, aes(x = VarX, y = Value, fill = VarY)) +
  geom_boxplot() + facet_grid(~ Interaction) +
  labs(title = "Figure 3. Interaction Types", x = "Variable X", y = "Value", fill = "Variable Y") +
  theme_bw()

# Figure 4
chen.sample = loadCourseCSV(2025, "5_ANOVA", "Chen2020Sample.csv")

chen.avg = aggregate(Accept ~ participant + Group + InitialTone, FUN = mean, data = chen.sample)

ggplot(chen.avg, aes(x = Group, y = Accept, fill = InitialTone)) +
  geom_boxplot() + scale_y_continuous(limits = c(0, 1)) +
  labs(title = "Figure 4. Acceptance Probability by Initial Tone across Groups", x = "Group", 
       y = "Acceptance Probability (averaged within subjects)", fill = "Initial Tone", 
       subtitle = "Source: Chen (2020)") + theme_bw()