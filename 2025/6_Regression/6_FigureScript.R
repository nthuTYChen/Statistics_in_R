source("https://raw.githubusercontent.com/nthuTYChen/Statistics_in_R/main/courseUtil.R")

library(languageR)
library(ggplot2)
library(effects)

# Figure 1
ggplot(data = durationsOnt, mapping = aes(x = Frequency, y = DurationPrefixNasal)) +
  geom_point(size = 1.5, alpha = 0.7, color = "grey40") +
  geom_smooth(method = "lm", se = F, color = "red", lwd = 1.5) +
  labs(title = "Figure 1. Duration of ont- Prefix Nasal ~ Word Frequency", 
       subtitle = "r = -0.156, t(100) = -1.58, p = .117", x = "log Word Frequency",
       y = "Duration of Nasal (s)") +
  theme_bw()

# Figure 2
dur.lm = lm(formula = DurationPrefixNasal ~ Frequency, data = durationsOnt)
dur.lm.int = lm(formula = DurationPrefixNasal ~ 1, data = durationsOnt)

dur.lm.model = data.frame(Frequency = dur.lm$model$Frequency,
                          Duration = dur.lm$fitted.values,
                          Residuals = dur.lm$residuals)
dur.lm.model$Model = "Duration ~ 1 + Frequency"

dur.lm.int.model = data.frame(Frequency = durationsOnt$Frequency)
dur.lm.int.model$Duration = mean(durationsOnt$DurationPrefixNasal)
dur.lm.int.model$Residuals = dur.lm.int$residuals
dur.lm.int.model$Model = "Duration ~ 1"

dur.lm.all = rbind(dur.lm.model, dur.lm.int.model)

ggplot() +
  geom_point(data = durationsOnt, mapping = aes(x = Frequency, y = DurationPrefixNasal),
             size = 1.5, alpha = 0.7, color = "grey40") +
  geom_smooth(data = dur.lm.all, mapping = aes(x = Frequency, y = Duration), 
              method = "lm", color = "red", lwd = 1.5) +
  geom_segment(data = dur.lm.all, mapping = aes(x = Frequency, y = Duration, 
                    xend = Frequency, yend = Duration + Residuals), alpha = 0.7, lty = 2) +
  facet_grid(~ Model) + 
  labs(title = "Figure 2. Fitted values + Residuals in null / target models", 
       y = "Fitted Duration of Prefix Nasal (s)") +
  theme_bw()

# Figure 3
ggplot(data = durationsOnt, mapping = aes(x = factor(PlosivePresent), y = DurationPrefixNasal)) +
  geom_boxplot(fatten = NULL) + 
  geom_smooth(mapping = aes(group=1), method = "lm", se = FALSE, color = "blue", lwd = 1.5) +
  labs(title = "Figure 3. Duration of Prefix Nasal by The Present of Prefix Stop",
       subtitle = "lm: B = -0.0254, SE = 0.004, t(100) = -6.281, p < .001, r2 = 0.2829",
       x = "Duration of Prefix Nasal (s)", y = "The Presencne of Prefix Stop") +
  theme_bw()

# Figure 4
english.sub = subset(english, WordCategory == "N")
english.sub = english.sub[c("RTlexdec", "NounFrequency", "Familiarity", "Word")]
english.sub$NounFreq.log = log(english.sub$NounFrequency + 1)
english.lm = lm(RTlexdec ~ NounFreq.log + Familiarity, data = english.sub)

english.fam2 = data.frame(Familiarity = 2, 
                          NounFreq.log = seq(min(english.sub$NounFreq.log), max(english.sub$NounFreq.log), 0.1))
english.fam2.pred = predict(object = english.lm, newdata = english.fam2)
english.fam2$RTlexdec.fit = english.fam2.pred

english.fam3 = data.frame(Familiarity = 3, 
                          NounFreq.log = seq(min(english.sub$NounFreq.log), max(english.sub$NounFreq.log), 0.1))
english.fam3.pred = predict(object = english.lm, newdata = english.fam3)
english.fam3$RTlexdec.fit = english.fam3.pred

english.fam4 = data.frame(Familiarity = 4, 
                          NounFreq.log = seq(min(english.sub$NounFreq.log), max(english.sub$NounFreq.log), 0.1))
english.fam4.pred = predict(object = english.lm, newdata = english.fam4)
english.fam4$RTlexdec.fit = english.fam4.pred

english.fam5 = data.frame(Familiarity = 5, 
                          NounFreq.log = seq(min(english.sub$NounFreq.log), max(english.sub$NounFreq.log), 0.1))
english.fam5.pred = predict(object = english.lm, newdata = english.fam5)
english.fam5$RTlexdec.fit = english.fam5.pred

english.fam6 = data.frame(Familiarity = 6, 
                          NounFreq.log = seq(min(english.sub$NounFreq.log), max(english.sub$NounFreq.log), 0.1))
english.fam6.pred = predict(object = english.lm, newdata = english.fam6)
english.fam6$RTlexdec.fit = english.fam6.pred

english.fam.all = rbind(english.fam2, english.fam3, english.fam4, english.fam5, english.fam6)

ggplot() + 
  geom_point(data = english.sub, mapping = aes(x = NounFreq.log, y = RTlexdec), 
            size = 1.5, alpha = 0.7, color = "grey40") +
  geom_line(data = english.fam.all, mapping = aes(x = NounFreq.log, y = RTlexdec.fit, color = factor(Familiarity)), 
            lwd = 1.5, alpha = 0.7) +
  labs(title = "Figure 4. Lexical decision RT by Frequency and Familiarity",
       subtitle = "Linear regression: RTlexdec ~ NounFreq.log + Familiarity",
       x = "log Noun Frequency", y = "log Lexical Decision RT", color = "Familiarity") +
  theme_bw()
  
# Figure 5
english.sub = subset(english, WordCategory == "N")
english.sub = english.sub[c("RTlexdec", "NounFrequency", "Familiarity", "Word")]
english.sub$NounFreq.log = log(english.sub$NounFrequency + 1)

english.lm.int = lm(RTlexdec ~ NounFreq.log * Familiarity, data = english.sub)

# The effect() function requires the "effects" package
english.ef = as.data.frame(effect(term = "NounFreq.log:Familiarity", mod = english.lm.int))

ggplot() + 
  geom_point(data = english.sub, mapping = aes(x = NounFreq.log, y = RTlexdec), 
             size = 1.5, alpha = 0.7, color = "grey40") +
  geom_line(data = english.ef, mapping = aes(x = NounFreq.log, y = fit, color = factor(Familiarity)), 
            lwd = 1.5, alpha = 0.7) +
  labs(title = "Figure 5. Lexical decision RT by Frequency and Familiarity",
       subtitle = "Linear regression: RTlexdec ~ NounFreq.log x Familiarity",
       x = "log Noun Frequency", y = "log Lexical Decision RT", color = "Familiarity") +
  theme_bw()

# Figure 6
ggplot(data = durationsOnt, mapping = aes(x = Plosive.fac, 
                                          y = DurationPrefixNasal)) +
  geom_boxplot() + labs(title = "Figure 6. Nasal duration in the ont- prefix", 
                          x = "The presence of prefix plosive", y = "Prefix Nasal Duration (s)")  +
  facet_grid(. ~ Sex) +
  theme_bw()

# Figure 7
Myers.withNB = loadCourseCSV(2024, "6_Regression", "MyersWithNB.csv")

ggplot(data = Myers.withNB, mapping = aes(x = NB, y = Response)) +
  geom_point(color = "grey40", alpha = .7) +
  geom_smooth(method = "lm", color = "red", lwd = 1.5) +
  labs(title = "Figure 7. Wordlikeness responses by phonological similarity", 
       x = "Number of phonologically similar words",
       y = "Response (0 = No, 1 = Yes)", subtitle = "Linear Regression") +
  scale_y_continuous(n.breaks = 2) +
  theme_bw()

# Figure 8
prob = seq(0.01, 0.99, by = .01)
logistic.df = data.frame(Prob = prob, Logits = log(prob / (1 - prob)))

ggplot(data = logistic.df, mapping = aes(x = Prob, y = Logits)) +
  geom_line(lwd = 2) +
  labs(title = "Figure 8. Logistic Function", x = "Probability", y = "Logits") +
  theme_bw()

# Figure 9
logOdds = 0
for(i in 1:100000) {
  x = rbinom(n = 10000, size = 1, prob = .5)
  n.1 = length(x[x==1])
  p = n.1 / 10000
  logOdds[i] = log(p / (1 - p))
}

logOdds.df = data.frame(x = logOdds)

ggplot() + geom_density(data = logOdds.df, mapping = aes(x = x), lwd = 2) +
  scale_x_continuous(limits = c(-0.075, 0.075)) +
  labs(x = "Logit", y = "Density", title = "Figure 9. Logit Distribution") +
  theme_bw()

# Figure 10
chen.sample = loadCourseCSV(2024, "6_Regression", "Chen2020Sample.csv")

chen.sample$InitialTone_Fac = as.factor(chen.sample$InitialTone)
chen.sample$Group_Fac = as.factor(chen.sample$Group)

contrasts(chen.sample$InitialTone_Fac) = contr.sum(2)
colnames(contrasts(chen.sample$InitialTone_Fac)) = levels(chen.sample$InitialTone_Fac)[2]
contrasts(chen.sample$Group_Fac) = contr.sum(2)
colnames(contrasts(chen.sample$Group_Fac)) = levels(chen.sample$Group_Fac)[2]

chen.glm = glm(Accept ~ InitialTone_Fac * Group_Fac, family = "binomial",
                                                          data = chen.sample)

# The effect() function is from the effects package
chen.ef = as.data.frame(effect("InitialTone_Fac:Group_Fac", chen.glm))

ggplot(data = chen.ef, mapping = aes(x = InitialTone_Fac, y = fit, group = Group_Fac, color = Group_Fac)) +
  geom_line(stat = "identity", lwd = 1.5) + geom_point(stat = "identity", size = 4) +
  geom_errorbar(mapping = aes(ymax = upper, ymin = lower), lwd = 1.5, width = .1) +
  labs(title = "Figure 10. Logistic Regression for Chen (2020) Sample",
       subtitle = "Accept ~ Initial Tone x Group", x = "Initial Tone", 
       caption = "Error bars = SE",
       y = "Fitted Probability of Acceptance", color = "Group") + theme_bw()