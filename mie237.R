# MIE237 Final Project R Code - Mental Rotation Analysis
# Baron Liu, Ian Fong, Arya Lum

library(tidyverse)
library(emmeans)

# 1. load and prep data for ANOVA analysis

# read dataset
all_data <- read_csv("full_dataset.csv", show_col_types = FALSE)

# set up factors 
all_data <- all_data %>%
  mutate(
    participant_id = factor(participant_id),
    rotation_angle = factor(rotation_angle, levels = c(0, 90, 180, 270)),
    sex = factor(sex),
    gaming_group = factor(gaming_group)
  )

glimpse(all_data)

# 2. aggregate to participant-level means
# anova needs one value per person per condition
# so average each participant's 8 trials at each angle

summ <- all_data %>%
  group_by(participant_id, rotation_angle, sex, gaming_group, gaming_hrs_week) %>%
  summarize(
    mean_rt = mean(rt_ms),
    mean_acc = mean(correct),
    .groups = "drop"
  )

# 3. descriptive statistics

# rt by angle
summ %>%
  group_by(rotation_angle) %>%
  summarize(
    n = n(),
    mean = mean(mean_rt),
    sd = sd(mean_rt),
    median = median(mean_rt),
    .groups = "drop"
  )

# accuracy by angle
summ %>%
  group_by(rotation_angle) %>%
  summarize(
    n = n(),
    mean = mean(mean_acc),
    sd = sd(mean_acc),
    median = median(mean_acc),
    .groups = "drop"
  )

# by sex
summ %>%
  group_by(rotation_angle, sex) %>%
  summarize(mean_rt = mean(mean_rt), mean_acc = mean(mean_acc), .groups = "drop")

# by gaming group
summ %>%
  group_by(rotation_angle, gaming_group) %>%
  summarize(mean_rt = mean(mean_rt), mean_acc = mean(mean_acc), .groups = "drop")

# 4. descriptive plots 

# boxplots of RT by angle
ggplot(summ, aes(x = rotation_angle, y = mean_rt)) +
  geom_boxplot(fill = "lightblue") +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(title = "Reaction Time by Rotation Angle",
       x = "Rotation Angle (degrees)",
       y = "Mean RT (ms)") +
  theme_minimal()

# boxplot of accuracy by angle
ggplot(summ, aes(x = rotation_angle, y = mean_acc)) +
  geom_boxplot(fill = "lightgreen") +
  geom_jitter(width = 0.1, alpha = 0.5) +
  labs(title = "Accuracy by Rotation Angle",
       x = "Rotation Angle (degrees)",
       y = "Mean Accuracy (prop. correct)") +
  theme_minimal()

# line plots with error bars (mean +/- SE)
rt_summary <- summ %>%
  group_by(rotation_angle) %>%
  summarize(mean = mean(mean_rt),
            se = sd(mean_rt) / sqrt(n()),
            .groups = "drop") %>%
  mutate(angle_num = as.numeric(as.character(rotation_angle)))

ggplot(rt_summary, aes(x = angle_num, y = mean)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 10) +
  labs(title = "Mean RT Across Rotation Angles (Mean +/- 1 SE)",
       x = "Rotation Angle (degrees)", y = "Mean RT (ms)") +
  theme_minimal()

acc_summary <- summ %>%
  group_by(rotation_angle) %>%
  summarize(mean = mean(mean_acc),
            se = sd(mean_acc) / sqrt(n()),
            .groups = "drop") %>%
  mutate(angle_num = as.numeric(as.character(rotation_angle)))

ggplot(acc_summary, aes(x = angle_num, y = mean)) +
  geom_point(size = 3) +
  geom_line() +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 10) +
  labs(title = "Mean Accuracy Across Rotation Angles (Mean +/- 1 SE)",
       x = "Rotation Angle (degrees)", y = "Mean Accuracy") +
  theme_minimal()

# boxplots split by sex (lab 2 faceting approach)
ggplot(summ, aes(x = rotation_angle, y = mean_rt)) +
  geom_boxplot(fill = "lightblue") +
  facet_wrap(~ sex) +
  labs(title = "RT by Angle - Split by Sex",
       x = "Rotation Angle", y = "Mean RT (ms)") +
  theme_minimal()

# boxplots split by gaming group
ggplot(summ, aes(x = rotation_angle, y = mean_rt)) +
  geom_boxplot(fill = "lightyellow") +
  facet_wrap(~ gaming_group) +
  labs(title = "RT by Angle - Split by Gaming Group",
       x = "Rotation Angle", y = "Mean RT (ms)") +
  theme_minimal()

# histograms of RT at each angle
ggplot(summ, aes(x = mean_rt)) +
  geom_histogram(bins = 8, fill = "lightblue", color = "black") +
  facet_wrap(~ rotation_angle) +
  labs(title = "Distribution of RT at Each Angle",
       x = "Mean RT (ms)", y = "Count") +
  theme_minimal()

# 5. blocked (within-subjects) ANOVA (lab 10)
# each participant did all 4 angles, so participant is a block
# aov(accuracy ~ condition + participant)

# --- RT ---
rt_aov <- aov(mean_rt ~ rotation_angle + participant_id, data = summ)
summary(rt_aov)

# --- Accuracy ---
acc_aov <- aov(mean_acc ~ rotation_angle + participant_id, data = summ)
summary(acc_aov)

# 6. model diagnostics (labs 7, 10, 11)
# plot(model) gives the 4 standard diagnostic plots:
#   1. residuals vs fitted - check for constant variance / patterns
#   2. QQ plot - check normality of residuals
#   3. scale-location - another variance check
#   4. residuals vs leverage - influential points

# RT diagnostics
par(mfrow = c(2, 2))
plot(rt_aov)
par(mfrow = c(1, 1))

# accuracy diagnostics
par(mfrow = c(2, 2))
plot(acc_aov)
par(mfrow = c(1, 1))

# ggplot QQ plots
rt_residuals <- tibble(resid = residuals(rt_aov))

ggplot(rt_residuals, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "RT: Normal Q-Q Plot of Residuals",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

acc_residuals <- tibble(resid = residuals(acc_aov))

ggplot(acc_residuals, aes(sample = resid)) +
  stat_qq() +
  stat_qq_line(color = "red") +
  labs(title = "Accuracy: Normal Q-Q Plot of Residuals",
       x = "Theoretical Quantiles", y = "Sample Quantiles") +
  theme_minimal()

# histogram of residuals
ggplot(rt_residuals, aes(x = resid)) +
  geom_histogram(bins = 12, fill = "lightblue", color = "black") +
  labs(title = "RT: Histogram of Residuals", x = "Residuals", y = "Count") +
  theme_minimal()

ggplot(acc_residuals, aes(x = resid)) +
  geom_histogram(bins = 12, fill = "lightgreen", color = "black") +
  labs(title = "Accuracy: Histogram of Residuals", x = "Residuals", y = "Count") +
  theme_minimal()

# 7. post-hoc pairwise comparisons using emmeans with Tukey adjustment

emm_rt <- emmeans(rt_aov, ~ rotation_angle)
pairs(emm_rt, adjust = "tukey")

emm_acc <- emmeans(acc_aov, ~ rotation_angle)
pairs(emm_acc, adjust = "tukey")

# 8. secondary blocked two-factor ANOVAs with participant-level variables
# rotation_angle is the within-subject factor
# sex and gaming_group are participant-level categorical variables
# participant_id is included as the blocking factor

# sex x rotation angle
fit_sex <- aov(mean_rt ~ rotation_angle * sex + participant_id, data = summ)
summary(fit_sex)

fit_sex_acc <- aov(mean_acc ~ rotation_angle * sex + participant_id, data = summ)
summary(fit_sex_acc)

# gaming x rotation angle
fit_game <- aov(mean_rt ~ rotation_angle * gaming_group + participant_id, data = summ)
summary(fit_game)

fit_game_acc <- aov(mean_acc ~ rotation_angle * gaming_group + participant_id, data = summ)
summary(fit_game_acc)

# 9. interaction plots 

# sex x angle: RT
ggplot(summ, aes(x = rotation_angle, y = mean_rt, colour = sex)) +
  geom_jitter(width = 0.08, alpha = 0.4, size = 2) +
  stat_summary(fun = mean, geom = "point", size = 4) +
  stat_summary(fun = mean, geom = "line", aes(group = sex), linewidth = 1.2) +
  labs(title = "Interaction: Sex x Angle (RT)",
       x = "Rotation Angle", y = "Mean RT (ms)") +
  theme_classic(base_size = 14)

# sex x angle: accuracy
ggplot(summ, aes(x = rotation_angle, y = mean_acc, colour = sex)) +
  geom_jitter(width = 0.08, alpha = 0.4, size = 2) +
  stat_summary(fun = mean, geom = "point", size = 4) +
  stat_summary(fun = mean, geom = "line", aes(group = sex), linewidth = 1.2) +
  labs(title = "Interaction: Sex x Angle (Accuracy)",
       x = "Rotation Angle", y = "Mean Accuracy") +
  theme_classic(base_size = 14)

# gaming x angle: RT
ggplot(summ, aes(x = rotation_angle, y = mean_rt, colour = gaming_group)) +
  geom_jitter(width = 0.08, alpha = 0.4, size = 2) +
  stat_summary(fun = mean, geom = "point", size = 4) +
  stat_summary(fun = mean, geom = "line", aes(group = gaming_group), linewidth = 1.2) +
  labs(title = "Interaction: Gaming x Angle (RT)",
       x = "Rotation Angle", y = "Mean RT (ms)") +
  theme_classic(base_size = 14)

# gaming x angle: accuracy
ggplot(summ, aes(x = rotation_angle, y = mean_acc, colour = gaming_group)) +
  geom_jitter(width = 0.08, alpha = 0.4, size = 2) +
  stat_summary(fun = mean, geom = "point", size = 4) +
  stat_summary(fun = mean, geom = "line", aes(group = gaming_group), linewidth = 1.2) +
  labs(title = "Interaction: Gaming x Angle (Accuracy)",
       x = "Rotation Angle", y = "Mean Accuracy") +
  theme_classic(base_size = 14)

# simple effects if interaction is significant (lab 11)
emm_sex_rt <- emmeans(fit_sex, ~ rotation_angle | sex)
pairs(emm_sex_rt, adjust = "tukey")

emm_game_rt <- emmeans(fit_game, ~ rotation_angle | gaming_group)
pairs(emm_game_rt, adjust = "tukey")
