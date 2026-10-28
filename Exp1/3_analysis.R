# 2025-07-10 
# need exclusions list from Sara and to decide on criteria for exclusions. 

# 2025-07-25
# Add Sara's exclusions. 
# Import Task 2 and 3 data as well, begin Task 2 analysis script. 

# 2025-07-29
# Set up analysis task 2 and task 3. 
# Pause until data is collected then complete data analysis. 

# 2025-11-02 
# Data are in, updating analyses. 

# 2026-05-09
# Add Task 3 d-prime and lapse rate analyses to mirror Task 2.

# Libraries 

library(tidyverse)
library(lme4)
library(ggeffects)
library(stringr)
library(emmeans)
library(readxl)
library(ggpubr)



# Read data ---------------------------------------------------------------

df_task1_nozones <- read_csv("1_data/ASOP_Task1.csv") 
df_task1 <- read_csv("1_data/Task1_with_zones_withpreceding.csv") 
df_task2 <- read_csv("1_data/ASOP_Task2.csv") 
df_task3 <- read_csv("1_data/ASOP_Task3.csv") 

oa_demo <- read_csv("1_data/ASOP - OA Demographics Tracking - Demographics.csv")
ya_demo <- read_csv("1_data/ASOP - YA Demographics Tracking - Demographics.csv")

low_moca <- oa_demo %>% 
  filter(MoCA_Adjusted < 23) %>% 
  select(`Participant ID`) 
  

# Exclusions --------------------------------------------------------------
#beyond age range: 103, 156
#missed responses on 15.86% of trials in Task1 compared to average (<0.005%): 105
#technical error: 110
#missing output: 332
#read debrief from friend before coming in: 369

naughty_list <- unique(c(103, 156, 105, 110, 332, 369,
                  low_moca %>% pull(`Participant ID`)))

df_task1 <- df_task1 %>% 
  filter(!participant %in% naughty_list)

df_task1_nozones <- df_task1_nozones %>% 
  filter(!participant %in% naughty_list)

participants <- df_task1 %>% 
  group_by(participant, age_group) %>% 
  summarize(n = n())

participants %>% 
  group_by(age_group) %>% 
  summarize(n = n())


# TASK 1 ------------------------------------------------------------------

# check if any participants pressed the same key the whole time. 
key_press <- df_task1_nozones %>% 
  group_by(participant) %>% 
  count(participant_response)

ggplot(key_press, aes(x = participant_response, y = n)) +
  geom_col(position = "dodge") +
  geom_jitter()+
  labs(x = "Key Pressed", y = "Count") +
  theme_minimal()

# Get group means and sd for accuracy and RT on task 1 
accuracy <- df_task1 %>% 
  group_by(participant, age_group) %>% 
  summarize(acc = mean(acc_mean)) 

accuracy %>% 
  group_by(age_group) %>% 
  summarize(min = min(acc),
            max = max(acc),
            mean = mean(acc),
            sd = sd(acc))

t.test(acc ~ age_group, data = accuracy)

accuracy_plot_encoding <- ggplot(accuracy, aes(x = age_group, y = acc, fill = age_group)) +
  geom_violin() +
  geom_boxplot() +
  geom_jitter() +
  labs(x = "", y = "Proportion Correct", title = "...but just as accurate") +
  ylim(.925, 1) +
  theme_minimal() +
  scale_fill_manual(
    values = c("OA" = "#e8929a", "YA" = "#6ab0d4"),
    labels = c("OA" = "Older Adults", "YA" = "Younger Adults")
  ) +
  guides(fill = guide_legend(title = "Age Group")) +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14, face = "bold")
  )
accuracy_plot_encoding
#ggsave(filename = "accuracy_encoding.jpg", plot = accuracy_plot_encoding, dpi = 3000)


rt <- df_task1 %>% 
  group_by(participant, age_group) %>% 
  summarize(rt = mean(rt, na.rm = TRUE))

rt_plot_encoding <- ggplot(rt, aes(x = age_group, y = rt, fill = age_group)) +
  geom_violin() +
  geom_boxplot() +
  geom_jitter() +
  labs(x = "", y = "Reaction time (ms)", fill = "", title = "Older adults were slower, p < 0.05") +
  theme_minimal()+
  theme_minimal() +
  scale_fill_manual(
    values = c("OA" = "#e8929a", "YA" = "#6ab0d4"),
    labels = c("OA" = "Older Adults", "YA" = "Younger Adults")
  ) +
  guides(fill = guide_legend(title = "Age Group"))+
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14, face = "bold")
  )
#ggsave(filename = "rt_encoding.jpg", plot = rt_plot_encoding, dpi = 3000)

t.test(rt ~ age_group, data = rt)


# speedsters 
speedsters <- rt %>% 
  filter(rt < 400)

speedsters_alldat <- df_task1 %>% 
  filter(participant %in% speedsters$participant)

speedsters_alldat %>% 
  group_by(participant) %>% 
  summarize(min = min(rt),
            max = max(rt),
            mean = mean(rt),
            sd = sd(rt),
            av_acc = mean(accuracy))
# high accuracy despite how quickly they answered. 

ggplot(speedsters_alldat, aes(x = as.factor(participant), y = rt)) +
  geom_violin() +
  geom_boxplot() +
  geom_jitter()+
  labs(x = "Participant num", y = "RT") +
  theme_minimal()

rt_group_av <- rt %>% 
  group_by(age_group) %>% 
  summarize(min = min(rt),
            max = max(rt),
            mean = mean(rt),
            sd = sd(rt))


# COMPARISON OF SARAH VS. DECKER METHOD ----------------------------------

df_task1 <- df_task1 %>% 
  mutate(age_group_dummy = ifelse(age_group == "YA", -0.5, 0.5),
         frequecy_dummy = ifelse(frequency == "infrequent", -0.5, 0.5))

# Helper to build and compare a glmer model sequence
run_glmer_sequence <- function(predictor, data, interaction_term = NULL) {
  f0 <- glmer(accuracy ~ (1 | participant), data = data, family = "binomial")
  f1 <- glmer(as.formula(paste("accuracy ~", predictor, "+ (1 | participant)")), data = data, family = "binomial")
  f2 <- glmer(as.formula(paste("accuracy ~", predictor, "+ age_group_dummy + (1 | participant)")), data = data, family = "binomial")
  f3 <- glmer(as.formula(paste("accuracy ~", predictor, "* age_group_dummy + (1 | participant)")), data = data, family = "binomial")
  list(m0 = f0, m1 = f1, m2 = f2, m3 = f3)
}

# Does rtcv predict accuracy?
rtcv_models <- run_glmer_sequence("rtcv", df_task1)
anova(rtcv_models$m0, rtcv_models$m1, rtcv_models$m2, rtcv_models$m3)
summary(rtcv_models$m1)
# YA = -0.5 
plot(ggpredict(rtcv_models$m2, terms = c("rtcv [all]", "age_group_dummy")))
# Higher RT variability predicts worse task accuracy in both age groups. 

# Do Sarah's states predict task accuracy?
sarah_models <- run_glmer_sequence("sarah_state", df_task1)
anova(sarah_models$m0, sarah_models$m1, sarah_models$m2, sarah_models$m3)
summary(sarah_models$m3)
plot(ggpredict(sarah_models$m3, terms = c("sarah_state [0,1]", "age_group_dummy")))

em_sarah <- emmeans(sarah_models$m3, ~ sarah_state | age_group_dummy, type = "response")
contrast(em_sarah, "pairwise", adjust = "none")

# Stimulus effects 

m_base    <- glmer(accuracy ~ sarah_state * age_group_dummy + (1 | participant), 
                   data = df_task1, family = "binomial")

m_stim_i  <- glmer(accuracy ~ sarah_state * age_group_dummy + 
                     (1 | participant) + (1 | scene), 
                   data = df_task1, family = "binomial")

m_stim_s  <- glmer(accuracy ~ sarah_state * age_group_dummy + 
                     (1 | participant) + (sarah_state | scene), 
                   data = df_task1, family = "binomial")

anova(m_base, m_stim_i, m_stim_s)


## CONFERENCE PLOT 

ggpredict(sarah_models$m3, terms = c("sarah_state [0, 1]", "age_group_dummy")) %>%
  as.data.frame() %>%
  ggplot(aes(x = factor(x), y = predicted, color = group, group = group)) +
  geom_point(size = 3) +
 # geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.1) +
  geom_line(linewidth = .8) +
  scale_color_manual(
    values = c("-0.5" = "#e8929a", "0.5" = "#6ab0d4"),
    labels = c("-0.5" = "Older Adults", "0.5" = "Younger Adults")
  ) +
  scale_x_discrete(labels = c("0" = "In the Zone", "1" = "Out of the Zone")) +
  labs(x = "", y = "Predicted Accuracy", color = "Age Group", title = "Attentional state predicts categorization accuracy") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )
#ggsave(filename = "predicted_accuracy.jpg", plot = last_plot(), dpi = 3000)


# Do Decker's zones predict task accuracy?
decker_models <- run_glmer_sequence("decker_zone", df_task1)
anova(decker_models$m1, decker_models$m2, decker_models$m3)
summary(decker_models$m3)
plot(ggpredict(decker_models$m3, terms = c("decker_zone", "age_group_dummy")))

em_decker <- emmeans(decker_models$m3, ~ decker_zone | age_group_dummy, type = "response")
contrast(em_decker, "pairwise", adjust = "none")

#anova(decker_models$m3, sarah_models$m3)

# Calculate Lapse Rate ----------------------------------------------------

lapse_rates <- df_task1 %>% 
  group_by(participant, age_group, acc_mean, RT_mean, RT_sd, rtcv, frequency) %>% 
  summarise(out_state = sum(sarah_state, na.rm = TRUE),
            out_zone = sum(decker_zone, na.rm = TRUE),
            trials = n(),
            prop_zone = out_zone/trials,
            prop_state = out_state/trials) %>% 
  mutate(age_group_dummy = ifelse(age_group == "YA", -0.5, 0.5),
         frequecy_dummy = ifelse(frequency == "infrequent", -0.5, 0.5),
         prop_zone_c = prop_zone - mean(prop_zone, na.rm = TRUE),
         prop_state_c = prop_state - mean(prop_state, na.rm = TRUE))

prop_zone <- ggplot(lapse_rates, aes(x = age_group, y = prop_zone)) + 
  geom_violin() +
  geom_boxplot() +
  geom_jitter()+
  labs(x = "Age Group", y = "decker_zone") +
  theme_minimal()

prop_state <- ggplot(lapse_rates, aes(x = age_group, y = prop_state, fill = age_group)) + 
  geom_violin() +
  geom_boxplot() +
  geom_jitter() +
  labs(x = "Age Group", y = "Proportion of Attentional Lapses", title = "No age difference in lapse rates") +
  scale_fill_manual(values = c("OA" = "#e8929a", "YA" = "#6ab0d4")) +
  theme_minimal() +
  theme(legend.position = "none") +
  theme(
  plot.title = element_text(size = 18, face = "bold"),
  axis.title = element_text(size = 14, face = "bold"),
  axis.text = element_text(size = 14, face = "bold"),
  legend.title = element_text(size = 14, face = "bold"),
  legend.text = element_text(size = 12)
)
ggsave(filename = "state_age_diff.jpg", plot = last_plot(), dpi = 3000)

ggarrange(prop_zone, prop_state)

# Correlations with RT variability and mean 
for (prop_var in c("prop_zone", "prop_state")) {
  for (rt_var in c("RT_sd", "rtcv", "RT_mean")) {
    cat(prop_var, "~", rt_var, "\n")
    print(cor.test(lapse_rates[[prop_var]], lapse_rates[[rt_var]], method = "pearson"))
  }
}

# Age group t-tests
for (var in c("RT_mean", "prop_zone", "prop_state")) {
  cat("\n--- t.test:", var, "~ age_group ---\n")
  print(t.test(as.formula(paste(var, "~ age_group")), data = lapse_rates, var.equal = TRUE))
}


# Streak metrics ----------------------------------------------------------

# Helper to compute lapse streak metrics for a given state column
compute_streak_metrics <- function(data, state_col) {
  data %>%
    group_by(participant) %>%
    mutate(
      streak_id = with(rle(.data[[state_col]]), rep(seq_along(lengths), lengths))
    ) %>%
    ungroup() %>%
    group_by(participant, streak_id) %>%
    summarise(
      state = first(.data[[state_col]]),
      streak_length = n(),
      .groups = "drop"
    ) %>%
    filter(state == 1) %>%
    group_by(participant) %>%
    summarise(
      num_lapses = n(),
      max_streak = max(streak_length, na.rm = TRUE),
      median_streak = median(streak_length, na.rm = TRUE),
      .groups = "drop"
    )
}

df_lapse_metrics   <- compute_streak_metrics(df_task1, "sarah_state")
df_lapse_metrics_d <- compute_streak_metrics(df_task1, "decker_zone")

# Helper to summarise age differences in lapse metrics
summarise_age_diff <- function(data, lapse_metrics, state_col) {
  age_diff <- data %>%
    group_by(participant, age_group, acc_mean) %>%
    summarise(out_state = sum(.data[[state_col]], na.rm = TRUE),
              trials = n(),
              lapse_rate = out_state/trials,
              rt_sd = mean(RT_sd),
              rtcv = mean(rtcv),
            #  median_streak = mean(median_streak),
            #  max_streak = mean(max_streak),
              .groups = "drop")
  left_join(age_diff, lapse_metrics, by = "participant")
}

#df_task1_lapse_metrics   <- left_join(df_task1, df_lapse_metrics,   by = "participant")
#df_task1_lapse_metrics_d <- left_join(df_task1, df_lapse_metrics_d, by = "participant")

age_diff_zones   <- summarise_age_diff(df_task1,   df_lapse_metrics,   "sarah_state")
age_diff_zones_d <- summarise_age_diff(df_task1, df_lapse_metrics_d, "decker_zone")

# t-tests for age differences in lapse metrics
lapse_vars <- c("lapse_rate", "num_lapses", "median_streak")

cat("--- Sarah method ---\n")
for (var in lapse_vars) {
  cat("\n", var, "\n")
  print(t.test(as.formula(paste(var, "~ age_group")), data = age_diff_zones, var.equal = TRUE))
}

cat("\n--- Decker method ---\n")
for (var in lapse_vars) {
  cat("\n", var, "\n")
  print(t.test(as.formula(paste(var, "~ age_group")), data = age_diff_zones_d, var.equal = TRUE))
}


# Analyses from Decker paper (with frequency) -----------------------------

# Helper to build and compare frequency-inclusive glmer sequence
run_freq_glmer_sequence <- function(predictor, data) {
  f0 <- glmer(accuracy ~ (1 | participant), data = data, family = "binomial")
  f1 <- glmer(as.formula(paste("accuracy ~", predictor, "+ (1 | participant)")), data = data, family = "binomial")
  f2 <- glmer(as.formula(paste("accuracy ~", predictor, "+ age_group_dummy + (1 | participant)")), data = data, family = "binomial")
  f3 <- glmer(as.formula(paste("accuracy ~", predictor, "+ age_group_dummy + frequency + (1 | participant)")), data = data, family = "binomial")
  f4 <- glmer(as.formula(paste("accuracy ~", predictor, "* age_group_dummy * frequency + (1 | participant)")), data = data, family = "binomial")
  list(m0 = f0, m1 = f1, m2 = f2, m3 = f3, m4 = f4)
}

decker_freq_models <- run_freq_glmer_sequence("decker_zone", df_task1)
anova(decker_freq_models$m0, decker_freq_models$m1, decker_freq_models$m2, decker_freq_models$m3, decker_freq_models$m4)
summary(decker_freq_models$m4)
plot(ggpredict(decker_freq_models$m4, terms = c("frequency", "age_group_dummy")))
plot(ggpredict(decker_freq_models$m4, terms = c("decker_zone")))
plot(ggpredict(decker_freq_models$m4, terms = c("decker_zone", "age_group_dummy")))

sarah_freq_models <- run_freq_glmer_sequence("sarah_state", df_task1)
anova(sarah_freq_models$m0, sarah_freq_models$m1, sarah_freq_models$m2, sarah_freq_models$m3, sarah_freq_models$m4)
summary(sarah_freq_models$m4)
plot(ggpredict(sarah_freq_models$m4, terms = c("frequency", "age_group_dummy")))
plot(ggpredict(sarah_freq_models$m4, terms = c("sarah_state")))
plot(ggpredict(sarah_freq_models$m4, terms = c("sarah_state", "age_group_dummy")))


# TASK 2  -----------------------------------------------------------------

df_task2_mod <- df_task2 %>% 
  filter(!participant %in% naughty_list) %>% 
  mutate(sensitivity = case_when(background_condition == "old" & resp_2choice_background == "old" ~ "hit",
                            background_condition == "old" & resp_2choice_background == "new" ~ "miss",
                            background_condition == "new" & resp_2choice_background == "old" ~ "fa",
                            background_condition == "new" & resp_2choice_background == "new" ~ "corr_rej"), 
         rt_background = rt_background*1000) 

# Check data 
participants_t2 <- df_task2_mod %>% 
  group_by(participant, age_group) %>% 
  summarize(n = n())

participants_t2 %>% 
  group_by(age_group) %>% 
  summarize(n = n())

# Same key for entire task? 
key_press <- df_task2_mod %>% 
  group_by(participant) %>% 
  count(participant_response_background) %>% 
  mutate(prop = n/210)

# perfect memory for old pairs would be 80% 'a'
# perfect memory for new pairs would be 20% 'l'

same_key_press_90 <- key_press %>% 
  filter(prop >= .90)

df_task2_mod_90 <- df_task2_mod %>% 
  filter(participant %in% same_key_press_90$participant)

participants_t2 %>% 
  filter(participant %in% same_key_press_90$participant) %>% 
  group_by(age_group) %>% 
  summarize(n = n())

# Check for participants responding too speedy. 
rt_acc <- df_task2_mod %>% 
  group_by(participant, age_group) %>% 
  summarise(mean_rt = mean(rt_background),
            min_rt = min(rt_background),
            max_rt = max(rt_background),
            sd_rt = sd(rt_background)) %>% 
  group_by(age_group) %>% 
  mutate(outlier_value = mean(mean_rt) - 3*sd(mean_rt),
         outlier = ifelse(mean_rt < outlier_value, 1, 0))

hist(rt_acc$mean_rt, breaks = 50)
rt_acc %>% filter(outlier == 1)

low_rt <- df_task2_mod %>% 
  filter(rt_background < 200)

df_task2_mod %>% 
  filter(participant %in% low_rt$participant) %>%
  ggplot(aes(rt_background))+
  geom_histogram(bins = 200)+
  geom_vline(xintercept = 500, color = 'red')+
  facet_grid(~participant)

# Exclude trials < 200ms and add zone variables
df_task2_mod_200rm <- df_task2_mod %>% 
  group_by(participant) %>% 
  mutate(trials_200rm = n(),
         trials_old = sum(background_condition == 'old'),
         trials_new = trials_200rm - trials_old)

zones <- df_task1_lapse_metrics %>% 
  select(participant, scene, object, sarah_state, decker_zone, num_lapses, median_streak, preceding_rtcv, preceding_mean_abs_deviance_scaled)

df_task2_mod_200rm_zones <- left_join(df_task2_mod_200rm, zones, by = c("participant", "background" = "scene"))

# Wide format and d-prime
df_task2_wide <- df_task2_mod_200rm_zones %>%
  count(participant, age_group, trials_old, trials_new, sensitivity, resp_6choice_background) %>%
  mutate(col_name = paste(sensitivity, resp_6choice_background, sep = "_")) %>%
  select(-sensitivity, -resp_6choice_background) %>%
  pivot_wider(names_from = col_name, values_from = n, values_fill = 0)

df_task2_wide_hitfa <- df_task2_wide %>% 
  mutate(hits = (hit_probably + hit_sure)/trials_old,
         false_alarm = (fa_probably + fa_sure)/trials_new,
         hits_corr = ifelse(hits == 1, (trials_old - 0.5)/trials_old, hits),
         hits_corr = ifelse(hits_corr == 0, 0.05, hits_corr),
         false_alarm_corr = ifelse(false_alarm == 0, 0.5/trials_new, false_alarm),
         dprime = qnorm(hits_corr) - qnorm(false_alarm_corr))

ggplot(df_task2_wide_hitfa, aes(age_group, dprime)) +
  geom_violin() +
  geom_boxplot() +
  geom_jitter()+
  labs(x = "dprime", y = "d'") +
  theme_minimal()

## CONFERENCE
df_task2_wide_hitfa %>%
  pivot_longer(cols = c(hits_corr, false_alarm_corr), 
               names_to = "measure", 
               values_to = "value") %>%
  mutate(measure = fct_relevel(measure, "hits_corr", "false_alarm_corr")) %>%
  ggplot(aes(x = age_group, y = value, fill = measure)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(0.8), width = 0.7) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(0.8), width = 0.2) +
  scale_fill_manual(
    values = c("hits_corr" = "#4a9e8f", "false_alarm_corr" = "#E69F52"),
    labels = c("hits_corr" = "Hits", "false_alarm_corr" = "False Alarms")
  ) +
  scale_x_discrete(labels = c("OA" = "Older Adults", "YA" = "Younger Adults")) +
  labs(x = "", y = "Proportion", fill = "") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )
ggsave(filename = "Hits_False_alarm_task2.jpg", plot = last_plot(), dpi = 3000)


# Age differences in d-prime
t.test(dprime ~ age_group, data = df_task2_wide_hitfa, var.equal = TRUE)

# Do lapse rates predict background memory?
df_task2_wide_hitfa_lapse <- left_join(df_task2_wide_hitfa, age_diff_zones, by = c("participant", "age_group")) %>% 
  mutate(lapse_rate_c = scale(lapse_rate, scale = FALSE))

t2_1 <- lm(dprime ~ lapse_rate, data = df_task2_wide_hitfa_lapse)
t2_2 <- lm(dprime ~ lapse_rate + age_group, data = df_task2_wide_hitfa_lapse)
t2_3 <- lm(dprime ~ lapse_rate * age_group, data = df_task2_wide_hitfa_lapse)

anova(t2_1, t2_2, t2_3)
summary(t2_3)


emtrends(t2_3, pairwise ~ age_group, var = "lapse_rate")
emtrends(t2_3, ~ age_group, var = "lapse_rate", infer = TRUE)

ggplot(df_task2_wide_hitfa_lapse, aes(lapse_rate, dprime, color = age_group)) +
  geom_smooth(method = "lm") +
  geom_jitter() +
  scale_color_manual(
    values = c("OA" = "#e8929a", "YA" = "#6ab0d4"),
    labels = c("OA" = "Older Adults", "YA" = "Younger Adults")
  ) +
  labs(x = "Lapse Rate", y = "d-prime", color = "Age Group", title = "Lapse Rate Predicts Distractor Memory") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )
ggsave(filename = "Distractor_Memory_lapse.jpg", plot = last_plot(), dpi = 3000)


summary(lm(dprime ~ num_lapses * age_group, data = df_task2_wide_hitfa_lapse))
ggplot(df_task2_wide_hitfa_lapse, aes(num_lapses, dprime, color = age_group))+
  geom_smooth(method = "lm")+
  geom_jitter()

summary(lm(dprime ~ median_streak * age_group, data = df_task2_wide_hitfa_lapse))

# Do Sarah's states / Decker's zones predict background accuracy?
df_task2_mod_200rm_zones_only <- df_task2_mod_200rm_zones %>% 
  drop_na(sarah_state, decker_zone)

for (predictor in c("sarah_state", "decker_zone")) {
  cat("\n--- background accuracy:", predictor, "---\n")
  models <- run_glmer_sequence(predictor, df_task2_mod_200rm_zones_only)
  print(anova(models$m0, models$m1, models$m2, models$m3))
  print(summary(models$m1))
  print(summary(models$m2))
  print(plot(ggpredict(models$m3, terms = c(predictor, "age_group"))))
}

# Do Decker's states predict cued-recall object accuracy?
d0 <- glmer(background_accuracy_s_p ~ (1 | participant), data = df_task2_mod_200rm_zones_only, family = "binomial")
d1 <- glmer(background_accuracy_s_p ~ preceding_mean_abs_deviance_scaled + (1 | participant), data = df_task2_mod_200rm_zones_only, family = "binomial")
d2 <- glmer(background_accuracy_s_p ~ preceding_mean_abs_deviance_scaled + age_group + (1 | participant), data = df_task2_mod_200rm_zones_only, family = "binomial")
d3 <- glmer(background_accuracy_s_p ~ preceding_mean_abs_deviance_scaled*age_group + (1 | participant), data = df_task2_mod_200rm_zones_only, family = "binomial")

anova(d0, d1, d2, d3)
summary(d3)
plot(ggpredict(d3, terms = c("preceding_mean_abs_deviance_scaled", "age_group")))


# Do Sarah's state predict 
s0 <- glmer(background_accuracy_s_p ~ (1 | participant), data = df_task2_mod_200rm_zones_only, family = "binomial")
s1 <- glmer(background_accuracy_s_p ~ sarah_state + (1 | participant), data = df_task2_mod_200rm_zones_only, family = "binomial")
s2 <- glmer(background_accuracy_s_p ~ sarah_state + age_group + (1 | participant), data = df_task2_mod_200rm_zones_only, family = "binomial")
s3 <- glmer(background_accuracy_s_p ~ sarah_state*age_group + (1 | participant), data = df_task2_mod_200rm_zones_only, family = "binomial")

anova(s0, s1, s2, s3)
summary(s3)
plot(ggpredict(s3, terms = c("sarah_state", "age_group")))


# Do Sarah's preceding state predict 
o0 <- glmer(background_accuracy_s_p ~ (1 | participant), data = df_task2_mod_200rm_zones_only, family = "binomial")
o1 <- glmer(background_accuracy_s_p ~ preceding_rtcv + (1 | participant), data = df_task2_mod_200rm_zones_only, family = "binomial")
o2 <- glmer(background_accuracy_s_p ~ preceding_rtcv + age_group + (1 | participant), data = df_task2_mod_200rm_zones_only, family = "binomial")
o3 <- glmer(background_accuracy_s_p ~ preceding_rtcv*age_group + (1 | participant), data = df_task2_mod_200rm_zones_only, family = "binomial")

anova(o0, o1, o2, o3)
summary(o3)
plot(ggpredict(o3, terms = c("preceding_rtcv", "age_group")))

## Scenes ?
# Add scene random intercept to best-fitting model
o3_scene <- glmer(background_accuracy_s_p ~ preceding_rtcv * age_group + 
                    (1 | participant) + (1 | background), 
                  data = df_task2_mod_200rm_zones_only, family = "binomial")

# Does the preceding_rtcv effect vary across scenes?
o3_scene_slope <- glmer(background_accuracy_s_p ~ preceding_rtcv * age_group + 
                          (1 | participant) + (preceding_rtcv | background), 
                        data = df_task2_mod_200rm_zones_only, family = "binomial")

anova(o3, o3_scene, o3_scene_slope)
summary(o3_scene)
plot(ggpredict(o3_scene, terms = c("preceding_rtcv [all]", "age_group")))

## CONFERENCE PLOT 
ggpredict(o3, terms = c("preceding_rtcv [all]", "age_group")) %>%
  as.data.frame() %>%
  ggplot(aes(x = x, y = predicted, color = group, group = group)) +
  geom_line(linewidth = 1.2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.15, color = NA) +
  scale_color_manual(
    values = c("OA" = "#e8929a", "YA" = "#6ab0d4"),
    labels = c("OA" = "Older Adults", "YA" = "Younger Adults")
  ) +
  scale_fill_manual(
    values = c("OA" = "#e8929a", "YA" = "#6ab0d4"),
    guide = "none"
  ) +
  labs(x = "Preceding RTCV", y = "Predicted Accuracy", color = "Age Group",
       title = "Trial-by-trial attention does not predict distractor accuracy") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )
ggsave(filename = "predicted_accuracy_o3.jpg", plot = last_plot(), dpi = 3000)



# TASK 3 - Target Object Recognition --------------------------------------

df_task3_mod <- df_task3 %>% 
  filter(!participant %in% naughty_list) %>% 
  mutate(sensitivity = case_when(object_condition == "old" & resp_2choice == "old" ~ "hit",
                                 object_condition == "old" & resp_2choice == "new" ~ "miss",
                                 object_condition == "new" & resp_2choice == "old" ~ "fa",
                                 object_condition == "new" & resp_2choice == "new" ~ "corr_rej"),
         object_accuracy_s_p_g = case_when(object_condition == "old" & resp_2choice == "old" ~ 1,
                                           object_condition == "old" & resp_2choice == "new" ~ 0,
                                           object_condition == "new" & resp_2choice == "new" ~ 1,
                                           object_condition == "new" & resp_2choice == "old" ~ 0),
         object_accuracy_s_p = ifelse(object_accuracy_s_p_g == 1 & resp_6choice == "guess", 0, object_accuracy_s_p_g),
         age_group = ifelse(participant > 200, "OA", "YA"))

# Same key for entire task? 
key_press <- df_task3_mod %>% 
  group_by(participant) %>% 
  count(participant_response) %>% 
  mutate(prop = n/210)

same_key_press_90 <- key_press %>% 
  filter(prop >= .90)

df_task2_mod_exclude90 <- df_task2_mod %>% 
  filter(!participant %in% same_key_press_90$participant)

df_task3_mod_zones <- left_join(df_task3_mod, zones, by = c("participant", "object")) %>% 
  drop_na(preceding_rtcv)

o0 <- glmer(object_accuracy_s_p ~ (1 | participant), data = df_task3_mod_zones, family = "binomial")
o1 <- glmer(object_accuracy_s_p ~ preceding_rtcv + (1 | participant), data = df_task3_mod_zones, family = "binomial")
o2 <- glmer(object_accuracy_s_p ~ preceding_rtcv + age_group + (1 | participant), data = df_task3_mod_zones, family = "binomial")
o3 <- glmer(object_accuracy_s_p ~ preceding_rtcv*age_group + (1 | participant), data = df_task3_mod_zones, family = "binomial")

anova(o0, o1, o2, o3)
summary(o2)
plot(ggpredict(o2, terms = c("age_group")))


o0c <- glmer(object_accuracy_s ~ (1 | participant), data = df_task3_mod_zones, family = "binomial")
o1c <- glmer(object_accuracy_s ~ sarah_state + (1 | participant), data = df_task3_mod_zones, family = "binomial")
o2c <- glmer(object_accuracy_s ~ sarah_state + age_group + (1 | participant), data = df_task3_mod_zones, family = "binomial")
o3c <- glmer(object_accuracy_s ~ sarah_state*age_group + (1 | participant), data = df_task3_mod_zones, family = "binomial")

anova(o0c, o1c, o2c, o3c)
summary(o3c)
plot(ggpredict(o2c, terms = c("age_group")))

# D-prime and lapse rate analyses for Task 3 ------------------------------

df_task3_wide <- df_task3_mod %>%
  group_by(participant, age_group) %>%
  mutate(trials_old = sum(object_condition == "old"),
         trials_new = sum(object_condition == "new")) %>%
  count(participant, age_group, trials_old, trials_new, sensitivity, resp_6choice) %>%
  mutate(col_name = paste(sensitivity, resp_6choice, sep = "_")) %>%
  select(-sensitivity, -resp_6choice) %>%
  pivot_wider(names_from = col_name, values_from = n, values_fill = 0)

df_task3_wide_hitfa <- df_task3_wide %>%
  mutate(hits = (hit_probably + hit_sure) / trials_old,
         false_alarm = (fa_probably + fa_sure) / trials_new,
         hits_corr = ifelse(hits == 1, (trials_old - 0.5) / trials_old, hits),
         hits_corr = ifelse(hits_corr == 0, 0.5 / trials_old, hits_corr),
         false_alarm_corr = ifelse(false_alarm == 0, 0.5 / trials_new, false_alarm),
         dprime = qnorm(hits_corr) - qnorm(false_alarm_corr))

ggplot(df_task3_wide_hitfa, aes(age_group, dprime)) +
  geom_violin() +
  geom_boxplot() +
  geom_jitter() +
  labs(x = "Age Group", y = "d'") +
  theme_minimal()



## CONFERENCE
df_task3_wide_hitfa %>%
  pivot_longer(cols = c(hits_corr, false_alarm_corr), 
               names_to = "measure", 
               values_to = "value") %>%
  mutate(measure = fct_relevel(measure, "hits_corr", "false_alarm_corr")) %>%
  ggplot(aes(x = age_group, y = value, fill = measure)) +
  stat_summary(fun = mean, geom = "bar", position = position_dodge(0.8), width = 0.7) +
  stat_summary(fun.data = mean_se, geom = "errorbar", 
               position = position_dodge(0.8), width = 0.2) +
  scale_fill_manual(
    values = c("hits_corr" = "#4a9e8f", "false_alarm_corr" = "#E69F52"),
    labels = c("hits_corr" = "Hits", "false_alarm_corr" = "False Alarms")
  ) +
  scale_x_discrete(labels = c("OA" = "Older Adults", "YA" = "Younger Adults")) +
  labs(x = "", y = "Proportion", fill = "") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )
ggsave(filename = "Hits_False_alarm_task3.jpg", plot = last_plot(), dpi = 3000)


# Age differences in d-prime
t.test(dprime ~ age_group, data = df_task3_wide_hitfa, var.equal = TRUE)

# Do lapse rates predict object recognition memory?
df_task3_wide_hitfa_lapse <- left_join(df_task3_wide_hitfa, age_diff_zones, by = c("participant", "age_group")) %>%
  mutate(lapse_rate_c = scale(lapse_rate, scale = FALSE))

t3_1 <- lm(dprime ~ lapse_rate, data = df_task3_wide_hitfa_lapse)
t3_2 <- lm(dprime ~ lapse_rate + age_group, data = df_task3_wide_hitfa_lapse)
t3_3 <- lm(dprime ~ lapse_rate * age_group, data = df_task3_wide_hitfa_lapse)


anova(t3_1, t3_2, t3_3)
summary(t3_3)

emtrends(t3_3, pairwise ~ age_group, var = "lapse_rate")
emtrends(t3_3, ~ age_group, var = "lapse_rate", infer = TRUE)

ggplot(df_task3_wide_hitfa_lapse, aes(lapse_rate, dprime, color = age_group)) +
  geom_smooth(method = "lm") +
  geom_jitter() +
  scale_color_manual(
    values = c("OA" = "#e8929a", "YA" = "#6ab0d4"),
    labels = c("OA" = "Older Adults", "YA" = "Younger Adults")
  ) +
  labs(x = "Lapse Rate", y = "d-prime", color = "Age Group", title = "Lapse Rate Predicts Target Memory") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14, face = "bold"),
    legend.title = element_text(size = 14, face = "bold"),
    legend.text = element_text(size = 12)
  )
ggsave(filename = "Target_Memory_lapse.jpg", plot = last_plot(), dpi = 3000)






summary(lm(dprime ~ num_lapses * age_group, data = df_task3_wide_hitfa_lapse))
ggplot(df_task3_wide_hitfa_lapse, aes(num_lapses, dprime, color = age_group)) +
  geom_smooth(method = "lm") +
  geom_jitter()

summary(lm(dprime ~ median_streak * age_group, data = df_task3_wide_hitfa_lapse))





o0 <- glmer(object_accuracy_s_p ~ (1 | participant), data = df_task3_mod_zones, family = "binomial")
o1 <- glmer(object_accuracy_s_p ~ preceding_rtcv + (1 | participant), data = df_task3_mod_zones, family = "binomial")
o2 <- glmer(object_accuracy_s_p ~ preceding_rtcv + age_group + (1 | participant), data = df_task3_mod_zones, family = "binomial")
o3 <- glmer(object_accuracy_s_p ~ preceding_rtcv*age_group + (1 | participant), data = df_task3_mod_zones, family = "binomial")

anova(o0, o1, o2, o3)
summary(o2)
plot(ggpredict(o2, terms = c("age_group")))
plot(ggpredict(o3, terms = c("preceding_rtcv [all]", "age_group")))


o0 <- glmer(object_accuracy_s_p ~ (1 | participant), data = df_task3_mod_zones, family = "binomial")
o1 <- glmer(object_accuracy_s_p ~ sarah_state + (1 | participant), data = df_task3_mod_zones, family = "binomial")
o2 <- glmer(object_accuracy_s_p ~ sarah_state + age_group + (1 | participant), data = df_task3_mod_zones, family = "binomial")
o3 <- glmer(object_accuracy_s_p ~ sarah_state*age_group + (1 | participant), data = df_task3_mod_zones, family = "binomial")

anova(o0, o1, o2, o3)
summary(o2)
plot(ggpredict(o2, terms = c("age_group")))
plot(ggpredict(o3, terms = c("sarah_state", "age_group")))

