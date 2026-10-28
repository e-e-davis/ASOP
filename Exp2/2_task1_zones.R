# 2025-07-10
# Re-analysis of Sara P. data 
# First use Sarah H. method, then use Alex Decker method. 
# Further analysis will determine what method is most sensible to use. 


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(dplyr)
library(ggpubr)

# sara script libraries
library(jsonlite)
#library(ggpubr)
#library(rstatix)
#library(sjmisc)
#library(plotrix)
library(zoo)
library(lme4)
library(vctrs)
library(ggeffects)

# decker script libraries
library(imputeTS)
library(MazamaRollUtils)
#library(Smisc)  # movAvg2 not compatible with current R version - replaced with zoo::rollmean


OUTPUT_DIRECTORY = "3_output/cleaned_data_files/"
GRAPH_OUTPUT_DIRECTORY = "3_output/graphs/"

# Data read and organize --------------------------------------------------

#rolling_window = 8 # sarah study 
rolling_window = 3  # aligned with averaging done by decker 

df <- read_csv("3_output/cleaned_data_files/df_Task1.csv") 

df_c <- df %>% 
  mutate(age_group = ifelse(participant > 200, "OA", "YA")) %>% 
  mutate(rt = ifelse(rt < 10, NA, rt),   # exclude anticipatory trials (< 10ms)
         RT_seconds = rt/1000, 
         accuracy = ifelse(is.na(accuracy), 0, accuracy)) # set non-responses to incorrect


# SARAH METHOD  -----------------------------------------------------------

## rt ranges 
rt_ranges <- df_c %>% 
  group_by(participant) %>% 
  summarize(min = min(rt, na.rm = TRUE),
            max = max(rt, na.rm = TRUE))

rt_ranges %>% 
  filter(min < 10) # check: any remaining anticipatory trials


# get participant rt sd, mean and response time variability  
df_rtcv <- df_c %>% 
  group_by(participant) %>% 
  mutate(RT_sd = sd(RT_seconds, na.rm = TRUE),
         RT_mean = mean(RT_seconds, na.rm = TRUE),
         rtcv = sd(RT_seconds, na.rm = TRUE) / mean(RT_seconds, na.rm = TRUE),
         age_group_int = ifelse(age_group == "OA", 1, 0),
         acc_mean = mean(accuracy)) 

# does rtcv predict accuracy?
m0 <- glmer(accuracy ~ (1 | participant), data = df_rtcv, family = "binomial")
m1 <- glmer(accuracy ~ rtcv + (1 | participant), data = df_rtcv, family = "binomial")
m2 <- glmer(accuracy ~ rtcv + age_group_int + (1 | participant), data = df_rtcv, family = "binomial")
m3 <- glmer(accuracy ~ rtcv * age_group_int + (1 | participant), data = df_rtcv, family = "binomial")

anova(m0, m1, m2, m3)
summary(m1)

# YA = 0 
plot(ggpredict(m2, terms = c("rtcv [all]", "age_group_int")))
# Higher RT variability predicts task accuracy in both age groups. 


# calculate sliding window RT 
# only use accurate/frequent trials in rolling mean calculations
continuous_rtcv <- df_rtcv %>% 
  group_by(participant) %>% 
  mutate(trialcount = row_number()) %>% 
  mutate(RT_na = case_when(
    trialcount %in% c(1, 2)   ~ NA_real_,
    accuracy == 0             ~ NA_real_,
    frequency == "infrequent" ~ NA_real_,
    TRUE                      ~ RT_seconds
  )) %>% 
  mutate(rolling_mean = rollapply(RT_na, rolling_window, mean, fill = NA, align = "right", na.rm = TRUE),
         rolling_sd   = rollapply(RT_na, rolling_window, sd,   fill = NA, align = "right", na.rm = TRUE),
         rolling_rtcv = rolling_sd / rolling_mean)


# smooth all participants
subjects <- unique(continuous_rtcv$participant)

smoothed_data <- list()
loessMod10 <- list()
loessMod25 <- list()
loessMod50 <- list()

for (subject in subjects) {
  data_rtcv <- filter(continuous_rtcv, participant == subject)
  
  loessMod10[[subject]] <- loess(rolling_rtcv ~ trialcount, data = data_rtcv, span = 0.02)
  loessMod25[[subject]] <- loess(rolling_rtcv ~ trialcount, data = data_rtcv, span = 0.04)
  loessMod50[[subject]] <- loess(rolling_rtcv ~ trialcount, data = data_rtcv, span = 0.06)
  
  smoothed_data[[subject]] <- data_rtcv %>%
    mutate(
      smoothed10 = predict(loessMod10[[subject]], data_rtcv),
      smoothed25 = predict(loessMod25[[subject]], data_rtcv),
      smoothed50 = predict(loessMod50[[subject]], data_rtcv),
      Subject = subject
    )
}

# Combine all smoothed data
smoothed_rtcv <- bind_rows(smoothed_data)

# Re-assign trial count per subject
smoothed_rtcv <- smoothed_rtcv %>%
  group_by(participant) %>%
  mutate(trialcount = row_number()) %>%
  ungroup()

# Add lagged rolling RT variability (preceding_rtcv)
smoothed_rtcv <- smoothed_rtcv %>% 
  nest(data = c(-participant)) %>%
  mutate(
    data = map(data, ~ .x %>%
                 arrange(trialcount) %>%
                 mutate(
                   preceding_rtcv     = lag(roll_mean(smoothed10, width = 3)),
                   preceding_rtcv_lag = lag(smoothed10, 1)
                 ))
  ) %>% 
  unnest(data)


# Sort trials based on mean smoothed CVI (continuous variability index)
## CHANGED TRIAL COUNT FROM 15 to 5; max_fill FROM 7 to 3
sort_trials_cvi <- smoothed_rtcv %>% 
  group_by(participant) %>% 
  mutate(mean_cvi_subject = mean(smoothed10, na.rm = TRUE),
         sd_cvi_subject   = sd(smoothed10,   na.rm = TRUE),
         state            = NA,
         state            = if_else(trialcount > 5 & smoothed10 > mean_cvi_subject + (.67 * sd_cvi_subject), 1, NA_real_),
         state            = vec_fill_missing(state, max_fill = 3, "up"),
         state            = if_else(is.na(state), 0, state)) %>% 
  mutate(mean_preced_cvi_subject = mean(preceding_rtcv, na.rm = TRUE),
         sd_preced_cvi_subject   = sd(preceding_rtcv,   na.rm = TRUE),
         state_prec              = NA,
         state_prec              = if_else(trialcount > 5 & preceding_rtcv > mean_preced_cvi_subject + (.67 * sd_preced_cvi_subject), 1, NA_real_),
         state_prec              = vec_fill_missing(state_prec, max_fill = 3, "up"),
         state_prec              = if_else(is.na(state_prec), 0, state_prec)) %>% 
  drop_na(smoothed10) %>%
  rename(sarah_state = state)
  # filter(!is.na(participant_response)) # removed earlier in pipeline


# Participant count check
participants <- df_c %>% 
  group_by(participant, age_group) %>% 
  summarize(n = n(), .groups = "drop")

participants %>% 
  group_by(age_group) %>% 
  summarize(n = n())

# sarah_state models
s0 <- glmer(accuracy ~ (1 | participant), data = sort_trials_cvi, family = "binomial")
s1 <- glmer(accuracy ~ sarah_state + (1 | participant), data = sort_trials_cvi, family = "binomial")
s2 <- glmer(accuracy ~ sarah_state + age_group_int + (1 | participant), data = sort_trials_cvi, family = "binomial")
s3 <- glmer(accuracy ~ sarah_state * age_group_int + (1 | participant), data = sort_trials_cvi, family = "binomial")

anova(s0, s1, s2, s3)


# DECKER  -----------------------------------------------------------------

df_d <- sort_trials_cvi %>% 
  group_by(participant) %>% 
  mutate(rt_interp         = na_ma(RT_na, k = 2, weighting = "simple"),
         mean_abs_deviance = abs(rt_interp - mean(rt_interp)))

## scale vars and smooth (with preceding RT variables)
df_d_smoothed <- df_d %>% 
  nest(data = c(-participant)) %>%
  mutate(
    data = map(data, ~ .x %>%
                 mutate(
                   preceding_rt_interp        = lag(roll_mean(rt_interp, width = 3)),
                   preceding_mean_abs_deviance = lag(roll_mean(mean_abs_deviance, width = 3))
                 ))
  ) %>% 
  unnest(data) %>% 
  group_by(participant) %>% 
  mutate(preceding_rt_interp_scaled        = preceding_rt_interp - mean(preceding_rt_interp, na.rm = TRUE),
         preceding_mean_abs_deviance_scaled = preceding_mean_abs_deviance - mean(preceding_mean_abs_deviance, na.rm = TRUE),
         # smoothed_mean_absolute_deviation = movAvg2(y = mean_abs_deviance, bw = 2, type = "gaussian",
         #                                            center.weight = 1, furthest.weight = 0.5)  # Smisc - incompatible with current R version
         smoothed_mean_absolute_deviation   = rollmean(mean_abs_deviance, k = 5, fill = NA, align = "center"))  # zoo replacement; k=5 ~ bw=2 gaussian

## calculate zone variables 
df_d_zone <- df_d_smoothed %>% 
  group_by(participant) %>% 
  mutate(median_smoothed_abs_deviation  = median(smoothed_mean_absolute_deviation),
         median_preceding_abs_deviance  = median(preceding_mean_abs_deviance_scaled, na.rm = TRUE))

group_threshold <- df_d_zone %>%
  group_by(participant) %>%
  summarise(participant_median = median(smoothed_mean_absolute_deviation, na.rm = TRUE), .groups = "drop") %>%
  summarise(threshold = mean(participant_median)) %>%
  pull(threshold)

group_threshold_preceding <- df_d_zone %>%
  group_by(participant) %>%
  summarise(participant_median_preceding = median(preceding_mean_abs_deviance_scaled, na.rm = TRUE), .groups = "drop") %>%
  summarise(threshold_preceding = mean(participant_median_preceding)) %>%
  pull(threshold_preceding)

## out of zone if RT deviance exceeds group-level mean smoothed deviance
df_d_zone <- df_d_zone %>% 
  mutate(decker_zone           = ifelse(smoothed_mean_absolute_deviation > group_threshold, 1, 0),
         decker_zone_preceding = ifelse(preceding_mean_abs_deviance_scaled > group_threshold_preceding, 1, 0))


# Plots -------------------------------------------------------------------

# RT scatter for participant 305
df_d_smoothed %>% 
  filter(participant == "305") %>%
  ggplot(aes(trialcount, rt_interp)) +
  geom_jitter()

# Decker zone plot
decker <- df_d_zone %>%
  filter(participant == "305") %>%
  mutate(state_change = decker_zone != lag(decker_zone, default = first(decker_zone)),
         segment_id   = cumsum(state_change)) %>%
  ggplot(aes(x = trialcount, y = smoothed_mean_absolute_deviation)) +
  geom_line(aes(color = factor(decker_zone), group = 1), linewidth = 1) +
  theme_minimal() +
  theme(legend.position = "none")

# Sarah zone plot
sarah <- df_d_zone %>%
  filter(participant == "305") %>%
  mutate(state_change = sarah_state != lag(sarah_state, default = first(sarah_state)),
         segment_id   = cumsum(state_change)) %>%
  ggplot(aes(x = trialcount, y = smoothed10)) +
  geom_line(aes(color = factor(sarah_state), group = 1), linewidth = 1) +
  theme_minimal() +
  theme(legend.position = "none")

ggarrange(decker, sarah)


# Export ------------------------------------------------------------------

write_csv(df_d_zone, paste0(OUTPUT_DIRECTORY, "Task1_with_zones_withpreceding.csv"))




# Conference poster graphs  -----------------------------------------------

participant_plot = "564993"
plot_data <- df_d_zone %>%
  filter(participant == participant_plot) %>%
  mutate(
    threshold = mean(smoothed10, na.rm = TRUE) + 0.67 * sd(smoothed10, na.rm = TRUE),
    zone = ifelse(sarah_state == 1, "Out of the Zone", "In the Zone")
  )

smoothed_vals <- approx(plot_data$trialcount, plot_data$smoothed10, n = 3500)

plot_interp <- data.frame(
  trialcount = smoothed_vals$x,
  smoothed10 = smoothed_vals$y,
  zone = ifelse(smoothed_vals$y > unique(plot_data$threshold), "Out of the Zone", "In the Zone")
)

thresh <- unique(plot_data$threshold)

plot_continuous_rtcv <- continuous_rtcv %>% 
  filter(participant == participant_plot) 

ggplot(plot_interp, aes(x = trialcount, y = smoothed10, col = zone)) +
  geom_line(data = plot_continuous_rtcv, aes(x = trialcount, y = rolling_rtcv), 
            inherit.aes = FALSE, color = "lightgrey", linewidth = 0.4) +
  geom_path(group = 1, linewidth = 1.5, lineend = "round") +
  geom_hline(yintercept = thresh, linetype = "dashed", color = "grey40") +
  scale_color_manual(
    breaks = c("In the Zone", "Out of the Zone"),
    values = c("In the Zone" = "#c4566a", "Out of the Zone" = "#e8929a")
  ) +
  theme_minimal() +
  labs(y = "Smoothed Reaction time variability", x = "Trial Number", col = "", title = "Representative Older Adult") +
  theme(legend.position = "top", legend.direction = "horizontal")+
  theme(
    plot.title = element_text(size = 18, face = "bold"),
    axis.title = element_text(size = 14, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold"),
    axis.text = element_text(size = 14, face = "bold")
  )
ggsave(paste0(GRAPH_OUTPUT_DIRECTORY, "participant_", participant_plot, ".jpg"), last_plot())


#scale_color_manual(
#breaks = c("In the Zone", "Out of the Zone"),
#values = c("In the Zone" = "
#1a3a6b", "Out of the Zone" = "
#6ab0d4")