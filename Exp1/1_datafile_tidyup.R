# 2025-07-10
# Re-analysis of Sara P. undergraduate thesis data 
# Start by organizing the data files a bit. 

# 2025-07-18 
# Organize task 2 data. 
# Next time task 3. 
# 301 == CB_resp.keys, 302 = resp_CB.keys or something. 

#2025-07-22 
# Adapted task 3 to deal with inconsistent column naming conventions. 
# Import scored task 2 data, tidy and join it with task data. 

#2026-05-07
# With new computer and corrupted hard-drive, we are updating. Minor changes 
# new version of dplyr that is strict about what's a character
## Manually fixed 168 Task 2, 346 Task 1

# Libraries 
library(tidyverse)
library(dplyr)
library(stringr)
library(readxl)


# TASK 1 ------------------------------------------------------------------

# import data, clean up, then re-save data files under folder "cleaned_files"
data_to_read <- c(list.files("1_data/OA/Task_1", pattern = "*.csv" , full.names = T),
                  list.files("1_data/YA/Task_1", pattern = "*.csv" , full.names = T))

for (i in 1:length(data_to_read)){

# read in and tidy 
df <- read_csv(data_to_read[i]) %>% 
  select(participant, everything()) %>% 
  mutate(across(where(is.character), ~na_if(.x, "None")))

df_practice <- df %>% 
  select(participant, starts_with("Prac"), starts_with("practice")) %>% 
  drop_na(practice_trials.thisTrialN)

df_task <- df %>% 
  select(participant, expName, psychopyVersion, starts_with("scene"), 
         starts_with("object"),
         starts_with("correctAns"),
         starts_with("resp"))
names(df_task) <- gsub("[0-9]+", "", names(df_task))

df_task <- df_task %>% 
  drop_na(scene) %>% 
  select(where(~ !all(is.na(.)))) %>% 
  select(-contains("ready")) %>% 
  rename(rt = resp_CB.rt, 
         accuracy = resp_CB.corr,
         participant_response = resp_CB.keys) %>% 
  mutate(rt = as.double(rt)) %>% 
  mutate(rt = rt*1000,
         frequency = ifelse(correctAns == "h", "frequent", "infrequent"))


#save csvs
participant <- df$participant[1]
write.csv(df_practice, paste0("1_data/cleaned_files/practice_1_data/", participant,
                              "_practice.csv"))
write.csv(df_task, paste0("1_data/cleaned_files/task_1_data/", participant,
                              "_task_1.csv"))

}

# Import tidied data and write csv for easier future analysis
files <- list.files("1_data/cleaned_files/task_1_data", full.names = T)
combined_data <- map_dfr(files, ~ read_csv(.x) %>% mutate(source = basename(.x)))

# Drop some unnecessary. 
combined_data_cleaned <- combined_data %>% 
  select(-`...1`, 
         -ends_with("started"), 
         -ends_with("stopped"),
         -ends_with("duration"), 
         -source)

write.csv(combined_data_cleaned, "1_data/ASOP_Task1.csv")


# TASK 2 ------------------------------------------------------------------

# import data, clean up, then re-save data files under folder "cleaned_files"
data_to_read <- c(list.files("1_data/YA/Task_2", pattern = "*.csv" , full.names = T),
                  list.files("1_data/OA/Task_2", pattern = "*.csv" , full.names = T))

for (i in 1:length(data_to_read)){
  
  #i = 15
  
  # read in and tidy 
  df2 <- read_csv(data_to_read[i]) 
  
  #task data
  df_task2 <- df2 %>% 
    select(participant, expName, psychopyVersion, 
           starts_with("resp"),
           starts_with("textbox"),
           starts_with("scene"),
           starts_with("SceneCorr")) %>% 
   select(-contains("stopped")) %>% 
   select(-contains("started")) %>% 
   select(-contains("duration")) %>% 
   select(-contains("respReady")) %>% 
   select(-contains("respEnd")) %>% 
   select(-contains("respBackground")) %>% 
   select(-textbox.text) %>% 
   slice(15:n()) %>% 
   mutate(across(ends_with(".text"), ~ replace_na(as.character(.), "-999"))) %>% # need to keep this column even if filled with NAs
   select(where(~ !all(is.na(.)))) %>% 
   mutate(trialnum =row_number(),
          block = case_when(
            trialnum >= 1   & trialnum <= 70   ~ "block_1",
            trialnum >= 71  & trialnum <= 140  ~ "block_2",
            trialnum >= 141 & trialnum <= 210  ~ "block_3")) %>% 
  rename_with(~ str_remove_all(., "\\d+[A-Z]")) #remove counterbalance letters and numbers from column names
  

  block_1 <- df_task2 %>% 
    filter(block == "block_1") %>% 
    select(-contains("_2")) %>% 
    select(-contains("_3"))
 names(block_1) <- gsub("[0-9]+", "", names(block_1))
 
 block_2 <- df_task2 %>% 
   filter(block == "block_2") %>% 
   select(-contains("_1")) %>% 
   select(-contains("_3"))
 names(block_2) <- gsub("[0-9]+", "", names(block_2))
 
 block_3 <- df_task2 %>% 
   filter(block == "block_3") %>% 
   select(-contains("_1")) %>% 
   select(-contains("_2"))
 names(block_3) <- gsub("[0-9]+", "", names(block_3))
  
  
  df_combined <- bind_rows(block_1, block_2, block_3)
  
  
  df_combined_cleaned <- df_combined %>%
    rename_with(~ str_remove_all(., "_")) %>% 
    rename(cued_recall_response_object = textbox.text, 
           background = sceneCB, 
           correct_response_background = SceneCorrCB,
           participant_response_background = resp.keys,
           rt_background = resp.rt) %>% 
    select(-resp.corr) %>% 
    mutate(cued_recall_response_object = ifelse(cued_recall_response_object == "-999", NA, cued_recall_response_object)) %>% 
    mutate(resp_2choice_background = case_when(participant_response_background == "a" ~ "old", 
                                    participant_response_background == "s" ~ "old", 
                                    participant_response_background == "d" ~ "old", 
                                    participant_response_background == "j" ~ "new", 
                                    participant_response_background == "k" ~ "new", 
                                    participant_response_background == "l" ~ "new"),
           resp_6choice_background = case_when(participant_response_background == "a" ~ "sure", 
                                    participant_response_background == "s" ~ "probably", 
                                    participant_response_background == "d" ~ "guess", 
                                    participant_response_background == "j" ~ "guess", 
                                    participant_response_background == "k" ~ "probably", 
                                    participant_response_background == "l" ~ "sure"),
           background_condition = case_when(correct_response_background == "a, s, d" ~ "old",
                                            correct_response_background == "j, k, l" ~ "new"))
  
  
  #save csvs
  participant <- df_combined_cleaned$participant[1]
  write.csv(df_combined_cleaned, paste0("1_data/cleaned_files/task_2_data/", participant,
                            "_task_2.csv"))
  
}


# Import tidied data and write csv for easier future analysis
files <- list.files("1_data/cleaned_files/task_2_data", full.names = T)
combined_data <- map_dfr(files, ~ read_csv(.x) %>% mutate(source = basename(.x))) %>% 
  select(participant, expName, psychopyVersion, trialnum, block, background, 
         background_condition, correct_response_background, participant_response_background, 
         rt_background, resp_2choice_background, resp_6choice_background,
         cued_recall_response_object) %>%  # sensibly reorder these. 
  group_by(participant) %>% 
  mutate(total_trials_task = n()) %>% 
  ungroup() %>% 
  drop_na(participant)


# Import scored cued recall data

# Start with YA
df_scored_cr_subject_ya = read_excel("1_data/YA/ASOP_Task2_YA_Scored.xlsx", sheet = 1, na = "X")
columns_scored_cr_ya <- names(df_scored_cr_subject_ya)
subjects_ya <- str_subset(columns_scored_cr_ya, "^Subject")
df_scored_cr_ya = read_excel("1_data/YA/ASOP_Task2_YA_Scored.xlsx", sheet = 1, na = "X", skip = 1)

# OA 
df_scored_cr_subject_oa = read_excel("1_data/OA/ASOP_Task2_OA_Scored.xlsx", sheet = 1, na = "X")
columns_scored_cr_oa <- names(df_scored_cr_subject_oa)
subjects_oa <- str_subset(columns_scored_cr_oa, "^Subject")
df_scored_cr_oa = read_excel("1_data/OA/ASOP_Task2_OA_Scored.xlsx", sheet = 1, na = "X", skip = 1)

# Join age groups
scored_dat <- bind_cols(df_scored_cr_ya, df_scored_cr_oa)
all_subjects <- c(subjects_ya, subjects_oa)


# Need to pivot the data longer, somehow? 
n_vars <- 8 # known from data file
n_participants <- ncol(scored_dat) / n_vars

new_names <- c("Scene", "Answer", "Response", "GenerousFeedback", 
               "Feedback", "ActualObject", "ObjectGuess", "ObjectFeedback")


participant_data <- map(1:n_participants, function(i) {
  start_col <- (i - 1) * n_vars + 1
  end_col <- i * n_vars
  
  df_part <- scored_dat[, start_col:end_col]
  names(df_part) <- new_names  # Standardize variable names
  df_part$participant <- all_subjects[i]
  df_part
  
  df_part <- df_part %>% 
    mutate(across(everything(), as.character))
})

scored_cued_recall <- bind_rows(participant_data) %>%
  relocate(participant) %>% 
  filter(str_detect(Scene, "\\.jpg$")) %>% # Get rid of excel calculations
  group_by(participant) %>% 
  mutate(total_trials = n()) %>%  # make sure we didn't heck anything up, should be 210.
  ungroup() %>% 
  mutate(participant = as.double(str_remove(string = participant, "Subject_")),
         age_group = ifelse(participant > 200, "OA", "YA"))


# Do some checks across data files
missing_trials_task_data <- combined_data %>% 
  filter(total_trials_task < 210) 
unique(missing_trials_task_data$participant)

missing_trials_scored_data <- scored_cued_recall %>% 
  filter(total_trials < 210) 
unique(missing_trials_scored_data$participant)

#object_responses <- tibble(scored_cued_recall$ObjectGuess, combined_data$cued_recall_response_object) %>% 
  #mutate(across(where(is.character), ~ str_replace_all(., "[\\r\\n\\t]", ""))) %>% 
 # mutate(object_same=combined_data$cued_recall_response_object == scored_cued_recall$ObjectGuess)

object_responses <- tibble(
  participant_scored = scored_cued_recall$participant,
  guess = scored_cued_recall$ObjectGuess, 
  participant_combined = combined_data$participant,
  response = combined_data$cued_recall_response_object
) %>% 
  mutate(across(where(is.character), ~ str_replace_all(., "[^[:alnum:][:space:]]", ""))) %>%
  mutate(across(where(is.character), ~ str_trim(str_to_lower(.)))) %>%
  mutate(object_same = guess == response) 

object_responses %>% 
  filter(object_same == FALSE) # should be fine to join. 

#check for copy paste errors
scored_cued_recall %>%
  group_by(participant) %>%
  filter(!all(is.na(ObjectGuess))) %>%  # skip if all NA
  summarise(guesses = paste(ObjectGuess, collapse = ",")) %>%
  group_by(guesses) %>%
  filter(n() > 1) %>%
  arrange(guesses)


# Join data 
df_task2 <- left_join(combined_data, scored_cued_recall, by = c("participant", "background" = "Scene")) %>% 
  select(-Answer, -Response, -ObjectGuess) %>% # drop redundant columns
  rename(background_accuracy_s_p_g = GenerousFeedback, 
         background_accuracy_s_p = Feedback,
         object_accuracy = ObjectFeedback)

# ## solve why 301 is double in block 2
# combined_data_301 <- combined_data %>% 
#   filter(participant == 301)
# 
# scored_cued_recall_301 <- scored_cued_recall %>% 
#   filter(participant == 301)
# 
# df_task2_301 <- left_join(combined_data_301, scored_cued_recall_301, by = c("participant", "background" = "Scene")) %>% 
#   select(-Answer, -Response, -ObjectGuess) %>% # drop redundant columns
#   rename(background_accuracy_s_p_g = GenerousFeedback, 
#          background_accuracy_s_p = Feedback,
#          object_accuracy = ObjectFeedback)
# 
# dup <- df_task2 %>%
#   filter(participant == 301) %>%
#   group_by(block) %>% 
#   count(background, name = "n_after_join") %>%
#   arrange(desc(n_after_join))
# 
# combined_data %>%
#   filter(participant == 301) %>%
#   count(background, name = "n_combined")
# 
# # How many rows in scored_cued_recall?
# scored_cued_recall %>%
#   filter(participant == 301) %>% 
#   count(Scene, name = "n_scored")
# items were added to 301_cued_recall, I think Sara copy and pasted block 2 for 
# block 3 and that's where the duplicates are coming from. She will double check. 


# Write data file
write.csv(df_task2, "1_data/ASOP_Task2.csv")




# TASK 3 ------------------------------------------------------------------

# import data, clean up, then re-save data files under folder "cleaned_files"
data_to_read <- c(list.files("1_data/OA/Task_3", pattern = "*.csv" , full.names = T),
                  list.files("1_data/YA/Task_3", pattern = "*.csv" , full.names = T))

for (i in 1:length(data_to_read)){
  
  print(data_to_read[i])
  # read in and tidy 
  df3 <- read_csv(data_to_read[i]) 
  
  # Need to deal with counterbalance naming convention issues 
  getCB <- df3 %>% 
    select(starts_with("object")) %>% 
    select(ends_with("blk1")) %>% 
    names()
  CB <- str_extract(getCB, "(?<=CB)[0-9]+[A-Z]")
  
  if (CB == "1A"){
    df3 <- df3 %>% 
      rename(resp1A_1.keys = CB1A_resp.keys,
             resp1A_2.keys = CB1A_resp_2.keys,
             resp1A_3.keys = CB1A_resp_3.keys)
  }
  
  if (CB == "1B"){
    df3 <- df3 %>% 
      rename(resp1B_1.keys = resp_1B_1.keys,
             resp1B_2.keys = resp_1B_2.keys,
             resp1B_3.keys = resp1B_3.keys)
  }
  
  if (CB == "2A"){
    df3 <- df3 %>% 
      rename(resp1A_1.keys = CB2A_resp.keys,
             resp1A_2.keys = respCB2A_2.keys,
             resp1A_3.keys = respCB2A_3.keys)
  }
  
  
  #task data
  df_task3 <- df3 %>% 
    select(participant, expName, psychopyVersion, 
           starts_with("resp"),
           starts_with("object"), 
           starts_with("correct"),
           ends_with("keys")) %>% 
    select(-contains("instruc"),
           -contains("prac"),
           -contains("Prac"),
           -contains("ready"),
           -contains("start"),
           -contains("end"),
           -contains("duration"), ## added this 2025-07-23
           -ends_with("corr"),
           -ends_with("rt")) %>% ## to here
    slice(12:n()) %>% 
    select(where(~ !all(is.na(.)))) %>% 
    mutate(trialnum =row_number(),
           block = case_when(
             trialnum >= 1   & trialnum <= 70   ~ "block_1",
             trialnum >= 71  & trialnum <= 140  ~ "block_2",
             trialnum >= 141 & trialnum <= 210  ~ "block_3")) %>% 
    # rename_with(~ str_remove_all(., "1A|1B"))
    rename_with(~ str_replace(., "resp\\d+[A-Z]", "resp")) %>% 
    rename_with(~ str_remove_all(., "\\d+[A-Z]"))
  
  
  block_1 <- df_task3 %>% 
    filter(block == "block_1") %>% 
    select(-contains("_2"), 
           -contains("blk2"),
           -contains("_3"),
           -contains("blk3"))
  names(block_1) <- gsub("[0-9]+", "", names(block_1))
  
  
  block_2 <- df_task3 %>% 
    filter(block == "block_2") %>% 
    select(-contains("_1"), 
           -contains("blk1"),
           -contains("_3"),
           -contains("blk3"))
#  rename(CB_resp.keys = CB_resp_2.keys)
  names(block_2) <- gsub("[0-9]+", "", names(block_2))
  
  block_3 <- df_task3 %>% 
    filter(block == "block_3") %>% 
    select(-contains("_1"), 
           -contains("blk1"),
           -contains("_2"),
           -contains("blk2"))
  names(block_3) <- gsub("[0-9]+", "", names(block_3))
   
  df_combined <- bind_rows(block_1, block_2, block_3)

  df_combined_cleaned <- df_combined %>% 
    select(-starts_with("CB")) %>% 
    rename(participant_response = resp_.keys,
           correct_answer = correctCB_blk, 
           object = objectCB_blk) %>% 
    mutate(resp_2choice = case_when(participant_response == "a" ~ "old", 
                                    participant_response == "s" ~ "old", 
                                    participant_response == "d" ~ "old", 
                                    participant_response == "j" ~ "new", 
                                    participant_response == "k" ~ "new", 
                                    participant_response == "l" ~ "new"),
           resp_6choice = case_when(participant_response == "a" ~ "sure", 
                                    participant_response == "s" ~ "probably", 
                                    participant_response == "d" ~ "guess", 
                                    participant_response == "j" ~ "guess", 
                                    participant_response == "k" ~ "probably", 
                                    participant_response == "l" ~ "sure"),
           object_condition = case_when(correct_answer == "a, s, d" ~ "old",
                                        correct_answer == "j, k, l" ~ "new"))
  
  
  #save csvs
  participant <- df_combined_cleaned$participant[1]
  if (participant == 1){
    participant = 111 #RA changed file name but not the participant column when wrong participant number was entered. 
    
    df_combined_cleaned <- df_combined_cleaned %>% 
      mutate(participant = 111)
  }
  
  write.csv(df_combined_cleaned, paste0("1_data/cleaned_files/task_3_data/", participant,
                                        "_task_3.csv"))
  
}


# Import tidied data and write csv for easier future analysis
files <- list.files("1_data/cleaned_files/task_3_data", full.names = T)
combined_data <- map_dfr(files, ~ read_csv(.x) %>% mutate(source = basename(.x)))

write.csv(combined_data, "1_data/ASOP_Task3.csv")

