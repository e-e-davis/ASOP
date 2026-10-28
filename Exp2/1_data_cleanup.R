## 2026-06-08 Data Clean-up for ASOP 
## Turn psychopy documents into three long data files per task 



# LIBRARIES AND GLOBAL VARIABLES ------------------------------------------

library(tidyverse)


OUTPUT_DIRECTORY = "3_output/cleaned_data_files/"

# READ DATA  --------------------------------------------------------------

individual_data_files <- list.files("1_data/", pattern = "*.csv" , full.names = T)


df <- map_dfr(individual_data_files, read_csv,
              col_types = cols(.default = col_character()),
              .id = "source_file") |>
  mutate(across(!ends_with(".text"), readr::parse_guess))
head(df)


# TASK 1 ------------------------------------------------------------------


df_task1 <- df %>% 
  select(contains(c("Participant","T1", "Task1"))) %>% 
  select(!contains(c("Instructions", "Prac", "Summary", "ReadyScreen", 
                     "started", "stopped", "textbox.text", "duration",
                     "PostTask", "participant_response"))) %>% 
  select(where(~ !all(is.na(.)))) %>% 
  rename(rt = RESP_T1.rt, 
         accuracy = RESP_T1.corr,
         participant_response = RESP_T1.keys) %>% 
  mutate(rt = as.double(rt)) %>% 
  mutate(rt = rt*1000,
         frequency = ifelse(correctAnsT1 == "h", "frequent", "infrequent")) %>% 
  group_by(participant) %>% 
  filter(!is.na(accuracy))

df_task1 %>% 
  group_by(participant) %>% 
  summarize(n())

write_csv(df_task1, paste0(OUTPUT_DIRECTORY, "df_task1.csv"))

# TASK 2 ------------------------------------------------------------------

df_task2 <- df %>% 
  filter(Task2 == "Task2") %>% 
  select(where(~ !all(is.na(.)))) %>% 
  rename(rt_background = RESP_T2_blk1.rt, 
         accuracy = RESP_T2_blk1.corr,
         participant_response_background = RESP_T2_blk1.keys,
         background = scene,
         correct_response_background = sceneCorr,
         cb = Task2CB) %>% 
  mutate(rt_background = rt_background*1000,
        resp_2choice_background = case_when(participant_response_background == "a" ~ "old", 
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
         background_condition = case_when(correct_response_background == "a,s,d" ~ "old",
                                          correct_response_background == "j,k,l" ~ "new")) 
  write_csv(df_task2, paste0(OUTPUT_DIRECTORY, "df_task2.csv"))

  
# TASK 3 ------------------------------------------------------------------

df_task3 <- df %>% 
  filter(Task3 == "Task3") %>% 
  select(where(~ !all(is.na(.)))) %>% 
  rename(participant_response = RESP_T3_blk1.keys,
         correct_answer = objectCorr, 
         accuracy = RESP_T3_blk1.corr,
         rt_object = RESP_T3_blk1.rt,
         cb = Task3CB) %>% 
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
         object_condition = case_when(correct_answer == "a,s,d" ~ "old",
                                      correct_answer == "j,k,l" ~ "new"))
write_csv(df_task3, paste0(OUTPUT_DIRECTORY, "df_task3.csv"))

# CREATE DEMOGRAPHIC FILE -------------------------------------------------

# first 5 participants, no ADHD data
adhd_df <- df %>% 
  select(contains(c("participant", "ADHD"))) %>% 
  filter(!is.na(ADHDQuestion)) %>% 
  group_by(participant) %>% 
  summarize(adhd_score = sum(responses_adhd.response))

demo_df <- df %>% 
  select(contains(c("participant", "Dem", "ParticipantFeedback", "slider"))) %>% 
  select(!contains(c("duration", ".rt", "stopped", "started"))) %>% 
  filter(!if_all(-participant, is.na)) %>% 
  group_by(participant) %>%
  summarise(across(everything(), ~ first(na.omit(.)))) %>% 
  mutate(english_fluency = rowMeans(
    pick(DemFluency1_slider_2.response:DemFluency4_slider_2.response), 
    na.rm = TRUE)) %>% 
  select(!contains(c("slider_2"))) %>% 
  rename("time_awake" = "DemTime_textbox.text",
         "time_awake_ampm" = "slider.response",
         "age" = "DemAge_textbox.text",
         "gender" = "DemGender_textbox.text",
         "sex" = "DemSex_resp.keys",
         "ethnicity" = "DemEthnicity_textbox.text",
         "english_first_lang" = "DemEnglish_resp.keys",
         "education_total_years" = "DemEdu_textbox.text",
         "highest_degree_earned" = "DemDegree_textbox.text",
         "vision_problems" = "DemVision_resp.keys",
         "vision_problems_describe" = "DemVision_textbox.text",
         "neuro_problems" = "DemNeuro_resp.keys",
         "colourblind" = "DemColourBlind_resp.keys",
         "knocked_unconscious" = "DemUnconscious_resp.keys",
         "memory_concerns" = "DemMemConcernExp_textbox.text",
         "screen_time" = "DemScreenTime_textbox.text",
         "screen_time_from_cellphone" = "DemST_Est_Exact_resp.keys",
         "participant_feedback" = "ParticipantFeedback_textbox.text") %>% 
  mutate(across(where(is.character), ~ str_remove_all(., "\r|\n")),
         across(where(is.character), ~ na_if(., "q")),
         age = as.double(age),
         age_group = ifelse(age > 59, "old", ifelse(age < 31, "young", "other")),
         screen_time = as.double(str_remove(screen_time, "hours")),
         screen_time_from_cellphone = ifelse(screen_time_from_cellphone == "1", "from_cellphone", "estimate"),
         sex = case_when(sex == "0" ~ "male",
                         sex == "1" ~ "female", 
                         sex == "2" ~ "intersex", 
                         sex == "3" ~ "prefer_not_to_answer",
                         is.na(sex) ~ NA))
  
demo_df_adhd = left_join(demo_df, adhd_df, by = "participant")

write_csv(demo_df_adhd, paste0(OUTPUT_DIRECTORY, "demographics.csv"))

# CONFIRM CORRECT COUNTERBALANCES  ----------------------------------------

task1_cb <- df_task1 %>% 
  group_by(participant) %>% 
  summarize(t1_cb = Task1_CB[1])

task2_cb <- df_task2 %>% 
  group_by(participant) %>% 
  summarize(t2_cb = cb[1])

task3_cb <- df_task3 %>% 
  group_by(participant) %>% 
  summarize(t3_cb = cb[1])

counterbalances <- task1_cb %>% 
  left_join(task2_cb) %>% 
  left_join(task3_cb)

# MAKE TEST CASES
counterbalances <- counterbalances %>% 
  add_row(participant = "TEST_LETTER", t1_cb = "CB1", t2_cb = "CB1A", t3_cb = "CB1B") %>%
  add_row(participant = "TEST_NUMBER", t1_cb = "CB1", t2_cb = "CB2A", t3_cb = "CB1A")

counterbalances_check <- counterbalances %>% 
  mutate(correct_num = str_detect(t2_cb, t1_cb) & str_detect(t3_cb, t1_cb),
         t2_letter = str_extract(t2_cb, "(?<=CB\\d)\\w*"),
         t3_letter = str_extract(t3_cb, "(?<=CB\\d)\\w*"),
         correct_letter = t2_letter == t3_letter) %>% 
  filter(correct_letter == FALSE | correct_num == FALSE)

View(counterbalances_check)


attention_checks <- df %>% 
  select(contains("participant") | starts_with("AC")) %>% 
  select(!ends_with(c("duration", "rt"))) %>%
  group_by(participant) %>% 
  summarise(across(everything(), ~ first(na.omit(.)))) %>% 
  mutate(attention_check_accuracy = AC1_resp.corr+AC2_resp_2.corr+AC8_resp_2.corr) %>% 
  select(participant, attention_check_accuracy, AC5_textbox_2.text) %>% 
  rename(typed_attention_check = AC5_textbox_2.text)

write_csv(attention_checks, paste0(OUTPUT_DIRECTORY, "attentional_checks.csv"))

