# Classify open-ended task description responses using a local Ollama model
# Install ollamar: install.packages("ollamar")
# Install Ollama: https://ollama.com, then: ollama pull llama3.1

library(ollamar)
library(tidyverse)

OUTPUT_DIRECTORY = './3_output/cleaned_data_files/'

# ---- Configuration --------------------------------------------------------

MODEL <- "llama3.1"  # change if you pulled a different model

TASK_DESCRIPTION <- "Participants were shown scenes/background images from a previous
task and had to judge whether each image was old (seen during the first game) or new (never seen before),
and rate their confidence."

# Classification rubric
SYSTEM_PROMPT <- paste0(
  "You are a research assistant coding open-ended responses in a psychology study.\n\n",
  "The correct task was: ", TASK_DESCRIPTION, "\n\n",
  "For each participant response, classify it as one of:\n",
  "  CORRECT    - clearly describes recognising old vs new background images with or without mentions of confidence\n",
  "  PARTIAL    - gets the gist but missing a key element (e.g. vague about old/new)\n",
  "  INCORRECT  - describes something else entirely\n",
  "  UNCLEAR    - too vague or uninformative to judge\n\n",
  "Reply with ONLY a JSON object in this exact format, no preamble:\n",
  '{"classification": "CORRECT", "reason": "one sentence"}'
)

df <- read_csv("3_output/cleaned_data_files/attentional_checks.csv")

# ---- Helper function -------------------------------------------------------

classify_response <- function(response_text, participant_id = NA) {
  if (is.na(response_text) || trimws(response_text) == "") {
    return(tibble(
      participant    = participant_id,
      response       = response_text,
      classification = "UNCLEAR",
      reason         = "Empty or missing response"
    ))
  }

  result <- tryCatch({
    resp <- generate(
      model  = MODEL,
      prompt = paste0('Participant response: "', response_text, '"'),
      system = SYSTEM_PROMPT,
      output = "text"
    )

    parsed <- jsonlite::fromJSON(resp)

    tibble(
      participant    = participant_id,
      response       = response_text,
      classification = parsed$classification,
      reason         = parsed$reason
    )
  }, error = function(e) {
    tibble(
      participant    = participant_id,
      response       = response_text,
      classification = NA_character_,
      reason         = paste("Error:", e$message)
    )
  })

  result
}

# ---- Run classification ---------------------------------------------------

results <- df %>% 
  mutate(
    classified = map2(
       typed_attention_check, participant,
      ~ classify_response(.x, .y)
    )
  ) %>% 
  select(classified) %>% 
  unnest(classified)
print(results)


results <- left_join(results,df, by = "participant") %>% 
  select(-typed_attention_check)

write_csv(results, paste0(OUTPUT_DIRECTORY, "attention_checks_with_classified_responses.csv"))

