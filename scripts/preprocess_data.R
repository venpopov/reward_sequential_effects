library(dplyr)
library(tidyselect)
library(purrr)
source("scripts/prior_item.R")

clean_data <- function(filename) {
  read.csv(filename) |>
    set_names(tolower) |>
    select(
      exp_username,
      exp_trial,
      exp_condition.description,
      exp_serial_position,
      any_of(c("exp_serial_cue", "exp_serial_value")),
      exp_serial_answer,
      exp_serial_strictacc
    ) |>
    set_names(c("subject", "trial", "condition", "sp", "value", "stim", "acc")) |>
    filter(!is.na(sp))
}

preprocess_data <- function(cleaned_data) {
  cleaned_data |>
    group_by(subject, trial) |>
    prior_item_analysis("value", "value") |>
    mutate(
      subject = as.character(subject),
      value = as.numeric(value),
      value_prioritem = as.numeric(value_prioritem)
    )
}

exp1 <- clean_data("data/Middlebrooks2017_exp1.csv") |>
  preprocess_data() |>
  mutate(exp = 1)

exp2 <- clean_data("data/Middlebrooks2017_exp2.csv") |>
  preprocess_data() |>
  mutate(exp = 2)

bind_rows(exp1, exp2) |>
  write.csv("output/middlebrooks2017_combined_preprocessed.csv", row.names = FALSE)
