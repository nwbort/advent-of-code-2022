#--- Script details ------------------------------------------------------------
# Creation date: 02 December 2022
# Project:       advent-of-code-2022
# Description:   day_01
# Author:        Nick Twort

library(tidyverse)
library(lubridate)
library(magrittr)
library(here)
library(janitor)

source(here("source/auth.R"))

#--- Import data ---------------------------------------------------------------

day_number <- str_remove_all(rstudioapi::getActiveDocumentContext()$path, ".*source/|\\.R") |> 
    parse_number()
prep_data(day_number)
data <- read_csv(here(glue::glue("data/raw/{day_number}.csv")))

#--- Tidy data -----------------------------------------------------------------

data <- data |> 
    mutate(group = cumsum(is.na(value)))

#--- Tasks ---------------------------------------------------------------------

top_n_cals <- function(.data, n) {
    
    .data |> 
        group_by(group) |>
        summarise(cals = sum(value, na.rm = TRUE)) |> 
        arrange(desc(cals)) |> 
        head(n) |> 
        summarise(cals = sum(cals))
    
}

#--- Solution ------------------------------------------------------------------

# Part 1
top_n_cals(data, 1)
# Part 2
top_n_cals(data, 3)
