#--- Script details ------------------------------------------------------------
# Creation date: 10 December 2022
# Project:       advent-of-code-2022
# Description:   day_10
# Author:        Nick Twort

library(tidyverse)
library(lubridate)
library(magrittr)
library(here)
library(janitor)

source(here("source/auth.R"))

#--- Import data ---------------------------------------------------------------

day_number <- rstudioapi::getActiveDocumentContext()$path |> 
    str_remove_all(".*source/|\\.R") |> 
    parse_number()

prep_data(day_number)

data <- read_csv(here(glue::glue("data/raw/{day_number}.csv")))

data <- data |> 
    filter(!is.na(value))

#--- Go go go ------------------------------------------------------------------

ch <- data |> 
    mutate(amount = parse_number(value)) |> 
    replace_na(list(amount = 0)) |> 
    mutate(length = ifelse(value == "noop", 1, 2)) |> 
    mutate(start_cycle = cumsum(length) - 1) |> 
    mutate(end_cycle = start_cycle + length) |> 
    select(end_cycle, amount) 

# Part 1
tibble(end_cycle = 1:1000) |> 
    left_join(ch, by = "end_cycle") |> 
    replace_na(list(amount = 0)) |> 
    group_by(end_cycle) |> 
    summarise(amount = sum(amount)) |> 
    mutate(amount = amount + ifelse(end_cycle == 1, 1, 0)) |> 
    mutate(tot = cumsum(amount)) |> 
    mutate(sig_strength = tot * end_cycle) |> 
    filter(row_number() %in% seq.int(20, 220, 40)) |> 
    summarise(sig_strength = sum(sig_strength))

poss <- tibble(end_cycle = 1:1000) |> 
    left_join(ch, by = "end_cycle") |> 
    replace_na(list(amount = 0)) |> 
    group_by(end_cycle) |> 
    summarise(amount = sum(amount)) |> 
    mutate(amount = amount + ifelse(end_cycle == 1, 1, 0)) |> 
    mutate(tot = cumsum(amount))

# Part 2
poss |> 
    mutate(pixel_pos = end_cycle - 1) |> 
    mutate(across(tot, as.integer)) |> 
    mutate(pp_row = pixel_pos %% 40) |> 
    mutate(draw = ifelse((pp_row == tot | pp_row == (tot - 1) | pp_row == (tot + 1)), "#", ".")) |> 
    mutate(rn = pixel_pos %/% 40) |> 
    group_by(rn) |> 
    summarise(draw = paste0(draw, collapse = ""))
