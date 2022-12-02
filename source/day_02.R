#--- Script details ------------------------------------------------------------
# Creation date: 02 December 2022
# Project:       advent-of-code-2022
# Description:   day_02
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
    separate(value, into = c("them", "me"))

#--- Tasks ---------------------------------------------------------------------

rounds <- data |> 
    count(them, me) |> 
    filter(!is.na(them))

#--- Solution ------------------------------------------------------------------

# Part 1
rounds |> 
    mutate(score = case_when(
        me == "X" ~ 1,
        me == "Y" ~ 2,
        me == "Z" ~ 3
    ),
    score = score + case_when(
        
        (me == "X" & them == "C") | (me == "Y" & them == "A") | (me == "Z" & them == "B") ~ 6,
        (me == "X" & them == "A") | (me == "Y" & them == "B") | (me == "Z" & them == "C") ~ 3,
        TRUE ~ 0
        
    )
    ) |> 
    mutate(score = score * n) |> 
    summarise(score = sum(score, na.rm = TRUE))

# Part 2
rounds |> 
    mutate(win = case_when(
        them == "A" ~ "Y",
        them == "B" ~ "Z",
        them == "C" ~ "X"
    )) |>
    mutate(lose = case_when(
        them == "A" ~ "Z",
        them == "B" ~ "X",
        them == "C" ~ "Y"
    )) |>
    mutate(draw = case_when(
        them == "A" ~ "X",
        them == "B" ~ "Y",
        them == "C" ~ "Z"
    )) |>
    mutate(me = case_when(
        me == "X" ~ lose,
        me == "Y" ~ draw,
        me == "Z" ~ win
    )) |> 
    mutate(score = case_when(
        me == "X" ~ 1,
        me == "Y" ~ 2,
        me == "Z" ~ 3
    ),
    score = score + case_when(
        
        (me == "X" & them == "C") | (me == "Y" & them == "A") | (me == "Z" & them == "B") ~ 6,
        (me == "X" & them == "A") | (me == "Y" & them == "B") | (me == "Z" & them == "C") ~ 3,
        TRUE ~ 0
        
    )
    ) |> 
    mutate(score = score * n) |> 
    summarise(score = sum(score, na.rm = TRUE))
