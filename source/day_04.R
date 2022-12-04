#--- Script details ------------------------------------------------------------
# Creation date: 04 December 2022
# Project:       advent-of-code-2022
# Description:   day_04
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

data <- data |> 
    separate(value, c("l", "r"), ",") |> 
    separate(l, c("ll", "lr"), "-") |> 
    separate(r, c("rl", "rr"), "-") |> 
    mutate(across(.fns = as.numeric))

# Part 1
data |> 
    mutate(ch = (ll <= rl & lr >= rr) | (rl <= ll & rr >= lr)) |> 
    count(wt = ch)

# Part 2
data |> 
    mutate(ch = (ll <= rl & lr >= rl) | (rl <= ll & rr >= ll)) |> 
    count(wt = ch)
