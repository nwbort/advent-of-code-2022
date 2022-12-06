#--- Script details ------------------------------------------------------------
# Creation date: 06 December 2022
# Project:       advent-of-code-2022
# Description:   day_06
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
    filter(!is.na(value)) |> 
    pull(value)


funs <- function(n) {
    map(1:n, function(i) { function(x) {lead(x, i)}})
}


leader <- function(n_leads) {
    
    data |> 
        str_split("") |> 
        unlist() |> 
        enframe() |> 
        mutate(across(value, .fns = funs(n_leads))) |> 
        rename(value_0 = value) |> 
        pivot_longer(cols = -name, names_to = "lag") |> 
        group_by(name) |> 
        summarise(n = n_distinct(value)) |> 
        filter(n == (n_leads + 1)) |> 
        head(1) |> 
        pull(name) |> 
        add(n_leads)
    
}

# Part 1
leader(3)

# Part 2
leader(13)
