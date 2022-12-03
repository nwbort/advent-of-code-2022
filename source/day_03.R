#--- Script details ------------------------------------------------------------
# Creation date: 03 December 2022
# Project:       advent-of-code-2022
# Description:   day_03
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

data <- data |> filter(!is.na(value))

#--- Go go go ------------------------------------------------------------------

priorities <- 1:52
names(priorities) <- c(letters, LETTERS)

# Part 1
data |> 
    mutate(len = nchar(value)) |> 
    mutate(lhs = str_sub(value, 1, len / 2),
           rhs = str_sub(value, len / 2 + 1)) |> 
    mutate(across(c(lhs, rhs), str_split, "")) |> 
    rowwise() |> 
    mutate(common = intersect(lhs, rhs)) |> 
    ungroup() |> 
    mutate(score = priorities[common]) |> 
    count(wt = score)

# Part 2
data |> 
    mutate(group = (row_number() - 1) %/% 3) |> 
    group_by(group) |> 
    mutate(id = row_number()) |> 
    ungroup() |> 
    pivot_wider(names_from = id, values_from = value, names_prefix = "elf_") |> 
    mutate(across(starts_with("elf"), str_split, "")) |> 
    rowwise() |> 
    mutate(common = reduce(list(elf_1, elf_2, elf_3), intersect)) |> 
    ungroup() |> 
    mutate(score = priorities[common]) |> 
    count(wt = score)
