#--- Script details ------------------------------------------------------------
# Creation date: 07 December 2022
# Project:       advent-of-code-2022
# Description:   day_07
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

#--- Go go go ------------------------------------------------------------------

data_with_dir <- data |> 
    mutate(movement = str_extract(value, "cd (.*)")) |> 
    mutate(movement = str_remove_all(movement, "cd ")) |> 
    replace_na(list(movement = "")) |> 
    mutate(movement = mcp:::cumpaste(movement, .sep = "/")) |> 
    mutate(movement = str_replace_all(movement, "/+", "/"))

while(any(str_detect(data_with_dir$movement, "\\."))) {
    
    data_with_dir <- data_with_dir |> 
        mutate(movement = str_replace_all(movement, "/[A-z]+/\\.\\.", ""))
    
}

data_with_dir <- data_with_dir |> 
    mutate(file_size = parse_number(value)) |> 
    filter(!is.na(file_size))

data_with_dir_by_subdir <- data_with_dir |> 
    group_by(movement) |> 
    summarise(file_size = sum(file_size))

go_up_one <- function(my_folder) {
    
    str_remove(my_folder, "[A-z]+/$")
    
}

get_all_folders_above <- function(my_folder) {
    
    if (str_count(my_folder, "/") > 1) {
        return(c(my_folder, get_all_folders_above(go_up_one(my_folder))))
    } else {
        return(my_folder)
    }
    
}

# Part 1
data_with_dir_by_subdir |> 
    rowwise() |> 
    mutate(folders_above = list(get_all_folders_above(movement))) |> 
    unnest(folders_above) |> 
    group_by(folders_above) |> 
    summarise(file_size = sum(file_size)) |> 
    filter(file_size < 100000) |> 
    count(wt = file_size)

# Part 2
used_space <- data_with_dir_by_subdir |> 
    rowwise() |> 
    mutate(folders_above = list(get_all_folders_above(movement))) |> 
    unnest(folders_above) |> 
    group_by(folders_above) |> 
    summarise(file_size = sum(file_size)) |> 
    filter(folders_above == "/") |> 
    pull(file_size)

data_with_dir_by_subdir |> 
    rowwise() |> 
    mutate(folders_above = list(get_all_folders_above(movement))) |> 
    unnest(folders_above) |> 
    group_by(folders_above) |> 
    summarise(file_size = sum(file_size)) |> 
    filter(file_size >= (30000000 - (70000000 - used_space))) |> 
    filter(file_size == min(file_size))
