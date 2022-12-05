#--- Script details ------------------------------------------------------------
# Creation date: 05 December 2022
# Project:       advent-of-code-2022
# Description:   day_05
# Author:        Nick Twort

library(tidyverse)
library(lubridate)
library(magrittr)
library(here)
library(janitor)

source(here("source/auth.R"))

#--- Import data ---------------------------------------------------------------

stacks <- read_lines(here(glue::glue("data/raw/{day_number}.csv")),
                     skip = 1,
                     n_max = 8)
stacks <- stacks |> 
    paste0(" ") |> 
    enframe() |> 
    rowwise() |> 
    mutate(box = list(substring(value, seq(1, 9*4-1, 4), seq(4, 9*4, 4)))) |> 
    unnest_wider(box, names_sep = "_") |> 
    mutate(across(starts_with("box"), str_squish)) |> 
    select(name, starts_with("box")) |> 
    pivot_longer(starts_with("box"), names_to = "box") |> 
    filter(value != "") |> 
    pivot_wider() |> 
    arrange(box) |> 
    rowwise() |> 
    mutate(boxy = list(c(`1`, `2`, `3`, `4`, `5`, `6`, `7`, `8`))) |> 
    ungroup() |> 
    select(boxy)
    
stacks <- as.list(stacks)$boxy
stacks <- map(stacks, na.omit)

moves <- read_lines(here(glue::glue("data/raw/{day_number}.csv")),
                    skip = 11) |> 
    enframe() |> 
    filter(!is.na(value) & value != "NA") |> 
    separate(value, into = c("item", "num_moved", "text", "stack_start", "to", "stack_end")) |> 
    mutate(across(c(num_moved, stack_start, stack_end), as.numeric))

move <- function(stacks, stack_start, stack_end, num_moved) {
    
    my_stack_start <- stacks[[stack_start]]
    my_stack_end <- stacks[[stack_end]]
    
    if ((num_moved + 1) > length(my_stack_start)) {
        new_stack_start <- character()
    } else if((num_moved + 1) == length(my_stack_start)) {
        new_stack_start <- my_stack_start[length(my_stack_start)]
    } else {
        new_stack_start <- my_stack_start[(num_moved + 1):length(my_stack_start)]
    }
    
    new_stack_end <- c(rev(my_stack_start[1:num_moved]), my_stack_end)
    
    stacks[[stack_start]] <- new_stack_start
    stacks[[stack_end]] <- new_stack_end
    
    stacks
    
}

move2 <- function(stacks, stack_start, stack_end, num_moved) {
    
    my_stack_start <- stacks[[stack_start]]
    my_stack_end <- stacks[[stack_end]]
    
    if ((num_moved + 1) > length(my_stack_start)) {
        new_stack_start <- character()
    } else if((num_moved + 1) == length(my_stack_start)) {
        new_stack_start <- my_stack_start[length(my_stack_start)]
    } else {
        new_stack_start <- my_stack_start[(num_moved + 1):length(my_stack_start)]
    }
    
    new_stack_end <- c((my_stack_start[1:num_moved]), my_stack_end)
    
    stacks[[stack_start]] <- new_stack_start
    stacks[[stack_end]] <- new_stack_end
    
    stacks
    
}

stacks_original <- stacks

for (i in 1:nrow(moves)) {
    
    stack_start <- moves$stack_start[i]
    stack_end <- moves$stack_end[i]
    num_moved <- moves$num_moved[i]
    
    stacks <- move(stacks,
                   stack_start,
                   stack_end,
                   num_moved
                   )

}

map(stacks, function(s) {s[1]}) |>
    paste0(collapse = "") |> 
    str_remove_all("\\[|\\]")

stacks <- stacks_original

for (i in 1:nrow(moves)) {
    
    stack_start <- moves$stack_start[i]
    stack_end <- moves$stack_end[i]
    num_moved <- moves$num_moved[i]
    
    stacks <- move2(stacks,
                    stack_start,
                    stack_end,
                    num_moved
    )
    
}

map(stacks, function(s) {s[1]}) |>
    paste0(collapse = "") |> 
    str_remove_all("\\[|\\]")