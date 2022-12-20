#--- Script details ------------------------------------------------------------
# Creation date: 18 December 2022
# Project:       advent-of-code-2022
# Description:   day_18
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

resp <- GET(
    url = glue::glue("https://adventofcode.com/2022/day/{day_number}/input"),
    add_headers("cookie" = cookie)
)

data <- str_split(content(resp, encoding = "UTF-8"), "\n") |> 
    unlist() |> 
    as_tibble() |>
    mutate(value = ifelse(value == "", NA_character_, value)) |> 
    filter(!is.na(value))

# data <- tibble(
#     value = c(
#         "2,2,2",
#         "1,2,2",
#         "3,2,2",
#         "2,1,2",
#         "2,3,2",
#         "2,2,1",
#         "2,2,3",
#         "2,2,4",
#         "2,2,6",
#         "1,2,5",
#         "3,2,5",
#         "2,1,5",
#         "2,3,5"
#     )
# )


data <- data |> 
    separate(value, into = c("x", "y", "z"), sep = ",") |> 
    mutate(across(c(x, y, z), as.integer))

check_adjacent <- function(c1x, c1y, c1z, c2x, c2y, c2z) {
    
    
    
    return(
        c1x == c2x & c1y == c2y & ((c1z == c2z - 1) | (c1z == c2z + 1)) |
            c1x == c2x & c1z == c2z & ((c1y == c2y - 1) | (c1y == c2y + 1)) |
            c1z == c2z & c1y == c2y & ((c1x == c2x - 1) | (c1x == c2x + 1))
    )
    
}

data <- data |> 
    mutate(dummy = TRUE)


d_full <- inner_join(data, data, by = "dummy")

d_full <- d_full |> 
    mutate(match = check_adjacent(x.x, y.x, z.x, x.y, y.y, z.y)) |> 
    group_by(x.x, y.x, z.x) |> 
    summarise(faces = sum(match), .groups = "drop")

faces <- d_full |> 
    summarise(f = sum(6 - faces)) |> 
    pull(f)

# Part 1
faces


rrr <- min(data$x, data$y, data$z):max(data$x, data$y, data$z)

all_cubes <- expand_grid(x = rrr, y = rrr, z = rrr)

holes <- all_cubes |> 
    anti_join(data, by = c("x", "y", "z"))

holes <- holes |> 
    mutate(can_get_out = pmin(x, y, z) == 0) |> 
    mutate(dummy = TRUE) |> 
    mutate(key = paste(x,y,z))

#--- Figure out which holes have a path to the fresh air
h_adj <- inner_join(holes |> filter(!can_get_out), holes |> filter(can_get_out), by = "dummy")

h_adj <- h_adj |> 
    mutate(match = check_adjacent(x.x, y.x, z.x, x.y, y.y, z.y)) |> 
    filter(match) |> 
    select(key = key.x, can_get_out = can_get_out.y) |> 
    distinct()

while (nrow(h_adj) > 0) {
    holes <- holes |> 
        rows_update(h_adj, by = c("key"))
    
    h_adj <- inner_join(holes |> filter(!can_get_out), holes |> filter(can_get_out), by = "dummy")
    
    h_adj <- h_adj |> 
        mutate(match = check_adjacent(x.x, y.x, z.x, x.y, y.y, z.y)) |> 
        filter(match) |> 
        select(key = key.x, can_get_out = can_get_out.y) |> 
        distinct()
}

cant_escape <- holes |> 
    filter(!can_get_out)

d_full <- inner_join(data, cant_escape, by = "dummy")

# Remove faces that are adjacent to internal air pockets
faces_to_remove <- d_full |> 
    mutate(match = check_adjacent(x.x, y.x, z.x, x.y, y.y, z.y)) |> 
    filter(match) |> 
    nrow()

# Part 2
faces - faces_to_remove
