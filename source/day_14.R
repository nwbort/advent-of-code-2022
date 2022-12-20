#--- Script details ------------------------------------------------------------
# Creation date: 12 December 2022
# Project:       advent-of-code-2022
# Description:   day_12
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

# path <- data$value[[2]]

# points <- str_extract_all(path, "[0-9]+,[0-9]+") |> 
    # unlist()

lines <- data |> 
    rowwise() |> 
    mutate(points = str_extract_all(value, "[0-9]+,[0-9]+")
    ) |> 
    ungroup() |> 
    mutate(rn = row_number()) |> 
    select(rn, points) |> 
    unnest_longer(points) |> 
    mutate(x = str_extract(points, "^[0-9]+")) |> 
    mutate(y = str_extract(points, "[0-9]+$"))

lines <- lines |> 
    mutate(across(c(x, y), as.integer))

floor <- lines |> 
    filter(y == max(y)) |> 
    head(1) |> 
    pull(y)


offset <- 1000

my_grid <- matrix(data = TRUE, nrow = 200, ncol = 2000)

lines <- lines |> 
    (\(x) split(x, x$rn))()

for (line in lines) {
    
    cur_x <- line$x[1]
    cur_y <- line$y[1]
    
    my_grid[cur_y, cur_x] <- FALSE
    
    for (i in 2:nrow(line)) {
        
        new_x <- line$x[i]
        new_y <- line$y[i]
        
        my_grid[cur_y:new_y,(cur_x):(new_x)] <- FALSE
        
        cur_x <- new_x
        cur_y <- new_y
        
    }
    
    print("done")
    
}



drop_sand <- function(grid) {
    
    falling <- TRUE
    sand_x <- 500
    sand_y <- 0
    
    
    while (falling) {
        
        if (grid[sand_y+1,sand_x]) {
            sand_y <- sand_y+1
        } else if (grid[sand_y+1,sand_x-1]) {
            sand_y <- sand_y+1
            sand_x <- sand_x-1
        } else if (grid[sand_y+1,sand_x+1]) {
            sand_y <- sand_y+1
            sand_x <- sand_x+1
        } else {
            falling <- FALSE
            if (sand_x == 500 & sand_y == 0) {
                return("STOP")
            }
            grid[sand_y,sand_x] <- FALSE
        }
        
        # if (sand_y > floor) {
        #     return("STOP!")
        # }
        # 
    }
    
    return(grid)
    
}


my_grid[floor+2, 1:ncol(my_grid)] <- FALSE

okay <- TRUE
i <- 0

while (okay) {
    
    my_grid <- drop_sand(my_grid)
    i <- i+1
    if (any(my_grid == "STOP")) {
        print(i)
        okay <- FALSE
    }
    
    if ((i %% 10) == 0) {
        print(i)
    }
    
}

