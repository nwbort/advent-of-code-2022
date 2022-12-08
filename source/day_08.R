#--- Script details ------------------------------------------------------------
# Creation date: 08 December 2022
# Project:       advent-of-code-2022
# Description:   day_08
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

data <- data |> 
    filter(!is.na(data)) |> 
    separate(value, into = paste0("var", 1:100), "") |> 
    mutate(across(.fns = as.numeric)) |> 
    select(-var1) |>
    as.matrix()

# Part 1
visible <- matrix(FALSE, nrow = nrow(data), ncol = ncol(data))

visible[1, ] <- TRUE
visible[nrow(visible), ] <- TRUE
visible[, ncol(visible)] <- TRUE
visible[, 1] <- TRUE

for (i in 2:(nrow(visible) - 1)) {
    for (j in 2:(ncol(visible) - 1)) {
        
        visible[i, j] = all(data[1:(i-1), j] < data[i, j]) ||
            all(data[(i+1):nrow(visible), j] < data[i, j]) ||
            all(data[i, 1:(j-1)] < data[i, j]) ||
            all(data[i, (j+1):ncol(visible)] < data[i, j])
        
    }
}


# Part 2
best_score <- 0

for (i in 2:(nrow(visible) - 1)) {
    for (j in 2:(ncol(visible) - 1)) {
        
        found_up <- 0
        u <- i - 1
        blocked <- FALSE
        while (!blocked & u >= 1) {
            found_up <- found_up + 1
            if (data[u, j] < data[i, j]) {
                
            } else {
                blocked <- TRUE
            }
            u <- u - 1
        }
        
        found_down <- 0
        d <- i + 1
        blocked <- FALSE
        while (!blocked & d <= nrow(data)) {
            found_down <- found_down + 1
            if (data[d, j] < data[i, j]) {
                
            } else {
                blocked <- TRUE
            }
            d <- d + 1
        }
        
        found_left <- 0
        l <- j - 1
        blocked <- FALSE
        while (!blocked & l >= 1) {
            found_left <- found_left + 1
            if (data[i, l] < data[i, j]) {
                
            } else {
                blocked <- TRUE
            }
            l <- l - 1
        }
        
        found_right <- 0
        r <- j + 1
        blocked <- FALSE
        while (!blocked & r <= ncol(data)) {
            found_right <- found_right + 1
            if (data[i, r] < data[i, j]) {
                
            } else {
                blocked <- TRUE
            }
            r <- r + 1
        }
        
        newscore <- found_right * found_left * found_down * found_up
        
        if (newscore > best_score) {
            best_score <- newscore
        }
        
    }
}

best_score
