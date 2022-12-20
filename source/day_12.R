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

lines <- readLines(here(glue::glue("data/raw/{day_number}.csv")), n = 42)
lines <- lines[2:length(lines)]

dd <- lines |> 
    str_split("") |> 
    unlist() |> 
    matrix(nrow = length(lines), ncol = nchar(lines[1]), byrow = TRUE)

# nodes <- nrow(dd) * ncol(dd)
# visited <- which(dd == "S")

move_possible <- function(a, b) {
    
    which(letters == a) + 1 >= which(letters == b)
    
}

ee <- dd

ee[which(ee == "S")] <- "a"
ee[which(ee == "E")] <- "z"

library(igraph)
library(tidygraph)

valid_moves <- tibble()

for (i in 1:nrow(dd)) {
    for (j in 1:ncol(dd)) {
        
        idx <- (i - 1) * ncol(dd) + j
        
        if (i+1 <= nrow(dd) && move_possible(ee[i, j], ee[i+1, j])) {
            valid_moves <- bind_rows(valid_moves, tibble(from = idx, to = idx + ncol(dd)))
        }
        
        if (i-1 >= 1 && move_possible(ee[i, j], ee[i-1, j])) {
            valid_moves <- bind_rows(valid_moves, tibble(from = idx, to = idx - ncol(dd)))
        }
        
        if (j+1 <= ncol(dd) && move_possible(ee[i, j], ee[i, j+1])) {
            valid_moves <- bind_rows(valid_moves, tibble(from = idx, to = idx + 1))
        }
        
        if (j-1 >= 1 && move_possible(ee[i, j], ee[i, j-1])) {
            valid_moves <- bind_rows(valid_moves, tibble(from = idx, to = idx - 1))
        }
        
    }
}

g <- as_tbl_graph(valid_moves, directed = TRUE)

st <- which(dd == "S", arr.ind = TRUE)

st <- (st[[1]] - 1) * ncol(dd) + (st[[2]])

en <- which(dd == "E", arr.ind = TRUE)

en <- (en[[1]] - 1) * ncol(dd) + (en[[2]])


sp <- shortest_paths(g, from = st, to = en)

sp$vpath[[1]] |> length() |> add(-1)


start_nodes <- which(ee == "a", arr.ind = TRUE)
# get_index <- function(a) {
#     
#     (a[[1]] - 1) * ncol(dd) + a[[2]]
#     
# }

index_vals <- start_nodes |> 
    as_tibble() |> 
    mutate(index = (row - 1) * ncol(dd) + col) |> 
    pull(index)

# map_dbl(index_vals, function(v) {
#     
#     shortest_paths(g, from = v, to = en)
#     
#     
#     
# }) |> 
#     min() |> 
#     add(-1)

# map(start_nodes, get_index)

nodes <- g |> pull(name)

ppp <- tibble()

for (node in nodes) {
    
    sp <- shortest_paths(g, from = node, to = en)
    lll <- length(sp$vpath[[1]]) - 1
    ppp <- bind_rows(ppp, tibble(node = node, length = lll))
    
}

ppp |> 
    filter(node %in% index_vals) |> 
    filter(length > 0) |> 
    pull(length) |> 
    range()

# best <- 9999
# 
# for (v in index_vals) {
#     
#     sp <- shortest_paths(g, from = v, to = en)
#     lll <- sp$vpath[[1]] |> length()
#     
#     if (lll < best & lll > 0) {
#         best <- lll
#         print(v)
#         print(best)
#     }
#     
# }
# best