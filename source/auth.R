library(httr)

cookie <- Sys.getenv("AOC_COOKIE")

prep_data <- function(day_number) {
    
    if (!file.exists(here(glue::glue("data/raw/{day_number}.csv")))) {
        resp <- GET(
            url = glue::glue("https://adventofcode.com/2022/day/{day_number}/input"),
            add_headers("cookie" = cookie)
        )
        
        raw_data <- str_split(content(resp, encoding = "UTF-8"), "\n") |> 
            unlist() |> 
            as_tibble() |>
            mutate(value = ifelse(value == "", NA_character_, value))
        
        write_csv(raw_data, here(glue::glue("data/raw/{day_number}.csv")))
        
    }
}
