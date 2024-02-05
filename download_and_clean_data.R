library(rvest)
library(xml2)
library(tidyverse)
library(dplyr)
library(janitor)
library(knitr)

raw_data <-
  read_html(
    "https://en.wikipedia.org/wiki/List_of_prime_ministers_of_New_Zealand"
  )
write_html(raw_data, "pms.html")

parse_data_selector_gadget <-
  raw_data |>
  html_element(".wikitable") |>
  html_table()

parsed_data <-
  parse_data_selector_gadget |> 
  clean_names() |> 
  rename(raw_text = name_constituency_birth_death) |> 
  select(raw_text) |> 
  filter(raw_text != "Name(Birth–Death)Constituency") |> 
  distinct() 

parsed_data <- parsed_data[-1, ] # Deleting first row

initial_clean <-
  parsed_data |>
  separate(
    raw_text, into = c("name", "not_name"), sep = "\\,", extra = "merge",
  ) |> 
  mutate(date = str_extract(not_name, "[[:digit:]]{4}–[[:digit:]]{4}"),
         born = str_extract(not_name, "born[[:space:]][[:digit:]]{4}")
  ) |>
  select(name, date, born)

cleaned_data <-
  initial_clean |>
  separate(date, into = c("birth", "died"), 
           sep = "–") |>
  mutate(
    born = str_remove_all(born, "born[[:space:]]"),
    birth = if_else(!is.na(born), born, birth)
  ) |>
  select(-born) |>
  rename(born = birth) |> 
  mutate(across(c(born, died), as.integer)) |> 
  mutate(Age_at_Death = died - born) |> 
  distinct()

cleaned_data |>
  head() |>
  kable(
    col.names = c("Prime Minister", "Birth year", "Death year", "Age at death")
  )

write_csv(cleaned_data, "cleaned_data.csv")