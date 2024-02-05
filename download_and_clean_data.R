library(rvest)
library(xml2)
library(tidyverse)
library(dplyr)
library(janitor)
library(knitr)

raw_data <-
  read_html(
    "https://en.wikipedia.org/wiki/List_of_prime_ministers_of_India"
  )
write_html(raw_data, "pms.html")

parse_data_selector_gadget <-
  raw_data |>
  html_element(".wikitable") |>
  html_table()

parsed_data <-
  parse_data_selector_gadget |> 
  clean_names() |> 
  rename(raw_text = name_born_died_constituency) |> 
  select(raw_text) |> 
  filter(raw_text != "Name(born – died)Constituency") |> 
  distinct() 

initial_clean <-
  parsed_data |>
  separate(
    raw_text, into = c("name", "not_name"), sep = "\\(", extra = "merge",
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
  mutate("Age at Death" = died - born) |> 
  distinct()

write_csv(cleaned_data, "cleaned_data.csv")

cleaned_data |>
  mutate(
    still_alive = if_else(is.na(died), "Yes", "No"),
    died = if_else(is.na(died), as.integer(2023), died)
  ) |>
  mutate("name" = as_factor(name)) |>
  ggplot(
    aes(x = born, xend = died, y = name, yend = name, color = still_alive)
  ) +
  geom_segment() +
  labs(
    x = "Year of birth", y = "Prime minister", color = "PM is currently alive"
  ) +
  theme_minimal() +
  scale_color_brewer(palette = "Set1") +
  theme(legend.position = "bottom")


