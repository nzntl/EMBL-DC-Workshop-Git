library(tidyverse)

surveys <- read_csv("data_raw/portal_data_joined.csv")
str(surveys)

select(surveys, plot_id, species_id, weight)

select(surveys, -record_id, -species_id)

filter(surveys, year == "1995")
filter(surveys, year == "1995" & sex == "M")

surveys2 <- filter(surveys, weight <5)
surveys_sml <- select(surveys2, species_id, sex, weight)

surveys_sml2 <- select(filter(surveys, weight <5), species_id, sex, weight)

surveys %>% 
  filter(weight<5) %>% 
  select(species_id, sex, weight)

# challange 3.1
surveys %>% 
  filter (year <1995) %>% 
  select(year, sex, weight)