library(tidyverse)
surveys <- read_csv("data_raw/portal_data_joined.csv")
str(surveys)
select(surveys)
s <- <- <- <- <- <- <- 
  surveys_long %>% 
    group_by(year, measurement, plot_type) %>% 
    summarise(mean_value = mean (value, na.rm = T)) %>%
    pivot_wider(names_from = measurement, values_from = mean_value)
  
surveys_complete <- surveys %>% 
  filter(!is.na(weight), 
         !is.na(hindfoot_length), 
         !is.na(sex))

write_csv(surveys_complete, file = "surveys_complete.csv")



  
  
  
  
  
  
  