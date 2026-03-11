library(tidyverse)
library(janitor)

batch <- read.csv("batch_20260309.csv") 

batch <- batch %>% 
  clean_names()

batch_filtered <- batch %>%
  filter(publicationdate != "" & publication_date == "")

batch_filtered_itemtype <- batch_filtered %>% 
  filter(item_type == "journal contribution" | item_type == "conference contribution")
