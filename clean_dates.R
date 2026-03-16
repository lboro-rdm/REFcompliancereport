library(tidyverse)
library(janitor)

batch <- read.csv("batch_20260309.csv") 

batch <- batch %>% 
  clean_names()

# publicationdate is the custom field, publication_date is the timeline field
batch_filtered <- batch %>%
  filter(publicationdate != "" & publication_date == "")

batch_filtered_selected <- batch_filtered %>% 
  filter(item_type == "journal contribution" | item_type == "conference contribution") %>% select(article_id, title, item_type, handle, publication_date, publicationdate)

write.csv(batch_filtered_selected, "CleanDates-JournalConf.csv")

