library(tidyverse)
library(janitor)

all_data <- read.csv("batch.csv")

all_data <- all_data %>% 
  clean_names() %>% 
  mutate(embargo_date = as.Date(embargo_date))

embargoed_data <- all_data %>%
  filter(
    is_embargoed == 1,
    is.na(embargo_date) | embargo_date > today(),
    item_type %in% c(
      "journal contribution",
      "conference contribution"
    )
  ) %>% 
  arrange(embargo_date)

filtered_data <- embargoed_data %>% 
  select(article_id, title, authors, item_type, handle, embargo_reason, embargo_date, acceptance_date, publication_date, first_online_date, version) %>% 
  mutate(
    months_to_embargo = case_when(
      !is.na(embargo_date) ~ interval(today(), embargo_date) %/% months(1),
      TRUE ~ NA_integer_
    )
  )

filename <- paste0("REFReport_", today(), ".csv")

write.csv(filtered_data, filename, row.names = FALSE)